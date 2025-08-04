library(shiny)
library(shinythemes)  
library(dplyr)
library(readr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(stringr)
library(purrr)
library(DT)
library(plotly)

setwd(dirname(normalizePath(sys.frame(1)$ofile %||% getwd())))

#### UI ####
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("📰 Google RSS ニュース記事（国内ニュース・重複除外）"),
  sidebarLayout(
    sidebarPanel(
      width = 3,  
      checkboxGroupInput("selected_keywords", "表示するキーワード：",
                         choices = c("急性呼吸器感染症", "インフルエンザ", "新型コロナ", "RSウイルス", "感染症"),
                         selected = c("急性呼吸器感染症", "インフルエンザ", "新型コロナ", "RSウイルス", "感染症")),
      dateRangeInput("date_range_daily", "日別流行曲線の表示期間:",
                     start = Sys.Date() - 29,
                     end = Sys.Date(),
                     min = Sys.Date() - 365,
                     max = Sys.Date(),
                     format = "yyyy-mm-dd",
                     separator = " ～ ")
    ),
    mainPanel(
      width = 9,  
      tabsetPanel(
        tabPanel("📊 週別流行曲線（直近1年）", br(), plotlyOutput("trendPlot")),
        tabPanel("📋 集計テーブル（直近1年）", br(), DTOutput("summaryTable")),
        tabPanel("📈 日別流行曲線（期間指定）", br(), plotlyOutput("trendDailyPlot")),
        tabPanel("📰 過去7日間のニュース", br(), DTOutput("recentNewsTable")),
        tabPanel("🌐 Google Trends（過去12か月）", br(), plotlyOutput("trendGooglePlot"))
      )
    )
  )
)

#### SERVER ####
server <- function(input, output, session) {
  
  # 都道府県リスト
  pref_names <- c("北海道","青森","岩手","宮城","秋田","山形","福島",
                  "茨城","栃木","群馬","埼玉","千葉","東京","神奈川",
                  "新潟","富山","石川","福井","山梨","長野",
                  "岐阜","静岡","愛知","三重",
                  "滋賀","京都","大阪","兵庫","奈良","和歌山",
                  "鳥取","島根","岡山","広島","山口",
                  "徳島","香川","愛媛","高知",
                  "福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄")
  
  # ---- 週別集計用 ----
  load_weekly_rss_data <- reactive({
    files_weekly <- list.files("results", pattern = "^google_rss_epi_\\d{4}w\\d{2}\\.csv$", full.names = TRUE)
    
    rss_week <- map_dfr(files_weekly, function(path) {
      df <- read_csv(path, locale = locale(encoding = "CP932"), show_col_types = FALSE)
      if (!"posted_date" %in% colnames(df)) return(NULL)
      df %>%
        mutate(
          posted_date = as.Date(posted_date),
          epiyear = isoyear(posted_date),
          epiweek = isoweek(posted_date),
          epi_yearweek = sprintf("%dw%02d", epiyear, epiweek)
        )
    })
    
    # 国内ニュースのみ
    is_domestic <- str_detect(rss_week$title, paste(pref_names, collapse = "|")) |
      str_detect(rss_week$snippet, paste(pref_names, collapse = "|")) |
      str_detect(rss_week$link, "\\.jp")
    rss_week <- rss_week[is_domestic, ]
    
    # タイトル冒頭10文字で重複排除
    rss_week %>%
      mutate(title_head10 = str_sub(title, 1, 10)) %>%
      distinct(keyword, title_head10, .keep_all = TRUE)
  })
  
  # 週別集計（直近1年、欠損週補完）
  summary_data <- reactive({
    df <- load_weekly_rss_data()
    
    start_date <- Sys.Date() - 365
    end_date <- Sys.Date()
    df <- df %>% filter(posted_date >= start_date & posted_date <= end_date)
    
    if (nrow(df) == 0) {
      return(tibble(keyword = character(), epi_yearweek = character(), n_articles = integer()))
    }
    
    all_weeks <- df %>%
      select(epi_yearweek, epiyear, epiweek) %>%
      distinct() %>%
      arrange(epiyear, epiweek) %>%
      pull(epi_yearweek)
    
    df %>%
      count(keyword, epi_yearweek, name = "n_articles") %>%
      complete(keyword, epi_yearweek = all_weeks, fill = list(n_articles = 0)) %>%
      arrange(keyword, epi_yearweek)
  })
  
  # ---- 日別集計（スライダー）----
  summary_daily_data <- reactive({
    files_all <- list.files("results", pattern = "^google_rss_all_\\d{4}-\\d{2}-\\d{2}\\.csv$", full.names = TRUE)
    file_dates <- as.Date(str_extract(basename(files_all), "\\d{4}-\\d{2}-\\d{2}"))
    
    req(input$date_range_daily)
    start_date <- input$date_range_daily[1]
    end_date <- input$date_range_daily[2]
    date_seq <- seq(start_date, end_date, by = "day")
    
    df_list <- map(date_seq, function(day_date) {
      target_file_date <- day_date + 1
      if (!(target_file_date %in% file_dates)) {
        tibble(keyword = input$selected_keywords, posted_date = day_date, n_articles = 0)
      } else {
        file_path <- files_all[file_dates == target_file_date]
        df <- read_csv(file_path, locale = locale(encoding = "CP932"), show_col_types = FALSE) %>%
          mutate(posted_date = as.Date(posted_date))
        
        is_domestic <- str_detect(df$title, paste(pref_names, collapse = "|")) |
          str_detect(df$snippet, paste(pref_names, collapse = "|")) |
          str_detect(df$link, "\\.jp")
        df <- df[is_domestic, ]
        
        df <- df %>%
          mutate(title_head10 = str_sub(title, 1, 10)) %>%
          distinct(keyword, title_head10, .keep_all = TRUE)
        
        df_day <- df %>%
          filter(posted_date == day_date) %>%
          count(keyword, posted_date, name = "n_articles")
        
        if (nrow(df_day) == 0) {
          tibble(keyword = input$selected_keywords, posted_date = day_date, n_articles = 0)
        } else {
          df_day
        }
      }
    })
    
    bind_rows(df_list)
  })
  
  # ---- 集計テーブル ----
  output$summaryTable <- renderDT({
    summary_data() %>%
      filter(keyword %in% input$selected_keywords)
  }, options = list(pageLength = 10))
  
  # ---- 週別流行曲線（plotly, 凡例重複解消）----
  output$trendPlot <- renderPlotly({
    df <- summary_data() %>%
      filter(keyword %in% input$selected_keywords)
    
    p <- ggplot(df, aes(
      x = epi_yearweek, 
      y = n_articles, 
      group = keyword, 
      color = keyword,
      text = paste0(
        "疫学週: ", epi_yearweek, "<br>",
        "キーワード: ", keyword, "<br>",
        "記事数: ", n_articles
      )
    )) +
      geom_line(size = 1) +
      geom_point(size = 2, show.legend = FALSE) +
      labs(title = "キーワード別・週別ニュース記事数（直近1年・国内ニュース・重複除外）",
           x = "疫学週", y = "記事数", color = "キーワード") +
      theme_minimal(base_family = "sans") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(size = 16, face = "bold"))
    
    ggplotly(p, tooltip = "text") %>% layout(legend = list(title = list(text = "キーワード")))
  })
  
  # ---- 日別流行曲線（plotly, 凡例重複解消）----
  output$trendDailyPlot <- renderPlotly({
    df <- summary_daily_data() %>%
      filter(keyword %in% input$selected_keywords)
    
    # ホバー情報に日付・件数を追加
    p <- ggplot(df, aes(
      x = posted_date, 
      y = n_articles, 
      group = keyword, 
      color = keyword,
      text = paste0(
        "日付: ", posted_date, "<br>",
        "キーワード: ", keyword, "<br>",
        "記事数: ", n_articles
      )
    )) +
      geom_line(size = 1) +
      geom_point(size = 2, show.legend = FALSE) +
      labs(title = "キーワード別・日別ニュース件数（期間指定・国内ニュース・重複除外）",
           x = "日付", y = "記事数", color = "キーワード") +
      scale_x_date(date_breaks = "3 day", date_labels = "%m/%d") +
      theme_minimal(base_family = "sans") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(size = 16, face = "bold"))
    
    ggplotly(p, tooltip = "text") %>% layout(legend = list(title = list(text = "キーワード")))
  })
  
  # ---- 過去7日間ニュース ----
  output$recentNewsTable <- renderDT({
    start_date <- Sys.Date() - 6
    
    files_all <- list.files("results", pattern = "^google_rss_all_\\d{4}-\\d{2}-\\d{2}\\.csv$", full.names = TRUE)
    latest_file <- files_all[which.max(file.info(files_all)$mtime)]
    
    df <- read_csv(latest_file, locale = locale(encoding = "CP932"), show_col_types = FALSE) %>%
      mutate(posted_date = as.Date(posted_date)) %>%
      filter(posted_date >= start_date) %>%
      mutate(title_head10 = str_sub(title, 1, 10)) %>%
      distinct(keyword, title_head10, .keep_all = TRUE) %>%
      filter(str_detect(title, paste(pref_names, collapse = "|")) |
               str_detect(snippet, paste(pref_names, collapse = "|")) |
               str_detect(link, "\\.jp")) %>%
      arrange(desc(posted_date)) %>%
      mutate(Link = paste0("<a href='", link, "' target='_blank'>", title, "</a>")) %>%
      select(keyword, posted_date, Link)
    
    if (nrow(df) == 0) return(data.frame(メッセージ = "直近7日間の国内ニュースはありません。"))
    
    datatable(df, escape = FALSE, rownames = FALSE, options = list(pageLength = 10))
  })
  
  # ---- Google Trends 過去12か月（plotly, 凡例重複解消）----
  trends_data <- reactive({
    files_trend <- list.files("results", pattern = "^google_trends_12months_\\d{4}-\\d{2}-\\d{2}\\.csv$", full.names = TRUE)
    if (length(files_trend) == 0) return(NULL)
    
    latest_file <- files_trend[which.max(file.info(files_trend)$mtime)]
    
    read.csv(latest_file, fileEncoding = "CP932", row.names = NULL) %>%
      as_tibble() %>%
      mutate(
        date = as.Date(date),
        hits = as.numeric(hits)
      ) %>%
      filter(keyword %in% input$selected_keywords)
  })
  
  output$trendGooglePlot <- renderPlotly({
    df <- trends_data()
    req(!is.null(df), nrow(df) > 0)
    
    p <- ggplot(df, aes(
      x = date, 
      y = hits, 
      group = keyword, 
      color = keyword,
      text = paste0(
        "日付: ", date, "<br>",
        "キーワード: ", keyword, "<br>",
        "検索人気度: ", hits
      )
    )) +
      geom_line(size = 1) +
      geom_point(size = 1.5, show.legend = FALSE) +
      labs(title = "Google Trends 過去12か月（日本）",
           x = "日付", y = "検索人気度 (0-100)", color = "キーワード") +
      scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
      theme_minimal(base_family = "sans") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(size = 16, face = "bold"))
    
    ggplotly(p, tooltip = "text") %>% layout(legend = list(title = list(text = "キーワード")))
  })
}

#### 実行 ####
shinyApp(ui = ui, server = server)
