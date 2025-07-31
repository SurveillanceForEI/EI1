# app.R
Sys.setenv(http_proxy = "http://proxy.nih.go.jp:8080",
           https_proxy = "http://proxy.nih.go.jp:8080")
Sys.setlocale("LC_TIME", "C")

library(shiny)
library(shinythemes)  
library(dplyr)
library(readr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(here)
library(stringr)


#### UI ####
ui <- fluidPage(
  theme = shinytheme("flatly"),  # 👈 テーマを適用
  titlePanel("📰 Google RSS ニュース記事（キーワード×週別）"),
  sidebarLayout(
    sidebarPanel(
      helpText(),
      checkboxGroupInput("selected_keywords", "表示するキーワード：",
                         choices = c("急性呼吸器感染症", "インフルエンザ", "新型コロナ", "RSウイルス", "感染症"),
                         selected = c("急性呼吸器感染症", "インフルエンザ", "新型コロナ", "RSウイルス", "感染症"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("📊 流行曲線グラフ", br(), plotOutput("trendPlot")),
        tabPanel("📋 集計テーブル", br(), dataTableOutput("summaryTable"))
      )
    )
  )
)

#### SERVER ####
server <- function(input, output, session) {
  
  load_rss_data <- reactive({
    files <- list.files(here("results"), pattern = "^google_rss_epi_\\d{4}w\\d{2}\\.csv$", full.names = TRUE)
    
    rss_all <- map_dfr(files, function(path) {
      df <- read_csv(path, locale = locale(encoding = "CP932"), show_col_types = FALSE)
      if (!"posted_date" %in% colnames(df)) return(NULL)
      df %>%
        mutate(epiyear = isoyear(posted_date),
               epiweek = isoweek(posted_date),
               epi_yearweek = sprintf("%dw%02d", epiyear, epiweek))
    })
    
    rss_all
  })
  
  summary_data <- reactive({
    df <- load_rss_data()
    df %>%
      filter(!is.na(keyword), !is.na(epi_yearweek)) %>%
      count(keyword, epi_yearweek, name = "n_articles") %>%
      complete(keyword, epi_yearweek, fill = list(n_articles = 0)) %>%
      arrange(keyword, epi_yearweek)
  })
  
  output$summaryTable <- renderDataTable({
    summary_data() %>%
      filter(keyword %in% input$selected_keywords)
  })
  
  output$trendPlot <- renderPlot({
    df <- summary_data() %>%
      filter(keyword %in% input$selected_keywords)
    
    ggplot(df, aes(x = epi_yearweek, y = n_articles, group = keyword, color = keyword)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(title = "キーワード別・週別ニュース記事数（Google RSS）",
           x = "疫学週",
           y = "記事数",
           color = "キーワード") +
      theme_minimal(base_family = "sans") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(size = 18, face = "bold"))
  })
}

#### 実行 ####
shinyApp(ui = ui, server = server)
