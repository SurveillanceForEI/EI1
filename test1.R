#### プロキシ設定（必要に応じて）####
Sys.setenv(http_proxy = "http://proxy.nih.go.jp:8080",
           https_proxy = "http://proxy.nih.go.jp:8080")

cat(Sys.time(), "開始\n", file = "test1.log", append = TRUE)

Sys.setlocale("LC_TIME", "C")
#### ライブラリ読み込み ####
library(xml2)
library(dplyr)
library(purrr)
library(lubridate)
library(stringr)
library(tibble)


setwd("C:/Users/kobayashi/Documents/R/EI1")

#### 検索キーワード ####
keywords <- c("急性呼吸器感染症", "インフルエンザ", "新型コロナ", "RSウイルス")

#### GoogleニュースRSS取得関数（posted_date対応）####
get_google_news_rss <- function(keyword) {
  feed_url <- paste0(
    "https://news.google.com/rss/search?q=", URLencode(keyword),
    "&hl=ja&gl=JP&ceid=JP:ja"
  )
  
  res <- tryCatch(read_xml(feed_url), error = function(e) return(NULL))
  if (is.null(res)) return(NULL)
  
  items <- xml_find_all(res, "//item")
  
  map_dfr(items, function(item) {
    title <- xml_text(xml_find_first(item, "title"))
    link  <- xml_text(xml_find_first(item, "link"))
    
    # スニペット
    desc_html <- xml_text(xml_find_first(item, "description"))
    snippet <- xml_text(read_html(paste0("<body>", desc_html, "</body>")) %>%
                          xml_find_first("//body"))
    
    # pubDate（例: "Sun, 28 Jul 2025 22:27:24 GMT"）
    pub_raw <- xml_text(xml_find_first(item, "pubDate")) %>%
      str_replace_all('["\r\n\t]', "") %>%
      str_squish()
    
    # ログ出力：pub_rawの確認（ファイルに書き込み）
    cat("pub_raw: ", pub_raw, "\n", file = "test1.log", append = TRUE)
    
    # 日付変換（タイムゾーン部分を強制的に削除して処理）
    pub_clean <- str_remove(pub_raw, " GMT$")
    pub_dt <- tryCatch({
      as.POSIXct(pub_clean, format = "%a, %d %b %Y %H:%M:%S", tz = "GMT")
    }, error = function(e) {
      cat("日付変換エラー: ", pub_raw, "\n", file = "test1.log", append = TRUE)
      NA
    })
    
    tibble(
      keyword      = keyword,
      source       = "Google RSS",
      title        = title,
      link         = link,
      snippet      = snippet,
      posted       = pub_raw,
      posted_date  = as_date(pub_dt)
      
      
    )
  })
  
}


#### ニュース取得（全件）####

google_rss_all <- map_dfr(keywords, get_google_news_rss)

#### 保存（全件）####
write.csv(google_rss_all,
          paste0("google_rss_all_", Sys.Date(), ".csv"),
          row.names = FALSE, fileEncoding = "CP932")

#### 昨日分抽出・保存 ####
yesterday <- Sys.Date() - 1
google_rss_yesterday <- google_rss_all %>%
  filter(posted_date == yesterday)

write.csv(google_rss_yesterday,
          paste0("google_rss_yesterday_", Sys.Date(), ".csv"),
          row.names = FALSE, fileEncoding = "CP932")

#### 直近1週間分抽出・保存 ####
google_rss_week <- google_rss_all %>%
  filter(posted_date >= Sys.Date() - 7)

write.csv(google_rss_week,
          paste0("google_rss_week_", Sys.Date(), ".csv"),
          row.names = FALSE, fileEncoding = "CP932")

#### 出力確認（先頭）####
print(head(google_rss_all, 10))


