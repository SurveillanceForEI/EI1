#### プロキシ設定（必要に応じて）####
Sys.setenv(http_proxy = "http://proxy.nih.go.jp:8080",
           https_proxy = "http://proxy.nih.go.jp:8080")
Sys.setlocale("LC_TIME", "C")

logfile <- paste0("log/test_", Sys.Date(), ".log")
cat(Sys.time(), "開始\n", file = logfile, append = TRUE)

#### ライブラリ読み込み ####
library(xml2)
library(dplyr)
library(purrr)
library(lubridate)
library(stringr)
library(tibble)

setwd("C:/Users/kobayashi/Documents/R/EI1")

#### 検索キーワード ####
keywords <- c("急性呼吸器感染症", "インフルエンザ", "新型コロナ", "RSウイルス", "感染症")

#### GoogleニュースRSS取得関数 ####
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
    desc_html <- xml_text(xml_find_first(item, "description"))
    snippet <- xml_text(read_html(paste0("<body>", desc_html, "</body>")) %>%
                          xml_find_first("//body"))
    pub_raw <- xml_text(xml_find_first(item, "pubDate")) %>%
      str_replace_all('["\r\n\t]', "") %>%
      str_squish()
    
    pub_clean <- str_remove(pub_raw, " GMT$")
    pub_dt <- tryCatch(
      as.POSIXct(pub_clean, format = "%a, %d %b %Y %H:%M:%S", tz = "GMT"),
      error = function(e) NA
    )
    
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


#### ニュース取得 ####
google_rss_all <- map_dfr(keywords, get_google_news_rss)

#### 保存処理 ####
write.csv(google_rss_all, paste0("results/google_rss_all_", Sys.Date(), ".csv"), row.names = FALSE, fileEncoding = "CP932", quote = TRUE)

#### 前週・前々週含む疫学週ごとの処理 ####
for (offset in c(0)) {
  ref_date <- Sys.Date() - 1 - offset
  target_year <- isoyear(ref_date)
  target_week <- isoweek(ref_date)
  
  google_rss_epiweek <- google_rss_all %>%
    filter(!is.na(posted_date)) %>%
    mutate(epiyear = isoyear(posted_date), epiweek = isoweek(posted_date)) %>%
    filter(epiyear == target_year, epiweek == target_week)
  
  epi_filename <- sprintf("results/google_rss_epi_%dw%02d.csv", target_year, target_week)
  
  # 既存ファイルは読み込まず、上書き保存
  write.csv(google_rss_epiweek, epi_filename, row.names = FALSE, fileEncoding = "CP932", quote = TRUE)
}
