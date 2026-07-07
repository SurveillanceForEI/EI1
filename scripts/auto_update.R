# ============================================================
# auto_update.R — 毎日自動実行用データ取得スクリプト
# Windowsタスクスケジューラから Rscript.exe で呼び出す
# ============================================================

setwd("C:/Users/kobayashi/Documents/R/japan_surveillance")

log_file <- file.path("data", "auto_update.log")
log <- function(...) {
  msg <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ...)
  cat(msg, "\n")
  write(msg, log_file, append = TRUE)
}

log("===== 自動更新開始 =====")

# 必要パッケージ読み込み
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(httr)
  library(xml2)
  library(jsonlite)
})

# プロジェクトのRファイルを読み込み
source("R/data_loader.R",  local = FALSE)
source("R/zensu_loader.R", local = FALSE)
source("R/ebs_loader.R",   local = FALSE)

dir.create("data",       showWarnings = FALSE)
dir.create("data/cache", showWarnings = FALSE)

# ① 定点把握データ
log("定点把握データ取得中...")
tryCatch({
  d <- get_surveillance_data(years = 2012:as.integer(format(Sys.Date(), "%Y")))
  if (!is.null(d) && nrow(d) > 0)
    log("定点把握データ完了: ", nrow(d), " 行")
  else
    log("定点把握データ: データなし")
}, error = function(e) log("定点把握データ エラー: ", e$message))

# ② 全数把握データ
log("全数把握データ取得中...")
tryCatch({
  d <- get_zensu_data(years = 2012:as.integer(format(Sys.Date(), "%Y")))
  if (!is.null(d) && nrow(d) > 0)
    log("全数把握データ完了: ", nrow(d), " 行")
  else
    log("全数把握データ: データなし")
}, error = function(e) log("全数把握データ エラー: ", e$message))

# ③ EBSニュース
log("EBSニュース取得中...")
tryCatch({
  ebs <- fetch_all_ebs(use_gnews = TRUE, bearer_token = NULL)
  if (!is.null(ebs) && nrow(ebs) > 0) {
    cache_path <- "data/ebs_startup_cache.rds"
    keep_days  <- 60
    cutoff     <- Sys.Date() - keep_days
    old <- tryCatch(readRDS(cache_path), error = function(e) NULL)
    if (!is.null(old)) {
      merged <- dplyr::bind_rows(ebs, old) %>%
        dplyr::filter(is.na(pub_date) | pub_date >= cutoff) %>%
        dplyr::distinct(title, pub_date, source_id, .keep_all = TRUE)
    } else {
      merged <- ebs
    }
    saveRDS(merged, cache_path)
    log("EBSニュース完了: ", nrow(merged), " 件 -> data/ebs_startup_cache.rds")
  } else {
    log("EBSニュース: 記事なし")
  }
}, error = function(e) log("EBSニュース エラー: ", e$message))

log("===== 自動更新完了 =====")

# タイムスタンプファイルを書き出す（Shinyアプリが読み取り表示を更新）
writeLines(format(Sys.time(), "%Y-%m-%d %H:%M"), "data/last_update.txt")
