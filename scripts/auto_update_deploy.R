setwd("C:/Users/kobayashi/Documents/R/japan_surveillance")

log_file <- "data/auto_update_deploy.log"
log <- function(...) {
  msg <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ...)
  cat(msg, "\n")
  cat(msg, "\n", file = log_file, append = TRUE)
}

log("=== 自動更新・デプロイ開始 ===")

# ── パッケージ読み込み ─────────────────────────────────────
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(httr)
  library(xml2)
})

source("R/data_loader.R")
source("R/ebs_rule_screening.R")
source("R/ebs_loader.R")

# ── 1. IBS 定点把握データ取得 ────────────────────────────
log("IBS データ取得開始...")
tryCatch({
  current_year  <- as.integer(format(Sys.Date(), "%Y"))
  current_week  <- as.integer(format(Sys.Date(), "%V"))
  years_to_fetch <- (current_year - 2):current_year
  for (yr in years_to_fetch) {
    max_wk <- if (yr == current_year) current_week else 53
    for (wk in 1:max_wk) {
      fetch_week_cached(yr, wk, force = FALSE)
    }
  }
  log("IBS データ取得完了")
}, error = function(e) {
  log("IBS データ取得エラー: ", e$message)
})

# ── 2. EBS ニュース取得・キャッシュ更新 ──────────────────
log("EBS ニュース取得開始...")
tryCatch({
  new_data <- fetch_all_ebs(use_gnews = TRUE, bearer_token = NULL)
  if (!is.null(new_data) && nrow(new_data) > 0) {
    cache_path <- "data/ebs_startup_cache.rds"
    keep_days  <- 365
    cutoff     <- Sys.Date() - keep_days
    old <- tryCatch(readRDS(cache_path), error = function(e) NULL)
    if (!is.null(old)) {
      # new_dataを先にしてdistinctで新しい方を優先（signal_levelの更新を反映）
      merged <- dplyr::bind_rows(new_data, old) %>%
        dplyr::filter(is.na(pub_date) | pub_date >= cutoff) %>%
        dplyr::distinct(title, pub_date, source_id, .keep_all = TRUE)
    } else {
      merged <- new_data
    }
    # フィード表示範囲外に出て再取得されなくなった過去記事も含め、
    # マージ後の全件を再スクリーニング（screen_entry修正の遡及反映）
    merged <- rescreen_ebs_data(merged)
    saveRDS(merged, cache_path)
    log("EBS キャッシュ更新完了: ", nrow(merged), " 件")
  } else {
    log("EBS: 新規データなし")
  }
}, error = function(e) {
  log("EBS 取得エラー: ", e$message)
})

# ── 3. IASR キャッシュ更新 ───────────────────────────────
log("IASR データ取得開始...")
tryCatch({
  source("R/iasr_loader.R")
  # 強制リフレッシュ: キャッシュ削除してから再取得
  if (dir.exists(IASR_CACHE_DIR)) {
    old_files <- list.files(IASR_CACHE_DIR, pattern="\\.rds$", full.names=TRUE)
    file.remove(old_files)
  }
  load_all_iasr()
  log("IASR データ取得完了")
}, error = function(e) {
  log("IASR 取得エラー: ", e$message)
})

# ── 4. デプロイ ──────────────────────────────────────────
log("shinyapps.io デプロイ開始...")
tryCatch({
  cache_files <- file.path("data/cache",
    list.files("data/cache", pattern = "\\.rds$"))

  cache_zensu_files <- file.path("data/cache_zensu",
    list.files("data/cache_zensu", pattern = "\\.rds$"))

  cache_iasr_files <- if (dir.exists("data/cache_iasr"))
    file.path("data/cache_iasr", list.files("data/cache_iasr", pattern="\\.rds$"))
  else character(0)

  hosp_data_file <- if (file.exists("data/cache_hosp/hosp_data.rds"))
    "data/cache_hosp/hosp_data.rds"
  else character(0)

  std_data_file <- if (file.exists("data/cache_hosp/std_data.rds"))
    "data/cache_hosp/std_data.rds"
  else character(0)

  ari_data_file <- if (file.exists("data/cache_ari/ari_pathogen_data.rds"))
    "data/cache_ari/ari_pathogen_data.rds"
  else character(0)

  # 注意: app.Rにsource()を新規追加した場合は、必ずここにも同じファイルを
  # 追加すること。追加を忘れると、このappFiles固定リストに載っていない
  # ファイルがデプロイバンドルから漏れ、"cannot open file ... No such file
  # or directory" でアプリがクラッシュする（実際に2026-07-21に発生した
  # 障害の原因: R/idsc_links_data.Rの追加漏れ）
  app_files <- c(
    "app.R",
    "R/ebs_loader.R",
    "R/ebs_rule_screening.R",
    "R/ebs_screening_module.R",
    "R/data_loader.R",
    "R/rt_estimation.R",
    "R/zensu_loader.R",
    "R/iasr_loader.R",
    "R/hosp_loader.R",
    "R/std_loader.R",
    "R/ari_pathogen_loader.R",
    "R/correlation.R",
    "R/change_tracker.R",
    "R/forecast_ts.R",
    "R/idsc_links_data.R",
    "www/custom.css",
    "data/japan_map.rds",
    "data/ebs_startup_cache.rds",
    "data/gtrends_cache_JP.rds",
    "data/last_update.txt",
    "data/data_change_log.rds",
    cache_files,
    cache_zensu_files,
    cache_iasr_files,
    hosp_data_file,
    std_data_file,
    ari_data_file
  )

  rsconnect::deployApp(
    appDir         = getwd(),
    appName        = "japan_surveillance",
    appFiles       = app_files,
    forceUpdate    = TRUE,
    launch.browser = FALSE
  )
  log("デプロイ完了")
}, error = function(e) {
  log("デプロイエラー: ", e$message)
})

log("=== 自動更新・デプロイ終了 ===")
