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
source("R/hosp_loader.R",  local = FALSE)
source("R/std_loader.R",   local = FALSE)
source("R/ari_pathogen_loader.R", local = FALSE)

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
    keep_days  <- 365
    cutoff     <- Sys.Date() - keep_days
    old <- tryCatch(readRDS(cache_path), error = function(e) NULL)
    if (!is.null(old)) {
      merged <- dplyr::bind_rows(ebs, old) %>%
        dplyr::filter(is.na(pub_date) | pub_date >= cutoff) %>%
        dplyr::distinct(title, pub_date, source_id, .keep_all = TRUE)
    } else {
      merged <- ebs
    }
    # フィード表示範囲外に出て再取得されなくなった過去記事も含め、
    # マージ後の全件を再スクリーニング（screen_entry修正の遡及反映）
    merged <- rescreen_ebs_data(merged)
    saveRDS(merged, cache_path)
    log("EBSニュース完了: ", nrow(merged), " 件 -> data/ebs_startup_cache.rds")
  } else {
    log("EBSニュース: 記事なし")
  }
}, error = function(e) log("EBSニュース エラー: ", e$message))

# ④ 入院サーベイランス（IDWR週報PDF）
log("入院サーベイランスデータ取得中...")
tryCatch({
  this_year <- as.integer(format(Sys.Date(), "%Y"))
  d <- update_hosp_data(years = (this_year - 1):this_year, force_latest_n = 3)
  if (!is.null(d) && nrow(d) > 0)
    log("入院サーベイランスデータ完了: ", nrow(d), " 行")
  else
    log("入院サーベイランスデータ: データなし")
}, error = function(e) log("入院サーベイランスデータ エラー: ", e$message))

# ⑤ 月報疾患（性感染症・薬剤耐性菌、IDWR週報PDF月報）
log("月報疾患データ取得中...")
tryCatch({
  this_year <- as.integer(format(Sys.Date(), "%Y"))
  d <- update_std_data(years = (this_year - 1):this_year)
  if (!is.null(d) && nrow(d) > 0)
    log("月報疾患データ完了: ", nrow(d), " 行")
  else
    log("月報疾患データ: データなし")
}, error = function(e) log("月報疾患データ エラー: ", e$message))

# ⑥ ARI病原体（急性呼吸器感染症サーベイランス週報PDF）
log("ARI病原体データ取得中...")
tryCatch({
  d <- update_ari_pathogen_data()
  if (!is.null(d) && !is.null(d$counts) && nrow(d$counts) > 0)
    log("ARI病原体データ完了: ", nrow(d$counts), " 行")
  else
    log("ARI病原体データ: データなし")
}, error = function(e) log("ARI病原体データ エラー: ", e$message))

# ⑦ 保健所別データ（最新週をhokenjo_history.rdsへ追記し、hokenjo_current.rdsも同期）
log("保健所別データ取得中...")
tryCatch({
  hokenjo_log <- system2("Rscript", c("scripts/refresh_hokenjo_history.R"), stdout = TRUE, stderr = TRUE)
  log("保健所別履歴データ完了: ", paste(utils::tail(hokenjo_log, 3), collapse = " / "))

  if (file.exists("data/hokenjo_history.rds")) {
    h <- readRDS("data/hokenjo_history.rds")
    # week_num（1〜52）は年をまたいで再利用されるため、単純にmax(week_num)で
    # 「最新週」を選ぶと、当年の第32週などより2025年の第52週（過去の
    # バックフィルデータ）の方が数値が大きく誤って選ばれてしまう。
    # week_labelから年を抜き出し、年*100+週番号で「真の最新週」を判定する
    .extract_year <- function(label) {
      if (is.na(label)) return(NA_integer_)
      s <- chartr("０１２３４５６７８９", "0123456789", label)
      m <- regmatches(s, regexpr("(20[0-9]{2})年", s))
      if (length(m) > 0 && nzchar(m)) return(as.integer(sub("年", "", m)))
      m <- regmatches(s, regexec("令和\\s*([0-9]+)\\s*年", s))[[1]]
      if (length(m) == 2) return(as.integer(m[2]) + 2018L)
      # 福井県「R. 7. 8.18」のような、漢字なしの略式和暦表記
      m <- regmatches(s, regexec("R\\.?\\s*([0-9]+)\\s*\\.", s))[[1]]
      if (length(m) == 2) return(as.integer(m[2]) + 2018L)
      # 愛媛県「2025.8.18」のような、「年」の付かない西暦表記
      m <- regmatches(s, regexpr("(?<![0-9])(20[0-9]{2})\\.[0-9]{1,2}\\.[0-9]{1,2}", s, perl = TRUE))
      if (length(m) > 0 && nzchar(m)) return(as.integer(sub("\\..*", "", m)))
      NA_integer_
    }
    yr <- vapply(h$week_label, .extract_year, integer(1))
    latest <- h[!is.na(h$week_num) & !is.na(yr), ]
    latest$.sort_key <- yr[!is.na(h$week_num) & !is.na(yr)] * 100L + latest$week_num
    latest <- do.call(rbind, lapply(split(latest, latest$pref), function(df) df[df$.sort_key == max(df$.sort_key), ]))
    latest$.sort_key <- NULL
    saveRDS(latest, "data/hokenjo_current.rds")
    log("保健所別最新週データ完了: data/hokenjo_current.rds (", nrow(latest), " 行)")
  }
}, error = function(e) log("保健所別データ エラー: ", e$message))

log("===== 自動更新完了 =====")

# タイムスタンプファイルを書き出す（Shinyアプリが読み取り表示を更新）
writeLines(format(Sys.time(), "%Y-%m-%d %H:%M"), "data/last_update.txt")
