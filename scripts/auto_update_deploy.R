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
    # new_dataはfetch_all_ebs()内で既にスクリーニング済み、oldは保存時点の判定を
    # 保持しているため、ここでの全件再スクリーニングは行わない（コスト増を避けるため）。
    # screen_entry()等のルール変更を過去記事にも遡及適用したい場合は
    # scripts/rescreen_ebs_full.R を手動実行すること。
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

# ── 3.5 保健所別データ（最新週をhokenjo_history.rdsへ追記し、hokenjo_current.rdsも同期） ──
log("保健所別データ取得開始...")
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
}, error = function(e) {
  log("保健所別データ エラー: ", e$message)
})

# ── 4. GitHubへpush（Connect Cloud側のデータ鮮度を保つため） ──
# shinyapps.ioへは直接ファイル一式をデプロイするが、Posit Connect Cloud
# 版はGitHubリポジトリの中身からデプロイする方式のため、pushしないと
# Connect Cloud側のデータが更新されないまま取り残されてしまう
# （2026-07-24、ユーザー指摘により追加）。
log("GitHub push開始...")
tryCatch({
  data_paths <- c(
    "data/cache", "data/cache_zensu", "data/cache_hosp",
    "data/cache_iasr", "data/cache_ari",
    "data/japan_map.rds", "data/last_update.txt",
    "data/ebs_startup_cache.rds", "data/gtrends_cache_JP.rds",
    "data/data_change_log.rds",
    "data/hokenjo_current.rds", "data/hokenjo_history.rds"
  )
  data_paths <- data_paths[file.exists(data_paths)]
  # タスクスケジューラ等の非対話実行環境ではgitのglobal設定(HOME解決)が
  # 正しく引き継がれず"Author identity unknown"で失敗することがあるため、
  # -cでuser.name/user.emailを明示的に指定する
  git_id <- c("-c", "user.name=japan_surveillance-auto-update",
              "-c", "user.email=kobayashi.yus@jihs.go.jp")
  system2("git", c("add", "-f", shQuote(data_paths)))
  status_out <- system2("git", c("status", "--porcelain"), stdout = TRUE)
  if (length(status_out) > 0) {
    commit_msg <- paste0("自動データ更新 ", format(Sys.time(), "%Y-%m-%d %H:%M"))
    commit_result <- system2("git", c(git_id, "commit", "-m", shQuote(commit_msg)), stdout = TRUE, stderr = TRUE)
    log("GitHub commit結果: ", paste(tail(commit_result, 2), collapse = " / "))
    push_result <- system2("git", c("push", "origin", "main"), stdout = TRUE, stderr = TRUE)
    log("GitHub push完了: ", paste(tail(push_result, 3), collapse = " / "))
    log("※ Posit Connect Cloud側は自動反映されません。ダッシュボードで手動Republishが必要です。")
  } else {
    log("GitHub: コミット対象の変更なし")
  }
}, error = function(e) {
  log("GitHub pushエラー: ", e$message)
})

# ── 5. デプロイ ──────────────────────────────────────────
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

  # app.R内のsource("R/....R")呼び出しを自動抽出してRファイル一覧を作る。
  # 手動の固定リストだと新規追加ファイルの記載漏れでデプロイバンドルから
  # 漏れ、"cannot open file ... No such file or directory" でアプリが
  # クラッシュする事故が起きる（実際に2026-07-21、R/idsc_links_data.Rの
  # 記載漏れで発生）。app.R側にsource()さえ書けば自動的に含まれるため、
  # このスクリプトを更新し忘れるミスが起こらない
  app_lines  <- readLines("app.R", warn = FALSE)
  r_source_files <- unique(unlist(regmatches(
    app_lines,
    gregexpr('(?<=source\\(")R/[^"]+\\.R(?=")', app_lines, perl = TRUE)
  )))
  if (length(r_source_files) == 0) {
    stop("app.R からsource()されるRファイルを検出できませんでした。正規表現を確認してください。")
  }
  log("デプロイに含めるRファイル: ", paste(r_source_files, collapse = ", "))

  www_js_files <- if (dir.exists("www/js"))
    file.path("www/js", list.files("www/js", pattern = "\\.js$"))
  else character(0)

  hokenjo_boundary_files <- if (dir.exists("data/geo/hokenjo_boundaries"))
    file.path("data/geo/hokenjo_boundaries",
              list.files("data/geo/hokenjo_boundaries", pattern = "\\.geojson$"))
  else character(0)
  hokenjo_name_map_file <- if (file.exists("data/geo/hokenjo_name_map.csv"))
    "data/geo/hokenjo_name_map.csv"
  else character(0)
  hokenjo_current_file <- if (file.exists("data/hokenjo_current.rds"))
    "data/hokenjo_current.rds"
  else character(0)
  hokenjo_history_file <- if (file.exists("data/hokenjo_history.rds"))
    "data/hokenjo_history.rds"
  else character(0)

  app_files <- c(
    "app.R",
    r_source_files,
    "www/custom.css",
    www_js_files,
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
    ari_data_file,
    hokenjo_current_file,
    hokenjo_history_file,
    hokenjo_name_map_file,
    hokenjo_boundary_files
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
