# ============================================================
# 保健所別データ 2025年分バックフィル（第2弾）
# hokenjo_data_sources.R のurl_patternから追加で自動化できる県
# （山形県・島根県・山口県）を対象にする。
# ============================================================

setwd(if (basename(getwd()) == "scripts") ".." else ".")
source("R/hokenjo_fetch/hokenjo_fetch_schema.R")
source("R/hokenjo_fetch/pdf_table_utils.R")
source("R/hokenjo_data_sources.R")

FETCH_DIR <- "R/hokenjo_fetch"
for (f in list.files(FETCH_DIR, pattern = "\\.R$", full.names = TRUE)) {
  if (basename(f) %in% c("hokenjo_fetch_schema.R", "pdf_table_utils.R")) next
  source(f)
}

HISTORY_PATH <- "data/hokenjo_history.rds"
CURRENT_YEAR <- 2025
MAX_WEEK <- 53

extract_year_num <- function(week_label) {
  if (is.na(week_label)) return(NA_integer_)
  wl <- chartr("０１２３４５６７８９", "0123456789", week_label)
  m <- regmatches(wl, regexec("([0-9]{4})年", wl))[[1]]
  if (length(m) >= 2) return(as.integer(m[2]))
  m <- regmatches(wl, regexec("令和\\s*([0-9]{1,2})\\s*年", wl))[[1]]
  if (length(m) >= 2) return(as.integer(m[2]) + 2018)
  NA_integer_
}
make_key <- function(pref, week_num, week_label) {
  yr <- extract_year_num(week_label)
  if (!is.na(week_num) && !is.na(yr)) paste(pref, yr, week_num)
  else if (!is.na(week_num)) paste(pref, "unkyear", week_num)
  else paste(pref, "label:", week_label)
}

history <- if (file.exists(HISTORY_PATH)) readRDS(HISTORY_PATH) else NULL
existing_keys <- if (!is.null(history) && nrow(history) > 0) {
  mapply(make_key, history$pref, history$week_num, history$week_label)
} else character(0)

new_rows <- list()
status_log <- character(0)

# ---- 山形県: fetch_yamagata_history(year)が1回の呼び出しで年間全週を返す ----
yamagata_res <- tryCatch(
  fetch_yamagata_history(year = CURRENT_YEAR),
  error = function(e) { status_log <<- c(status_log, sprintf("[NG] 山形県: %s", conditionMessage(e))); NULL }
)
if (!is.null(yamagata_res) && nrow(yamagata_res) > 0) {
  yamagata_res$key <- mapply(make_key, yamagata_res$pref, yamagata_res$week_num, yamagata_res$week_label)
  new_yamagata <- yamagata_res[!(yamagata_res$key %in% existing_keys), setdiff(names(yamagata_res), "key")]
  if (nrow(new_yamagata) > 0) {
    if (!"fetched_at" %in% names(new_yamagata)) new_yamagata$fetched_at <- as.character(Sys.time())
    new_rows[[length(new_rows) + 1]] <- new_yamagata
    for (wk in sort(unique(new_yamagata$week_num))) {
      status_log <- c(status_log, sprintf("[OK] 山形県 第%d週 (%d行)", wk, sum(new_yamagata$week_num == wk)))
    }
  } else {
    status_log <- c(status_log, "[==] 山形県 (新規週なし)")
  }
}

# ---- 島根県・山口県: DIDSS系、weeklyreport_y{YEAR}w{WEEK}.pdf ----
PDF_LOOP_DISPATCH <- list(
  "島根県" = function(week) fetch_shimane(
    sprintf("https://pref.shimane.didss.dsvc.jp/files/report/week/weeklyreport_y%dw%d.pdf", CURRENT_YEAR, week)),
  "山口県" = function(week) fetch_yamaguchi(
    sprintf("https://pref.yamaguchi.didss.dsvc.jp/files/report/week/weeklyreport_y%dw%d.pdf", CURRENT_YEAR, week))
)

for (pref in names(PDF_LOOP_DISPATCH)) {
  for (week in 1:MAX_WEEK) {
    key <- paste(pref, CURRENT_YEAR, week)
    if (key %in% existing_keys) next
    res <- tryCatch(PDF_LOOP_DISPATCH[[pref]](week), error = function(e) NULL)
    if (!is.null(res) && is.data.frame(res) && nrow(res) > 0) {
      res$week_num <- week
      res$fetched_at <- as.character(Sys.time())
      new_rows[[length(new_rows) + 1]] <- res
      status_log <- c(status_log, sprintf("[OK] %s 第%d週 (%d行)", pref, week, nrow(res)))
    } else {
      status_log <- c(status_log, sprintf("[--] %s 第%d週 (未公開/取得不可)", pref, week))
    }
  }
}

cat("\n=== 実行ログ（2025年 第2弾） ===\n")
cat(paste(status_log, collapse = "\n"), "\n")

if (length(new_rows) > 0) {
  # 再読み込みして最新状態とマージ（wave1と時間差があるため）
  history <- if (file.exists(HISTORY_PATH)) readRDS(HISTORY_PATH) else NULL
  added <- do.call(rbind, new_rows)
  common_cols <- if (!is.null(history)) intersect(names(history), names(added)) else names(added)
  added <- added[, common_cols]
  if (!is.null(history)) history <- history[, common_cols]
  combined <- if (!is.null(history)) rbind(history, added) else added
  saveRDS(combined, HISTORY_PATH)
  cat(sprintf("\n追記: %d行（新規） / 累計: %d行\n", nrow(added), nrow(combined)))
} else {
  cat("\n新規追加データはありませんでした\n")
}
