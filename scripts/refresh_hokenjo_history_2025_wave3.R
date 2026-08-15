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

h <- readRDS(HISTORY_PATH)
existing_keys <- mapply(make_key, h$pref, h$week_num, h$week_label)

new_rows <- list()
status_log <- character(0)

add_result <- function(res, pref, week) {
  if (!is.null(res) && is.data.frame(res) && nrow(res) > 0) {
    res$week_num <- week
    res$fetched_at <- as.character(Sys.time())
    new_rows[[length(new_rows) + 1]] <<- res
    status_log <<- c(status_log, sprintf("[OK] %s 第%d週 (%d行)", pref, week, nrow(res)))
  } else {
    status_log <<- c(status_log, sprintf("[--] %s 第%d週", pref, week))
  }
}

# ---- 埼玉県: /documents/262567/2025_{WW}w.pdf ----
for (week in 1:52) {
  key <- paste("埼玉県", 2025, week)
  if (key %in% existing_keys) next
  u <- sprintf("https://www.pref.saitama.lg.jp/documents/262567/2025_%02dw.pdf", week)
  res <- tryCatch(fetch_saitama(u), error = function(e) NULL)
  add_result(res, "埼玉県", week)
}

# ---- 神奈川県: wrR07_{WW}.pdf ----
for (week in 1:52) {
  key <- paste("神奈川県", 2025, week)
  if (key %in% existing_keys) next
  u <- sprintf("https://www.pref.kanagawa.jp/sys/eiken/003_center/0001_weekly/pdf/wrR07_%02d.pdf", week)
  res <- tryCatch(fetch_kanagawa(u), error = function(e) NULL)
  add_result(res, "神奈川県", week)
}

# ---- 千葉県: wr25{WW}.pdf ----
for (week in 1:52) {
  key <- paste("千葉県", 2025, week)
  if (key %in% existing_keys) next
  u <- sprintf("https://www.pref.chiba.lg.jp/eiken/c-idsc/documents/wr25%02d.pdf", week)
  res <- tryCatch(fetch_chiba(u), error = function(e) NULL)
  add_result(res, "千葉県", week)
}

# ---- 香川県: /documents/7135/2025syuuhou{WW}.pdf（一部週は表記ゆれ） ----
kagawa_urls <- function(week) c(
  sprintf("https://www.pref.kagawa.lg.jp/documents/7135/2025syuuhou%d.pdf", week),
  sprintf("https://www.pref.kagawa.lg.jp/documents/7135/2025syuuhou%02d.pdf", week),
  sprintf("https://www.pref.kagawa.lg.jp/documents/7135/2025syuhou%d.pdf", week),
  sprintf("https://www.pref.kagawa.lg.jp/documents/7135/2025syuuou%d.pdf", week),
  sprintf("https://www.pref.kagawa.lg.jp/documents/7135/2025kansensyuhou%d.pdf", week)
)
for (week in 1:52) {
  key <- paste("香川県", 2025, week)
  if (key %in% existing_keys) next
  res <- NULL
  for (u in kagawa_urls(week)) {
    res <- tryCatch(fetch_kagawa(u), error = function(e) NULL)
    if (!is.null(res)) break
  }
  add_result(res, "香川県", week)
}

# ---- 長野県: 2025-{WW}w.pdf（第37週のみ-2サフィックス） ----
for (week in 1:52) {
  key <- paste("長野県", 2025, week)
  if (key %in% existing_keys) next
  suffix <- if (week == 37) "-2" else ""
  u <- sprintf("https://www.pref.nagano.lg.jp/shippei-kansen/kenko/kenko/kansensho/joho/documents/2025-%02dw%s.pdf", week, suffix)
  res <- tryCatch(fetch_nagano(u), error = function(e) NULL)
  add_result(res, "長野県", week)
}

cat("\n=== 実行ログ（2025年 第3弾） ===\n")
cat(paste(status_log, collapse = "\n"), "\n")

if (length(new_rows) > 0) {
  h <- readRDS(HISTORY_PATH)
  added <- do.call(rbind, new_rows)
  common_cols <- intersect(names(h), names(added))
  combined <- rbind(h[, common_cols], added[, common_cols])
  saveRDS(combined, HISTORY_PATH)
  cat(sprintf("\n追記: %d行（新規） / 累計: %d行\n", nrow(added), nrow(combined)))
} else {
  cat("\n新規追加データはありませんでした\n")
}
