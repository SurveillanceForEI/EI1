setwd(if (basename(getwd()) == "scripts") ".." else ".")
source("R/hokenjo_fetch/hokenjo_fetch_schema.R")
source("R/hokenjo_fetch/pdf_table_utils.R")
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

shizuoka_urls <- function(week) c(
  sprintf("https://www.pref.shizuoka.jp/_res/projects/default_project/_page_/001/068/844/2025idwr%d.pdf", week),
  sprintf("https://www.pref.shizuoka.jp/_res/projects/default_project/_page_/001/068/844/2025idwr%d-2.pdf", week)
)
for (week in 1:52) {
  key <- paste("静岡県", 2025, week)
  if (key %in% existing_keys) next
  res <- NULL
  for (u in shizuoka_urls(week)) {
    res <- tryCatch(fetch_shizuoka(u), error = function(e) NULL)
    if (!is.null(res)) break
  }
  add_result(res, "静岡県", week)
}

cat("\n=== 実行ログ（2025年 第7弾：静岡県） ===\n")
cat(paste(status_log, collapse = "\n"), "\n")

if (length(new_rows) > 0) {
  h <- readRDS(HISTORY_PATH)
  added <- do.call(rbind, new_rows)
  common_cols <- intersect(names(h), names(added))
  combined <- rbind(h[, common_cols], added[, common_cols])
  saveRDS(combined, HISTORY_PATH)
  cat(sprintf("\n追記: %d行（新規） / 累計: %d行\n", nrow(added), nrow(combined)))
}
