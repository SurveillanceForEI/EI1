setwd(if (basename(getwd()) == "scripts") ".." else ".")
source("R/hokenjo_fetch/hokenjo_fetch_schema.R")
source("R/hokenjo_fetch/pdf_table_utils.R")
source("R/hokenjo_fetch/gifu.R")
HISTORY_PATH <- "data/hokenjo_history.rds"
h <- readRDS(HISTORY_PATH)

scan <- readRDS("scripts/_gifu_2025_scan.rds")
r2025 <- scan[!is.na(scan$year) & scan$year == "2025", ]
r2025$week <- as.integer(r2025$week)

new_rows <- list()
status_log <- character(0)
for (wk in sort(unique(r2025$week))) {
  ids_for_week <- r2025$id[r2025$week == wk]
  res <- NULL
  for (id in ids_for_week) {
    u <- sprintf("https://www.pref.gifu.lg.jp/uploaded/attachment/%d.pdf", id)
    res <- tryCatch(fetch_gifu(u), error = function(e) NULL)
    if (!is.null(res) && nrow(res) > 0) break
  }
  if (is.null(res) || nrow(res) == 0) {
    status_log <- c(status_log, sprintf("[NG] 第%d週", wk))
    next
  }
  res$week_num <- wk
  res$fetched_at <- as.character(Sys.time())
  new_rows[[length(new_rows) + 1]] <- res
  status_log <- c(status_log, sprintf("[OK] 第%d週 (%d行) label=%s", wk, nrow(res), unique(res$week_label)[1]))
}

cat("\n=== 実行ログ(岐阜県 2025年分取得) ===\n")
cat(paste(status_log, collapse = "\n"), "\n")

if (length(new_rows) > 0) {
  added <- do.call(rbind, new_rows)
  common_cols <- intersect(names(h), names(added))
  combined <- rbind(h[, common_cols], added[, common_cols])
  saveRDS(combined, HISTORY_PATH)
  cat(sprintf("\n追記: %d行(新規) / 累計: %d行\n", nrow(added), nrow(combined)))
}
