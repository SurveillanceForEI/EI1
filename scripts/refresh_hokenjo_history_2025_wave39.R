setwd(if (basename(getwd()) == "scripts") ".." else ".")
source("R/hokenjo_fetch/hokenjo_fetch_schema.R")
source("R/hokenjo_fetch/pdf_table_utils.R")
source("R/hokenjo_fetch/shiga.R")
HISTORY_PATH <- "data/hokenjo_history.rds"
h <- readRDS(HISTORY_PATH)

items <- list(
  list(wk = 15, url = "https://www.pref.shiga.lg.jp/file/attachment/5536989.pdf"),
  list(wk = 16, url = "https://www.pref.shiga.lg.jp/file/attachment/5536990.pdf"),
  list(wk = 17, url = "https://www.pref.shiga.lg.jp/file/attachment/5539334.pdf"),
  list(wk = 25, url = "https://www.pref.shiga.lg.jp/file/attachment/5551055.pdf"),
  list(wk = 45, url = "https://www.pref.shiga.lg.jp/file/attachment/5574813.pdf"),
  list(wk = 47, url = "https://www.pref.shiga.lg.jp/file/attachment/5576938.pdf")
)

new_rows <- list()
status_log <- character(0)
for (it in items) {
  res <- tryCatch(fetch_shiga(it$url), error = function(e) { status_log <<- c(status_log, sprintf("[NG] 第%d週: %s", it$wk, conditionMessage(e))); NULL })
  if (is.null(res) || nrow(res) == 0) next
  res$week_num <- it$wk
  res$fetched_at <- as.character(Sys.time())
  new_rows[[length(new_rows) + 1]] <- res
  status_log <- c(status_log, sprintf("[OK] 第%d週 (%d行) label=%s", it$wk, nrow(res), unique(res$week_label)[1]))
}

cat("\n=== 実行ログ(滋賀県 歯抜け週取得) ===\n")
cat(paste(status_log, collapse = "\n"), "\n")

if (length(new_rows) > 0) {
  added <- do.call(rbind, new_rows)
  common_cols <- intersect(names(h), names(added))
  combined <- rbind(h[, common_cols], added[, common_cols])
  saveRDS(combined, HISTORY_PATH)
  cat(sprintf("\n追記: %d行(新規) / 累計: %d行\n", nrow(added), nrow(combined)))
}
