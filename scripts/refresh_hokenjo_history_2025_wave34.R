setwd(if (basename(getwd()) == "scripts") ".." else ".")
source("R/hokenjo_fetch/hokenjo_fetch_schema.R")
source("R/hokenjo_fetch/pdf_table_utils.R")
source("R/hokenjo_fetch/nara.R")
HISTORY_PATH <- "data/hokenjo_history.rds"
h <- readRDS(HISTORY_PATH)

urls <- sprintf("https://www.pref.nara.lg.jp/documents/4352/07%02d_3.pdf", 1:51)

new_rows <- list()
status_log <- character(0)
for (wk in seq_along(urls)) {
  res <- tryCatch(fetch_nara(urls[wk]), error = function(e) { status_log <<- c(status_log, sprintf("[NG] 第%d週: %s", wk, conditionMessage(e))); NULL })
  if (is.null(res) || nrow(res) == 0) next
  res$week_num <- wk
  res$fetched_at <- as.character(Sys.time())
  new_rows[[length(new_rows) + 1]] <- res
  status_log <- c(status_log, sprintf("[OK] 第%d週 (%d行) label=%s", wk, nrow(res), unique(res$week_label)[1]))
}

cat("\n=== 実行ログ(奈良県 2025年分取得) ===\n")
cat(paste(status_log, collapse = "\n"), "\n")

if (length(new_rows) > 0) {
  added <- do.call(rbind, new_rows)
  common_cols <- intersect(names(h), names(added))
  combined <- rbind(h[, common_cols], added[, common_cols])
  saveRDS(combined, HISTORY_PATH)
  cat(sprintf("\n追記: %d行(新規) / 累計: %d行\n", nrow(added), nrow(combined)))
}
