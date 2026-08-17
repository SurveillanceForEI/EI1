setwd(if (basename(getwd()) == "scripts") ".." else ".")
source("R/hokenjo_fetch/hokenjo_fetch_schema.R")
source("R/hokenjo_fetch/pdf_table_utils.R")
source("R/hokenjo_fetch/miyagi.R")
HISTORY_PATH <- "data/hokenjo_history.rds"
h <- readRDS(HISTORY_PATH)

hrefs <- c(
  "/documents/1967/shyuuhou01w.pdf", "/documents/1967/shuuhou02.pdf", "/documents/1967/shuuhou03.pdf",
  "/documents/1967/shyuuhou04w.pdf", "/documents/1967/shyuuhou05w.pdf", "/documents/1967/shuuhou06.pdf",
  "/documents/1967/shuuhou07.pdf", "/documents/1967/shuuhou08w.pdf", "/documents/1967/shuuhou09.pdf",
  "/documents/1967/shyuuhou10w.pdf", "/documents/1967/shyuuhou11w.pdf", "/documents/1967/shuuhou12w.pdf",
  "/documents/1967/shyuuhou13w.pdf", "/documents/1967/shyuuhou14w.pdf", "/documents/1967/shyuuhou15w.pdf",
  "/documents/1967/shyuuhou16w.pdf", "/documents/1967/shyuuhou17w.pdf", "/documents/1967/shyuuhou18w.pdf",
  "/documents/1967/syuuhou19w.pdf", "/documents/1967/shuhou20w.pdf", "/documents/1967/shuho21w.pdf",
  "/documents/1967/syuho22w.pdf", "/documents/1967/shuho23w.pdf", "/documents/1967/shuuho24w.pdf",
  "/documents/1967/syuhou25w.pdf", "/documents/1967/syuho26w.pdf", "/documents/1967/shuho27w.pdf",
  "/documents/1967/shuuhou2025_28.pdf", "/documents/1967/shuho29w.pdf", "/documents/1967/shuhp30w.pdf",
  "/documents/1967/syuho31w_revision.pdf", "/documents/1967/shuho32w.pdf", "/documents/1967/33wshuho.pdf",
  "/documents/1967/syuhou34w_3.pdf", "/documents/1967/syuho202535w.pdf", "/documents/1967/syuho202536w.pdf",
  "/documents/1967/syuho202537w.pdf", "/documents/1967/shuho2025_38w.pdf", "/documents/1967/syuho202539w.pdf",
  "/documents/1967/syuho202540w.pdf", "/documents/1967/shuho202541teisei.pdf", "/documents/1967/shuho202542teisei.pdf",
  "/documents/1967/shuho202543teisei.pdf", "/documents/1967/syuho202544w.pdf", "/documents/1967/syuho202545w.pdf",
  "/documents/1967/syuho202546w.pdf", "/documents/1967/syuho202547w.pdf", "/documents/1967/syuho202548w.pdf",
  "/documents/1967/syuho202549w.pdf", "/documents/1967/shuho-2025_50w_syusei.pdf", "/documents/1967/syuho202551w.pdf",
  "/documents/1967/syuho202552w.pdf"
)
stopifnot(length(hrefs) == 52)

new_rows <- list()
status_log <- character(0)
for (wk in seq_along(hrefs)) {
  u <- paste0("https://www.pref.miyagi.jp", hrefs[wk])
  res <- tryCatch(fetch_miyagi(u), error = function(e) { status_log <<- c(status_log, sprintf("[NG] 第%d週: %s", wk, conditionMessage(e))); NULL })
  if (is.null(res) || nrow(res) == 0) next
  res$week_num <- wk
  res$fetched_at <- as.character(Sys.time())
  new_rows[[length(new_rows) + 1]] <- res
  status_log <- c(status_log, sprintf("[OK] 第%d週 (%d行) label=%s", wk, nrow(res), unique(res$week_label)[1]))
}

cat("\n=== 実行ログ(宮城県 2025年分取得) ===\n")
cat(paste(status_log, collapse = "\n"), "\n")

if (length(new_rows) > 0) {
  added <- do.call(rbind, new_rows)
  common_cols <- intersect(names(h), names(added))
  combined <- rbind(h[, common_cols], added[, common_cols])
  saveRDS(combined, HISTORY_PATH)
  cat(sprintf("\n追記: %d行(新規) / 累計: %d行\n", nrow(added), nrow(combined)))
}
