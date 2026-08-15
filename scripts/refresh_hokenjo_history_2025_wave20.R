setwd(if (basename(getwd()) == "scripts") ".." else ".")
source("R/hokenjo_fetch/hokenjo_fetch_schema.R")
source("R/hokenjo_fetch/pdf_table_utils.R")
source("R/hokenjo_fetch/kochi.R")
HISTORY_PATH <- "data/hokenjo_history.rds"
h <- readRDS(HISTORY_PATH)
new_rows <- list()
status_log <- character(0)

kochi_retry <- c(
  "7"="file_2025219313476_1.pdf","11"="file_202531931192_1.pdf","17"="file_202552592757_1.pdf",
  "25"="file_2025734141514_1.pdf","29"="file_20257244112429_1.pdf","33"="file_20258214114639_1.pdf"
)
for (wk_str in names(kochi_retry)) {
  week <- as.integer(wk_str)
  u <- paste0("https://www.pref.kochi.lg.jp/doc/2025011000151/file_contents/", kochi_retry[[wk_str]])
  res <- tryCatch(fetch_kochi(u), error = function(e) { status_log <<- c(status_log, sprintf("[NG] 高知県 第%d週: %s", week, conditionMessage(e))); NULL })
  if (is.null(res) || nrow(res) == 0) next
  res$week_num <- week
  res$fetched_at <- as.character(Sys.time())
  new_rows[[length(new_rows) + 1]] <- res
  status_log <- c(status_log, sprintf("[OK] 高知県 第%d週 (%d行)", week, nrow(res)))
}

cat("\n=== 実行ログ（2025年 第20弾：高知県） ===\n")
cat(paste(status_log, collapse = "\n"), "\n")

if (length(new_rows) > 0) {
  added <- do.call(rbind, new_rows)
  common_cols <- intersect(names(h), names(added))
  combined <- rbind(h[, common_cols], added[, common_cols])
  saveRDS(combined, HISTORY_PATH)
  cat(sprintf("\n追記: %d行（新規） / 累計: %d行\n", nrow(added), nrow(combined)))
}
