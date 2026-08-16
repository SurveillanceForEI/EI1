setwd(if (basename(getwd()) == "scripts") ".." else ".")
source("R/hokenjo_fetch/hokenjo_fetch_schema.R")
source("R/hokenjo_fetch/pdf_table_utils.R")
source("R/hokenjo_fetch/fukui.R")
HISTORY_PATH <- "data/hokenjo_history.rds"
h <- readRDS(HISTORY_PATH)
new_rows <- list()
status_log <- character(0)

for (hokenjo in names(.fukui_hokenjo_codes)) {
  code <- .fukui_hokenjo_codes[[hokenjo]]
  res <- tryCatch(.fukui_one_hokenjo_history(hokenjo, code, year_prefix = "1"),
                   error = function(e) { status_log <<- c(status_log, sprintf("[NG] 福井県 %s: %s", hokenjo, conditionMessage(e))); NULL })
  if (is.null(res) || nrow(res) == 0) next
  res$fetched_at <- as.character(Sys.time())
  new_rows[[length(new_rows) + 1]] <- res
  status_log <- c(status_log, sprintf("[OK] 福井県 %s (%d行, %d週)", hokenjo, nrow(res), length(unique(res$week_num))))
}

cat("\n=== 実行ログ(2025年 第23弾:福井県) ===\n")
cat(paste(status_log, collapse = "\n"), "\n")

if (length(new_rows) > 0) {
  added <- do.call(rbind, new_rows)
  common_cols <- intersect(names(h), names(added))
  combined <- rbind(h[, common_cols], added[, common_cols])
  saveRDS(combined, HISTORY_PATH)
  cat(sprintf("\n追記: %d行(新規) / 累計: %d行\n", nrow(added), nrow(combined)))
}
