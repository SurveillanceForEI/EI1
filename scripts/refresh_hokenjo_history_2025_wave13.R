setwd(if (basename(getwd()) == "scripts") ".." else ".")
source("R/hokenjo_fetch/hokenjo_fetch_schema.R")
source("R/hokenjo_fetch/pdf_table_utils.R")
source("R/hokenjo_fetch/hyogo.R")
source("R/hokenjo_fetch/mie.R")
HISTORY_PATH <- "data/hokenjo_history.rds"
h <- readRDS(HISTORY_PATH)
new_rows <- list()
status_log <- character(0)

for (week in 1:52) {
  u <- sprintf("https://web.pref.hyogo.lg.jp/iphs01/kansensho_jyoho/documents/2025-%dw-newt3201-t3203.xls", week)
  res <- tryCatch(fetch_hyogo(week_url = u), error = function(e) { status_log <<- c(status_log, sprintf("[NG] 兵庫県 第%d週: %s", week, conditionMessage(e))); NULL })
  if (is.null(res) || nrow(res) == 0) next
  res$week_num <- week
  res$fetched_at <- as.character(Sys.time())
  new_rows[[length(new_rows) + 1]] <- res
  status_log <- c(status_log, sprintf("[OK] 兵庫県 第%d週 (%d行) label=%s", week, nrow(res), unique(res$week_label)[1]))
}

for (week in 1:52) {
  u <- sprintf("https://www.kenkou.pref.mie.jp/weekly_fp_past/2025/%d.html", week)
  res <- tryCatch(fetch_mie(u), error = function(e) { status_log <<- c(status_log, sprintf("[NG] 三重県 第%d週: %s", week, conditionMessage(e))); NULL })
  if (is.null(res) || nrow(res) == 0) next
  res$week_num <- week
  res$fetched_at <- as.character(Sys.time())
  new_rows[[length(new_rows) + 1]] <- res
  status_log <- c(status_log, sprintf("[OK] 三重県 第%d週 (%d行) label=%s", week, nrow(res), unique(res$week_label)[1]))
}

cat("\n=== 実行ログ（2025年 第13弾：兵庫県・三重県） ===\n")
cat(paste(status_log, collapse = "\n"), "\n")

if (length(new_rows) > 0) {
  added <- do.call(rbind, new_rows)
  common_cols <- intersect(names(h), names(added))
  combined <- rbind(h[, common_cols], added[, common_cols])
  saveRDS(combined, HISTORY_PATH)
  cat(sprintf("\n追記: %d行（新規） / 累計: %d行\n", nrow(added), nrow(combined)))
}
