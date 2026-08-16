setwd(if (basename(getwd()) == "scripts") ".." else ".")
source("R/hokenjo_fetch/hokenjo_fetch_schema.R")
source("R/hokenjo_fetch/pdf_table_utils.R")
source("R/hokenjo_fetch/nagano.R")
HISTORY_PATH <- "data/hokenjo_history.rds"
h <- readRDS(HISTORY_PATH)
# 長野県の既存2025年データ(あれば)を除去してから追加(重複防止)
h <- h[!(h$pref == "長野県" & grepl("^2025年", h$week_label)), ]

base <- "https://www.pref.nagano.lg.jp/shippei-kansen/kenko/kenko/kansensho/joho/documents/2025-%02dw%s.pdf"
new_rows <- list()
status_log <- character(0)

for (week in 1:52) {
  suffix <- if (week == 37) "-2" else ""
  u <- sprintf(base, week, suffix)
  res <- tryCatch(fetch_nagano_weekly_report(u, year = 2025, week_num = week),
                   error = function(e) { status_log <<- c(status_log, sprintf("[NG] 長野県 第%d週: %s", week, conditionMessage(e))); NULL })
  if (is.null(res) || nrow(res) == 0) next
  res$week_num <- week
  res$fetched_at <- as.character(Sys.time())
  new_rows[[length(new_rows) + 1]] <- res
  status_log <- c(status_log, sprintf("[OK] 長野県 第%d週 (%d行)", week, nrow(res)))
}

cat("\n=== 実行ログ(2025年 第24弾:長野県) ===\n")
cat(paste(status_log, collapse = "\n"), "\n")

if (length(new_rows) > 0) {
  added <- do.call(rbind, new_rows)
  common_cols <- intersect(names(h), names(added))
  combined <- rbind(h[, common_cols], added[, common_cols])
  saveRDS(combined, HISTORY_PATH)
  cat(sprintf("\n追記: %d行(新規) / 累計: %d行\n", nrow(added), nrow(combined)))
}
