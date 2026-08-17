setwd(if (basename(getwd()) == "scripts") ".." else ".")
source("R/hokenjo_fetch/hokenjo_fetch_schema.R")
source("R/hokenjo_fetch/pdf_table_utils.R")
source("R/hokenjo_fetch/shizuoka.R")
HISTORY_PATH <- "data/hokenjo_history.rds"
h <- readRDS(HISTORY_PATH)

# 静岡県の週表記(表紙の空白入りフォーマット)を正規表現が拾えず、文書内の
# 無関係な年（2011/2019/2024等）や誤った週の値を掴んでいたバグを修正した
# ため、既存の静岡県2025年データを全て削除して取り直す
before_n <- sum(h$pref == "静岡県")
bad_label <- !is.na(h$week_label) & grepl("^2025年|^2011年|^2019年|^2024年", h$week_label)
h <- h[!(h$pref == "静岡県" & bad_label), ]
cat("静岡県の既存行を削除:", before_n, "->", sum(h$pref=="静岡県"), "\n")

shizuoka_urls <- function(week) c(
  sprintf("https://www.pref.shizuoka.jp/_res/projects/default_project/_page_/001/068/844/2025idwr%d.pdf", week),
  sprintf("https://www.pref.shizuoka.jp/_res/projects/default_project/_page_/001/068/844/2025idwr%d-2.pdf", week)
)

new_rows <- list()
status_log <- character(0)
for (week in 1:52) {
  res <- NULL
  for (u in shizuoka_urls(week)) {
    res <- tryCatch(fetch_shizuoka(u), error = function(e) NULL)
    if (!is.null(res) && nrow(res) > 0) break
  }
  if (is.null(res) || nrow(res) == 0) {
    status_log <- c(status_log, sprintf("[NG] 静岡県 第%d週", week))
    next
  }
  res$week_num <- week
  res$fetched_at <- as.character(Sys.time())
  new_rows[[length(new_rows) + 1]] <- res
  status_log <- c(status_log, sprintf("[OK] 静岡県 第%d週 (%d行) label=%s", week, nrow(res), unique(res$week_label)[1]))
}

cat("\n=== 実行ログ(2025年 第25弾:静岡県 再取得) ===\n")
cat(paste(status_log, collapse = "\n"), "\n")

if (length(new_rows) > 0) {
  added <- do.call(rbind, new_rows)
  common_cols <- intersect(names(h), names(added))
  combined <- rbind(h[, common_cols], added[, common_cols])
  saveRDS(combined, HISTORY_PATH)
  cat(sprintf("\n追記: %d行(新規) / 累計: %d行\n", nrow(added), nrow(combined)))
}
