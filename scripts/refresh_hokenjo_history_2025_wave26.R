setwd(if (basename(getwd()) == "scripts") ".." else ".")
source("R/hokenjo_fetch/hokenjo_fetch_schema.R")
source("R/hokenjo_fetch/pdf_table_utils.R")
source("R/hokenjo_fetch/ehime.R")
HISTORY_PATH <- "data/hokenjo_history.rds"
h <- readRDS(HISTORY_PATH)

before_n <- sum(h$pref == "愛媛県")
h <- h[!(h$pref == "愛媛県" & is.na(h$week_label)), ]
cat("愛媛県の週表記NA行を削除:", before_n, "->", sum(h$pref=="愛媛県"), "\n")

hrefs <- c(
"1"="135182.pdf","2"="135972.pdf","3"="135973.pdf","4"="136864.pdf","5"="137423.pdf",
"6"="137880.pdf","7"="138697.pdf","8"="140932.pdf","9"="140931.pdf","10"="141532.pdf",
"11"="142578.pdf","12"="143524.pdf","13"="144479.pdf","14"="145112.pdf","15"="145635.pdf",
"16"="146249.pdf","17"="146589.pdf","18"="147050.pdf","19"="147541.pdf","20"="147833.pdf",
"21"="148407.pdf","22"="149172.pdf","23"="149786.pdf","24"="150534.pdf","25"="150891.pdf",
"26"="151991.pdf"
)

new_rows <- list()
status_log <- character(0)
for (wk_str in names(hrefs)) {
  week <- as.integer(wk_str)
  u <- paste0("https://www.pref.ehime.jp/uploaded/attachment/", hrefs[[wk_str]])
  res <- tryCatch(fetch_ehime(u), error = function(e) { status_log <<- c(status_log, sprintf("[NG] 第%d週: %s", week, conditionMessage(e))); NULL })
  if (is.null(res) || nrow(res) == 0) next
  res$week_num <- week
  res$fetched_at <- as.character(Sys.time())
  new_rows[[length(new_rows) + 1]] <- res
  status_log <- c(status_log, sprintf("[OK] 第%d週 (%d行) label=%s", week, nrow(res), unique(res$week_label)[1]))
}

cat("\n=== 実行ログ(2025年 第26弾:愛媛県 再取得) ===\n")
cat(paste(status_log, collapse = "\n"), "\n")

if (length(new_rows) > 0) {
  added <- do.call(rbind, new_rows)
  common_cols <- intersect(names(h), names(added))
  combined <- rbind(h[, common_cols], added[, common_cols])
  saveRDS(combined, HISTORY_PATH)
  cat(sprintf("\n追記: %d行(新規) / 累計: %d行\n", nrow(added), nrow(combined)))
}
