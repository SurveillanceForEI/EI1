setwd(if (basename(getwd()) == "scripts") ".." else ".")
source("R/hokenjo_fetch/hokenjo_fetch_schema.R")
source("R/hokenjo_fetch/pdf_table_utils.R")
source("R/hokenjo_fetch/hyogo.R")
source("R/hokenjo_fetch/okayama.R")
HISTORY_PATH <- "data/hokenjo_history.rds"
h <- readRDS(HISTORY_PATH)
new_rows <- list()
status_log <- character(0)

# ---- 兵庫県: fetch_hyogo(year, week) ----
for (week in 1:52) {
  res <- tryCatch(fetch_hyogo(year = 2025, week = week), error = function(e) { status_log <<- c(status_log, sprintf("[NG] 兵庫県 第%d週: %s", week, conditionMessage(e))); NULL })
  if (is.null(res) || nrow(res) == 0) next
  res$week_num <- week
  res$fetched_at <- as.character(Sys.time())
  new_rows[[length(new_rows) + 1]] <- res
  status_log <- c(status_log, sprintf("[OK] 兵庫県 第%d週 (%d行) label=%s", week, nrow(res), unique(res$week_label)[1]))
}

# ---- 岡山県: 2025年ブロックのURLマップ ----
okayama_urls <- c(
"1"="382406.pdf","14"="388620.pdf","27"="393508.pdf","40"="397537.pdf",
"2"="382407.pdf","15"="391244.pdf","28"="393823.pdf","41"="397777.pdf",
"3"="382887.pdf","16"="389691.pdf","29"="394134.pdf","42"="398241.pdf",
"4"="383314.pdf","17"="389948.pdf","30"="394439.pdf","43"="399591.pdf",
"5"="383635.pdf","18"="390181.pdf","31"="394636.pdf","44"="400363.pdf",
"6"="394671.pdf","19"="390450.pdf","32"="395223.pdf","45"="400756.pdf",
"7"="394672.pdf","20"="391199.pdf","33"="395214.pdf","46"="401076.pdf",
"8"="394673.pdf","21"="391196.pdf","34"="395726.pdf","47"="401245.pdf",
"9"="394674.pdf","22"="391627.pdf","35"="396122.pdf","48"="401566.pdf",
"10"="394675.pdf","23"="391920.pdf","36"="396426.pdf","49"="401985.pdf",
"11"="394676.pdf","24"="392371.pdf","37"="396676.pdf","50"="402522.pdf",
"12"="394677.pdf","25"="392652.pdf","38"="396931.pdf","51"="402923.pdf",
"13"="394678.pdf","26"="393122.pdf","39"="397270.pdf","52"="403351.pdf"
)
for (wk_str in names(okayama_urls)) {
  week <- as.integer(wk_str)
  u <- paste0("https://www.pref.okayama.jp/uploaded/attachment/", okayama_urls[[wk_str]])
  res <- tryCatch(fetch_okayama(u), error = function(e) { status_log <<- c(status_log, sprintf("[NG] 岡山県 第%d週: %s", week, conditionMessage(e))); NULL })
  if (is.null(res) || nrow(res) == 0) next
  res$week_num <- week
  res$fetched_at <- as.character(Sys.time())
  new_rows[[length(new_rows) + 1]] <- res
  status_log <- c(status_log, sprintf("[OK] 岡山県 第%d週 (%d行) label=%s", week, nrow(res), unique(res$week_label)[1]))
}

cat("\n=== 実行ログ（2025年 第12弾：兵庫県・岡山県） ===\n")
cat(paste(status_log, collapse = "\n"), "\n")

if (length(new_rows) > 0) {
  added <- do.call(rbind, new_rows)
  common_cols <- intersect(names(h), names(added))
  combined <- rbind(h[, common_cols], added[, common_cols])
  saveRDS(combined, HISTORY_PATH)
  cat(sprintf("\n追記: %d行（新規） / 累計: %d行\n", nrow(added), nrow(combined)))
}
