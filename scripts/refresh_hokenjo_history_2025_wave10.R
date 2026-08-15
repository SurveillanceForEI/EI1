setwd(if (basename(getwd()) == "scripts") ".." else ".")
source("R/hokenjo_fetch/hokenjo_fetch_schema.R")
source("R/hokenjo_fetch/pdf_table_utils.R")
source("R/hokenjo_fetch/tokushima.R")
HISTORY_PATH <- "data/hokenjo_history.rds"
h <- readRDS(HISTORY_PATH)

hrefs <- c(
"1"="/file/attachment/991089.pdf","2"="/file/attachment/991091.pdf","3"="/file/attachment/991094.pdf",
"4"="/file/attachment/991092.pdf","5"="/file/attachment/991090.pdf","6"="/file/attachment/991093.pdf",
"7"="/file/attachment/991095.pdf","8"="/file/attachment/991096.pdf","9"="/file/attachment/991098.pdf",
"10"="/file/attachment/991099.pdf","11"="/file/attachment/991100.pdf","12"="/file/attachment/991101.pdf",
"13"="/file/attachment/991102.pdf","14"="/file/attachment/991103.pdf","15"="/file/attachment/991104.pdf",
"16"="/file/attachment/991105.pdf","17"="/file/attachment/991180.pdf","18"="/file/attachment/992218.pdf",
"19"="/file/attachment/993470.pdf","20"="/file/attachment/994978.pdf","21"="/file/attachment/996357.pdf",
"22"="/file/attachment/997541.pdf","23"="/file/attachment/998610.pdf","24"="/file/attachment/1000256.pdf",
"25"="/file/attachment/1001272.pdf","26"="/file/attachment/1002444.pdf","27"="/file/attachment/1004117.pdf",
"28"="/file/attachment/1005204.pdf","29"="/file/attachment/1005841.pdf","30"="/file/attachment/1007097.pdf",
"31"="/file/attachment/1008847.pdf","32"="/file/attachment/1009811.pdf","33"="/file/attachment/1010860.pdf",
"34"="/file/attachment/1011884.pdf","35"="/file/attachment/1013410.pdf","36"="/file/attachment/1014587.pdf",
"37"="/file/attachment/1015388.pdf","38"="/file/attachment/1016089.pdf","39"="/file/attachment/1017226.pdf",
"40"="/file/attachment/1018875.pdf","41"="/file/attachment/1019338.pdf","42"="/file/attachment/1020220.pdf",
"43"="/file/attachment/1021110.pdf","44"="/file/attachment/1021894.pdf","45"="/file/attachment/1022779.pdf",
"46"="/file/attachment/1024250.pdf","47"="/file/attachment/1024964.pdf","48"="/file/attachment/1026282.pdf",
"49"="/file/attachment/1027520.pdf","50"="/file/attachment/1028811.pdf","51"="/file/attachment/1029833.pdf",
"52"="/file/attachment/1030628.pdf"
)

new_rows <- list()
status_log <- character(0)
for (wk_str in names(hrefs)) {
  week <- as.integer(wk_str)
  u <- paste0("https://www.pref.tokushima.lg.jp", hrefs[[wk_str]])
  res <- tryCatch(fetch_tokushima(u), error = function(e) { status_log <<- c(status_log, sprintf("[NG] 第%d週: %s", week, conditionMessage(e))); NULL })
  if (is.null(res) || nrow(res) == 0) next
  res$week_num <- week
  res$fetched_at <- as.character(Sys.time())
  new_rows[[length(new_rows) + 1]] <- res
  status_log <- c(status_log, sprintf("[OK] 第%d週 (%d行) label=%s", week, nrow(res), unique(res$week_label)[1]))
}

cat("\n=== 実行ログ（2025年 第10弾：徳島県） ===\n")
cat(paste(status_log, collapse = "\n"), "\n")

if (length(new_rows) > 0) {
  added <- do.call(rbind, new_rows)
  common_cols <- intersect(names(h), names(added))
  combined <- rbind(h[, common_cols], added[, common_cols])
  saveRDS(combined, HISTORY_PATH)
  cat(sprintf("\n追記: %d行（新規） / 累計: %d行\n", nrow(added), nrow(combined)))
}
