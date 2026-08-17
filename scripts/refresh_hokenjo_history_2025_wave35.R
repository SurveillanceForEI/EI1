setwd(if (basename(getwd()) == "scripts") ".." else ".")
source("R/hokenjo_fetch/hokenjo_fetch_schema.R")
source("R/hokenjo_fetch/pdf_table_utils.R")
source("R/hokenjo_fetch/wakayama.R")
HISTORY_PATH <- "data/hokenjo_history.rds"
h <- readRDS(HISTORY_PATH)

urls <- c(
  "WIDR202452202501.pdf", "WIDR202502Dec.pdf", "WIDR202503.pdf", "WIDR202504.pdf", "WIDR202505.pdf",
  "WIDR202506Jan.pdf", "WIDR202507.pdf", "WIDR202508.pdf", "WIDR202509.pdf", "WIDR202510Feb.pdf",
  "WIDR202511.pdf", "WIDR202512.pdf", "WIDR202513.pdf", "WIDR202514.pdf", "WIDR202515Mar.pdf",
  "WIDR202516.pdf", "WIDR202517.pdf", "WIDR202518.pdf", "WIDR202519Apr.pdf", "WIDR202520.pdf",
  "WIDR202521.pdf", "WIDR202522.pdf", "WIDR202523May.pdf", "WIDR202524.pdf", "WIDR202525.pdf",
  "WIDR202526.pdf", "WIDR202527.pdf", "WIDR202528Jun.pdf", "WIDR202529.pdf", "WIDR202530.pdf",
  "WIDR202531.pdf", "WIDR202532Jul.pdf", "WIDR202533.pdf", "WIDR202534.pdf", "WIDR202535.pdf",
  "WIDR202536.pdf", "WIDR202537Aug.pdf", "WIDR202538.pdf", "WIDR202539.pdf", "WIDR202540.pdf",
  "WIDR202541Sep.pdf", "WIDR202542.pdf", "WIDR202543.pdf", "WIDR202544.pdf", "WIDR202545Oct.pdf",
  "WIDR202546.pdf", "WIDR202547.pdf", "WIDR202548.pdf", "WIDR202549.pdf", "WIDR202550Nov.pdf",
  "WIDR202551.pdf"
)
stopifnot(length(urls) == 51)
base <- "https://www.pref.wakayama.lg.jp/prefg/031801/idsw/khdc/d00222558_d/fil/"

new_rows <- list()
status_log <- character(0)
for (wk in seq_along(urls)) {
  u <- paste0(base, urls[wk])
  res <- tryCatch(fetch_wakayama(u), error = function(e) { status_log <<- c(status_log, sprintf("[NG] 第%d週: %s", wk, conditionMessage(e))); NULL })
  if (is.null(res) || nrow(res) == 0) next
  res$week_num <- wk
  res$fetched_at <- as.character(Sys.time())
  new_rows[[length(new_rows) + 1]] <- res
  status_log <- c(status_log, sprintf("[OK] 第%d週 (%d行) label=%s", wk, nrow(res), unique(res$week_label)[1]))
}

cat("\n=== 実行ログ(和歌山県 2025年分取得) ===\n")
cat(paste(status_log, collapse = "\n"), "\n")

if (length(new_rows) > 0) {
  added <- do.call(rbind, new_rows)
  common_cols <- intersect(names(h), names(added))
  combined <- rbind(h[, common_cols], added[, common_cols])
  saveRDS(combined, HISTORY_PATH)
  cat(sprintf("\n追記: %d行(新規) / 累計: %d行\n", nrow(added), nrow(combined)))
}
