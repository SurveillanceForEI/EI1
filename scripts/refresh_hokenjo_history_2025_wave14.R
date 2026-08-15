setwd(if (basename(getwd()) == "scripts") ".." else ".")
source("R/hokenjo_fetch/hokenjo_fetch_schema.R")
source("R/hokenjo_fetch/pdf_table_utils.R")
source("R/hokenjo_fetch/yamanashi.R")
source("R/hokenjo_fetch/fukushima.R")
HISTORY_PATH <- "data/hokenjo_history.rds"
h <- readRDS(HISTORY_PATH)
new_rows <- list()
status_log <- character(0)

# ---- 山梨県: 2つのバンドルPDF（1-26週, 26-52週）----
for (u in c("https://www.pref.yamanashi.jp/documents/92705/202501w26w.pdf",
            "https://www.pref.yamanashi.jp/documents/92705/202526w52w.pdf")) {
  res <- tryCatch(fetch_yamanashi_bundle(u), error = function(e) { status_log <<- c(status_log, sprintf("[NG] 山梨県 %s: %s", u, conditionMessage(e))); NULL })
  if (is.null(res) || nrow(res) == 0) next
  wl <- gsub("[^0-9]", "", regmatches(res$week_label, regexpr("第[0-9]+週", res$week_label)))
  res$week_num <- as.integer(wl)
  res$fetched_at <- as.character(Sys.time())
  new_rows[[length(new_rows) + 1]] <- res
  status_log <- c(status_log, sprintf("[OK] 山梨県 %s (%d行, 週%s)", u, nrow(res), paste(range(res$week_num), collapse="-")))
}

# ---- 福島県: 2025年ブロック（52週）----
fukushima_ids <- c(
  665617,666271,668666,668579,670681,671732,673071,674350,676317,678182,
  679572,681159,683441,684582,686084,686746,687727,688555,689698,690927,
  692037,712175,694444,695630,696991,698550,699525,700669,702464,712176,
  703513,704134,704757,705573,706703,712173,708356,709101,712174,711550,
  712338,713556,714154,714907,715875,716949,717771,718405,719265,719992,
  722437,722438
)
for (i in seq_along(fukushima_ids)) {
  week <- i
  u <- sprintf("https://www.pref.fukushima.lg.jp/uploaded/attachment/%d.pdf", fukushima_ids[i])
  res <- tryCatch(fetch_fukushima(u), error = function(e) { status_log <<- c(status_log, sprintf("[NG] 福島県 第%d週: %s", week, conditionMessage(e))); NULL })
  if (is.null(res) || nrow(res) == 0) next
  res$week_num <- week
  res$fetched_at <- as.character(Sys.time())
  new_rows[[length(new_rows) + 1]] <- res
  status_log <- c(status_log, sprintf("[OK] 福島県 第%d週 (%d行) label=%s", week, nrow(res), unique(res$week_label)[1]))
}

cat("\n=== 実行ログ（2025年 第14弾：山梨県・福島県） ===\n")
cat(paste(status_log, collapse = "\n"), "\n")

if (length(new_rows) > 0) {
  added <- do.call(rbind, new_rows)
  common_cols <- intersect(names(h), names(added))
  combined <- rbind(h[, common_cols], added[, common_cols])
  saveRDS(combined, HISTORY_PATH)
  cat(sprintf("\n追記: %d行（新規） / 累計: %d行\n", nrow(added), nrow(combined)))
}
