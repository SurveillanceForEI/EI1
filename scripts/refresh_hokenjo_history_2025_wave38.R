setwd(if (basename(getwd()) == "scripts") ".." else ".")
source("R/hokenjo_fetch/hokenjo_fetch_schema.R")
source("R/hokenjo_fetch/pdf_table_utils.R")
source("R/hokenjo_fetch/kochi.R")
HISTORY_PATH <- "data/hokenjo_history.rds"
h <- readRDS(HISTORY_PATH)

# 週番号 -> URL, week_num（表示上の代表週）
items <- list(
  list(wk = 1,  url = "https://www.pref.kochi.lg.jp/doc/2024011500036/file_contents/file_2025183144021_1.pdf"),
  list(wk = 52, url = "https://www.pref.kochi.lg.jp/doc/2025011000151/file_contents/file_2026184132629_1.pdf"),
  list(wk = 2,  url = "https://www.pref.kochi.lg.jp/doc/2026011400132/file_contents/file_20261154145725_1.pdf"),
  list(wk = 8,  url = "https://www.pref.kochi.lg.jp/doc/2026011400132/file_contents/file_20262264133810_1.pdf"),
  list(wk = 10, url = "https://www.pref.kochi.lg.jp/doc/2026011400132/file_contents/file_20263124143025_1.pdf"),
  list(wk = 12, url = "https://www.pref.kochi.lg.jp/doc/2026011400132/file_contents/file_20263264112431_1.pdf"),
  list(wk = 14, url = "https://www.pref.kochi.lg.jp/doc/2026011400132/file_contents/file_202649492243_1.pdf"),
  list(wk = 19, url = "https://www.pref.kochi.lg.jp/doc/2026011400132/file_contents/file_20265144132929_1.pdf"),
  list(wk = 23, url = "https://www.pref.kochi.lg.jp/doc/2026011400132/file_contents/file_20266114114436_1.pdf"),
  list(wk = 28, url = "https://www.pref.kochi.lg.jp/doc/2026011400132/file_contents/file_2026716494238_1.pdf"),
  list(wk = 30, url = "https://www.pref.kochi.lg.jp/doc/2026011400132/file_contents/file_2026730413750_1.pdf")
)

new_rows <- list()
status_log <- character(0)
for (it in items) {
  res <- tryCatch(fetch_kochi(it$url), error = function(e) { status_log <<- c(status_log, sprintf("[NG] 週%s: %s", it$wk, conditionMessage(e))); NULL })
  if (is.null(res) || nrow(res) == 0) next
  res$week_num <- it$wk
  res$fetched_at <- as.character(Sys.time())
  new_rows[[length(new_rows) + 1]] <- res
  status_log <- c(status_log, sprintf("[OK] 週%s (%d行) label=%s", it$wk, nrow(res), unique(res$week_label)[1]))
}

cat("\n=== 実行ログ(高知県 歯抜け週取得) ===\n")
cat(paste(status_log, collapse = "\n"), "\n")

if (length(new_rows) > 0) {
  added <- do.call(rbind, new_rows)
  common_cols <- intersect(names(h), names(added))
  combined <- rbind(h[, common_cols], added[, common_cols])
  saveRDS(combined, HISTORY_PATH)
  cat(sprintf("\n追記: %d行(新規) / 累計: %d行\n", nrow(added), nrow(combined)))
}
