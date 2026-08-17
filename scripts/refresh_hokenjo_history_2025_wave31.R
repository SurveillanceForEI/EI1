setwd(if (basename(getwd()) == "scripts") ".." else ".")
source("R/hokenjo_fetch/hokenjo_fetch_schema.R")
source("R/hokenjo_fetch/pdf_table_utils.R")
source("R/hokenjo_fetch/hiroshima.R")
HISTORY_PATH <- "data/hokenjo_history.rds"
h <- readRDS(HISTORY_PATH)

week_ids <- c(
  1,604120, 2,604393, 3,604880, 4,605732, 5,606366,
  10,613251, 11,614939, 12,616295, 13,617896, 14,618742, 15,620154,
  16,621018, 17,621608, 18,622309, 19,623438, 20,624424, 21,625253,
  22,625962, 23,627959, 24,628097, 25,628934, 26,629933, 27,630847,
  28,631638, 29,632386, 30,632937, 31,633816, 32,634322, 33,634867,
  34,635667, 35,636460, 36,637798, 37,638267, 38,638954, 39,639892,
  40,640640, 41,641226, 42,642578, 43,644290, 44,645531, 45,646293,
  46,647174, 47,647734, 48,648413, 49,649057, 50,649806, 51,650516,
  52,650951
)
week_ids <- matrix(week_ids, ncol = 2, byrow = TRUE)

new_rows <- list()
status_log <- character(0)
for (i in seq_len(nrow(week_ids))) {
  wk <- week_ids[i, 1]
  id <- week_ids[i, 2]
  u <- sprintf("https://www.pref.hiroshima.lg.jp/uploaded/attachment/%d.pdf", id)
  res <- tryCatch(fetch_hiroshima(u), error = function(e) { status_log <<- c(status_log, sprintf("[NG] 第%d週: %s", wk, conditionMessage(e))); NULL })
  if (is.null(res) || nrow(res) == 0) next
  res$week_num <- wk
  res$fetched_at <- as.character(Sys.time())
  new_rows[[length(new_rows) + 1]] <- res
  status_log <- c(status_log, sprintf("[OK] 第%d週 (%d行) disease=%s", wk, nrow(res), unique(res$disease)[1]))
}

cat("\n=== 実行ログ(広島県 2025年分取得) ===\n")
cat(paste(status_log, collapse = "\n"), "\n")

if (length(new_rows) > 0) {
  added <- do.call(rbind, new_rows)
  common_cols <- intersect(names(h), names(added))
  combined <- rbind(h[, common_cols], added[, common_cols])
  saveRDS(combined, HISTORY_PATH)
  cat(sprintf("\n追記: %d行(新規) / 累計: %d行\n", nrow(added), nrow(combined)))
}
