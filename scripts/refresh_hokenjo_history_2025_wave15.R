setwd(if (basename(getwd()) == "scripts") ".." else ".")
source("R/hokenjo_fetch/hokenjo_fetch_schema.R")
source("R/hokenjo_fetch/pdf_table_utils.R")
source("R/hokenjo_fetch/nagano.R")
source("R/hokenjo_fetch/akita.R")
HISTORY_PATH <- "data/hokenjo_history.rds"
h <- readRDS(HISTORY_PATH)
new_rows <- list()
status_log <- character(0)

# ---- 長野県: {YEAR}-{WW}w_data.pdf（合併週は_XXm_data.pdf等のサフィックス違いあり）----
nagano_candidates <- function(week) {
  wwp <- sprintf("%02d", week)
  c(
    sprintf("https://www.pref.nagano.lg.jp/shippei-kansen/kenko/kenko/kansensho/joho/documents/2025-%sw_data.pdf", wwp),
    sprintf("https://www.pref.nagano.lg.jp/shippei-kansen/kenko/kenko/kansensho/joho/documents/2025-%sw_01m_data.pdf", wwp),
    sprintf("https://www.pref.nagano.lg.jp/shippei-kansen/kenko/kenko/kansensho/joho/documents/2025-%sw-01m_data.pdf", wwp)
  )
}
for (week in 1:52) {
  res <- NULL
  for (u in nagano_candidates(week)) {
    res <- tryCatch(fetch_nagano(u), error = function(e) NULL)
    if (!is.null(res)) break
  }
  if (is.null(res) || nrow(res) == 0) { status_log <- c(status_log, sprintf("[--] 長野県 第%d週", week)); next }
  res$week_num <- week
  res$fetched_at <- as.character(Sys.time())
  new_rows[[length(new_rows) + 1]] <- res
  status_log <- c(status_log, sprintf("[OK] 長野県 第%d週 (%d行) label=%s", week, nrow(res), unique(res$week_label)[1]))
}

# ---- 秋田県: RAPIDS_25{WW}.pdf ----
for (week in 1:52) {
  u <- sprintf("https://idsc.pref.akita.jp/kss/back/RAPIDS_25%02d.pdf", week)
  res <- tryCatch(fetch_akita(u), error = function(e) { status_log <<- c(status_log, sprintf("[NG] 秋田県 第%d週: %s", week, conditionMessage(e))); NULL })
  if (is.null(res) || nrow(res) == 0) next
  res$week_num <- week
  res$fetched_at <- as.character(Sys.time())
  new_rows[[length(new_rows) + 1]] <- res
  status_log <- c(status_log, sprintf("[OK] 秋田県 第%d週 (%d行) label=%s", week, nrow(res), unique(res$week_label)[1]))
}

cat("\n=== 実行ログ（2025年 第15弾：長野県・秋田県） ===\n")
cat(paste(status_log, collapse = "\n"), "\n")

if (length(new_rows) > 0) {
  added <- do.call(rbind, new_rows)
  common_cols <- intersect(names(h), names(added))
  combined <- rbind(h[, common_cols], added[, common_cols])
  saveRDS(combined, HISTORY_PATH)
  cat(sprintf("\n追記: %d行（新規） / 累計: %d行\n", nrow(added), nrow(combined)))
}
