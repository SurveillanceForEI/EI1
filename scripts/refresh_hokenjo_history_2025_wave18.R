setwd(if (basename(getwd()) == "scripts") ".." else ".")
source("R/hokenjo_fetch/hokenjo_fetch_schema.R")
source("R/hokenjo_fetch/pdf_table_utils.R")
source("R/hokenjo_fetch/shizuoka.R")
source("R/hokenjo_fetch/kochi.R")
source("R/hokenjo_fetch/kagawa.R")
HISTORY_PATH <- "data/hokenjo_history.rds"
h <- readRDS(HISTORY_PATH)
new_rows <- list()
status_log <- character(0)
add_result <- function(res, pref, week) {
  if (!is.null(res) && is.data.frame(res) && nrow(res) > 0) {
    res$week_num <- week
    res$fetched_at <- as.character(Sys.time())
    new_rows[[length(new_rows) + 1]] <<- res
    status_log <<- c(status_log, sprintf("[OK] %s 第%d週 (%d行)", pref, week, nrow(res)))
  } else {
    status_log <<- c(status_log, sprintf("[--] %s 第%d週", pref, week))
  }
}

# ---- 静岡県: 判明した個別ファイル名 ----
shizuoka_hrefs <- c(
  "1"="2025idwr1-2.pdf","2"="2025idwr2-1.pdf","3"="2025idwr3.pdf","4"="2025idwr4-2.pdf",
  "5"="2025idwr5-2.pdf","6"="2025idwr6.pdf","7"="2025idwr7-2.pdf","8"="2025idwr8.pdf",
  "9"="2025idwr9-2.pdf","10"="2025idwr10-2.pdf","12"="2025idwr12-2.pdf","13"="2025idwr13.pdf",
  "16"="2025idwr16.pdf","17"="250502idwr.pdf","18"="250512idwr1802.pdf","19"="250516idwr19.pdf",
  "20"="2025idwr20.pdf","21"="2025idwr21.pdf","22"="2025idwr22.pdf","23"="2025idwr23-2.pdf",
  "24"="2025idwr24-2.pdf","25"="2025idwr25.pdf","26"="2025idwr26-2.pdf","27"="2025idwr27-2.pdf",
  "35"="25idwr35.pdf","36"="250912idwr.pdf","37"="250919idwr.pdf","38"="25idwr38.pdf",
  "40"="25idwr40.pdf","43"="43idwr.pdf","46"="2546idwr.pdf"
)
for (wk_str in names(shizuoka_hrefs)) {
  week <- as.integer(wk_str)
  u <- paste0("https://www.pref.shizuoka.jp/_res/projects/default_project/_page_/001/068/844/", shizuoka_hrefs[[wk_str]])
  res <- tryCatch(fetch_shizuoka(u), error = function(e) { status_log <<- c(status_log, sprintf("[NG] 静岡県 第%d週: %s", week, conditionMessage(e))); NULL })
  add_result(res, "静岡県", week)
}

# ---- 高知県: 前回パーサーエラーだった週の再試行 ----
kochi_retry <- c(
  "7"="file_2025219313476_1.pdf","11"="file_202531931192_1.pdf","17"="file_202552592757_1.pdf",
  "25"="file_2025734141514_1.pdf","29"="file_20257244112429_1.pdf","33"="file_20258214114639_1.pdf"
)
for (wk_str in names(kochi_retry)) {
  week <- as.integer(wk_str)
  u <- paste0("https://www.pref.kochi.lg.jp/doc/2025011000151/file_contents/", kochi_retry[[wk_str]])
  res <- tryCatch(fetch_kochi(u), error = function(e) { status_log <<- c(status_log, sprintf("[NG] 高知県 第%d週: %s", week, conditionMessage(e))); NULL })
  add_result(res, "高知県", week)
}

# ---- 香川県: 判明した個別ファイル名 ----
kagawa_hrefs <- c("10"="2025syuuhou10-2.pdf", "15"="2025kansensyuhou15.pdf")
for (wk_str in names(kagawa_hrefs)) {
  week <- as.integer(wk_str)
  u <- paste0("https://www.pref.kagawa.lg.jp/documents/7135/", kagawa_hrefs[[wk_str]])
  res <- tryCatch(fetch_kagawa(u), error = function(e) { status_log <<- c(status_log, sprintf("[NG] 香川県 第%d週: %s", week, conditionMessage(e))); NULL })
  add_result(res, "香川県", week)
}

cat("\n=== 実行ログ（2025年 第18弾：静岡・高知・香川） ===\n")
cat(paste(status_log, collapse = "\n"), "\n")

if (length(new_rows) > 0) {
  added <- do.call(rbind, new_rows)
  common_cols <- intersect(names(h), names(added))
  combined <- rbind(h[, common_cols], added[, common_cols])
  saveRDS(combined, HISTORY_PATH)
  cat(sprintf("\n追記: %d行（新規） / 累計: %d行\n", nrow(added), nrow(combined)))
}
