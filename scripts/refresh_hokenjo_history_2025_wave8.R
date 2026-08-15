setwd(if (basename(getwd()) == "scripts") ".." else ".")
source("R/hokenjo_fetch/hokenjo_fetch_schema.R")
source("R/hokenjo_fetch/pdf_table_utils.R")
source("R/hokenjo_fetch/kumamoto.R")
HISTORY_PATH <- "data/hokenjo_history.rds"
extract_year_num <- function(week_label) {
  if (is.na(week_label)) return(NA_integer_)
  wl <- chartr("０１２３４５６７８９", "0123456789", week_label)
  m <- regmatches(wl, regexec("([0-9]{4})年", wl))[[1]]
  if (length(m) >= 2) return(as.integer(m[2]))
  NA_integer_
}
make_key <- function(pref, week_num, week_label) {
  yr <- extract_year_num(week_label)
  if (!is.na(week_num) && !is.na(yr)) paste(pref, yr, week_num)
  else if (!is.na(week_num)) paste(pref, "unkyear", week_num)
  else paste(pref, "label:", week_label)
}
h <- readRDS(HISTORY_PATH)
existing_keys <- mapply(make_key, h$pref, h$week_num, h$week_label)

urls <- c(
  "298334.pdf","298333.pdf","297717.pdf","302329.pdf","296585.pdf","296268.pdf",
  "295887.pdf","295527.pdf","295179.pdf","294744.pdf","294286.pdf","293314.pdf",
  "292800.pdf","292627.pdf","291139.pdf","290533.pdf","290157.pdf","289693.pdf",
  "289052.pdf","288532.pdf","288081.pdf","287678.pdf","287267.pdf","287726.pdf",
  "286280.pdf","285256.pdf","284717.pdf","284115.pdf","283242.pdf","282646.pdf",
  "282243.pdf","281350.pdf","280849.pdf","280843.pdf","280845.pdf","280846.pdf",
  "280847.pdf","280848.pdf","278268.pdf","277831.pdf","277342.pdf","276053.pdf",
  "274930.pdf","274187.pdf","272654.pdf","271622.pdf","271180.pdf","270908.pdf",
  "270428.pdf","269906.pdf","269900.pdf","268482.pdf","268463.pdf"
)
new_rows <- list()
status_log <- character(0)
for (fn in urls) {
  u <- paste0("https://www.pref.kumamoto.jp/uploaded/attachment/", fn)
  res <- tryCatch(fetch_kumamoto(u), error = function(e) { status_log <<- c(status_log, sprintf("[NG] %s: %s", fn, conditionMessage(e))); NULL })
  if (is.null(res) || nrow(res) == 0) { status_log <- c(status_log, sprintf("[--] %s (0行)", fn)); next }
  wk_num <- suppressWarnings(as.integer(regmatches(unique(res$week_label)[1], regexpr("(?<=第)[0-9]+(?=週)", unique(res$week_label)[1], perl=TRUE))))
  key <- make_key("熊本県", wk_num, unique(res$week_label)[1])
  if (key %in% existing_keys) { status_log <- c(status_log, sprintf("[==] %s 第%s週 (取得済み)", fn, wk_num)); next }
  res$week_num <- wk_num
  res$fetched_at <- as.character(Sys.time())
  new_rows[[length(new_rows) + 1]] <- res
  existing_keys <- c(existing_keys, key)
  status_log <- c(status_log, sprintf("[OK] %s 第%s週 (%d行)", fn, wk_num, nrow(res)))
}

cat("\n=== 実行ログ（2025年 第8弾：熊本県） ===\n")
cat(paste(status_log, collapse = "\n"), "\n")

if (length(new_rows) > 0) {
  h <- readRDS(HISTORY_PATH)
  added <- do.call(rbind, new_rows)
  common_cols <- intersect(names(h), names(added))
  combined <- rbind(h[, common_cols], added[, common_cols])
  saveRDS(combined, HISTORY_PATH)
  cat(sprintf("\n追記: %d行（新規） / 累計: %d行\n", nrow(added), nrow(combined)))
}
