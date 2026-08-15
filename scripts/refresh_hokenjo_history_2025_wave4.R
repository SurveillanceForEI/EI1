setwd(if (basename(getwd()) == "scripts") ".." else ".")
source("R/hokenjo_fetch/hokenjo_fetch_schema.R")
source("R/hokenjo_fetch/pdf_table_utils.R")
FETCH_DIR <- "R/hokenjo_fetch"
for (f in list.files(FETCH_DIR, pattern = "\\.R$", full.names = TRUE)) {
  if (basename(f) %in% c("hokenjo_fetch_schema.R", "pdf_table_utils.R")) next
  source(f)
}
HISTORY_PATH <- "data/hokenjo_history.rds"
extract_year_num <- function(week_label) {
  if (is.na(week_label)) return(NA_integer_)
  wl <- chartr("０１２３４５６７８９", "0123456789", week_label)
  m <- regmatches(wl, regexec("([0-9]{4})年", wl))[[1]]
  if (length(m) >= 2) return(as.integer(m[2]))
  m <- regmatches(wl, regexec("令和\\s*([0-9]{1,2})\\s*年", wl))[[1]]
  if (length(m) >= 2) return(as.integer(m[2]) + 2018)
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

# ---- 栃木県: intidwr2025{WW}.pdf ----
for (week in 1:52) {
  key <- paste("栃木県", 2025, week)
  if (key %in% existing_keys) next
  u <- sprintf("https://www.pref.tochigi.lg.jp/e60/tidc/documents/intidwr2025%02d.pdf", week)
  res <- tryCatch(fetch_tochigi(u), error = function(e) NULL)
  add_result(res, "栃木県", week)
}

# ---- 高知県: アーカイブページから取得した個別URL（規則性なし） ----
kochi_urls <- list(
  `2`="file_2025116493849_1.pdf", `3`="file_2025122311143_1.pdf", `4`="file_2025129313577_1.pdf",
  `5`="file_2025253104239_1.pdf", `6`="file_2025213412053_1.pdf", `7`="file_2025219313476_1.pdf",
  `8`="file_20252274113023_1.pdf", `9`="file_2025353112614_1.pdf", `10`="file_2025312313353_1.pdf",
  `11`="file_202531931192_1.pdf", `12`="file_20253263112910_1.pdf", `13`="file_202542393540_1.pdf",
  `14`="file_202541049181_1.pdf", `15`="file_20254222142229_1.pdf", `16`="file_2025424415222_1.pdf",
  `17`="file_202552592757_1.pdf", `18`="file_202559594924_1.pdf", `19`="file_20255154142230_1.pdf",
  `20`="file_202552241240_1.pdf", `21`="file_20255294103634_1.pdf", `22`="file_202565414301_1.pdf",
  `23`="file_202561249260_1.pdf", `24`="file_20256194105243_1.pdf", `25`="file_2025734141514_1.pdf",
  `26`="file_2025734141750_1.pdf", `27`="file_2025710413636_1.pdf", `28`="file_20257174114824_1.pdf",
  `29`="file_20257244112429_1.pdf", `30`="file_2025731492136_1.pdf", `31`="file_20258741373_1.pdf",
  `32`="file_20258144105912_1.pdf", `33`="file_20258214114639_1.pdf", `34`="file_2025828413237_1.pdf",
  `35`="file_20259819211_1.pdf", `36`="file_2025911491843_1.pdf", `37`="file_20259184132653_1.pdf",
  `38`="file_202592659232_1.pdf", `39`="file_20251013134019_1.pdf", `40`="file_20251094102819_1.pdf",
  `41`="file_20251016410290_1.pdf", `42`="file_2025102349375_1.pdf", `43`="file_20251030411567_1.pdf",
  `44`="file_20251164151336_1.pdf", `45`="file_202511134132648_1.pdf", `46`="file_20251121510586_1.pdf",
  `47`="file_202511274102625_1.pdf", `48`="file_202512449194_1.pdf", `49`="file_20251211413417_1.pdf",
  `50`="file_202512184141143_1.pdf", `51`="file_20251225495254_1.pdf"
)
for (wk_str in names(kochi_urls)) {
  week <- as.integer(wk_str)
  key <- paste("高知県", 2025, week)
  if (key %in% existing_keys) next
  u <- paste0("https://www.pref.kochi.lg.jp/doc/2025011000151/file_contents/", kochi_urls[[wk_str]])
  res <- tryCatch(fetch_kochi(u), error = function(e) NULL)
  add_result(res, "高知県", week)
}

cat("\n=== 実行ログ（2025年 第4弾） ===\n")
cat(paste(status_log, collapse = "\n"), "\n")

if (length(new_rows) > 0) {
  h <- readRDS(HISTORY_PATH)
  added <- do.call(rbind, new_rows)
  common_cols <- intersect(names(h), names(added))
  combined <- rbind(h[, common_cols], added[, common_cols])
  saveRDS(combined, HISTORY_PATH)
  cat(sprintf("\n追記: %d行（新規） / 累計: %d行\n", nrow(added), nrow(combined)))
}
