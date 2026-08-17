setwd(if (basename(getwd()) == "scripts") ".." else ".")
source("R/hokenjo_fetch/hokenjo_fetch_schema.R")
source("R/hokenjo_fetch/pdf_table_utils.R")
row_text <- function(rdf) paste(rdf$text, collapse = " ")
source("R/hokenjo_fetch/kochi.R")
HISTORY_PATH <- "data/hokenjo_history.rds"
h <- readRDS(HISTORY_PATH)

before_n <- sum(h$pref == "高知県")
h <- h[!(h$pref == "高知県" & is.na(h$week_label)), ]
cat("高知県の週表記NA行を削除:", before_n, "->", sum(h$pref=="高知県"), "\n")

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

new_rows <- list()
status_log <- character(0)
for (wk_str in names(kochi_urls)) {
  week <- as.integer(wk_str)
  u <- paste0("https://www.pref.kochi.lg.jp/doc/2025011000151/file_contents/", kochi_urls[[wk_str]])
  res <- tryCatch(fetch_kochi(u), error = function(e) { status_log <<- c(status_log, sprintf("[NG] 第%d週: %s", week, conditionMessage(e))); NULL })
  if (is.null(res) || nrow(res) == 0) next
  res$week_num <- week
  res$fetched_at <- as.character(Sys.time())
  new_rows[[length(new_rows) + 1]] <- res
  status_log <- c(status_log, sprintf("[OK] 第%d週 (%d行) label=%s", week, nrow(res), unique(res$week_label)[1]))
}

cat("\n=== 実行ログ(2025年 第27弾:高知県 再取得) ===\n")
cat(paste(status_log, collapse = "\n"), "\n")

if (length(new_rows) > 0) {
  added <- do.call(rbind, new_rows)
  common_cols <- intersect(names(h), names(added))
  combined <- rbind(h[, common_cols], added[, common_cols])
  saveRDS(combined, HISTORY_PATH)
  cat(sprintf("\n追記: %d行(新規) / 累計: %d行\n", nrow(added), nrow(combined)))
}
