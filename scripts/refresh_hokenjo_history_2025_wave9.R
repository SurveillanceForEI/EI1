setwd(if (basename(getwd()) == "scripts") ".." else ".")
source("R/hokenjo_fetch/hokenjo_fetch_schema.R")
source("R/hokenjo_fetch/pdf_table_utils.R")
source("R/hokenjo_fetch/oita.R")
HISTORY_PATH <- "data/hokenjo_history.rds"
h <- readRDS(HISTORY_PATH)

hrefs <- c(
"52"="/uploaded/attachment/2258498.pdf","51"="/uploaded/attachment/2257927.pdf",
"50"="/uploaded/attachment/2257203.pdf","49"="/uploaded/attachment/2256856.pdf",
"48"="/uploaded/attachment/2256570.pdf","47"="/uploaded/attachment/2256131.pdf",
"46"="/uploaded/attachment/2255687.pdf","45"="/uploaded/attachment/2255330.pdf",
"44"="/uploaded/attachment/2254761.pdf","43"="/uploaded/attachment/2254375.pdf",
"42"="/uploaded/attachment/2254053.pdf","41"="/uploaded/attachment/2253783.pdf",
"40"="/uploaded/attachment/2253482.pdf","39"="/uploaded/attachment/2252308.pdf",
"38"="/uploaded/attachment/2251531.pdf","37"="/uploaded/attachment/2251185.pdf",
"36"="/uploaded/attachment/2250904.pdf","35"="/uploaded/attachment/2250535.pdf",
"34"="/uploaded/attachment/2249982.pdf","33"="/uploaded/attachment/2249681.pdf",
"32"="/uploaded/attachment/2249412.pdf","31"="/uploaded/attachment/2249075.pdf",
"30"="/uploaded/attachment/2248599.pdf","29"="/uploaded/attachment/2248149.pdf",
"28"="/uploaded/attachment/2247847.pdf","27"="/uploaded/attachment/2246964.pdf",
"26"="/uploaded/attachment/2246578.pdf","25"="/uploaded/attachment/2245827.pdf",
"24"="/uploaded/attachment/2245114.pdf","23"="/uploaded/attachment/2244639.pdf",
"22"="/uploaded/attachment/2244081.pdf","21"="/uploaded/attachment/2243395.pdf",
"20"="/uploaded/attachment/2242540.pdf","19"="/uploaded/attachment/2241999.pdf",
"18"="/uploaded/attachment/2241818.pdf","17"="/uploaded/attachment/2241817.pdf",
"16"="/uploaded/attachment/2240766.pdf","15"="/uploaded/attachment/2241934.pdf",
"14"="/uploaded/attachment/2241933.pdf","13"="/uploaded/attachment/2239006.pdf",
"12"="/uploaded/attachment/2237941.pdf","11"="/uploaded/attachment/2237148.pdf",
"10"="/uploaded/attachment/2235973.pdf","9"="/uploaded/attachment/2235112.pdf",
"8"="/uploaded/attachment/2234287.pdf","7"="/uploaded/attachment/2233897.pdf",
"6"="/uploaded/attachment/2233136.pdf","5"="/uploaded/attachment/2232781.pdf",
"4"="/uploaded/attachment/2232248.pdf","3"="/uploaded/attachment/2231904.pdf",
"2"="/uploaded/attachment/2231161.pdf","1"="/uploaded/attachment/2231040.pdf"
)

new_rows <- list()
status_log <- character(0)
for (wk_str in names(hrefs)) {
  week <- as.integer(wk_str)
  u <- paste0("https://www.pref.oita.jp", hrefs[[wk_str]])
  res <- tryCatch(fetch_oita(u), error = function(e) { status_log <<- c(status_log, sprintf("[NG] 第%d週: %s", week, conditionMessage(e))); NULL })
  if (is.null(res) || nrow(res) == 0) next
  res$week_num <- week
  res$fetched_at <- as.character(Sys.time())
  new_rows[[length(new_rows) + 1]] <- res
  status_log <- c(status_log, sprintf("[OK] 第%d週 (%d行) label=%s", week, nrow(res), unique(res$week_label)[1]))
}

cat("\n=== 実行ログ（2025年 第9弾：大分県） ===\n")
cat(paste(status_log, collapse = "\n"), "\n")

if (length(new_rows) > 0) {
  added <- do.call(rbind, new_rows)
  common_cols <- intersect(names(h), names(added))
  combined <- rbind(h[, common_cols], added[, common_cols])
  saveRDS(combined, HISTORY_PATH)
  cat(sprintf("\n追記: %d行（新規） / 累計: %d行\n", nrow(added), nrow(combined)))
}
