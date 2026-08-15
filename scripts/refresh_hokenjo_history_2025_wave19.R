setwd(if (basename(getwd()) == "scripts") ".." else ".")
source("R/hokenjo_fetch/hokenjo_fetch_schema.R")
source("R/hokenjo_fetch/pdf_table_utils.R")
source("R/hokenjo_fetch/saitama.R")
source("R/hokenjo_fetch/chiba.R")
source("R/hokenjo_fetch/nagasaki.R")
source("R/hokenjo_fetch/niigata.R")
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

# ---- 埼玉県 ----
saitama_hrefs <- c("15"="2025_15w_2.pdf", "34"="2025_34w_2.pdf", "49"="2025_49w_.pdf")
for (wk_str in names(saitama_hrefs)) {
  week <- as.integer(wk_str)
  u <- paste0("https://www.pref.saitama.lg.jp/documents/262567/", saitama_hrefs[[wk_str]])
  res <- tryCatch(fetch_saitama(u), error = function(e) NULL)
  add_result(res, "埼玉県", week)
}

# ---- 千葉県 ----
chiba_hrefs <- c("35"="wr2535-2.pdf", "31"="wr2531-3.pdf", "1"="wr2501.pdf")
for (wk_str in names(chiba_hrefs)) {
  week <- as.integer(wk_str)
  u <- paste0("https://www.pref.chiba.lg.jp/eiken/c-idsc/documents/", chiba_hrefs[[wk_str]])
  res <- tryCatch(fetch_chiba(u), error = function(e) NULL)
  add_result(res, "千葉県", week)
}

# ---- 長崎県 ----
res <- tryCatch(fetch_nagasaki("https://www.pref.nagasaki.jp/uploads/2025/09/1757556557.pdf"), error = function(e) NULL)
add_result(res, "長崎県", 35)

# ---- 新潟県 ----
niigata_hrefs <- c(
  "3"="432213","7"="435744","10"="438913","12"="442413","14"="446105",
  "22"="453032","24"="454500","26"="457517","28"="459292","30"="460950",
  "31"="461605","33"="462764","34"="463439","37"="465430","40"="467387","42"="468343"
)
for (wk_str in names(niigata_hrefs)) {
  week <- as.integer(wk_str)
  u <- sprintf("https://www.pref.niigata.lg.jp/uploaded/attachment/%s.pdf", niigata_hrefs[[wk_str]])
  res <- tryCatch(fetch_niigata(u), error = function(e) NULL)
  add_result(res, "新潟県", week)
}

cat("\n=== 実行ログ（2025年 第19弾：埼玉・千葉・長崎・新潟） ===\n")
cat(paste(status_log, collapse = "\n"), "\n")

if (length(new_rows) > 0) {
  added <- do.call(rbind, new_rows)
  common_cols <- intersect(names(h), names(added))
  combined <- rbind(h[, common_cols], added[, common_cols])
  saveRDS(combined, HISTORY_PATH)
  cat(sprintf("\n追記: %d行（新規） / 累計: %d行\n", nrow(added), nrow(combined)))
}
