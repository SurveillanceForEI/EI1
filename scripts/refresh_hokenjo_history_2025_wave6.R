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

UA <- c(`User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)")
for (week in 1:52) {
  key <- paste("新潟県", 2025, week)
  if (key %in% existing_keys) next
  listing_url <- sprintf("https://www.pref.niigata.lg.jp/sec/kanyaku/shuho07%02d.html", week)
  doc <- tryCatch(xml2::read_html(httr::content(httr::GET(listing_url, httr::timeout(15)), "raw")), error = function(e) NULL)
  if (is.null(doc)) { status_log <- c(status_log, sprintf("[--] 新潟県 第%d週 (一覧ページ取得失敗)", week)); next }
  hrefs <- rvest::html_attr(rvest::html_elements(doc, "a"), "href")
  pdfs <- hrefs[grepl("\\.pdf$", hrefs, ignore.case = TRUE)]
  pdfs <- pdfs[!grepl("423107", pdfs)]  # 警報・注意報基準の解説（固定の別紙）を除外
  if (length(pdfs) == 0) { status_log <- c(status_log, sprintf("[--] 新潟県 第%d週 (PDFリンクなし)", week)); next }
  u <- paste0("https://www.pref.niigata.lg.jp", pdfs[1])
  res <- tryCatch(fetch_niigata(u), error = function(e) NULL)
  add_result(res, "新潟県", week)
}

cat("\n=== 実行ログ（2025年 第6弾：新潟県） ===\n")
cat(paste(status_log, collapse = "\n"), "\n")

if (length(new_rows) > 0) {
  h <- readRDS(HISTORY_PATH)
  added <- do.call(rbind, new_rows)
  common_cols <- intersect(names(h), names(added))
  combined <- rbind(h[, common_cols], added[, common_cols])
  saveRDS(combined, HISTORY_PATH)
  cat(sprintf("\n追記: %d行（新規） / 累計: %d行\n", nrow(added), nrow(combined)))
}
