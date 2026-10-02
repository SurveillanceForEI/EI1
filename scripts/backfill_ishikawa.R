# 石川県の保健所別データ（hokenjo_history.rds）を、テンプレート照合方式で
# 指定年の第1週〜最新週まで作り直して置き換える。
# 石川県の週報PDF1件には直近5週分の推移表が載っているので、5週ごとにPDFを取得し、
# 重なり確認用に数週分も余分に取得して、同じ週の値が号をまたいで一致するか検証する。
args <- commandArgs(trailingOnly = TRUE)
YEAR <- if (length(args) >= 1) as.integer(args[1]) else 2026L
LAST_WEEK <- if (length(args) >= 2) as.integer(args[2]) else 39L
DRY <- length(args) >= 3 && args[3] == "dry"
setwd("//episynq-NAS1/public1/episynq/japan_surveillance")
source("R/hokenjo_fetch/ishikawa.R")

pdf_weeks <- sort(unique(c(seq(LAST_WEEK, 5, by = -5), 5)))
pdf_weeks <- pdf_weeks[pdf_weeks >= 5 & pdf_weeks <= LAST_WEEK]
all_rows <- list()
for (w in pdf_weeks) {
  url <- sprintf("https://www.pref.ishikawa.lg.jp/hokan/kansenjoho/stock/%d/documents/%d-%d.pdf", YEAR, YEAR, w)
  d <- tryCatch(fetch_ishikawa_history(url, year = YEAR), error = function(e) { message("NG ", w, ": ", conditionMessage(e)); NULL })
  if (is.null(d) || !nrow(d)) next
  d$src_pdf <- w; all_rows[[length(all_rows) + 1]] <- d
  cat("PDF", w, ":", nrow(d), "rows\n")
}
new <- do.call(rbind, all_rows)
saveRDS(new, "C:/Users/kobayashi/AppData/Local/Temp/k/ishikawa_backfill_raw.rds")

# 重なり検証用に W(last-1), W(last-2) も取得して比較（同じ週・保健所・疾患の値は一致するはず）
chk <- list()
for (w in c(LAST_WEEK - 1, LAST_WEEK - 2)) {
  d <- tryCatch(fetch_ishikawa_history(sprintf("https://www.pref.ishikawa.lg.jp/hokan/kansenjoho/stock/%d/documents/%d-%d.pdf", YEAR, YEAR, w), year = YEAR), error = function(e) NULL)
  if (!is.null(d)) { d$src_pdf <- w; chk[[length(chk) + 1]] <- d }
}
if (length(chk)) {
  cmp <- merge(new[, c("week_num", "hokenjo", "disease", "count", "rate", "src_pdf")],
               do.call(rbind, chk)[, c("week_num", "hokenjo", "disease", "count", "rate", "src_pdf")],
               by = c("week_num", "hokenjo", "disease"), suffixes = c(".a", ".b"))
  cmp <- cmp[cmp$src_pdf.a != cmp$src_pdf.b, ]
  diff_cnt <- cmp[!is.na(cmp$count.a) & !is.na(cmp$count.b) & cmp$count.a != cmp$count.b, ]
  diff_rate <- cmp[!is.na(cmp$rate.a) & !is.na(cmp$rate.b) & abs(cmp$rate.a - cmp$rate.b) > 0.005, ]
  cat("号またぎ比較:", nrow(cmp), "セル / 件数不一致", nrow(diff_cnt), " / 率不一致", nrow(diff_rate), "\n")
  if (nrow(diff_cnt)) print(head(diff_cnt, 20)); if (nrow(diff_rate)) print(head(diff_rate, 20))
}

# 各週は「その週を含むPDF」から1つ採用（重複週は取得元PDFが新しい方を優先）
new <- new[order(new$week_num, -new$src_pdf), ]
new <- new[!duplicated(new[, c("week_num", "hokenjo", "disease")]), ]
new$src_pdf <- NULL
new$fetched_at <- as.character(Sys.time()); new$hokenjo_year <- YEAR
cat("新規行:", nrow(new), " 週範囲:", range(new$week_num), "\n")
if (DRY) quit(save = "no")

h <- readRDS("data/hokenjo_history.rds")
keep <- !(h$pref == "石川県" & h$hokenjo_year == YEAR)
cols <- names(h)
for (cn in setdiff(cols, names(new))) new[[cn]] <- NA
h2 <- rbind(h[keep, cols], new[, cols])
saveRDS(h, sprintf("data/hokenjo_history_before_ishikawa_fix_%s.rds", format(Sys.Date(), "%Y%m%d")))
saveRDS(h2, "data/hokenjo_history.rds")
cat("置換完了: 石川県", sum(!keep), "行 ->", nrow(new), "行\n")
