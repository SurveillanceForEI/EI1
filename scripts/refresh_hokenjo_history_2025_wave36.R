setwd(if (basename(getwd()) == "scripts") ".." else ".")
source("R/hokenjo_fetch/hokenjo_fetch_schema.R")
source("R/hokenjo_fetch/pdf_table_utils.R")
source("R/hokenjo_fetch/kagoshima.R")
HISTORY_PATH <- "data/hokenjo_history.rds"
h <- readRDS(HISTORY_PATH)

# 週番号(表示上のweek_num) -> URL。第18・19週は合併号のため両方に同じPDFを割り当てる
week_urls <- list(
  "1" = "118046_20250109140321-1.pdf",
  "2" = "118046_20250116150509-1.pdf",
  "3" = "118046_20250123142613-1.pdf",
  "4" = "118046_20250130134805-1.pdf",
  "5" = "118046_20250206114654-1.pdf",
  "6" = "118046_20250213140542-1.pdf",
  "7" = "118046_20250220131257-1.pdf",
  "8" = "118046_20250227130532-1.pdf",
  "9" = "118046_20250306160555-1.pdf",
  "10" = "118046_20250313144416-1.pdf",
  "11" = "118046_20250321091943-1.pdf",
  "12" = "118046_20250327143648-1.pdf",
  "13" = "118046_20250403142552-1.pdf",
  "14" = "118046_20250410132053-1.pdf",
  "15" = "118046_20250425125323-1.pdf",
  "16" = "118046_20250425125245-1.pdf",
  "17" = "118046_20250501144044-1.pdf",
  "18" = "118046_20250515144925-1.pdf",
  "19" = "118046_20250515144925-1.pdf",
  "20" = "118046_20250522143832-1.pdf",
  "21" = "118046_20250529145246-1.pdf",
  "22" = "118046_20250605142235-1.pdf",
  "23" = "118046_20250612135140-1.pdf",
  "24" = "118046_20250619134558-1.pdf",
  "25" = "118046_20250626145931-1.pdf",
  "26" = "118046_20250703133710-1.pdf",
  "27" = "118046_20250710124915-1.pdf",
  "28" = "118046_20250717125802-1.pdf",
  "29" = "118046_20250724152451-1.pdf",
  "30" = "118046_20250731150144-1.pdf",
  "31" = "118046_20250807144328-1.pdf",
  "32" = "118046_20250818160046-1.pdf",
  "33" = "118046_20250826134320-1.pdf",
  "34" = "118046_20250828153647-1.pdf",
  "35" = "118046_20250904151716-1.pdf",
  "36" = "118046_20250911142155-1.pdf",
  "37" = "118046_20250918135944-1.pdf",
  "38" = "118046_20250925103113-1.pdf",
  "39" = "118046_20251002144300-1.pdf",
  "40" = "118046_20251009150602-1.pdf",
  "41" = "118046_20251016144322-1.pdf",
  "42" = "118046_20251023120033-1.pdf",
  "43" = "118046_20251030140437-1.pdf",
  "44" = "118046_20251106144755-1.pdf",
  "45" = "118046_20251112200112-1.pdf",
  "46" = "118046_20251120135435-1.pdf",
  "47" = "118046_20251127140653-1.pdf",
  "48" = "118046_20251204135921-1.pdf",
  "49" = "118046_20251211150041-1.pdf",
  "50" = "118046_20251218160955-1.pdf",
  "51" = "118046_20251225141607-1.pdf"
)
base <- "https://www.pref.kagoshima.jp/ae06/kenko-fukushi/kenko-iryo/kansen/hasseidoko/week/documents/"

new_rows <- list()
status_log <- character(0)
for (wk_s in names(week_urls)) {
  wk <- as.integer(wk_s)
  u <- paste0(base, week_urls[[wk_s]])
  res <- tryCatch(fetch_kagoshima(u), error = function(e) { status_log <<- c(status_log, sprintf("[NG] 第%d週: %s", wk, conditionMessage(e))); NULL })
  if (is.null(res) || nrow(res) == 0) next
  res$week_num <- wk
  res$fetched_at <- as.character(Sys.time())
  new_rows[[length(new_rows) + 1]] <- res
  status_log <- c(status_log, sprintf("[OK] 第%d週 (%d行) label=%s", wk, nrow(res), unique(res$week_label)[1]))
}

cat("\n=== 実行ログ(鹿児島県 2025年分取得) ===\n")
cat(paste(status_log, collapse = "\n"), "\n")

if (length(new_rows) > 0) {
  added <- do.call(rbind, new_rows)
  common_cols <- intersect(names(h), names(added))
  combined <- rbind(h[, common_cols], added[, common_cols])
  saveRDS(combined, HISTORY_PATH)
  cat(sprintf("\n追記: %d行(新規) / 累計: %d行\n", nrow(added), nrow(combined)))
}
