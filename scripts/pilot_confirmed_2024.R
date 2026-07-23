setwd("C:/Users/kobayashi/Documents/R/japan_surveillance")
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
})
source("R/data_loader.R")

path <- "scratch_xlsx/syu10_2_2024.xlsx"
sheets <- excel_sheets(path)
sokei_sheets <- sheets[grepl("・総数$", sheets)]

# シート名(疾患名)→disease_id 変換。半角/全角括弧ゆれを吸収する
norm_name <- function(x) gsub("\\(ロタウイルス\\)", "（ロタウイルス）", x)
disease_of_sheet <- function(sheet_name) {
  dname <- norm_name(sub("・総数$", "", sheet_name))
  did <- TEITEN_LABEL_MAP[dname]
  if (is.na(did)) NA_character_ else unname(did)
}

map_tbl <- tibble(sheet = sokei_sheets, disease = sapply(sokei_sheets, disease_of_sheet))
cat("=== シート→疾患IDマッピング ===\n")
print(as.data.frame(map_tbl))
unmapped <- map_tbl %>% filter(is.na(disease))
if (nrow(unmapped) > 0) {
  cat("\n!!! マッピング失敗:\n"); print(unmapped)
}

# 1シート分をパース: 都道府県×週の定点当たり報告数
parse_confirmed_sheet <- function(path, sheet, year) {
  raw <- suppressMessages(read_excel(path, sheet = sheet, col_names = FALSE))
  raw <- as.data.frame(raw)
  # 行4(R側1始まり、Excel行5相当)が週ラベル行のはず。「総数」データ行を探す
  # B列(2列目)の値が「総数(total No.)」に一致する行から46行後まで(総数+47都道府県)を使う
  col1 <- as.character(raw[[1]])
  start_row <- which(grepl("^総\\s*数", col1))[1]
  if (is.na(start_row)) stop("総数行が見つかりません: ", sheet)
  data_rows <- raw[start_row:(start_row + 47), ]  # 総数 + 47都道府県

  # 週列: 3列目以降で、そのシートの週数分（52 or 53）
  n_weeks <- ncol(raw) - 2
  week_cols <- 3:(2 + n_weeks)

  pref_names_raw <- as.character(data_rows[[1]])
  # "北海道(Hokkaido)" → "北海道"
  pref_names <- sub("\\(.*\\)$", "", pref_names_raw)
  pref_names <- trimws(gsub("　", "", pref_names))
  pref_names[1] <- "総数"

  vals <- data_rows[, week_cols, drop = FALSE]
  vals[] <- lapply(vals, function(x) suppressWarnings(as.numeric(x)))
  names(vals) <- paste0("w", seq_len(n_weeks))

  out <- vals
  out$pref_name_raw <- pref_names
  long <- tidyr::pivot_longer(out, cols = starts_with("w"), names_to = "col", values_to = "reports_per_site_confirmed")
  long$week <- as.integer(sub("^w", "", long$col))
  long$year <- year
  long$date <- as.Date(paste0(year, "-01-01")) + (long$week - 1) * 7
  long %>% select(year, week, date, pref_name_raw, reports_per_site_confirmed)
}

# インフルエンザで試験実行
test_disease <- parse_confirmed_sheet(path, "インフルエンザ・総数", 2024)
cat("\n=== インフルエンザ・総数 パース結果(先頭) ===\n")
print(head(test_disease, 15))
cat("\n行数:", nrow(test_disease), " / 都道府県数(総数含む):", length(unique(test_disease$pref_name_raw)),
    " / 週数:", length(unique(test_disease$week)), "\n")

cat("\n=== 都道府県名チェック ===\n")
pref_check <- unique(test_disease$pref_name_raw)
pref_check <- pref_check[pref_check != "総数"]
print(pref_check)
mismatch <- setdiff(pref_check, PREF_MASTER$pref_name)
cat("PREF_MASTERに存在しない名前:", if(length(mismatch)==0) "なし" else paste(mismatch, collapse=", "), "\n")
missing <- setdiff(PREF_MASTER$pref_name, pref_check)
cat("確定データ側に存在しない都道府県:", if(length(missing)==0) "なし" else paste(missing, collapse=", "), "\n")

# 既存キャッシュ(速報値)との比較: 2024年インフルエンザ
prov <- load_all_cached() %>% filter(year==2024, disease=="flu", pref_name != "全国")
comp <- test_disease %>% filter(pref_name_raw != "総数") %>%
  rename(pref_name = pref_name_raw, reports_per_site_confirmed_v = reports_per_site_confirmed) %>%
  inner_join(prov %>% select(week, pref_name, reports_per_site), by = c("week","pref_name"))
comp$diff <- comp$reports_per_site_confirmed_v - comp$reports_per_site
cat("\n=== 速報値との比較(インフルエンザ2024) ===\n")
cat("比較行数:", nrow(comp), "\n")
cat("平均絶対差:", mean(abs(comp$diff), na.rm=TRUE), "\n")
cat("最大絶対差:", max(abs(comp$diff), na.rm=TRUE), "\n")
cat("差が大きい上位10件:\n")
print(head(comp[order(-abs(comp$diff)),], 10))
