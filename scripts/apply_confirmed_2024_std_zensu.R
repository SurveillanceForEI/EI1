setwd("C:/Users/kobayashi/Documents/R/japan_surveillance")
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
})
source("R/data_loader.R")
source("R/std_loader.R")
source("R/zensu_loader.R")

strip_disease_name <- function(x) sub("[\r\n]+.*$", "", x)

# 年報側の疾患名表記ゆれ（半角/全角、注記の有無等）を吸収するエイリアス
ZENSU_LABEL_ALIASES <- c(
  "鳥インフルエンザ(H5N1)"                     = "avian_h5n1",
  "鳥インフルエンザ(H7N9)"                     = "avian_h7n9",
  "鳥インフルエンザ"                           = "avian_other",  # "鳥インフルエンザ(H5N1を除く)"の行内改行で末尾が欠落
  "急性弛緩性麻痺（急性灰白髄炎を除く。）"     = "acute_flaccid",
  "後天性免疫不全症候群（ＨＩＶ感染症を含む）" = "aids",
  "水痘（入院例に限る。）"                     = "varicella_hosp"
)

# 都道府県名を「北海道(Hokkaido)」→「北海道」に正規化
clean_pref_name <- function(x) {
  x <- sub("\\(.*\\)$", "", x)
  trimws(gsub("　", "", x))
}

# ── 汎用: 「総数/男/女」3列1組のワイド表を都道府県×疾患のロング形式に変換 ──
# raw: read_excel(col_names=FALSE)の結果(data.frame化済み)
# n_data_rows: 総数行+都道府県行の行数（通常48）
parse_triplet_wide <- function(raw, label_map, n_data_rows = 48) {
  col1 <- as.character(raw[[1]])
  start_row <- which(grepl("^総\\s*数", col1))[1]
  if (is.na(start_row)) stop("総数行が見つかりません")
  data_rows <- raw[start_row:(start_row + n_data_rows - 1), ]

  header_row <- raw[which(!is.na(raw[[2]]) | !is.na(raw[[1]]))[1], ]  # 未使用、保険
  disease_hdr_row <- 4  # 疾患名は常に4行目
  n_col <- ncol(raw)
  # 3列ごとに (総数,男,女)。1列目は都道府県名。
  triplet_starts <- seq(2, n_col, by = 3)

  pref_names_raw <- as.character(data_rows[[1]])
  pref_names <- clean_pref_name(pref_names_raw)
  pref_names[1] <- "総数"

  results <- list()
  for (ts in triplet_starts) {
    dname_raw <- as.character(raw[[ts]][disease_hdr_row])
    if (is.na(dname_raw) || dname_raw == "") next
    dname <- strip_disease_name(dname_raw)
    did <- label_map[dname]
    if (is.na(did) && dname %in% names(ZENSU_LABEL_ALIASES)) did <- ZENSU_LABEL_ALIASES[dname]
    if (is.na(did)) next
    vals <- suppressWarnings(as.numeric(data_rows[[ts]]))
    results[[unname(did)]] <- tibble(pref_name = pref_names, disease = unname(did), value_confirmed = vals)
  }
  bind_rows(results)
}

# ============================================================
# ① 月報疾患（STD）確定値 — 第9-1表(報告数) + 第9-2表(定点当たり)
# ============================================================
cat("=== 月報疾患（STD）確定値処理 ===\n")
month_labels <- c("１月","２月","３月","４月","５月","６月","７月","８月","９月","１０月","１１月","１２月")

STD_LABEL_MAP <- setNames(names(STD_DISEASE_CONFIG), sapply(STD_DISEASE_CONFIG, function(x) x$label))

parse_std_month <- function(path, sheet, month, value_col_name) {
  raw <- as.data.frame(suppressMessages(read_excel(path, sheet = sheet, col_names = FALSE)))
  long <- parse_triplet_wide(raw, STD_LABEL_MAP)
  long$year <- 2024
  long$month <- month
  names(long)[names(long) == "value_confirmed"] <- value_col_name
  long
}

reports_path  <- "scratch_xlsx/syu09_1_2024.xlsx"
persite_path  <- "scratch_xlsx/syu09_2_2024.xlsx"

std_reports_all <- bind_rows(lapply(seq_along(month_labels), function(i) {
  tryCatch(parse_std_month(reports_path, month_labels[i], i, "reports_confirmed"),
           error = function(e) { cat("月報reportsエラー", month_labels[i], ":", conditionMessage(e), "\n"); NULL })
}))
std_persite_all <- bind_rows(lapply(seq_along(month_labels), function(i) {
  tryCatch(parse_std_month(persite_path, month_labels[i], i, "per_site_confirmed"),
           error = function(e) { cat("月報per_siteエラー", month_labels[i], ":", conditionMessage(e), "\n"); NULL })
}))

cat("STD reports確定値:", nrow(std_reports_all), "行 / per_site確定値:", nrow(std_persite_all), "行\n")

std_confirmed <- std_reports_all %>%
  select(year, month, pref_name, disease, reports_confirmed) %>%
  left_join(std_persite_all %>% select(year, month, pref_name, disease, per_site_confirmed),
             by = c("year","month","pref_name","disease")) %>%
  mutate(pref_name = ifelse(pref_name == "総数", "全国", pref_name))

old_std <- load_std_cached()
cat("既存std_dataの2024年行数:", sum(old_std$year == 2024, na.rm = TRUE), "\n")

# 全国行が既存キャッシュにあるか確認（無ければconfirmed側も除外して都道府県のみ扱う）
has_national <- any(old_std$pref_name == "全国", na.rm = TRUE)
if (!has_national) std_confirmed <- std_confirmed %>% filter(pref_name != "全国")

updated_std <- old_std %>%
  left_join(std_confirmed, by = c("year","month","pref_name","disease")) %>%
  mutate(
    reports  = ifelse(!is.na(reports_confirmed), reports_confirmed, reports),
    per_site = ifelse(!is.na(per_site_confirmed), per_site_confirmed, per_site),
    is_provisional = ifelse(!is.na(reports_confirmed), FALSE, TRUE),
    data_source    = ifelse(!is.na(reports_confirmed), "JIHS 感染症発生動向調査事業年報（確定値）", "JIHS IDWR週報PDF（暫定値）")
  ) %>%
  select(-reports_confirmed, -per_site_confirmed)

n_std_changed <- sum(updated_std$year == 2024 & updated_std$is_provisional == FALSE)
cat("STD確定値に置き換えた行数:", n_std_changed, "\n")
saveRDS(updated_std, STD_DATA_CACHE)

# 検証
chk <- updated_std %>% filter(year == 2024, disease == "gonorrhea", pref_name == "東京都") %>% arrange(month)
cat("\n=== 検証(2024年 東京都 淋菌感染症) ===\n")
print(chk %>% select(month, reports, per_site, is_provisional))

# ============================================================
# ② 全数把握疾患確定値 — 第1-1表(報告数、週別)
# ============================================================
cat("\n\n=== 全数把握疾患確定値処理 ===\n")
zensu_path <- "scratch_xlsx/syu01_1_2024.xlsx"
zensu_sheets <- excel_sheets(zensu_path)
week_sheets <- zensu_sheets[grepl("週$", zensu_sheets) & zensu_sheets != "総　数"]
cat("週シート数:", length(week_sheets), "\n")

parse_zensu_week_sheet <- function(path, sheet, week) {
  raw <- as.data.frame(suppressMessages(read_excel(path, sheet = sheet, col_names = FALSE)))
  long <- parse_triplet_wide(raw, ZENSU_LABEL_MAP)
  long$year <- 2024
  long$week <- week
  names(long)[names(long) == "value_confirmed"] <- "cases_confirmed"
  long
}

zensu_confirmed_list <- list()
for (sh in week_sheets) {
  wk <- as.integer(sub("週$", "", chartr("０１２３４５６７８９", "0123456789", sh)))
  zensu_confirmed_list[[sh]] <- tryCatch(parse_zensu_week_sheet(zensu_path, sh, wk),
    error = function(e) { cat("全数エラー week", wk, ":", conditionMessage(e), "\n"); NULL })
}
zensu_confirmed <- bind_rows(zensu_confirmed_list) %>%
  mutate(pref_name = ifelse(pref_name == "総数", "全国", pref_name))
cat("全数確定値パース完了:", nrow(zensu_confirmed), "行 /", length(unique(zensu_confirmed$disease)), "疾患\n")

zensu_files <- list.files(ZENSU_CACHE_DIR, pattern = "^2024-\\d{2}\\.rds$", full.names = TRUE)
cat("対象zensuキャッシュファイル数:", length(zensu_files), "\n")

n_zensu_files_changed <- 0
n_zensu_rows_changed <- 0
for (f in zensu_files) {
  wk <- as.integer(sub(".*-(\\d{2})\\.rds$", "\\1", f))
  d <- readRDS(f)
  if (!"disease" %in% names(d) || nrow(d) == 0) next
  conf_wk <- zensu_confirmed %>% filter(week == wk, pref_name != "全国")
  if (nrow(conf_wk) == 0) next

  if (!"is_provisional" %in% names(d)) d$is_provisional <- TRUE
  if (!"data_source" %in% names(d)) d$data_source <- "JIHS IDWR速報（暫定値）"

  before_prov <- d$is_provisional
  d <- d %>%
    left_join(conf_wk %>% select(pref_name, disease, cases_confirmed), by = c("pref_name","disease")) %>%
    mutate(
      cases          = ifelse(!is.na(cases_confirmed), cases_confirmed, cases),
      is_provisional = ifelse(!is.na(cases_confirmed), FALSE, is_provisional),
      data_source    = ifelse(!is.na(cases_confirmed), "JIHS 感染症発生動向調査事業年報（確定値）", data_source)
    ) %>%
    select(-cases_confirmed)

  n_changed <- sum(d$is_provisional == FALSE & before_prov == TRUE, na.rm = TRUE)
  if (n_changed > 0) {
    saveRDS(d, f)
    n_zensu_files_changed <- n_zensu_files_changed + 1
    n_zensu_rows_changed <- n_zensu_rows_changed + n_changed
  }
}
cat("更新した全数キャッシュファイル数:", n_zensu_files_changed, "\n")
cat("確定値に置き換えた全数行数の合計:", n_zensu_rows_changed, "\n")

chk2 <- load_all_zensu_cached() %>% filter(year == 2024, disease == "measles", pref_name == "東京都") %>% arrange(week)
cat("\n=== 検証(2024年 東京都 麻しん) ===\n")
print(head(chk2 %>% select(week, cases, is_provisional, data_source), 10))

cat("\n=== デバッグ: measles確定値の有無 ===\n")
print(zensu_confirmed %>% filter(disease == "measles", pref_name == "東京都") %>% arrange(week) %>% head(10))
cat("measles確定値行数:", sum(zensu_confirmed$disease == "measles"), "\n")

cat("\n=== 疾患カバレッジ確認 ===\n")
all_ids <- names(ZENSU_DISEASE_CONFIG)
matched_ids <- unique(zensu_confirmed$disease)
missing_ids <- setdiff(all_ids, matched_ids)
cat("ZENSU_DISEASE_CONFIG登録数:", length(all_ids), "\n")
cat("確定値が取れた疾患数:", length(intersect(all_ids, matched_ids)), "\n")
cat("確定値が取れなかった疾患:", if(length(missing_ids)==0) "なし" else paste(sapply(missing_ids, function(i) ZENSU_DISEASE_CONFIG[[i]]$label), collapse=", "), "\n")
