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

norm_name <- function(x) gsub("\\(ロタウイルス\\)", "（ロタウイルス）", x)
disease_of_sheet <- function(sheet_name) {
  dname <- norm_name(sub("・総数$", "", sheet_name))
  did <- TEITEN_LABEL_MAP[dname]
  if (is.na(did)) NA_character_ else unname(did)
}

parse_confirmed_sheet <- function(path, sheet, year) {
  raw <- suppressMessages(read_excel(path, sheet = sheet, col_names = FALSE))
  raw <- as.data.frame(raw)
  col1 <- as.character(raw[[1]])
  start_row <- which(grepl("^総\\s*数", col1))[1]
  if (is.na(start_row)) stop("総数行が見つかりません: ", sheet)
  data_rows <- raw[start_row:(start_row + 47), ]

  n_weeks <- ncol(raw) - 2
  week_cols <- 3:(2 + n_weeks)

  pref_names_raw <- as.character(data_rows[[1]])
  pref_names <- sub("\\(.*\\)$", "", pref_names_raw)
  pref_names <- trimws(gsub("　", "", pref_names))
  pref_names[1] <- "総数"

  vals <- data_rows[, week_cols, drop = FALSE]
  vals[] <- lapply(vals, function(x) suppressWarnings(as.numeric(x)))
  names(vals) <- paste0("w", seq_len(n_weeks))

  out <- vals
  out$pref_name <- pref_names
  long <- tidyr::pivot_longer(out, cols = starts_with("w"), names_to = "col", values_to = "reports_per_site_confirmed")
  long$week <- as.integer(sub("^w", "", long$col))
  long$year <- year
  long %>% select(year, week, pref_name, reports_per_site_confirmed)
}

# ── 全19疾患をパース ──────────────────────────────────
all_confirmed <- list()
for (sh in sokei_sheets) {
  did <- disease_of_sheet(sh)
  if (is.na(did)) { cat("スキップ(未マッピング):", sh, "\n"); next }
  d <- parse_confirmed_sheet(path, sh, 2024)
  d$disease <- did
  all_confirmed[[did]] <- d
}
confirmed_all <- bind_rows(all_confirmed)
cat("確定値パース完了:", nrow(confirmed_all), "行 /", length(unique(confirmed_all$disease)), "疾患\n")

# 全国("総数"行)は pref_name="全国" として扱う（既存キャッシュに合わせる）
confirmed_all <- confirmed_all %>%
  mutate(pref_name = ifelse(pref_name == "総数", "全国", pref_name))

# ── data/cache/2024-WW.rds を確定値で上書き ──────────────────
cache_files <- list.files("data/cache", pattern = "^2024-\\d{2}\\.rds$", full.names = TRUE)
cat("\n対象キャッシュファイル数:", length(cache_files), "\n")

n_updated_total <- 0
n_files_changed <- 0
for (f in cache_files) {
  wk <- as.integer(sub(".*-(\\d{2})\\.rds$", "\\1", f))
  d <- readRDS(f)
  if (!"disease" %in% names(d)) next

  conf_wk <- confirmed_all %>% filter(week == wk)
  if (nrow(conf_wk) == 0) next

  before <- d
  d <- d %>%
    left_join(conf_wk %>% select(pref_name, disease, reports_per_site_confirmed),
               by = c("pref_name", "disease")) %>%
    mutate(
      reports_per_site = ifelse(!is.na(reports_per_site_confirmed), reports_per_site_confirmed, reports_per_site),
      is_provisional    = ifelse(!is.na(reports_per_site_confirmed), FALSE, is_provisional),
      data_source       = ifelse(!is.na(reports_per_site_confirmed), "JIHS 感染症発生動向調査事業年報（確定値）", data_source)
    ) %>%
    select(-reports_per_site_confirmed)

  n_changed <- sum(d$is_provisional == FALSE & before$is_provisional == TRUE, na.rm = TRUE)
  if (n_changed > 0) {
    saveRDS(d, f)
    n_files_changed <- n_files_changed + 1
    n_updated_total <- n_updated_total + n_changed
  }
}
cat("更新したファイル数:", n_files_changed, "\n")
cat("確定値に置き換えた行数の合計:", n_updated_total, "\n")

# ── 検証: 更新後のキャッシュを再読込し、確定値と一致するか確認 ──────
check <- load_all_cached() %>% filter(year == 2024, disease == "flu", pref_name == "東京都") %>% arrange(week)
cat("\n=== 検証(2024年 東京都 インフルエンザ) ===\n")
print(head(check %>% select(week, reports_per_site, is_provisional, data_source), 10))
