setwd("C:/Users/kobayashi/Documents/R/japan_surveillance")
suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr); library(httr)
})
source("R/data_loader.R")

DOWNLOAD_DIR <- "scratch_xlsx_batch"
dir.create(DOWNLOAD_DIR, showWarnings = FALSE)
CONFIRMED_SOURCE <- "JIHS 感染症発生動向調査事業年報（確定値）"

get_annual_url_base <- function(year) {
  if (year >= 2021) sprintf("https://id-info.jihs.go.jp/surveillance/idwr/annual/%d/syulist/", year)
  else if (year >= 2011) sprintf("https://id-info.jihs.go.jp/niid/images/idwr/ydata/%d/Syuukei/", year)
  else sprintf("https://idsc.niid.go.jp/idwr/CDROM/Kako/H%d/Syuukei/", year - 1988)
}
download_table <- function(year, table_code) {
  for (ext in c("xlsx", "xls")) {
    dest <- file.path(DOWNLOAD_DIR, sprintf("%d_%s.%s", year, table_code, ext))
    if (file.exists(dest)) return(dest)
  }
  for (ext in c("xlsx", "xls")) {
    url  <- paste0(get_annual_url_base(year), "Syu_", table_code, ".", ext)
    dest <- file.path(DOWNLOAD_DIR, sprintf("%d_%s.%s", year, table_code, ext))
    resp <- tryCatch(GET(url, timeout(30), write_disk(dest, overwrite = TRUE),
                          add_headers("User-Agent" = "JapanSurveillanceDashboard/1.0")), error = function(e) NULL)
    if (!is.null(resp) && status_code(resp) == 200 && file.exists(dest) && file.size(dest) >= 1000) return(dest)
    if (file.exists(dest)) file.remove(dest)
  }
  NULL
}
clean_pref_name <- function(x) { x <- sub("\\(.*\\)$", "", x); trimws(gsub("　", "", x)) }

parse_teiten_sheet <- function(path, sheet, year) {
  raw <- as.data.frame(suppressMessages(read_excel(path, sheet = sheet, col_names = FALSE)))
  col1 <- as.character(raw[[1]])
  start_row <- which(grepl("^総\\s*数", col1))[1]
  if (is.na(start_row)) return(NULL)
  data_rows <- raw[start_row:(start_row + 47), ]
  n_weeks <- ncol(raw) - 2
  if (n_weeks < 40 || n_weeks > 53) return(NULL)
  week_cols <- 3:(2 + n_weeks)
  pref_names <- clean_pref_name(as.character(data_rows[[1]])); pref_names[1] <- "総数"
  vals <- data_rows[, week_cols, drop = FALSE]
  vals[] <- lapply(vals, function(x) suppressWarnings(as.numeric(x)))
  names(vals) <- paste0("w", seq_len(n_weeks))
  out <- vals; out$pref_name <- pref_names
  long <- tidyr::pivot_longer(out, cols = starts_with("w"), names_to = "col", values_to = "reports_per_site")
  long$week <- as.integer(sub("^w", "", long$col)); long$year <- year
  long %>% select(year, week, pref_name, reports_per_site)
}

# 対象疾患のシート名バリアント（年によって表記が異なる）
TARGET_SHEET_NAMES <- list(
  varicella = c("水　痘・総数", "水痘・総数"),
  roseola   = c("突発性発疹・総数", "突発性発しん・総数"),
  chlamydia = c("クラミジア肺炎(オウム病を除く)・総数", "クラミジア肺炎・総数")
)

status_log <- character(0)
for (year in 2001:2007) {
  cat("\n===", year, "===\n")
  p10_2 <- tryCatch(download_table(year, "10_2"), error = function(e) NULL)
  if (is.null(p10_2)) { status_log <- c(status_log, sprintf("[NG] %d年: Syu_10_2取得失敗", year)); next }
  sheets <- tryCatch(excel_sheets(p10_2), error = function(e) character(0))

  for (did in names(TARGET_SHEET_NAMES)) {
    sh <- TARGET_SHEET_NAMES[[did]][TARGET_SHEET_NAMES[[did]] %in% sheets][1]
    if (is.na(sh)) { status_log <- c(status_log, sprintf("[NG] %d年 %s: シートなし", year, did)); next }
    d <- tryCatch(parse_teiten_sheet(p10_2, sh, year), error = function(e) NULL)
    if (is.null(d) || nrow(d) == 0) { status_log <- c(status_log, sprintf("[NG] %d年 %s: 解析失敗", year, did)); next }
    d <- d %>% mutate(pref_name = ifelse(pref_name == "総数", "全国", pref_name))

    n_added <- 0
    for (wk in sort(unique(d$week))) {
      wkd <- d %>% filter(week == wk)
      week_start <- as.Date(paste0(year, "-01-01")) + (wk - 1) * 7
      rows <- wkd %>% rowwise() %>% mutate(
        pref_code = if (pref_name == "全国") NA_integer_ else PREF_MASTER$pref_code[match(pref_name, PREF_MASTER$pref_name)],
        region    = if (pref_name == "全国") "全国" else PREF_MASTER$region[match(pref_name, PREF_MASTER$pref_name)],
        disease = did, disease_label = DISEASE_CONFIG[[did]]$label
      ) %>% ungroup() %>%
        transmute(year = year, week = wk, date = week_start, pref_code, pref_name, region,
                  disease, disease_label, reports_per_site,
                  is_provisional = FALSE, data_source = CONFIRMED_SOURCE)

      f <- get_cache_path(year, wk)
      if (!file.exists(f)) next  # その週のファイル自体が無ければ対象外（他の主要疾患すら無い異常週）
      existing <- readRDS(f)
      existing <- existing[existing$disease != did, ]  # 念のため既存の同疾患行があれば置き換え
      combined <- bind_rows(existing, rows)
      saveRDS(combined, f)
      n_added <- n_added + nrow(rows)
    }
    status_log <- c(status_log, sprintf("[OK] %d年 %s: %d行追加", year, did, n_added))
  }
}

cat("\n=== 実行ログ ===\n")
cat(paste(status_log, collapse = "\n"), "\n")
