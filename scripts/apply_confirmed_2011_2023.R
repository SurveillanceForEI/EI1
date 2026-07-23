setwd("C:/Users/kobayashi/Documents/R/japan_surveillance")
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(httr)
})
source("R/data_loader.R")
source("R/std_loader.R")
source("R/zensu_loader.R")

DOWNLOAD_DIR <- "scratch_xlsx_batch"
dir.create(DOWNLOAD_DIR, showWarnings = FALSE)

get_annual_url_base <- function(year) {
  if (year >= 2021) {
    sprintf("https://id-info.jihs.go.jp/surveillance/idwr/annual/%d/syulist/", year)
  } else if (year >= 2011) {
    sprintf("https://id-info.jihs.go.jp/niid/images/idwr/ydata/%d/Syuukei/", year)
  } else {
    # 2001〜2010年: 旧CDROM期（表番号は10-2等の現行体系と共通）
    sprintf("https://idsc.niid.go.jp/idwr/CDROM/Kako/H%d/Syuukei/", year - 1988)
  }
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
                          add_headers("User-Agent" = "JapanSurveillanceDashboard/1.0")),
                      error = function(e) NULL)
    if (!is.null(resp) && status_code(resp) == 200 && file.exists(dest) && file.size(dest) >= 1000) {
      return(dest)
    }
    if (file.exists(dest)) file.remove(dest)
  }
  NULL
}

strip_disease_name <- function(x) sub("[\r\n]+.*$", "", x)

ZENSU_LABEL_ALIASES <- c(
  "鳥インフルエンザ(H5N1)"                     = "avian_h5n1",
  "鳥インフルエンザ(H7N9)"                     = "avian_h7n9",
  "鳥インフルエンザ"                           = "avian_other",
  "急性弛緩性麻痺（急性灰白髄炎を除く。）"     = "acute_flaccid",
  "後天性免疫不全症候群（ＨＩＶ感染症を含む）" = "aids",
  "水痘（入院例に限る。）"                     = "varicella_hosp",
  "新型コロナウイルス感染症"                   = "covid19_zensu_unused"  # 全数把握対象だった時期があるが現行アプリでは扱わない
)
TEITEN_LABEL_ALIASES <- c(
  "感染性胃腸炎(ロタウイルス)" = "gi_rota"
)

clean_pref_name <- function(x) {
  x <- sub("\\(.*\\)$", "", x)
  trimws(gsub("　", "", x))
}

# ── 週報定点(第10-2表)パーサー ─────────────────────────
parse_teiten_sheet <- function(path, sheet, year) {
  raw <- as.data.frame(suppressMessages(read_excel(path, sheet = sheet, col_names = FALSE)))
  col1 <- as.character(raw[[1]])
  start_row <- which(grepl("^総\\s*数", col1))[1]
  if (is.na(start_row)) return(NULL)
  data_rows <- raw[start_row:(start_row + 47), ]
  n_weeks <- ncol(raw) - 2
  if (n_weeks < 40 || n_weeks > 53) return(NULL)
  week_cols <- 3:(2 + n_weeks)
  pref_names_raw <- as.character(data_rows[[1]])
  pref_names <- clean_pref_name(pref_names_raw)
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

# ── 汎用トリプレット(総数/男/女)パーサー ──────────────────
parse_triplet_wide <- function(raw, label_map, aliases = character(0), n_data_rows = 48) {
  col1 <- as.character(raw[[1]])
  start_row <- which(grepl("^総\\s*数", col1))[1]
  if (is.na(start_row)) return(NULL)
  data_rows <- raw[start_row:(start_row + n_data_rows - 1), ]
  disease_hdr_row <- 4
  n_col <- ncol(raw)
  triplet_starts <- seq(2, n_col, by = 3)
  pref_names_raw <- as.character(data_rows[[1]])
  pref_names <- clean_pref_name(pref_names_raw)
  pref_names[1] <- "総数"

  results <- list()
  for (ts in triplet_starts) {
    if (ts > ncol(raw)) next
    dname_raw <- as.character(raw[[ts]][disease_hdr_row])
    if (is.na(dname_raw) || dname_raw == "") next
    dname <- strip_disease_name(dname_raw)
    did <- label_map[dname]
    if (is.na(did) && dname %in% names(aliases)) did <- aliases[dname]
    if (is.na(did)) next
    vals <- suppressWarnings(as.numeric(data_rows[[ts]]))
    results[[unname(did)]] <- tibble(pref_name = pref_names, disease = unname(did), value_confirmed = vals)
  }
  if (length(results) == 0) return(NULL)
  bind_rows(results)
}

STD_LABEL_MAP <- setNames(names(STD_DISEASE_CONFIG), sapply(STD_DISEASE_CONFIG, function(x) x$label))
month_labels_zenkaku <- c("１月","２月","３月","４月","５月","６月","７月","８月","９月","１０月","１１月","１２月")
to_hankaku <- function(x) chartr("０１２３４５６７８９", "0123456789", x)

# ============================================================
# 年ごとの処理
# ============================================================
summary_rows <- list()

process_year <- function(year) {
  cat("\n########## ", year, "年 ##########\n")
  res <- list(year = year, teiten_rows = 0, std_rows = 0, zensu_rows = 0, errors = character(0))

  # ── ① 週報定点(第10-2表) ──
  p10_2 <- tryCatch(download_table(year, "10_2"), error = function(e) NULL)
  if (is.null(p10_2)) {
    res$errors <- c(res$errors, "Syu_10_2ダウンロード失敗")
  } else {
    sheets <- tryCatch(excel_sheets(p10_2), error = function(e) character(0))
    sokei_sheets <- sheets[grepl("・総数$", sheets)]
    all_confirmed <- list()
    for (sh in sokei_sheets) {
      dname <- sub("・総数$", "", sh)
      did <- TEITEN_LABEL_MAP[dname]
      if (is.na(did) && dname %in% names(TEITEN_LABEL_ALIASES)) did <- TEITEN_LABEL_ALIASES[dname]
      if (is.na(did)) next
      d <- tryCatch(parse_teiten_sheet(p10_2, sh, year), error = function(e) NULL)
      if (is.null(d)) next
      d$disease <- unname(did)
      all_confirmed[[unname(did)]] <- d
    }
    if (length(all_confirmed) > 0) {
      confirmed_all <- bind_rows(all_confirmed) %>%
        mutate(pref_name = ifelse(pref_name == "総数", "全国", pref_name))

      cache_files <- list.files("data/cache", pattern = sprintf("^%d-\\d{2}\\.rds$", year), full.names = TRUE)
      for (f in cache_files) {
        wk <- as.integer(sub(".*-(\\d{2})\\.rds$", "\\1", f))
        d <- readRDS(f)
        if (!"disease" %in% names(d)) next
        conf_wk <- confirmed_all %>% filter(week == wk)
        if (nrow(conf_wk) == 0) next
        before <- d$is_provisional
        d <- d %>%
          left_join(conf_wk %>% select(pref_name, disease, reports_per_site_confirmed), by = c("pref_name", "disease")) %>%
          mutate(
            reports_per_site = ifelse(!is.na(reports_per_site_confirmed), reports_per_site_confirmed, reports_per_site),
            is_provisional    = ifelse(!is.na(reports_per_site_confirmed), FALSE, is_provisional),
            data_source       = ifelse(!is.na(reports_per_site_confirmed), "JIHS 感染症発生動向調査事業年報（確定値）", data_source)
          ) %>%
          select(-reports_per_site_confirmed)
        n_changed <- sum(d$is_provisional == FALSE & before == TRUE, na.rm = TRUE)
        if (n_changed > 0) { saveRDS(d, f); res$teiten_rows <- res$teiten_rows + n_changed }
      }
    }
  }
  cat("週報定点 確定値行数:", res$teiten_rows, "\n")

  # ── ② 月報疾患(第9-1/9-2表) ──
  p9_1 <- tryCatch(download_table(year, "09_1"), error = function(e) NULL)
  p9_2 <- tryCatch(download_table(year, "09_2"), error = function(e) NULL)
  if (is.null(p9_1) || is.null(p9_2)) {
    res$errors <- c(res$errors, "Syu_09_1/09_2ダウンロード失敗")
  } else {
    reports_list <- list(); persite_list <- list()
    for (i in seq_along(month_labels_zenkaku)) {
      sh <- month_labels_zenkaku[i]
      r_raw <- tryCatch(as.data.frame(suppressMessages(read_excel(p9_1, sheet = sh, col_names = FALSE))), error = function(e) NULL)
      p_raw <- tryCatch(as.data.frame(suppressMessages(read_excel(p9_2, sheet = sh, col_names = FALSE))), error = function(e) NULL)
      if (!is.null(r_raw)) {
        rl <- parse_triplet_wide(r_raw, STD_LABEL_MAP)
        if (!is.null(rl)) { rl$year <- year; rl$month <- i; reports_list[[i]] <- rl }
      }
      if (!is.null(p_raw)) {
        pl <- parse_triplet_wide(p_raw, STD_LABEL_MAP)
        if (!is.null(pl)) { pl$year <- year; pl$month <- i; persite_list[[i]] <- pl }
      }
    }
    reports_all <- bind_rows(reports_list)
    persite_all <- bind_rows(persite_list)
    if (nrow(reports_all) > 0) {
      names(reports_all)[names(reports_all) == "value_confirmed"] <- "reports_confirmed"
      names(persite_all)[names(persite_all) == "value_confirmed"] <- "per_site_confirmed"
      std_confirmed <- reports_all %>%
        select(year, month, pref_name, disease, reports_confirmed) %>%
        left_join(persite_all %>% select(year, month, pref_name, disease, per_site_confirmed),
                   by = c("year","month","pref_name","disease")) %>%
        mutate(pref_name = ifelse(pref_name == "総数", "全国", pref_name))

      old_std <- load_std_cached()
      has_national <- any(old_std$pref_name == "全国", na.rm = TRUE)
      if (!has_national) std_confirmed <- std_confirmed %>% filter(pref_name != "全国")

      before <- old_std$is_provisional
      updated_std <- old_std %>%
        left_join(std_confirmed, by = c("year","month","pref_name","disease")) %>%
        mutate(
          reports  = ifelse(!is.na(reports_confirmed), reports_confirmed, reports),
          per_site = ifelse(!is.na(per_site_confirmed), per_site_confirmed, per_site),
          is_provisional = ifelse(!is.na(reports_confirmed), FALSE, is_provisional),
          data_source    = ifelse(!is.na(reports_confirmed), "JIHS 感染症発生動向調査事業年報（確定値）", data_source)
        ) %>%
        select(-reports_confirmed, -per_site_confirmed)
      n_changed <- sum(updated_std$is_provisional == FALSE & before == TRUE, na.rm = TRUE)
      if (n_changed > 0) { saveRDS(updated_std, STD_DATA_CACHE); res$std_rows <- n_changed }
    }
  }
  cat("月報疾患(STD) 確定値行数:", res$std_rows, "\n")

  # ── ③ 全数把握(第1-1表) ──
  p1_1 <- tryCatch(download_table(year, "01_1"), error = function(e) NULL)
  if (is.null(p1_1)) {
    res$errors <- c(res$errors, "Syu_01_1ダウンロード失敗")
  } else {
    zsheets <- tryCatch(excel_sheets(p1_1), error = function(e) character(0))
    week_sheets <- zsheets[grepl("週$", zsheets) & zsheets != "総　数"]
    zensu_list <- list()
    for (sh in week_sheets) {
      wk <- suppressWarnings(as.integer(sub("週$", "", to_hankaku(sh))))
      if (is.na(wk)) next
      raw <- tryCatch(as.data.frame(suppressMessages(read_excel(p1_1, sheet = sh, col_names = FALSE))), error = function(e) NULL)
      if (is.null(raw)) next
      long <- tryCatch(parse_triplet_wide(raw, ZENSU_LABEL_MAP, ZENSU_LABEL_ALIASES), error = function(e) NULL)
      if (is.null(long)) next
      long$year <- year; long$week <- wk
      zensu_list[[sh]] <- long
    }
    zensu_confirmed <- bind_rows(zensu_list)
    if (nrow(zensu_confirmed) > 0) {
      names(zensu_confirmed)[names(zensu_confirmed) == "value_confirmed"] <- "cases_confirmed"
      zensu_confirmed <- zensu_confirmed %>% mutate(pref_name = ifelse(pref_name == "総数", "全国", pref_name))

      zfiles <- list.files(ZENSU_CACHE_DIR, pattern = sprintf("^%d-\\d{2}\\.rds$", year), full.names = TRUE)
      for (f in zfiles) {
        wk <- as.integer(sub(".*-(\\d{2})\\.rds$", "\\1", f))
        d <- readRDS(f)
        if (!"disease" %in% names(d) || nrow(d) == 0) next
        conf_wk <- zensu_confirmed %>% filter(week == wk, pref_name != "全国")
        if (nrow(conf_wk) == 0) next
        if (!"is_provisional" %in% names(d)) d$is_provisional <- TRUE
        if (!"data_source" %in% names(d)) d$data_source <- "JIHS IDWR速報（暫定値）"
        before <- d$is_provisional
        d <- d %>%
          left_join(conf_wk %>% select(pref_name, disease, cases_confirmed), by = c("pref_name","disease")) %>%
          mutate(
            cases          = ifelse(!is.na(cases_confirmed), cases_confirmed, cases),
            is_provisional = ifelse(!is.na(cases_confirmed), FALSE, is_provisional),
            data_source    = ifelse(!is.na(cases_confirmed), "JIHS 感染症発生動向調査事業年報（確定値）", data_source)
          ) %>%
          select(-cases_confirmed)
        n_changed <- sum(d$is_provisional == FALSE & before == TRUE, na.rm = TRUE)
        if (n_changed > 0) { saveRDS(d, f); res$zensu_rows <- res$zensu_rows + n_changed }
      }
    }
  }
  cat("全数把握 確定値行数:", res$zensu_rows, "\n")
  if (length(res$errors) > 0) cat("エラー:", paste(res$errors, collapse = "; "), "\n")
  res
}

for (yr in 2011:2023) {
  summary_rows[[as.character(yr)]] <- tryCatch(process_year(yr),
    error = function(e) { cat("!!! 年全体エラー", yr, ":", conditionMessage(e), "\n"); list(year=yr, teiten_rows=NA, std_rows=NA, zensu_rows=NA, errors=conditionMessage(e)) })
}

cat("\n\n========== サマリー ==========\n")
summary_df <- bind_rows(lapply(summary_rows, function(r) {
  tibble(year = r$year, teiten_rows = r$teiten_rows, std_rows = r$std_rows, zensu_rows = r$zensu_rows,
         errors = paste(r$errors, collapse = "; "))
}))
print(as.data.frame(summary_df))
