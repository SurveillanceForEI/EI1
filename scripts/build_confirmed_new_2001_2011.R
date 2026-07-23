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

CONFIRMED_SOURCE <- "JIHS 感染症発生動向調査事業年報（確定値）"

get_annual_url_base <- function(year) {
  if (year >= 2021) {
    sprintf("https://id-info.jihs.go.jp/surveillance/idwr/annual/%d/syulist/", year)
  } else if (year >= 2011) {
    sprintf("https://id-info.jihs.go.jp/niid/images/idwr/ydata/%d/Syuukei/", year)
  } else {
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
    if (!is.null(resp) && status_code(resp) == 200 && file.exists(dest) && file.size(dest) >= 1000) return(dest)
    if (file.exists(dest)) file.remove(dest)
  }
  NULL
}

strip_disease_name <- function(x) sub("[\r\n]+.*$", "", x)
clean_pref_name <- function(x) { x <- sub("\\(.*\\)$", "", x); trimws(gsub("　", "", x)) }

ZENSU_LABEL_ALIASES <- c(
  "鳥インフルエンザ(H5N1)" = "avian_h5n1", "鳥インフルエンザ(H7N9)" = "avian_h7n9",
  "鳥インフルエンザ" = "avian_other",
  "急性弛緩性麻痺（急性灰白髄炎を除く。）" = "acute_flaccid",
  "後天性免疫不全症候群（ＨＩＶ感染症を含む）" = "aids",
  "水痘（入院例に限る。）" = "varicella_hosp"
)
TEITEN_LABEL_ALIASES <- c("感染性胃腸炎(ロタウイルス)" = "gi_rota")
STD_LABEL_MAP <- setNames(names(STD_DISEASE_CONFIG), sapply(STD_DISEASE_CONFIG, function(x) x$label))
month_labels_zenkaku <- c("１月","２月","３月","４月","５月","６月","７月","８月","９月","１０月","１１月","１２月")
to_hankaku <- function(x) chartr("０１２３４５６７８９", "0123456789", x)

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

parse_triplet_wide <- function(raw, label_map, aliases = character(0), n_data_rows = 48) {
  col1 <- as.character(raw[[1]])
  start_row <- which(grepl("^総\\s*数", col1))[1]
  if (is.na(start_row)) return(NULL)
  data_rows <- raw[start_row:(start_row + n_data_rows - 1), ]
  disease_hdr_row <- 4
  triplet_starts <- seq(2, ncol(raw), by = 3)
  pref_names <- clean_pref_name(as.character(data_rows[[1]])); pref_names[1] <- "総数"
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
    results[[unname(did)]] <- tibble(pref_name = pref_names, disease = unname(did), value = vals)
  }
  if (length(results) == 0) return(NULL)
  bind_rows(results)
}

# ============================================================
build_year <- function(year) {
  cat("\n########## ", year, "年（新規データ構築） ##########\n")
  res <- list(year = year, teiten_files = 0, std_rows = 0, zensu_files = 0, errors = character(0))

  # ① 週報定点 → data/cache/{year}-{wk}.rds を新規作成
  p10_2 <- tryCatch(download_table(year, "10_2"), error = function(e) NULL)
  if (is.null(p10_2)) {
    res$errors <- c(res$errors, "Syu_10_2取得失敗")
  } else {
    sheets <- tryCatch(excel_sheets(p10_2), error = function(e) character(0))
    sokei_sheets <- sheets[grepl("・総数$", sheets)]
    all_d <- list()
    for (sh in sokei_sheets) {
      dname <- sub("・総数$", "", sh)
      did <- TEITEN_LABEL_MAP[dname]
      if (is.na(did) && dname %in% names(TEITEN_LABEL_ALIASES)) did <- TEITEN_LABEL_ALIASES[dname]
      if (is.na(did)) next
      d <- tryCatch(parse_teiten_sheet(p10_2, sh, year), error = function(e) NULL)
      if (is.null(d)) next
      d$disease <- unname(did)
      all_d[[unname(did)]] <- d
    }
    if (length(all_d) > 0) {
      full <- bind_rows(all_d) %>% mutate(pref_name = ifelse(pref_name == "総数", "全国", pref_name))
      for (wk in sort(unique(full$week))) {
        wkd <- full %>% filter(week == wk)
        week_start <- as.Date(paste0(year, "-01-01")) + (wk - 1) * 7
        rows <- wkd %>% rowwise() %>% mutate(
          pref_code = if (pref_name == "全国") NA_integer_ else PREF_MASTER$pref_code[match(pref_name, PREF_MASTER$pref_name)],
          region    = if (pref_name == "全国") "全国" else PREF_MASTER$region[match(pref_name, PREF_MASTER$pref_name)],
          disease_label = DISEASE_CONFIG[[disease]]$label
        ) %>% ungroup() %>%
          transmute(year = year, week = wk, date = week_start, pref_code, pref_name, region,
                    disease, disease_label, reports_per_site,
                    is_provisional = FALSE, data_source = CONFIRMED_SOURCE)
        f <- get_cache_path(year, wk)
        if (!file.exists(f)) { saveRDS(rows, f); res$teiten_files <- res$teiten_files + 1 }
      }
    }
  }
  cat("週報定点: 新規作成ファイル数", res$teiten_files, "\n")

  # ② 月報疾患(STD) → std_data.rdsに新規行を追記
  p9_1 <- tryCatch(download_table(year, "09_1"), error = function(e) NULL)
  p9_2 <- tryCatch(download_table(year, "09_2"), error = function(e) NULL)
  if (is.null(p9_1) || is.null(p9_2)) {
    res$errors <- c(res$errors, "Syu_09_1/09_2取得失敗")
  } else {
    reports_list <- list(); persite_list <- list()
    for (i in seq_along(month_labels_zenkaku)) {
      sh <- month_labels_zenkaku[i]
      r_raw <- tryCatch(as.data.frame(suppressMessages(read_excel(p9_1, sheet = sh, col_names = FALSE))), error = function(e) NULL)
      p_raw <- tryCatch(as.data.frame(suppressMessages(read_excel(p9_2, sheet = sh, col_names = FALSE))), error = function(e) NULL)
      if (!is.null(r_raw)) { rl <- parse_triplet_wide(r_raw, STD_LABEL_MAP); if (!is.null(rl)) { rl$month <- i; reports_list[[i]] <- rl } }
      if (!is.null(p_raw)) { pl <- parse_triplet_wide(p_raw, STD_LABEL_MAP); if (!is.null(pl)) { pl$month <- i; persite_list[[i]] <- pl } }
    }
    reports_all <- bind_rows(reports_list); persite_all <- bind_rows(persite_list)
    if (nrow(reports_all) > 0) {
      names(reports_all)[names(reports_all) == "value"] <- "reports"
      names(persite_all)[names(persite_all) == "value"] <- "per_site"
      new_std <- reports_all %>%
        left_join(persite_all, by = c("month","pref_name","disease")) %>%
        mutate(pref_name = ifelse(pref_name == "総数", "全国", pref_name),
               year = year, date = as.Date(sprintf("%d-%02d-01", year, month)),
               is_provisional = FALSE, data_source = CONFIRMED_SOURCE) %>%
        select(year, month, date, pref_name, disease, reports, per_site, is_provisional, data_source)

      old_std <- load_std_cached()
      key_exists <- new_std %>% semi_join(old_std, by = c("year","month","pref_name","disease"))
      new_std_only <- new_std %>% anti_join(old_std, by = c("year","month","pref_name","disease"))
      if (nrow(new_std_only) > 0) {
        merged <- bind_rows(old_std, new_std_only) %>% arrange(date, pref_name, disease)
        saveRDS(merged, STD_DATA_CACHE)
        res$std_rows <- nrow(new_std_only)
      }
    }
  }
  cat("月報疾患(STD): 新規追加行数", res$std_rows, "\n")

  # ③ 全数把握 → data/cache_zensu/{year}-{wk}.rds を新規作成
  p1_1 <- tryCatch(download_table(year, "01_1"), error = function(e) NULL)
  if (is.null(p1_1)) {
    res$errors <- c(res$errors, "Syu_01_1取得失敗")
  } else {
    zsheets <- tryCatch(excel_sheets(p1_1), error = function(e) character(0))
    week_sheets <- zsheets[grepl("週$", zsheets) & zsheets != "総　数"]
    zlist <- list()
    for (sh in week_sheets) {
      wk <- suppressWarnings(as.integer(sub("週$", "", to_hankaku(sh))))
      if (is.na(wk)) next
      raw <- tryCatch(as.data.frame(suppressMessages(read_excel(p1_1, sheet = sh, col_names = FALSE))), error = function(e) NULL)
      if (is.null(raw)) next
      long <- tryCatch(parse_triplet_wide(raw, ZENSU_LABEL_MAP, ZENSU_LABEL_ALIASES), error = function(e) NULL)
      if (is.null(long)) next
      long$week <- wk
      zlist[[sh]] <- long
    }
    zfull <- bind_rows(zlist)
    if (nrow(zfull) > 0) {
      names(zfull)[names(zfull) == "value"] <- "cases"
      zfull <- zfull %>% mutate(pref_name = ifelse(pref_name == "総数", "全国", pref_name))
      for (wk in sort(unique(zfull$week))) {
        wkd <- zfull %>% filter(week == wk)
        week_start <- as.Date(paste0(year, "-01-01")) + (wk - 1) * 7
        rows <- wkd %>% rowwise() %>% mutate(
          pref_code = if (pref_name == "全国") NA_integer_ else PREF_MASTER$pref_code[match(pref_name, PREF_MASTER$pref_name)],
          region    = if (pref_name == "全国") "全国" else PREF_MASTER$region[match(pref_name, PREF_MASTER$pref_name)],
          disease_label = ZENSU_DISEASE_CONFIG[[disease]]$label,
          disease_class = ZENSU_DISEASE_CONFIG[[disease]]$class
        ) %>% ungroup() %>%
          transmute(year = year, week = wk, date = week_start, pref_code, pref_name, region,
                    disease, disease_label, disease_class, cases, cumulative = NA_integer_,
                    is_provisional = FALSE, data_source = CONFIRMED_SOURCE)
        f <- get_zensu_cache_path(year, wk)
        if (!file.exists(f)) { saveRDS(rows, f); res$zensu_files <- res$zensu_files + 1 }
      }
    }
  }
  cat("全数把握: 新規作成ファイル数", res$zensu_files, "\n")
  if (length(res$errors) > 0) cat("エラー:", paste(res$errors, collapse = "; "), "\n")
  res
}

results <- list()
for (yr in 2001:2011) {
  results[[as.character(yr)]] <- tryCatch(build_year(yr),
    error = function(e) { cat("!!! 年全体エラー", yr, ":", conditionMessage(e), "\n"); list(year=yr, teiten_files=NA, std_rows=NA, zensu_files=NA, errors=conditionMessage(e)) })
}

cat("\n\n========== サマリー ==========\n")
summary_df <- bind_rows(lapply(results, function(r) {
  tibble(year = r$year, teiten_files = r$teiten_files, std_rows = r$std_rows, zensu_files = r$zensu_files,
         errors = paste(r$errors, collapse = "; "))
}))
print(as.data.frame(summary_df))
