# ============================================================
# iasr_loader.R — IASR ウイルス・細菌検出状況データ取得・解析
# ソース（ウイルス直近）: https://kansen-levelmap.mhlw.go.jp/Byogentai/Csv/
# ソース（ウイルス過去）: https://id-info.jihs.go.jp/surveillance/iasr/data/previous-summary-of-virus/
# ソース（細菌直近）:     https://kansen-levelmap.mhlw.go.jp/Byogentai/Csv/
# ============================================================

library(httr)
library(dplyr)
library(tidyr)

IASR_CACHE_DIR   <- "data/cache_iasr"
IASR_CACHE_HOURS <- 24
IASR_HIST_BASE   <- "https://id-info.jihs.go.jp/surveillance/iasr/data/previous-summary-of-virus/"

# 細菌アーカイブURL テンプレート（{NN}=ファイル番号2桁、古い順）
# 2012-2018: ドット区切り形式 data{YYYY}.{NN}j.csv (id-info.jihs.go.jp/niid/images/...)
# 2019-2022: 連続形式 data{YYYY}{NN}j.csv (id-info.jihs.go.jp/niid/images/...)
.BACTERIA_ARCHIVES <- list(
  list(tpl = "https://id-info.jihs.go.jp/niid/images/iasr/arc/gb/2016/data2016.{NN}j.csv"),  # 2012-2016
  list(tpl = "https://id-info.jihs.go.jp/niid/images/iasr/arc/gb/2018/data2018.{NN}j.csv"),  # 2014-2018
  list(tpl = "https://id-info.jihs.go.jp/niid/images/iasr/arc/gb/2021/data2021{NN}j.csv"),   # 2017-2021
  list(tpl = "https://id-info.jihs.go.jp/niid/images/iasr/arc/gb/2022/data2022{NN}j.csv"),   # 2018-2022
  list(tpl = "https://id-info.jihs.go.jp/surveillance/iasr/graph/bacteria-archive/2023/data2023{NN}j.csv"),  # 2019-2023
  list(tpl = "https://id-info.jihs.go.jp/surveillance/iasr/graph/bacteria-archive/2024/data2024{NN}j.csv"),  # 2020-2024
  list(tpl = "https://id-info.jihs.go.jp/surveillance/iasr/graphdata/2025year/bacteria2025/data2025{NN}j.csv")  # 2021-2025
)

# ── ウイルス・細菌カテゴリ定義 ────────────────────────────────
# 細菌ファイルのfmt:
#   "year_rows"      行=年, 列=月01..12
#   "pathogen_rows"  行=病原体, 列=YYYY/M（ウイルスと同形式）
#   "case_reports"   long形式: 細菌名, 国内例, 海外例, 年月
# file_num: アーカイブURL構築用のファイル番号
IASR_CATEGORIES <- list(
  # ── ウイルス ──────────────────────────────────────────────
  enterovirus1 = list(
    label            = "エンテロウイルス(1)",
    type             = "virus",
    url_mon          = "https://kansen-levelmap.mhlw.go.jp/Byogentai/Csv/data57j.csv",
    url_yr           = "https://kansen-levelmap.mhlw.go.jp/Byogentai/Csv/data58j.csv",
    hist_prefix      = "v1",
    hist_old_prefix  = "ev1-"
  ),
  enterovirus2 = list(
    label            = "エンテロウイルス(2)",
    type             = "virus",
    url_mon          = "https://kansen-levelmap.mhlw.go.jp/Byogentai/Csv/data59j.csv",
    url_yr           = "https://kansen-levelmap.mhlw.go.jp/Byogentai/Csv/data60j.csv",
    hist_prefix      = "v2",
    hist_old_prefix  = "ev2-"
  ),
  influenza = list(
    label            = "インフルエンザ&呼吸器ウイルス",
    type             = "virus",
    url_mon          = "https://kansen-levelmap.mhlw.go.jp/Byogentai/Csv/data61j.csv",
    url_yr           = "https://kansen-levelmap.mhlw.go.jp/Byogentai/Csv/data62j.csv",
    hist_prefix      = "v3",
    hist_old_prefix  = "infl"
  ),
  gastro = list(
    label            = "胃腸炎ウイルス",
    type             = "virus",
    url_mon          = "https://kansen-levelmap.mhlw.go.jp/Byogentai/Csv/data63j.csv",
    url_yr           = "https://kansen-levelmap.mhlw.go.jp/Byogentai/Csv/data64j.csv",
    hist_prefix      = "v4",
    hist_old_prefix  = "rota"
  ),
  adeno = list(
    label            = "アデノウイルス",
    type             = "virus",
    url_mon          = "https://kansen-levelmap.mhlw.go.jp/Byogentai/Csv/data65j.csv",
    url_yr           = "https://kansen-levelmap.mhlw.go.jp/Byogentai/Csv/data66j.csv",
    hist_prefix      = "v5",
    hist_old_prefix  = "adv"
  ),
  herpes = list(
    label            = "ヘルペス群ウイルス&その他",
    type             = "virus",
    url_mon          = "https://kansen-levelmap.mhlw.go.jp/Byogentai/Csv/data67j.csv",
    url_yr           = "https://kansen-levelmap.mhlw.go.jp/Byogentai/Csv/data68j.csv",
    hist_prefix      = "v6",
    hist_old_prefix  = "others"
  ),
  # ── 細菌 ──────────────────────────────────────────────────
  vtec = list(
    label = "腸管出血性大腸菌（VTEC）",
    type  = "bacteria",
    files = list(
      list(name=NULL, url="https://kansen-levelmap.mhlw.go.jp/Byogentai/Csv/data30j.csv", fmt="vtec_weekly", file_num="30")
    )
  ),
  enteric_bact = list(
    label = "腸管感染症（赤痢・チフス・コレラ）",
    type  = "bacteria",
    files = list(
      list(name="赤痢菌",                  url="https://kansen-levelmap.mhlw.go.jp/Byogentai/Csv/data45j.csv", fmt="case_reports", file_num="45"),
      list(name="チフス菌・パラチフスA菌", url="https://kansen-levelmap.mhlw.go.jp/Byogentai/Csv/data46j.csv", fmt="case_reports", file_num="46"),
      list(name="コレラ菌",                url="https://kansen-levelmap.mhlw.go.jp/Byogentai/Csv/data49j.csv", fmt="case_reports", file_num="49")
    )
  ),
  foodborne = list(
    label = "食中毒菌",
    type  = "bacteria",
    files = list(
      list(name="サルモネラ",               url="https://kansen-levelmap.mhlw.go.jp/Byogentai/Csv/data47j.csv", fmt="year_rows", file_num="47"),
      list(name="カンピロバクター",         url="https://kansen-levelmap.mhlw.go.jp/Byogentai/Csv/data51j.csv", fmt="year_rows", file_num="51"),
      list(name="腸炎ビブリオ",             url="https://kansen-levelmap.mhlw.go.jp/Byogentai/Csv/data50j.csv", fmt="year_rows", file_num="50"),
      list(name="病原大腸菌（VTECを除く）", url="https://kansen-levelmap.mhlw.go.jp/Byogentai/Csv/data44j.csv", fmt="year_rows", file_num="44"),
      list(name="黄色ブドウ球菌",           url="https://kansen-levelmap.mhlw.go.jp/Byogentai/Csv/data52j.csv", fmt="year_rows", file_num="52"),
      list(name="ウエルシュ菌",             url="https://kansen-levelmap.mhlw.go.jp/Byogentai/Csv/data53j.csv", fmt="year_rows", file_num="53"),
      list(name="セレウス菌",               url="https://kansen-levelmap.mhlw.go.jp/Byogentai/Csv/data54j.csv", fmt="year_rows", file_num="54")
    )
  )
)

# 英語月名 → 月番号
.MONTH_MAP <- c(Jan=1,Feb=2,Mar=3,Apr=4,May=5,Jun=6,
                Jul=7,Aug=8,Sep=9,Oct=10,Nov=11,Dec=12)

# ── ダウンロードユーティリティ ─────────────────────────────
.download_to_tmp <- function(url) {
  tmp <- tempfile(fileext = ".csv")
  resp <- httr::GET(url, httr::timeout(30))
  if (httr::status_code(resp) != 200) return(NULL)
  writeBin(httr::content(resp, "raw"), tmp)
  tmp
}

# ── Format B / ウイルス直近CSVパース（行=病原体, 列=YYYY/M or YYYY/MM）──────
.parse_iasr_csv <- function(url, time_type) {
  tryCatch({
    tmp <- .download_to_tmp(url)
    if (is.null(tmp)) return(NULL)
    on.exit(unlink(tmp))

    raw <- read.csv(tmp, skip = 1, header = TRUE, check.names = FALSE,
                    fileEncoding = "CP932", stringsAsFactors = FALSE,
                    na.strings = c("", "-", "NA"))
    if (ncol(raw) < 2) return(NULL)

    d <- raw %>%
      rename(virus = 1) %>%
      filter(!is.na(virus), trimws(virus) != "") %>%
      pivot_longer(-virus, names_to = "period", values_to = "count") %>%
      mutate(count = suppressWarnings(as.integer(count)),
             count = ifelse(is.na(count), 0L, count))

    if (time_type == "monthly") {
      d <- d %>%
        mutate(date = suppressWarnings(
          as.Date(paste0(period, "/01"), "%Y/%m/%d"))) %>%
        filter(!is.na(date))
    } else {
      d <- d %>%
        mutate(date = suppressWarnings(
          as.Date(paste0(trimws(period), "-01-01")))) %>%
        filter(!is.na(date))
    }

    d %>% mutate(time_type = time_type) %>%
      select(virus, period, date, count, time_type)
  }, error = function(e) {
    message("IASR parse error (", url, "): ", e$message); NULL
  })
}

# ── Format A 細菌: 行=年(2022-2026), 列=月01..12 ──────────────────────────
.parse_bacteria_year_rows <- function(url, pathogen_name) {
  tryCatch({
    tmp <- .download_to_tmp(url)
    if (is.null(tmp)) return(NULL)
    on.exit(unlink(tmp))

    raw <- read.csv(tmp, skip = 1, header = TRUE, check.names = FALSE,
                    fileEncoding = "CP932", stringsAsFactors = FALSE,
                    na.strings = c("", "-", "NA"))
    if (ncol(raw) < 13) return(NULL)

    names(raw)[1] <- "year"
    month_cols <- names(raw)[2:13]

    raw %>%
      mutate(year_str = trimws(as.character(year))) %>%
      filter(grepl("^[0-9]{4}$", year_str)) %>%
      mutate(year = as.integer(year_str)) %>%
      select(year, all_of(month_cols)) %>%
      mutate(across(-year, as.character)) %>%
      pivot_longer(-year, names_to = "month_col", values_to = "count") %>%
      mutate(
        month_num = suppressWarnings(as.integer(trimws(month_col))),
        date      = suppressWarnings(as.Date(sprintf("%d-%02d-01", year, month_num))),
        period    = format(date, "%Y/%m"),
        count     = suppressWarnings(as.integer(gsub("[^0-9]", "", count))),
        count     = ifelse(is.na(count), 0L, count),
        virus     = pathogen_name,
        time_type = "monthly"
      ) %>%
      filter(!is.na(date), !is.na(month_num), month_num >= 1, month_num <= 12) %>%
      select(virus, period, date, count, time_type)
  }, error = function(e) {
    message("Bacteria year_rows error (", url, "): ", e$message); NULL
  })
}

# ── VTEC週別血清型CSVパース → 月別集計（行=年×血清型, 列=週01..53）────────
.parse_vtec_weekly <- function(url) {
  tryCatch({
    tmp <- .download_to_tmp(url)
    if (is.null(tmp)) return(NULL)
    on.exit(unlink(tmp))

    raw <- read.csv(tmp, skip = 1, header = TRUE, check.names = FALSE,
                    fileEncoding = "CP932", stringsAsFactors = FALSE,
                    na.strings = c("", "-", "NA"))
    if (ncol(raw) < 3) return(NULL)

    # col1=年, col2=細菌名, col3以降=週番号01..53
    names(raw)[1:2] <- c("year", "virus")
    week_cols <- names(raw)[3:ncol(raw)]

    raw %>%
      filter(!is.na(year), grepl("^[0-9]{4}$", trimws(as.character(year))),
             !is.na(virus), trimws(virus) != "") %>%
      mutate(year = as.integer(trimws(as.character(year))),
             virus = trimws(virus)) %>%
      select(year, virus, all_of(week_cols)) %>%
      mutate(across(-c(year, virus), as.character)) %>%
      pivot_longer(-c(year, virus), names_to = "week_col", values_to = "count") %>%
      mutate(
        week_num = suppressWarnings(as.integer(trimws(week_col))),
        # 週の月曜日を近似日付として使用
        date_approx = as.Date(paste0(year, "-01-01")) + (week_num - 1L) * 7L,
        month_num   = as.integer(format(date_approx, "%m")),
        date        = as.Date(sprintf("%d-%02d-01", year, month_num)),
        period      = format(date, "%Y/%m"),
        count       = suppressWarnings(as.integer(gsub("[^0-9]", "", count))),
        count       = ifelse(is.na(count), 0L, count),
        time_type   = "monthly"
      ) %>%
      filter(!is.na(date), !is.na(week_num)) %>%
      group_by(virus, period, date, time_type) %>%
      summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
      select(virus, period, date, count, time_type)
  }, error = function(e) {
    message("VTEC weekly parse error (", url, "): ", e$message); NULL
  })
}

# ── Format C 細菌: long形式（細菌名, 国内例, 海外例, 年月）──────────────────
.parse_bacteria_case_reports <- function(url) {
  tryCatch({
    tmp <- .download_to_tmp(url)
    if (is.null(tmp)) return(NULL)
    on.exit(unlink(tmp))

    raw <- read.csv(tmp, skip = 1, header = TRUE, check.names = FALSE,
                    fileEncoding = "CP932", stringsAsFactors = FALSE,
                    na.strings = c("", "-", "NA"))
    # 必要列の確認
    cn <- names(raw)
    col_bact <- cn[1]
    col_dom  <- cn[grep("国内", cn)[1]]
    col_imp  <- cn[grep("海外", cn)[1]]
    col_ym   <- cn[grep("年月", cn)[1]]
    if (any(is.na(c(col_dom, col_imp, col_ym)))) return(NULL)

    raw %>%
      rename(virus = all_of(col_bact), domestic = all_of(col_dom),
             imported = all_of(col_imp), period = all_of(col_ym)) %>%
      filter(!is.na(virus), trimws(virus) != "",
             !is.na(period), trimws(period) != "") %>%
      mutate(
        domestic = suppressWarnings(as.integer(gsub("[^0-9]", "", domestic))),
        imported = suppressWarnings(as.integer(gsub("[^0-9]", "", imported))),
        domestic = ifelse(is.na(domestic), 0L, domestic),
        imported = ifelse(is.na(imported), 0L, imported),
        count    = domestic + imported,
        date     = suppressWarnings(as.Date(paste0(trimws(period), "/01"), "%Y/%m/%d")),
        period   = format(date, "%Y/%m"),
        time_type = "monthly"
      ) %>%
      filter(!is.na(date)) %>%
      group_by(virus, period, date, time_type) %>%
      summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
      select(virus, period, date, count, time_type)
  }, error = function(e) {
    message("Bacteria case_reports error (", url, "): ", e$message); NULL
  })
}

# ── ウイルス過去月別CSVパース（UTF-8: id-info.jihs.go.jp 年別ファイル）───────
.parse_iasr_hist_monthly <- function(url, year) {
  tryCatch({
    tmp <- .download_to_tmp(url)
    if (is.null(tmp)) return(NULL)
    on.exit(unlink(tmp))

    lines <- suppressWarnings(readLines(tmp, encoding = "UTF-8", warn = FALSE))
    header_idx <- which(grepl(",TOTAL,", lines, fixed = TRUE))[1]
    if (is.na(header_idx)) return(NULL)

    data_lines <- lines[(header_idx + 2):length(lines)]
    # 旧フォーマット（2012-2015）は行頭に空列(カンマ)があるので除去
    data_lines <- sub("^,", "", data_lines)
    data_lines <- data_lines[nchar(trimws(data_lines)) > 0]
    data_lines <- data_lines[!grepl("^NT:|^\\s*$", data_lines)]
    if (length(data_lines) == 0) return(NULL)

    con <- textConnection(paste(data_lines, collapse = "\n"))
    raw <- tryCatch(
      read.csv(con, header = FALSE, check.names = FALSE,
               stringsAsFactors = FALSE, na.strings = c("", "-", "NA")),
      error = function(e) NULL)
    close(con)
    if (is.null(raw) || ncol(raw) < 13) return(NULL)

    names(raw)[1] <- "virus"
    n_month <- min(12, ncol(raw) - 2)
    for (i in seq_len(n_month)) names(raw)[i + 2] <- month.abb[i]

    raw %>%
      filter(!is.na(virus), trimws(virus) != "") %>%
      select(virus, all_of(month.abb[seq_len(n_month)])) %>%
      mutate(across(-virus, as.character)) %>%
      pivot_longer(-virus, names_to = "month_name", values_to = "count") %>%
      mutate(
        month_num = match(month_name, month.abb),
        date      = as.Date(sprintf("%d-%02d-01", as.integer(year), month_num)),
        period    = format(date, "%Y/%m"),
        count     = suppressWarnings(as.integer(gsub("[^0-9]", "", count))),
        count     = ifelse(is.na(count), 0L, count),
        time_type = "monthly"
      ) %>%
      filter(!is.na(date)) %>%
      select(virus, period, date, count, time_type)
  }, error = function(e) {
    message("IASR hist error (", url, ", year=", year, "): ", e$message); NULL
  })
}

# ── ウイルス過去月別データ一括取得（2012〜2022）──────────────────────────────
# 2016+: v{N}m{YYYY}.csv  2012-2015: {old_prefix}{YY}.csv (旧命名規則)
.load_iasr_hist_all <- function(hist_prefix, hist_old_prefix = NULL) {
  old_parts <- if (!is.null(hist_old_prefix)) {
    lapply(2012:2015, function(yr) {
      yy <- substr(as.character(yr), 3, 4)
      .parse_iasr_hist_monthly(paste0(IASR_HIST_BASE, hist_old_prefix, yy, ".csv"), yr)
    })
  } else list()
  new_parts <- lapply(2016:2022, function(yr) {
    .parse_iasr_hist_monthly(paste0(IASR_HIST_BASE, hist_prefix, "m", yr, ".csv"), yr)
  })
  all_parts <- Filter(function(x) !is.null(x) && nrow(x) > 0, c(old_parts, new_parts))
  if (length(all_parts) == 0) return(NULL)
  bind_rows(all_parts)
}

# ── カテゴリ別キャッシュ取得 ─────────────────────────────────
load_iasr_category <- function(cat_id) {
  if (!dir.exists(IASR_CACHE_DIR)) dir.create(IASR_CACHE_DIR, recursive = TRUE)
  cache_path <- file.path(IASR_CACHE_DIR, paste0(cat_id, ".rds"))

  if (file.exists(cache_path)) {
    age_h <- as.numeric(difftime(Sys.time(), file.mtime(cache_path), units = "hours"))
    if (age_h < IASR_CACHE_HOURS) return(readRDS(cache_path))
  }

  cfg <- IASR_CATEGORIES[[cat_id]]

  if (isTRUE(cfg$type == "bacteria")) {
    parts <- lapply(cfg$files, function(f) {
      # アーカイブ（古い順）+ 直近の順で読み込み、直近データを優先
      arch_parts <- lapply(.BACTERIA_ARCHIVES, function(a) {
        arch_url <- gsub("{NN}", f$file_num, a$tpl, fixed = TRUE)
        switch(f$fmt,
          year_rows     = .parse_bacteria_year_rows(arch_url, f$name),
          pathogen_rows = .parse_iasr_csv(arch_url, "monthly"),
          case_reports  = .parse_bacteria_case_reports(arch_url),
          vtec_weekly   = .parse_vtec_weekly(arch_url),
          NULL)
      })
      recent <- switch(f$fmt,
        year_rows     = .parse_bacteria_year_rows(f$url, f$name),
        pathogen_rows = .parse_iasr_csv(f$url, "monthly"),
        case_reports  = .parse_bacteria_case_reports(f$url),
        vtec_weekly   = .parse_vtec_weekly(f$url),
        NULL)
      # 直近データの日付を除いたアーカイブ + 直近データ
      arch_parts_valid <- Filter(function(x) !is.null(x) && nrow(x) > 0, arch_parts)
      arch_all <- if (length(arch_parts_valid) > 0) bind_rows(arch_parts_valid) else NULL
      if (!is.null(recent) && nrow(recent) > 0 && !is.null(arch_all) && nrow(arch_all) > 0)
        arch_all <- arch_all %>% filter(!date %in% unique(recent$date))
      valid <- Filter(function(x) !is.null(x) && nrow(x) > 0, list(arch_all, recent))
      if (length(valid) == 0) return(NULL)
      bind_rows(valid)
    })
    d <- bind_rows(parts) %>%
      mutate(category = cat_id) %>%
      distinct(virus, date, time_type, .keep_all = TRUE) %>%
      arrange(date, virus)

  } else {
    mon  <- .parse_iasr_csv(cfg$url_mon, "monthly")
    yr   <- .parse_iasr_csv(cfg$url_yr,  "annual")
    hist <- .load_iasr_hist_all(cfg$hist_prefix, cfg$hist_old_prefix)

    if (!is.null(hist) && nrow(hist) > 0 && !is.null(mon) && nrow(mon) > 0)
      hist <- hist %>% filter(!date %in% unique(mon$date))

    d <- bind_rows(hist, mon, yr) %>%
      mutate(category = cat_id) %>%
      distinct(virus, date, time_type, .keep_all = TRUE) %>%
      arrange(time_type, date, virus)
  }

  if (!is.null(d) && nrow(d) > 0) saveRDS(d, cache_path)
  d
}

# ── 全カテゴリ一括ロード ──────────────────────────────────────
load_all_iasr <- function() {
  bind_rows(lapply(names(IASR_CATEGORIES), load_iasr_category))
}
