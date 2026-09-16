# 静岡県「定点報告感染症ダウンロード用（2015～2026）」Tableau Publicダッシュボードから
# shizuoka_tableau.py（Playwright）でダウンロードしたクロス集計CSVを、
# 他県と共通のhokenjo_history.rdsスキーマ（pref/week_label/week_num/hokenjo/disease/count/rate）
# に変換する。
#
# CSVはワイド形式（行=疾患×保健所×指標(報告数/定点当り報告数)、列=年×週）で、UTF-16LE・
# タブ区切り。先頭2行が年・週のヘッダー、3行目が列ラベル（感染症名/保健所名/空列/週番号...）。
# 年によって週の列数が52または53と異なるため、固定位置ではなくヘッダー行から
# 実際の(年,週)対応を都度読み取る。
#
# 注: このTableauダッシュボードは記事執筆時点(2026-09)でまだ2026年分のデータを
# 含んでおらず、2015〜2025年の過去データのみカバーする。現在の週のデータは
# 引き続きPDF方式のfetch_shizuoka()/fetch_shizuoka_history()で取得する必要がある。

.shizuoka_disease_name_fix <- function(x) {
  # Tableau側の表記とPDF側の表記の句読点の揺れを統一し、同一疾患が別名で
  # 二重登録されるのを防ぐ
  ifelse(x == "感染性胃腸炎（病原体がロタウイルスであるものに限る）",
         "感染性胃腸炎（病原体がロタウイルスであるものに限る。）", x)
}

parse_shizuoka_tableau_csv <- function(csv_path) {
  raw <- readBin(csv_path, what = "raw", n = file.info(csv_path)$size)
  txt <- iconv(list(raw), from = "UTF-16LE", to = "UTF-8")
  lines <- strsplit(txt, "\r\n|\n")[[1]]
  # BOM除去
  lines[1] <- sub("^﻿", "", lines[1])

  split_tsv <- function(l) strsplit(l, "\t", fixed = TRUE)[[1]]

  year_row <- split_tsv(lines[2])
  label_row <- split_tsv(lines[3])

  n_col <- length(label_row)
  # 先頭3列（感染症名・保健所名・空列）以降が年・週のデータ列
  data_col_idx <- 4:n_col
  years <- suppressWarnings(as.integer(year_row[data_col_idx]))
  weeks <- suppressWarnings(as.integer(label_row[data_col_idx]))

  valid_col <- !is.na(years) & !is.na(weeks)
  data_col_idx <- data_col_idx[valid_col]
  years <- years[valid_col]
  weeks <- weeks[valid_col]

  out <- list()
  for (i in 4:length(lines)) {
    toks <- split_tsv(lines[i])
    if (length(toks) < 3) next
    disease <- .shizuoka_disease_name_fix(toks[1])
    hokenjo <- toks[2]
    measure <- toks[3]
    if (!nzchar(disease) || !nzchar(hokenjo)) next
    vals <- suppressWarnings(as.numeric(toks[data_col_idx]))
    key <- paste(disease, hokenjo, years, weeks, sep = "")
    if (measure == "報告数") {
      out[[length(out) + 1]] <- data.frame(
        key = key, disease = disease, hokenjo = hokenjo, year = years, week_num = weeks,
        count = vals, stringsAsFactors = FALSE
      )
    } else if (measure == "定点当り報告数") {
      out[[length(out) + 1]] <- data.frame(
        key = key, disease = disease, hokenjo = hokenjo, year = years, week_num = weeks,
        rate = vals, stringsAsFactors = FALSE
      )
    }
  }
  if (length(out) == 0) return(NULL)

  counts <- do.call(rbind, Filter(function(d) "count" %in% names(d), out))
  rates  <- do.call(rbind, Filter(function(d) "rate"  %in% names(d), out))

  merged <- merge(
    counts[, c("disease", "hokenjo", "year", "week_num", "count")],
    rates[,  c("disease", "hokenjo", "year", "week_num", "rate")],
    by = c("disease", "hokenjo", "year", "week_num"), all = TRUE
  )
  merged <- merged[!(is.na(merged$count) & is.na(merged$rate)), ]
  merged$pref <- "静岡県"
  merged$week_label <- sprintf("%d年第%d週", merged$year, merged$week_num)
  merged[, c("pref", "week_label", "week_num", "hokenjo", "disease", "count", "rate")]
}
