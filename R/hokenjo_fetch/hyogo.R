# 兵庫県 感染症対策センター「週報集計表（保健所別、年齢階級別、週別）」Excel
# https://web.pref.hyogo.lg.jp/iphs01/kansensho_jyoho/download/documents/weekly_{YEAR}-{WEEK}w_t3201-t3203.xlsx

fetch_hyogo <- function(year = NULL, week = NULL, week_url = NULL) {
  if (!requireNamespace("readxl", quietly = TRUE)) stop("readxl パッケージが必要です")
  if (is.null(week_url)) {
    if (is.null(year) || is.null(week)) stop("year/week または week_url を指定してください")
    week_url <- sprintf("https://web.pref.hyogo.lg.jp/iphs01/kansensho_jyoho/download/documents/weekly_%d-%02dw_t3201-t3203.xlsx",
                         year, week)
  }
  ext <- if (grepl("\\.xls$", week_url, ignore.case = TRUE)) ".xls" else ".xlsx"
  tmp <- tempfile(fileext = ext)
  download.file(week_url, tmp, mode = "wb", quiet = TRUE)

  sheet_name <- if ("T3201_週報感染症保健所別" %in% readxl::excel_sheets(tmp)) "T3201_週報感染症保健所別" else "T3201"
  d <- readxl::read_excel(tmp, sheet = sheet_name, col_names = FALSE)
  d <- as.data.frame(d)

  # 1行目: タイトル, 2行目: "2026年31週（...）"（week_label）, 5行目: 保健所名ヘッダー
  # 6行目以降: データ本体（列1=定点区分, 列2=疾患名, 列3以降=保健所ごとに[患者数, 定点あたり患者数]の2列組）
  week_label <- as.character(d[2, 1])

  header_row <- 5
  hokenjo_row <- 4
  hokenjo_names <- character(0)
  count_col_of <- integer(0)
  rate_col_of  <- integer(0)
  j <- 3
  while (j <= ncol(d)) {
    hname <- as.character(d[hokenjo_row, j])
    label1 <- as.character(d[header_row, j])
    label2 <- if (j + 1 <= ncol(d)) as.character(d[header_row, j + 1]) else NA
    if (!is.na(hname) && nchar(trimws(hname)) > 0 && trimws(hname) != "総計" &&
        !is.na(label1) && grepl("患者数$", label1) &&
        !is.na(label2) && grepl("定点あたり", label2)) {
      hokenjo_names <- c(hokenjo_names, trimws(hname))
      count_col_of <- c(count_col_of, j)
      rate_col_of  <- c(rate_col_of, j + 1)
      j <- j + 2
    } else {
      j <- j + 1
    }
  }

  # シートは「保健所別患者数[男女合計]」「[男]」「[女]」の3ブロックが縦に
  # 積み重なっている。1つ目のブロック（男女合計）のみを使うため、次のブロックの
  # 見出し行（列2が再び"定点区分"になる行）より手前で打ち切る
  data_start <- header_row + 1
  next_header <- which(d[data_start:nrow(d), 1] == "定点区分")
  data_end <- if (length(next_header) > 0) data_start + next_header[1] - 2 else nrow(d)

  out <- list()
  for (i in data_start:data_end) {
    disease <- trimws(as.character(d[i, 2]))
    if (is.na(disease) || nchar(disease) == 0) next
    for (k in seq_along(hokenjo_names)) {
      out[[length(out) + 1]] <- data.frame(
        pref = "兵庫県", week_label = week_label, hokenjo = hokenjo_names[k],
        disease = disease,
        count = parse_hokenjo_number(d[i, count_col_of[k]]),
        rate  = parse_hokenjo_number(d[i, rate_col_of[k]]),
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, out)
}
