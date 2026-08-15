# 兵庫県 感染症対策センター「週報集計表（保健所別、年齢階級別、週別）」Excel
# https://web.pref.hyogo.lg.jp/iphs01/kansensho_jyoho/download/documents/weekly_{YEAR}-{WEEK}w_t3201-t3203.xlsx

# 旧形式（2025年分、ファイル名末尾が -newt3201-t3203.xls）は新形式と
# 行・列の向きが逆転しており（新形式: 疾患が行・保健所が列、旧形式:
# 保健所が行・疾患が列）、「保健所別患者数－その{N}－［男女合計］
# （疾患名…）」という見出し行が約25行おきに複数ブロック繰り返される。
# ［男性］［女性］のみのブロックは無視し、［男女合計］ブロックだけを使う。
.fetch_hyogo_legacy <- function(tmp, week_label_hint = NULL) {
  d <- as.data.frame(readxl::read_excel(tmp, sheet = "T3201", col_names = FALSE))
  block_starts <- which(grepl("保健所別患者数.*［男女合計］", d[[1]]))
  if (length(block_starts) == 0) stop("兵庫県(旧形式): 保健所別患者数ブロックが見つかりません")

  week_label <- NA_character_
  wl <- regmatches(as.character(d[block_starts[1] + 1, 1]),
                    regexpr("20[0-9]{2}年\\s*0*[0-9]+週", as.character(d[block_starts[1] + 1, 1])))
  if (length(wl) > 0 && nchar(wl) > 0) {
    m <- regmatches(wl, regexec("(20[0-9]{2})年\\s*0*([0-9]+)週", wl))[[1]]
    if (length(m) == 3) week_label <- sprintf("%s年第%s週", m[2], m[3])
  }

  out <- list()
  for (bs in block_starts) {
    disease_row <- unlist(d[bs + 3, ])
    disease_cols <- which(!is.na(disease_row))
    diseases <- trimws(gsub("[　]", "", disease_row[disease_cols]))
    # 保健所データ行: ブロック見出し+3(疾患名行)+1(小計/定点当り見出し行)の
    # 次の行から、"総数"行を挟んで各保健所が1行ずつ並ぶ
    data_rows <- (bs + 5):(bs + 22)
    data_rows <- data_rows[data_rows <= nrow(d)]
    for (ri in data_rows) {
      hname <- trimws(gsub("[　]", "", as.character(d[ri, 1])))
      if (is.na(hname) || nchar(hname) == 0 || hname == "総数") next
      for (ci in seq_along(disease_cols)) {
        col <- disease_cols[ci]
        out[[length(out) + 1]] <- data.frame(
          pref = "兵庫県", week_label = week_label, hokenjo = hname,
          disease = diseases[ci],
          count = parse_hokenjo_number(d[ri, col]),
          rate  = parse_hokenjo_number(d[ri, col + 1]),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  do.call(rbind, out)
}

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

  sheet_names <- readxl::excel_sheets(tmp)
  if ("T3201_週報感染症保健所別" %in% sheet_names) {
    sheet_name <- "T3201_週報感染症保健所別"
  } else if ("T3201" %in% sheet_names && any(grepl("保健所別患者数", as.data.frame(readxl::read_excel(tmp, sheet = "T3201", col_names = FALSE, n_max = 1))[[1]]))) {
    return(.fetch_hyogo_legacy(tmp))
  } else {
    sheet_name <- "T3201"
  }
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
