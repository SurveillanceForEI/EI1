# 山形県 感染症情報センター CSVデータ（疾患カテゴリ別、6地域: 山形県計/山形市/村山/最上/置賜/庄内）
# https://www.eiken.yamagata.yamagata.jp/kansenCSV/{YEAR}/CSV（<カテゴリ名>）<YEAR>.csv
#
# カテゴリごとに列レイアウトが微妙に異なる（インフルエンザのみ「年」列が
# 先頭にあり1列ズレる、警報レベル列が末尾に付く等）ため、固定列番号では
# なく見出し文字列（"週数"を含む行）を探して動的にオフセットを決定する。

.yamagata_csv_urls <- function(year = 2026) {
  base <- sprintf("https://www.eiken.yamagata.yamagata.jp/kansenCSV/%d/", year)
  c(
    ped   = paste0(base, "CSV%EF%BC%88%E5%B0%8F%E5%85%90%E7%A7%91%E5%AE%9A%E7%82%B9%EF%BC%89", year, ".csv"),
    covid = paste0(base, "CSV%EF%BC%88%E6%96%B0%E5%9E%8B%E3%82%B3%E3%83%AD%E3%83%8A%E3%82%A6%E3%82%A4%E3%83%AB%E3%82%B9%E6%84%9F%E6%9F%93%E7%97%87%EF%BC%89", year, ".csv"),
    flu   = paste0(base, "CSV%EF%BC%88%E3%82%A4%E3%83%B3%E3%83%95%E3%83%AB%E3%82%A8%E3%83%B3%E3%82%B6%EF%BC%89", year - 1, "-", year, "%E3%82%B7%E3%83%BC%E3%82%BA%E3%83%B3.csv"),
    kikan = paste0(base, "CSV%EF%BC%88%E5%9F%BA%E5%B9%B9%E5%AE%9A%E7%82%B9%EF%BC%89", year, ".csv"),
    eye   = paste0(base, "CSV%EF%BC%88%E7%9C%BC%E7%A7%91%E5%AE%9A%E7%82%B9%EF%BC%89", year, ".csv")
  )
}

.yamagata_read_csv <- function(url) {
  tmp <- tempfile(fileext = ".csv")
  download.file(url, tmp, mode = "wb", quiet = TRUE)
  raw <- readLines(tmp, warn = FALSE)
  raw <- iconv(raw, from = "shift-jis", to = "UTF-8", sub = "")
  lapply(raw, function(l) strsplit(l, ",", fixed = TRUE)[[1]])
}

.yamagata_parse_one <- function(url, category_label) {
  lines <- .yamagata_read_csv(url)
  n_col <- max(sapply(lines, length))
  pad <- function(v) { length(v) <- n_col; v }
  mat <- do.call(rbind, lapply(lines, pad))

  # "週数"を含むセルの行=列見出し行
  header_row <- which(apply(mat, 1, function(r) any(grepl("週数", r), na.rm = TRUE)))[1]
  if (is.na(header_row)) stop("週数見出しが見つかりません: ", url)

  disease_row <- header_row - 2  # 疾患名の行（見出し行の2つ上）
  region_row  <- header_row - 1  # 地域名の行（見出し行の1つ上）

  # 「報告」列（各疾患×地域ブロックの1列目）の列インデックスを集める
  report_cols <- which(mat[header_row, ] == "報告")

  results <- list()
  for (rc in report_cols) {
    rate_col <- rc + 1
    region <- trimws(mat[region_row, rc])
    if (is.na(region) || nchar(region) == 0) next
    region <- if (region == "山形県") "山形県計" else region

    # 疾患名は disease_row 上でこのブロックの左端（直近の非空セル）から取る
    dcol <- rc
    while (dcol > 1 && (is.na(mat[disease_row, dcol]) || nchar(trimws(mat[disease_row, dcol])) == 0)) dcol <- dcol - 1
    disease <- trimws(mat[disease_row, dcol])
    if (nchar(disease) == 0) disease <- category_label

    # データ行のうち、最後に報告数・定点数どちらかが埋まっている行（＝最新週）を採用
    # 「計」（累計）行など週数が数値でない行は対象から除外する
    week_col <- header_row_week_col(mat, header_row)
    data_rows <- (header_row + 1):nrow(mat)
    data_rows <- data_rows[data_rows <= nrow(mat)]
    data_rows <- data_rows[grepl("^[0-9]+$", trimws(mat[data_rows, week_col]))]
    filled <- which(!is.na(mat[data_rows, rc]) & nchar(trimws(mat[data_rows, rc])) > 0)
    if (length(filled) == 0) next
    last_row <- data_rows[max(filled)]

    week_num <- trimws(mat[last_row, header_row_week_col(mat, header_row)])

    results[[length(results) + 1]] <- data.frame(
      pref = "山形県", week_label = paste0("2026年第", week_num, "週"),
      hokenjo = region, disease = disease,
      count = parse_hokenjo_number(mat[last_row, rc]),
      rate  = parse_hokenjo_number(mat[last_row, rate_col]),
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, results)
}

header_row_week_col <- function(mat, header_row) {
  which(mat[header_row, ] == "週数")[1]
}

# ARI（急性呼吸器感染症）は上記CSV群に含まれず、週報PDF3ページ目の
# 「＜定点把握感染症＞」表にのみ掲載されている。
# 列順は 全国(先週) 山形県(先週,今週) 山形市(先週,今週) 村山(先週,今週)
#        最上(先週,今週) 置賜(先週,今週) 庄内(先週,今週) [累積]
# の13個の数値（報告数の行、定点あたり報告数の行）が、疾患名を含む行の
# 直前・直後の行にそれぞれ現れる。
.yamagata_parse_ari <- function(pdf_url, week_label = NA_character_) {
  if (!requireNamespace("pdftools", quietly = TRUE)) stop("pdftools パッケージが必要です")
  tmp <- tempfile(fileext = ".pdf")
  download.file(pdf_url, tmp, mode = "wb", quiet = TRUE)
  txt <- pdftools::pdf_text(tmp)
  page <- txt[grepl("急性呼吸器感染症定点", txt) & grepl("報告数", txt)][1]
  if (is.na(page)) stop("ARI表のページが見つかりません")
  lines <- strsplit(page, "\n")[[1]]

  count_idx <- grep("^\\s*[0-9]{4,}\\s+[0-9]+\\s+[0-9]+", lines)[1]
  if (is.na(count_idx)) stop("報告数の行が見つかりません")
  rate_idx <- count_idx + 2  # 疾患名の行を挟んで2行下が定点あたり報告数の行

  num_line <- function(line) {
    toks <- strsplit(trimws(line), "\\s+")[[1]]
    toks <- toks[grepl("^[0-9]+(\\.[0-9]+)?$", toks)]
    as.numeric(toks)
  }
  counts <- num_line(lines[count_idx])
  rates  <- num_line(lines[rate_idx])
  if (length(counts) < 13 || length(rates) < 13) {
    warning("ARIデータの列数が想定と異なります（counts=", length(counts), " rates=", length(rates), "）")
    return(NULL)
  }

  # counts/rates: [全国31, 山形県31, 山形県32, 山形市31, 山形市32, 村山31, 村山32,
  #                最上31, 最上32, 置賜31, 置賜32, 庄内31, 庄内32]
  hokenjo_idx <- c("山形市" = 5, "村山" = 7, "最上" = 9, "置賜" = 11, "庄内" = 13)
  out <- list()
  for (h in names(hokenjo_idx)) {
    out[[length(out) + 1]] <- data.frame(
      pref = "山形県", week_label = week_label, hokenjo = h,
      disease = "急性呼吸器感染症(ARI)",
      count = counts[hokenjo_idx[[h]]], rate = rates[hokenjo_idx[[h]]],
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, out)
}

fetch_yamagata <- function(year = 2026, ari_pdf_url = NULL) {
  urls <- .yamagata_csv_urls(year)
  out <- list(
    .yamagata_parse_one(urls[["ped"]],   "小児科定点"),
    .yamagata_parse_one(urls[["covid"]], "新型コロナウイルス感染症"),
    .yamagata_parse_one(urls[["flu"]],   "インフルエンザ"),
    .yamagata_parse_one(urls[["kikan"]], "基幹定点"),
    .yamagata_parse_one(urls[["eye"]],   "眼科定点")
  )
  df <- do.call(rbind, out)
  # 「山形県計」（県全体）は保健所別データではないため除外
  df <- df[df$hokenjo != "山形県計", ]

  if (!is.null(ari_pdf_url)) {
    ari <- tryCatch(.yamagata_parse_ari(ari_pdf_url, week_label = unique(df$week_label)[1]),
                     error = function(e) { warning("ARI取得失敗: ", conditionMessage(e)); NULL })
    if (!is.null(ari)) df <- rbind(df, ari)
  }
  df
}
