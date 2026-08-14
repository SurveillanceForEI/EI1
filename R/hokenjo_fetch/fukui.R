# 福井県感染症情報センター 保健所別CSV「発生数一覧表」
# https://kansensyou-joho.pref.fukui.lg.jp/ih510100.htm からのダウンロードリンク
#
# ファイル命名規則: csv/ih51{hokenjo_code}{0|1}.csv
#   {hokenjo_code}: 00=全県, 10=福井市, 20=福井, 30=坂井, 60=奥越, 70=丹南, 40=二州, 50=若狭
#   末尾 0=定点当たり報告数（rate）, 1=報告実数（count）
# いずれも「週,期間,疾患1,疾患2,...」の列構成で、疾患数は約20種類、当年分。

.fukui_hokenjo_codes <- c(
  "福井市" = "1", "福井" = "2", "坂井" = "3",
  "奥越" = "6", "丹南" = "7", "二州" = "4", "若狭" = "5"
)

.fukui_read_csv <- function(url) {
  tmp <- tempfile(fileext = ".csv")
  download.file(url, tmp, mode = "wb", quiet = TRUE)
  raw <- readLines(tmp, warn = FALSE)
  raw <- iconv(raw, from = "shift-jis", to = "UTF-8", sub = "")
  # 各行はダブルクオートで囲まれたCSV。read.csvでまとめて読む
  con <- textConnection(raw)
  d <- tryCatch(read.csv(con, header = FALSE, stringsAsFactors = FALSE, quote = "\""),
                error = function(e) NULL)
  close(con)
  d
}

.fukui_one_hokenjo <- function(hokenjo, code) {
  base <- "https://kansensyou-joho.pref.fukui.lg.jp/csv/ih5100"
  d_rate  <- .fukui_read_csv(paste0(base, code, "0.csv"))
  d_count <- .fukui_read_csv(paste0(base, code, "1.csv"))
  if (is.null(d_rate) || is.null(d_count)) return(NULL)

  # 疾患名ヘッダーは3行目（週,期間,疾患1,疾患2,...）
  header_row <- which(apply(d_rate, 1, function(r) any(r == "週")))[1]
  if (is.na(header_row)) return(NULL)
  disease_cols <- 4:ncol(d_rate)
  diseases_raw <- as.character(d_rate[header_row, disease_cols])
  keep <- !is.na(diseases_raw) & nchar(trimws(diseases_raw)) > 0 & diseases_raw != "NA"
  disease_cols <- disease_cols[keep]
  diseases <- trimws(diseases_raw[keep])

  # 最新週＝最終データ行（週番号が"第N週"形式の行のうち、いずれかの疾患に値がある最後の行）
  data_rows <- (header_row + 1):nrow(d_rate)
  has_val <- sapply(data_rows, function(i) any(!is.na(d_rate[i, disease_cols]) &
                                                  nchar(trimws(as.character(d_rate[i, disease_cols]))) > 0))
  if (!any(has_val)) return(NULL)
  last_row <- data_rows[max(which(has_val))]
  week_txt <- trimws(as.character(d_rate[last_row, 1]))
  if (is.na(week_txt) || nchar(week_txt) == 0) week_txt <- trimws(as.character(d_rate[last_row, 2]))
  week_label <- paste0(week_txt, " (", trimws(d_rate[last_row, 3]), ")")

  out <- list()
  for (k in seq_along(diseases)) {
    col <- disease_cols[k]
    out[[k]] <- data.frame(
      pref = "福井県", week_label = week_label, hokenjo = hokenjo,
      disease = diseases[k],
      count = parse_hokenjo_number(d_count[last_row, col]),
      rate  = parse_hokenjo_number(d_rate[last_row, col]),
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, out)
}

fetch_fukui <- function() {
  out <- lapply(names(.fukui_hokenjo_codes), function(h) {
    .fukui_one_hokenjo(h, .fukui_hokenjo_codes[[h]])
  })
  do.call(rbind, out[!sapply(out, is.null)])
}
