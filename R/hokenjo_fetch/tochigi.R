# 栃木県「保健所管内別疾病別報告数・定点当り報告数」PDF（2ページ目）
# https://www.pref.tochigi.lg.jp/e60/tidc/documents/intidwr{YEAR}{WEEK}.pdf
#
# 列: 宇都宮市, 県西, 県東, 県南, 県北, 安足, 計。疾病ごとに「報告数」行→
# 疾病名（1〜2行折返し）→「定点当り」行の3行1組で並ぶ。定点当り数値には
# 警報/注意報基準超過を示す「●」「▲」が数値の前に付くことがある。

fetch_tochigi <- function(pdf_url = NULL) {
  if (!exists("pdf_words")) stop("pdf_table_utils.R を先に source してください")
  if (is.null(pdf_url)) {
    pdf_url <- "https://www.pref.tochigi.lg.jp/e60/tidc/documents/intidwr202631.pdf"
  }

  path <- tempfile(fileext = ".pdf")
  download.file(pdf_url, path, mode = "wb", quiet = TRUE)
  n_pages <- length(pdftools::pdf_data(path))

  target_page <- NA_integer_
  for (p in seq_len(n_pages)) {
    w <- pdftools::pdf_data(path)[[p]]
    if (any(grepl("保健所管内別疾病別報告数", w$text))) { target_page <- p; break }
  }
  if (is.na(target_page)) stop("保健所別データのページが見つかりません")
  w <- pdftools::pdf_data(path)[[target_page]]

  wl2 <- w$text[grepl("令和[0-9０-９]+年$", w$text)]
  wl3 <- w$text[grepl("第[0-9０-９]+週", w$text)]
  week_label <- if (length(wl2) > 0 && length(wl3) > 0) {
    y <- regmatches(wl2[1], regexpr("令和[0-9０-９]+年", wl2[1]))
    wk <- regmatches(wl3[1], regexpr("第[0-9０-９]+週", wl3[1]))
    paste0(y, wk)
  } else NA_character_

  hokenjo_names <- c("宇都宮市", "県西", "県東", "県南", "県北", "安足", "計")
  hdr <- w[w$y >= 100 & w$y <= 112 & w$text %in% hokenjo_names, ]
  hdr <- hdr[order(hdr$x), ]
  col_names <- hdr$text
  x_min <- min(hdr$x) - 20
  x_max <- max(hdr$x) + 30  # 「計」列の右端まで（基準値列は除外）

  rows <- group_words_into_rows(w, y_tol = 2)

  strip_mark <- function(s) gsub("^[●▲]", "", s)

  out <- list()
  cur_count <- NULL
  cur_name <- character(0)

  for (i in seq_along(rows)) {
    rr <- rows[[i]]
    rr <- rr[order(rr$x), ]
    txt <- rr$text
    in_range <- rr$x >= x_min & rr$x <= x_max
    numeric_like <- grepl("^[0-9,]+$", txt) | txt == "-"

    is_count_row <- any(txt == "報告数") && sum(in_range & (numeric_like | grepl("^[0-9,]+$", strip_mark(txt)))) >= 5
    is_rate_row <- any(txt == "定点当り")

    if (is_count_row) {
      cur_count <- rr$text[in_range & rr$text != "報告数"]
      cur_name <- character(0)
      next
    }

    if (is_rate_row) {
      cur_rate <- rr$text[in_range & rr$text != "定点当り"]
      cur_rate <- sapply(cur_rate, strip_mark, USE.NAMES = FALSE)
      name_extra <- rr$text[rr$x < x_min & rr$x >= 90 & !(rr$text %in% c("定点当り", "報告数"))]
      cur_name <- c(cur_name, name_extra)
      disease <- trimws(paste(cur_name, collapse = ""))
      if (disease != "" && !is.null(cur_count) && length(cur_rate) >= 5) {
        m <- min(length(col_names), length(cur_count), length(cur_rate))
        out[[length(out) + 1]] <- data.frame(
          pref = "栃木県", week_label = week_label,
          hokenjo = col_names[seq_len(m)],
          disease = disease,
          count = parse_hokenjo_number(cur_count[seq_len(m)]),
          rate = parse_hokenjo_number(cur_rate[seq_len(m)]),
          stringsAsFactors = FALSE
        )
      }
      cur_count <- NULL
      cur_name <- character(0)
      next
    }

    if (!is.null(cur_count)) {
      name_extra <- rr$text[rr$x < x_min & rr$x >= 90]
      cur_name <- c(cur_name, name_extra)
    }
  }

  df <- do.call(rbind, out)
  df <- df[df$hokenjo != "計", ]
  rownames(df) <- NULL
  df
}
