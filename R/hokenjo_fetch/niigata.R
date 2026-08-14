# 新潟県「5類感染症定点把握対象疾患（週報届出分）地域振興局等管内別報告数」PDF
# （新潟県感染症情報週報速報版の別紙、通常2ページ目）
# https://www.pref.niigata.lg.jp/uploaded/attachment/{ID}.pdf （IDは週ごとに変わる）
#
# 疾患ごとに「実数」行→「定点当」行の2行1組。値が0または非公表の
# 保健所（地域振興局）は数値セル自体が省略される（"-"すら無い）ため、
# x座標で列に割り当てる必要がある。先頭列「県計」は合計のため除外。
# 疾患名は各行左端(x<145)に1文字ずつ配置される。

.niigata_hokenjo_order <- c("県計", "新潟市", "新発田", "新津", "三条", "長岡", "魚沼",
                             "南魚沼", "十日町", "柏崎", "糸魚川", "村上", "佐渡", "上越")

fetch_niigata <- function(pdf_url = NULL) {
  if (!exists("pdf_words")) stop("pdf_table_utils.R を先に source してください")
  if (is.null(pdf_url)) {
    pdf_url <- "https://www.pref.niigata.lg.jp/uploaded/attachment/506688.pdf"
  }

  path <- tempfile(fileext = ".pdf")
  download.file(pdf_url, path, mode = "wb", quiet = TRUE,
                headers = c(`User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
  n_pages <- length(pdftools::pdf_data(path))

  target_page <- NA_integer_
  for (p in seq_len(n_pages)) {
    w <- pdftools::pdf_data(path)[[p]]
    if (any(grepl("地域振興局等管内別報告数", w$text))) { target_page <- p; break }
  }
  if (is.na(target_page)) stop("地域振興局等管内別報告数のページが見つかりません")

  w <- pdftools::pdf_data(path)[[target_page]]

  wl <- w$text[grepl("^令和[0-9]+年第[0-9]+週", w$text)]
  week_label <- if (length(wl) > 0) regmatches(wl[1], regexpr("^令和[0-9]+年第[0-9]+週", wl[1])) else NA_character_

  # 列のx中心をヘッダー行（県計/新潟市/...）から検出（"新津※"のように
  # 記号が付くトークンにも対応するため前方一致で判定する）
  hdr_names_pattern <- paste0("^(", paste(.niigata_hokenjo_order, collapse = "|"), ")")
  hdr <- w[w$y >= 68 & w$y <= 76 & grepl(hdr_names_pattern, w$text), ]
  hdr <- hdr[order(hdr$x), ]
  if (nrow(hdr) < length(.niigata_hokenjo_order)) {
    hdr <- w[grepl(hdr_names_pattern, w$text), ]
    hdr <- hdr[!duplicated(hdr$text), ]
    hdr <- hdr[order(hdr$x), ]
  }
  col_x <- hdr$x
  col_names <- sub("※$", "", hdr$text)
  match_col <- function(x) which.min(abs(col_x - x))

  rows <- group_words_into_rows(w, y_tol = 4)

  out <- list()
  name_buf <- character(0)
  cur_count <- NULL

  for (i in seq_along(rows)) {
    rr <- rows[[i]]
    if (rr$y[1] < 80) next  # タイトル行を除外
    rr <- rr[order(rr$x), ]
    txt <- rr$text

    is_count_row <- any(grepl("実数?$", txt)) || any(txt == "数")
    is_rate_row <- any(txt == "当") && any(txt == "定") && any(txt == "点")

    name_chars <- rr$text[rr$x < 145 & !(rr$text %in% c("実数", "数", "定", "点", "当"))]
    name_chars <- sub("実$", "", name_chars)
    if (length(name_chars) > 0) name_buf <- c(name_buf, name_chars[name_chars != ""])

    if (is_count_row) {
      data_tok <- rr[rr$x >= 145 & rr$x <= max(col_x) + 20, ]
      vec <- rep(NA_real_, length(col_x))
      for (k in seq_len(nrow(data_tok))) vec[match_col(data_tok$x[k])] <- parse_hokenjo_number(data_tok$text[k])
      cur_count <- vec
      next
    }
    if (is_rate_row) {
      data_tok <- rr[rr$x >= 145 & rr$x <= max(col_x) + 20, ]
      vec <- rep(NA_real_, length(col_x))
      for (k in seq_len(nrow(data_tok))) vec[match_col(data_tok$x[k])] <- parse_hokenjo_number(data_tok$text[k])
      disease <- trimws(paste(name_buf, collapse = ""))
      if (disease != "" && !is.null(cur_count)) {
        for (ci in seq_along(col_names)) {
          if (col_names[ci] == "県計") next
          out[[length(out) + 1]] <- data.frame(
            pref = "新潟県", week_label = week_label,
            hokenjo = col_names[ci], disease = disease,
            count = cur_count[ci], rate = vec[ci],
            stringsAsFactors = FALSE
          )
        }
      }
      name_buf <- character(0)
      cur_count <- NULL
      next
    }
  }

  df <- do.call(rbind, out)
  rownames(df) <- NULL
  df
}
