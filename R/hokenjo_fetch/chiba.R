# 千葉県「第◯週 保健所別、年齢群別報告数（男女合計）」PDF（週報後半、
# 疾患別・保健所別・年齢階級別集計表のページ群）
# https://www.pref.chiba.lg.jp/eiken/c-idsc/documents/wr{YEAR}{WEEK}.pdf
#
# 各ページに複数疾患が縦に並び、疾患ごとに年齢階級別の内訳＋「合計」行
# （保健所別合計数）が続く。疾患名は表左端(x~60-85)に1文字ずつ縦書きで
# 配置される。定点当たり報告数の列は無いため、区分（小児科定点数/ARI
# 定点数/眼科定点数/基幹定点数）行の定点数で count を割って rate を算出する。

.chiba_hokenjo_order <- c("野田", "柏市", "松戸", "市川", "船橋市", "習志野",
                           "千葉市", "印旛", "香取", "海匝", "山武", "長生",
                           "夷隅", "安房", "君津", "市原", "合計")

fetch_chiba <- function(pdf_url = NULL) {
  if (!exists("pdf_words")) stop("pdf_table_utils.R を先に source してください")
  if (is.null(pdf_url)) {
    pdf_url <- "https://www.pref.chiba.lg.jp/eiken/c-idsc/documents/wr2631.pdf"
  }

  path <- tempfile(fileext = ".pdf")
  download.file(pdf_url, path, mode = "wb", quiet = TRUE)
  n_pages <- length(pdftools::pdf_data(path))

  target_pages <- integer(0)
  for (p in seq_len(n_pages)) {
    w <- pdftools::pdf_data(path)[[p]]
    hdr_txt <- paste(w$text[w$y < 75], collapse = "")
    if (any(grepl("定点数", w$text)) && any(w$text == "合計") && grepl("年齢群別報告数", hdr_txt)) {
      target_pages <- c(target_pages, p)
    }
  }
  if (length(target_pages) == 0) stop("保健所別年齢群別報告数のページが見つかりません")

  out <- list()
  week_label <- NA_character_

  for (p in target_pages) {
    w <- pdftools::pdf_data(path)[[p]]

    wl <- w$text[grepl("^[0-9]+$", w$text)]
    wk <- w$text[w$text == "週" ]
    hdr_row <- w[w$y >= 60 & w$y <= 75, ]
    m <- regmatches(paste(hdr_row$text, collapse = ""), regexpr("第[0-9]+週", paste(hdr_row$text, collapse = "")))
    if (length(m) > 0 && nchar(m) > 0) week_label <- paste0("2026年", m)

    # 列順（x座標）を検出
    hdr <- w[w$y >= 70 & w$y <= 84 & nchar(w$text) == 1, ]
    hdr <- hdr[order(hdr$x), ]
    col_x <- unique(hdr$x)
    col_x <- sort(col_x)
    if (length(col_x) < 16) next  # ヘッダーが無い（表の続きページ）→列位置は前ページのものを流用
    x_centers <- col_x

    rows <- group_words_into_rows(w, y_tol = 2)

    teiten <- NULL
    name_buf <- character(0)
    pending_teiten <- FALSE

    match_col <- function(x) which.min(abs(x_centers - x))

    extract_num_vec <- function(rr, exclude_idx) {
      num_rows <- rr[!exclude_idx, ]
      num_rows <- num_rows[grepl("^[0-9,.]+$", num_rows$text) & num_rows$x >= min(x_centers) - 15, ]
      vec <- rep(NA_real_, length(x_centers))
      for (k in seq_len(nrow(num_rows))) {
        ci <- match_col(num_rows$x[k])
        vec[ci] <- parse_hokenjo_number(num_rows$text[k])
      }
      vec
    }

    for (i in seq_along(rows)) {
      rr <- rows[[i]]
      rr <- rr[order(rr$x), ]
      txt <- rr$text

      is_teiten_label <- any(grepl("定点数", txt))
      n_numeric_here <- sum(grepl("^[0-9,.]+$", txt) & rr$x >= min(x_centers) - 15)
      is_total_row <- any(txt == "合計") && any(rr$x[txt == "合計"] < 100)

      if (is_teiten_label) {
        lbl_idx <- grepl("定点数", txt) | !grepl("^[0-9,.]+$", txt)
        if (n_numeric_here >= 8) {
          teiten <- extract_num_vec(rr, lbl_idx)
          pending_teiten <- FALSE
        } else {
          pending_teiten <- TRUE
        }
        name_buf <- character(0)
        next
      }
      if (pending_teiten && n_numeric_here >= 8) {
        teiten <- extract_num_vec(rr, !grepl("^[0-9,.]+$", txt))
        pending_teiten <- FALSE
        next
      }
      if (pending_teiten) {
        # ラベルの続き行（数値なし）。読み飛ばす
        next
      }

      if (is_total_row) {
        num_rows <- rr[rr$text != "合計", ]
        vec <- rep(NA_real_, length(x_centers))
        for (k in seq_len(nrow(num_rows))) {
          ci <- match_col(num_rows$x[k])
          vec[ci] <- parse_hokenjo_number(num_rows$text[k])
        }
        disease <- trimws(paste(name_buf, collapse = ""))
        if (disease != "") {
          for (ci in seq_along(.chiba_hokenjo_order)) {
            if (.chiba_hokenjo_order[ci] == "合計") next
            cnt <- vec[ci]
            rt <- if (!is.null(teiten) && !is.na(teiten[ci]) && teiten[ci] > 0 && !is.na(cnt)) {
              round(cnt / teiten[ci], 2)
            } else NA_real_
            out[[length(out) + 1]] <- data.frame(
              pref = "千葉県", week_label = week_label,
              hokenjo = .chiba_hokenjo_order[ci],
              disease = disease,
              count = ifelse(is.na(cnt), NA_real_, cnt),
              rate = rt,
              stringsAsFactors = FALSE
            )
          }
        }
        name_buf <- character(0)
        next
      }

      # 疾患名候補文字（左端の縦書き列）。1文字トークンのみ採用
      # （定点数ラベルの折返し残骸などの複数文字トークンは除外）
      name_chars <- rr$text[rr$x >= 60 & rr$x <= 85 & nchar(rr$text) == 1]
      if (length(name_chars) > 0) name_buf <- c(name_buf, name_chars)
    }
  }

  df <- do.call(rbind, out)
  rownames(df) <- NULL
  df
}
