# 茨城県「保健所別 定点当たり報告数及び報告数」PDF
# https://www.pref.ibaraki.jp/hokenfukushi/eiken/idwr/weekly/documents/{YEAR}idwr{WEEK}.pdf
#
# レイアウト: 該当ページ内、疾病ごとに「定点当」行（定点当たり報告数、
# 数値のみ）→ 疾患名（1〜2行に折り返すことがある）→「報告数」行（実数）
# の3行1組で並ぶ。保健所名は表ヘッダーのx座標から列順を検出する。
# 「ひたちなか」は境界データ（hokenjo_boundaries/ibaraki.geojson）に
# 存在しないため、実患者数は「日立」に合算する（率は簡易合算・近似値）。

fetch_ibaraki <- function(pdf_url = NULL) {
  if (!exists("pdf_words")) stop("pdf_table_utils.R を先に source してください")
  if (is.null(pdf_url)) {
    pdf_url <- "https://www.pref.ibaraki.jp/hokenfukushi/eiken/idwr/weekly/documents/2026idwr31.pdf"
  }

  path <- tempfile(fileext = ".pdf")
  download.file(pdf_url, path, mode = "wb", quiet = TRUE)
  n_pages <- length(pdftools::pdf_data(path))

  # ヘッダーの表記は「疾患名/保健所」「疾 病/保健所」など年によって
  # 微妙に異なるため、より安定した「定点当たり報告数・報告件数（保健所別）」
  # のタイトル文言で判定する
  target_page <- NA_integer_
  for (p in seq_len(n_pages)) {
    w <- pdftools::pdf_data(path)[[p]]
    if (any(grepl("定点当たり報告数", w$text)) && any(grepl("保健所別", w$text)) && any(w$text == "定点当")) {
      target_page <- p
      break
    }
  }
  if (is.na(target_page)) stop("保健所別定点データのページが見つかりません")

  w <- pdftools::pdf_data(path)[[target_page]]

  week_line <- w$text[grepl("20[0-9]{2}年第[0-9]+週", w$text)]
  week_label <- if (length(week_line) > 0) {
    regmatches(week_line[1], regexpr("20[0-9]{2}年第[0-9]+週", week_line[1]))
  } else NA_character_

  # --- 列順（保健所名）をヘッダー部のx座標から検出 ---
  known_names <- c("中央", "日立", "潮来", "土浦", "つくば", "筑西", "古河",
                    "水戸市", "竜ケ崎", "ひたち", "なか", "計")
  hdr <- w[w$y >= 75 & w$y <= 130 & w$text %in% known_names, ]
  hdr <- hdr[order(hdr$x), ]
  # 「ひたち」+「なか」を1列に統合
  hn <- hdr[hdr$text %in% c("ひたち", "なか"), ]
  cols <- hdr[!(hdr$text %in% c("ひたち", "なか")), c("x", "text")]
  if (nrow(hn) > 0) {
    cols <- rbind(cols, data.frame(x = mean(hn$x), text = "ひたちなか"))
  }
  cols <- cols[order(cols$x), ]
  col_names <- cols$text  # x昇順の列名リスト（10保健所+計 or 9+ひたちなか+計）

  # --- データブロックを抽出 ---
  # 「定点当」ラベル(x付近177)を含む行、「報告数」ラベルを含む行を検出
  rows <- group_words_into_rows(w, y_tol = 2)

  out <- list()
  i <- 1
  n <- length(rows)
  pending_name_left <- character(0)
  pending_name_right <- character(0)

  flush_block <- function(rate_vals, count_vals, name_right, name_left) {
    disease <- if (length(name_right) > 0) paste(name_right, collapse = "") else paste(name_left, collapse = "")
    disease <- trimws(disease)
    if (disease == "" || length(rate_vals) == 0) return(NULL)
    m <- min(length(col_names), length(rate_vals), length(count_vals))
    data.frame(
      pref = "茨城県", week_label = week_label,
      hokenjo = col_names[seq_len(m)],
      disease = disease,
      count = parse_hokenjo_number(count_vals[seq_len(m)]),
      rate = parse_hokenjo_number(rate_vals[seq_len(m)]),
      stringsAsFactors = FALSE
    )
  }

  cur_rate <- NULL
  cur_name_left <- character(0)
  cur_name_right <- character(0)

  for (i in seq_len(n)) {
    rr <- rows[[i]]
    rr <- rr[order(rr$x), ]
    txt <- rr$text
    has_teitentou <- any(txt == "定点当")
    has_houkoku <- any(txt == "報告数")

    if (has_teitentou && sum(grepl("^[0-9.]+$|^-$", txt[rr$x > 190])) >= 3) {
      # 新しい定点当（率）行 → 前のブロックはまだflushしない（disease未確定の可能性）
      if (!is.null(cur_rate)) {
        # 前のブロックの報告数行がまだ来ていない異常系は無視
      }
      cur_rate <- rr$text[rr$x > 190]
      cur_name_left <- character(0)
      cur_name_right <- character(0)
      next
    }

    if (has_houkoku && sum(grepl("^[0-9,]+$|^-$", txt[rr$x > 190])) >= 3) {
      cur_count <- rr$text[rr$x > 190]
      left_extra <- rr$text[rr$x < 106 & !(rr$text %in% c("報告数"))]
      right_extra <- rr$text[rr$x >= 106 & rr$x < 200 & !(rr$text %in% c("報告数"))]
      cur_name_left <- c(cur_name_left, left_extra)
      cur_name_right <- c(cur_name_right, right_extra)
      block <- flush_block(cur_rate, cur_count, cur_name_right, cur_name_left)
      if (!is.null(block)) out[[length(out) + 1]] <- block
      cur_rate <- NULL
      cur_name_left <- character(0)
      cur_name_right <- character(0)
      next
    }

    # 中間行（疾患名の折り返し等）
    if (!is.null(cur_rate)) {
      left_extra <- rr$text[rr$x < 106]
      right_extra <- rr$text[rr$x >= 106 & rr$x < 200]
      cur_name_left <- c(cur_name_left, left_extra)
      cur_name_right <- c(cur_name_right, right_extra)
    }
  }

  df <- do.call(rbind, out)

  # 「ひたちなか」を「日立」へ合算（境界データに存在しないため）
  if (!is.null(df) && nrow(df) > 0 && "ひたちなか" %in% df$hokenjo && "日立" %in% df$hokenjo) {
    hn_rows <- df[df$hokenjo == "ひたちなか", ]
    for (k in seq_len(nrow(hn_rows))) {
      idx <- which(df$hokenjo == "日立" & df$disease == hn_rows$disease[k])
      if (length(idx) == 1) {
        df$count[idx] <- sum(df$count[idx], hn_rows$count[k], na.rm = TRUE)
        df$rate[idx] <- sum(df$rate[idx], hn_rows$rate[k], na.rm = TRUE)
      }
    }
    df <- df[df$hokenjo != "ひたちなか", ]
  }

  df <- df[df$hokenjo != "計", ]
  rownames(df) <- NULL
  df
}
