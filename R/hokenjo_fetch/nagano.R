# 長野県「定点把握対象疾患報告数（保健所別）」PDF（週報・月報合併号）
# https://www.pref.nagano.lg.jp/shippei-kansen/kenko/kenko/kansensho/joho/documents/{YEAR}-{WEEK}w_data_07m.pdf 型
#
# レイアウト: 該当ページ（"保健所別" というタイトルを含むページ、通常p.7）に
# 「報告数(count)行 → 疾患名（1〜数行）→ 定点当たり報告数(rate)行」が
# 疾患ごとに繰り返される表があり、列は佐久/上田/諏訪/伊那/飯田/木曽/松本/
# 大町/長野/北信/長野市/松本市（12保健所）+ 合計。眼科・基幹系疾患では
# 定点の無い保健所の列がまるごと欠落する（"-"すら出力されない）ため、
# pdf_text() の空白区切りでは列がズレる。pdf_data() の座標(x)を使い、
# 各保健所の列範囲に含まれる値だけを拾う。

fetch_nagano <- function(pdf_url = "https://www.pref.nagano.lg.jp/shippei-kansen/kenko/kenko/kansensho/joho/documents/2026-32w_data_07m.pdf") {
  if (!exists("pdf_words")) stop("pdf_table_utils.R を先に source してください")

  tmp <- tempfile(fileext = ".pdf")
  download.file(pdf_url, tmp, mode = "wb", quiet = TRUE)
  pages_txt <- pdftools::pdf_text(tmp)
  target_page <- which(grepl("報告数\\s*（保健所別）\\s*20[0-9]{2}年第[0-9]+週", pages_txt))
  target_page <- target_page[1]
  if (is.na(target_page)) stop("保健所別ページが見つかりません")

  hokenjo_order <- c("佐久", "上田", "諏訪", "伊那", "飯田", "木曽", "松本", "大町", "長野", "北信", "長野市", "松本市")

  wm <- regmatches(pages_txt[target_page], regexec("(20[0-9]{2})年第([0-9]+)週", pages_txt[target_page]))[[1]]
  week_label <- if (length(wm) == 3) sprintf("%s年第%s週", wm[2], wm[3]) else NA_character_

  words <- pdf_words(tmp, page = target_page)

  header_hits <- words[words$text %in% c(hokenjo_order, "合計"), ]
  header_hits <- header_hits[!duplicated(header_hits$text), ]
  header_hits <- header_hits[match(c(hokenjo_order, "合計"), header_hits$text), ]
  xs <- header_hits$x
  header_y <- max(header_hits$y)
  bounds <- c(xs[1] - (xs[2] - xs[1]) / 2, (xs[1:12] + xs[2:13]) / 2)  # 13境界(12列分の左右端)

  body <- words[words$y > header_y + 5, ]
  # 【保健所別定点数】の付随表（フッター）以降は除外
  footer_hits <- body[body$text == "【保健所別定点数】", ]
  if (nrow(footer_hits) > 0) body <- body[body$y < min(footer_hits$y) - 3, ]

  data_col <- body[body$x >= bounds[1] & body$x < bounds[13], ]
  data_rows <- group_words_into_rows(data_col, y_tol = 3)
  data_row_y <- vapply(data_rows, function(r) mean(r$y), numeric(1))

  name_col <- body[body$x >= 65 & body$x < bounds[1] - 5, ]
  name_lines <- group_words_into_rows(name_col, y_tol = 3)
  name_line_y <- vapply(name_lines, function(r) mean(r$y), numeric(1))

  ord_data <- order(data_row_y)
  ord_name <- order(name_line_y)
  events <- rbind(
    data.frame(y = data_row_y[ord_data], type = "data", idx = ord_data, stringsAsFactors = FALSE),
    data.frame(y = name_line_y[ord_name], type = "name", idx = ord_name, stringsAsFactors = FALSE)
  )
  events <- events[order(events$y), ]

  out <- list()
  state <- "expect_count"
  count_row <- NULL
  name_parts <- character(0)

  for (i in seq_len(nrow(events))) {
    ev <- events[i, ]
    if (ev$type == "data") {
      if (state == "expect_count") {
        count_row <- data_rows[[ev$idx]]
        state <- "expect_name"
      } else {
        # rate行が来た。疾患名とcount行を確定し出力する
        rate_row <- data_rows[[ev$idx]]
        disease <- paste(name_parts, collapse = "")
        if (nzchar(disease) && !is.null(count_row)) {
          for (k in seq_along(hokenjo_order)) {
            lo <- bounds[k]; hi <- bounds[k + 1]
            cnt_tok <- count_row$text[count_row$x >= lo & count_row$x < hi]
            rate_tok <- rate_row$text[rate_row$x >= lo & rate_row$x < hi]
            cnt_val <- if (length(cnt_tok) > 0) parse_hokenjo_number(cnt_tok[1]) else NA_real_
            rate_val <- if (length(rate_tok) > 0) parse_hokenjo_number(rate_tok[1]) else NA_real_
            out[[length(out) + 1]] <- data.frame(
              pref = "長野県", week_label = week_label, hokenjo = hokenjo_order[k],
              disease = disease, count = cnt_val, rate = rate_val,
              stringsAsFactors = FALSE
            )
          }
        }
        name_parts <- character(0)
        count_row <- NULL
        state <- "expect_count"
      }
    } else {
      name_parts <- c(name_parts, paste(events_text <- name_lines[[ev$idx]]$text[order(name_lines[[ev$idx]]$x)], collapse = ""))
    }
  }
  do.call(rbind, out)
}
