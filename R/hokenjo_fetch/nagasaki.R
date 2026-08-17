# 長崎県「疾病別・保健所管内別発生状況」PDF
# https://www.pref.nagasaki.jp/fs/3/3/4/7/0/_/{タイトル}.pdf 型
# （ファイル名が週ごとに変わるため、呼び出し側で最新PDFのURLを解決すること。
#  長崎県感染症情報センターのトップページから最新週報リンクを辿る）
#
# レイアウト: p.4に(1)週別推移表と(2)「疾病別・保健所管内別発生状況」表が
# ある。(2)は定点当たり患者数のみ（報告数の実数は記載なし）。10保健所
# （佐世保市/長崎市/壱岐/西彼/県央/県南/県北/五島/上五島/対馬）分の列が
# 存在するが、0件セルは空欄でトークンが無いため、x座標ベースで抽出する。
# ヘッダーの並び順は「県 佐世保市 長崎市 壱岐 西彼 県央 県南 県北 五島
# 上五島 対馬」（佐世保市が2番目、県全体列の直後）。

.NAGASAKI_HOKENJO <- c("佐世保市", "長崎市", "壱岐", "西彼", "県央", "県南", "県北", "五島", "上五島", "対馬")
.NAGASAKI_X_BINS <- c(209, 237, 271.5, 305, 337, 369, 401, 432.5, 462, 494, 542)

fetch_nagasaki <- function(pdf_url) {
  if (!exists("pdf_words")) stop("pdf_table_utils.R を先に source してください")

  local_path <- pdf_url
  if (grepl("^https?://", pdf_url)) {
    local_path <- tempfile(fileext = ".pdf")
    download.file(pdf_url, local_path, mode = "wb", quiet = TRUE)
  }

  words <- pdf_words(local_path, page = 4)
  rows <- group_words_into_rows(words, y_tol = 3)

  week_line <- Filter(function(r) grepl("第[0-9]+週", row_text(r)) && grepl("疾病別・保健所管内別", row_text(r)), rows)
  week_num_txt <- if (length(week_line) > 0) {
    regmatches(row_text(week_line[[1]]), regexpr("第[0-9]+週", row_text(week_line[[1]])))
  } else NA_character_

  week_label <- NA_character_
  if (!is.na(week_num_txt) && nzchar(week_num_txt)) {
    line_text <- row_text(week_line[[1]])
    m <- regmatches(line_text, regexpr("20[0-9]{2}年第[0-9]+週", line_text))
    if (length(m) > 0 && nchar(m) > 0) {
      week_label <- m
    } else {
      # 年がp.4に無い場合はp.4全体、それでも無ければ表紙(p.1)を探す
      # （表紙には「2026年第31週」のように年が明記されているが、
      #   (2)表の見出し行には週番号のみで年が書かれていないことがある）
      page_text <- paste(sapply(rows, row_text), collapse = " ")
      yr_m <- regmatches(page_text, regexpr("20[0-9]{2}年(?=.*第[0-9]+週)", page_text, perl = TRUE))
      if (length(yr_m) == 0 || !nzchar(yr_m)) {
        p1_words <- tryCatch(pdf_words(local_path, page = 1), error = function(e) NULL)
        if (!is.null(p1_words)) {
          p1_rows <- group_words_into_rows(p1_words, y_tol = 3)
          p1_text <- paste(sapply(p1_rows, row_text), collapse = " ")
          yr_m <- regmatches(p1_text, regexpr("20[0-9]{2}年", p1_text))
        }
      }
      if (length(yr_m) > 0 && nzchar(yr_m)) {
        week_label <- paste0(yr_m, week_num_txt)
      }
    }
  }

  hdr_idx <- which(sapply(rows, function(r) any(r$text == "県") && any(r$text == "対馬")))
  if (length(hdr_idx) == 0) stop("nagasaki: ヘッダー行が見つかりません")
  start <- hdr_idx[1] + 1

  end_idx <- which(sapply(rows[start:length(rows)], function(r) grepl("急性呼吸器感染症", row_text(r))))
  end <- if (length(end_idx) > 0) start + end_idx[1] - 1 else start + 19

  bins <- .NAGASAKI_X_BINS
  hokenjo <- .NAGASAKI_HOKENJO
  is_num <- function(s) grepl("^[0-9]+\\.[0-9]+$", s)

  out <- list()
  for (i in start:end) {
    r <- rows[[i]]
    name_tok <- r$text[r$x < 190]
    dname <- paste(name_tok, collapse = "")
    dname <- sub("[0-9]+\\.[0-9]+$", "", dname)  # 県計列の数値が同一トークンに連結される場合があるため除去
    if (dname == "") next
    for (k in seq_along(hokenjo)) {
      cell <- r[r$x >= bins[k] & r$x < bins[k + 1], ]
      tok <- if (nrow(cell) > 0) cell$text[vapply(cell$text, is_num, logical(1))] else character(0)
      val <- if (length(tok) >= 1) parse_hokenjo_number(tok[1]) else NA_real_
      out[[length(out) + 1]] <- data.frame(
        pref = "長崎県", week_label = week_label, hokenjo = hokenjo[k],
        disease = dname, count = NA_real_, rate = val, stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, out)
}
