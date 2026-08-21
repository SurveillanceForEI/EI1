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
          # 数値セル自体が省略されている（トークンが無い）保健所は
          # 「報告なし=0件」として扱う（ユーザー指示）
          cnt_v <- if (is.na(cur_count[ci])) 0 else cur_count[ci]
          rte_v <- if (is.na(vec[ci])) 0 else vec[ci]
          out[[length(out) + 1]] <- data.frame(
            pref = "新潟県", week_label = week_label,
            hokenjo = col_names[ci], disease = disease,
            count = cnt_v, rate = rte_v,
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

# ------------------------------------------------------------
# 新潟県は2026年第32週前後からPDF版に加えてExcel版
# 「5類感染症定点把握対象疾患報告数」も公開するようになった
# （https://www.pref.niigata.lg.jp/sec/kanyaku/shuho{RR}{WW}.html
#  の中の .xlsx リンク）。Excel版はセルが構造化されており、PDFの
# 座標合わせ（x座標での列マッチング）が不要で確実なため、xlsx版が
# 手に入る週はこちらを優先して使う。
#
# シート構成:「実数」「定点当」の2行1組×疾患。値が0または非報告の
# 保健所はセル自体が空欄（NA）になっており、ユーザー指示により
# 「非公表」ではなく「報告0件」として扱う（0埋め）。
# 1シートに「地域振興局等管内別報告数」表の下へ「最近６週間の推移」
# 「入院サーベイランス」の別表が続くため、疾患名が21種類出た時点
# （＝地域振興局別表の末尾）で読み取りを打ち切る
# ------------------------------------------------------------

.niigata_xlsx_hokenjo_order <- c("県計", "新潟市", "新発田", "新津", "三条", "長岡", "魚沼",
                                  "南魚沼", "十日町", "柏崎", "糸魚川", "村上", "佐渡", "上越")

# 令和年+週番号から週報速報版のランディングページURLを組み立て、
# 「5類感染症定点把握対象疾患報告数」のExcelリンクを取得する
resolve_niigata_xlsx_url <- function(reiwa_year, week) {
  if (!requireNamespace("rvest", quietly = TRUE)) stop("rvest パッケージが必要です")
  landing <- sprintf("https://www.pref.niigata.lg.jp/sec/kanyaku/shuho%02d%02d.html", reiwa_year, week)
  doc <- rvest::read_html(landing, encoding = "UTF-8")
  links <- rvest::html_elements(doc, "a")
  hrefs <- rvest::html_attr(links, "href")
  texts <- rvest::html_text(links)
  idx <- which(grepl("\\.xlsx$", hrefs, ignore.case = TRUE) & grepl("5類感染症定点把握対象疾患報告数", texts))
  if (length(idx) == 0) stop(sprintf("新潟県: %s にExcelリンクが見つかりません", landing))
  xml2::url_absolute(hrefs[idx[1]], landing)
}

fetch_niigata_xlsx <- function(xlsx_url) {
  if (!requireNamespace("readxl", quietly = TRUE)) stop("readxl パッケージが必要です")
  tmp <- tempfile(fileext = ".xlsx")
  download.file(xlsx_url, tmp, mode = "wb", quiet = TRUE)
  d <- as.data.frame(readxl::read_excel(tmp, sheet = 1, col_names = FALSE))

  # 週によっては先頭に「入力チェック」行が挿入されており、タイトル行/
  # ヘッダー行の位置が1行ずれることがあるため、固定インデックスではなく
  # 「地域振興局等管内別報告数」を含む行を検索してタイトル行を特定する
  title_row <- which(apply(d, 1, function(r) any(grepl("地域振興局等管内別報告数", r), na.rm = TRUE)))
  if (length(title_row) == 0) stop("新潟県(xlsx): タイトル行が見つかりません")
  title_row <- title_row[1]
  header_row <- title_row + 1

  wl_cell <- d[title_row, grepl("^令和[0-9]+年第[0-9]+週", as.character(d[title_row, ]))]
  wl_raw <- if (length(wl_cell) > 0 && !is.na(wl_cell[[1]])) as.character(wl_cell[[1]]) else NA_character_
  week_label <- if (!is.na(wl_raw)) {
    m <- regmatches(wl_raw, regexec("令和([0-9]+)年第([0-9]+)週", wl_raw))[[1]]
    if (length(m) == 3) sprintf("%d年第%s週", as.integer(m[2]) + 2018L, m[3]) else NA_character_
  } else NA_character_

  hdr <- sub("※$", "", as.character(d[header_row, ]))
  col_idx <- match(.niigata_xlsx_hokenjo_order, hdr)
  if (any(is.na(col_idx))) stop("新潟県(xlsx): 保健所名の列が見つかりません")

  out <- list()
  n_diseases <- 0
  i <- header_row + 1
  while (i < nrow(d) && n_diseases < 20) {
    disease <- if (!is.na(d[i, 1])) trimws(as.character(d[i, 1])) else NA_character_
    if (is.na(disease) || !identical(as.character(d[i, 2]), "実数")) { i <- i + 1; next }
    if (i + 1 > nrow(d) || !identical(as.character(d[i + 1, 2]), "定点当")) { i <- i + 1; next }
    cnt_row <- d[i, col_idx]
    rte_row <- d[i + 1, col_idx]
    for (k in seq_along(.niigata_xlsx_hokenjo_order)) {
      if (.niigata_xlsx_hokenjo_order[k] == "県計") next
      cnt_v <- suppressWarnings(as.numeric(cnt_row[[k]]))
      rte_v <- suppressWarnings(as.numeric(rte_row[[k]]))
      # 空欄セルは「非公表」ではなく「報告0件」として扱う（ユーザー指示）
      if (is.na(cnt_v)) cnt_v <- 0
      if (is.na(rte_v)) rte_v <- 0
      out[[length(out) + 1]] <- data.frame(
        pref = "新潟県", week_label = week_label,
        hokenjo = .niigata_xlsx_hokenjo_order[k], disease = disease,
        count = cnt_v, rate = rte_v,
        stringsAsFactors = FALSE
      )
    }
    n_diseases <- n_diseases + 1
    i <- i + 2
  }

  df <- do.call(rbind, out)
  rownames(df) <- NULL
  df
}
