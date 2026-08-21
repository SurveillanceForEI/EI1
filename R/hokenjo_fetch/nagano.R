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

# 長野県の週報PDFファイル名は年によって命名規則が揺れる（月合併号の
# "_data_07m"、"_data-teisei"、"-02m_data"、さらに第1週だけ"_date.pdf"
# という誤字まである）ため、URLパターンをsprintfで組み立てるのではなく、
# 掲載一覧ページを直接スクレイピングして該当週の実際のリンクを取得する。
# 各週は「第N週（xxxKB, 小さい方）」＝概要版と「第N週（xxxKB, 大きい方）」
# ＝保健所別データ付き版の2本が並ぶため、後者（_infoが付かない方）を選ぶ
resolve_nagano_data_url <- function(year, week) {
  if (!requireNamespace("rvest", quietly = TRUE)) stop("rvest パッケージが必要です")
  landing <- "https://www.pref.nagano.lg.jp/shippei-kansen/kenko/kenko/kansensho/joho/index.html"
  doc <- rvest::read_html(landing, encoding = "UTF-8")
  links <- rvest::html_elements(doc, "a")
  hrefs <- rvest::html_attr(links, "href")
  texts <- gsub("[\t\n]", "", rvest::html_text(links))
  idx <- which(grepl("\\.pdf$", hrefs, ignore.case = TRUE) &
                 grepl(sprintf("^第%d週", week), texts) &
                 grepl(as.character(year), hrefs))
  if (length(idx) == 0) stop(sprintf("長野県: %d年第%d週のリンクが見つかりません", year, week))
  cand <- hrefs[idx]
  cand <- cand[!grepl("_info\\.pdf$", cand)]
  if (length(cand) == 0) cand <- hrefs[idx]
  xml2::url_absolute(cand[length(cand)], landing)
}

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
            # セル自体が省略されている（トークンが無い）保健所は
            # 「報告なし=0件」として扱う（ユーザー指示）
            cnt_val <- if (length(cnt_tok) > 0) parse_hokenjo_number(cnt_tok[1]) else 0
            rate_val <- if (length(rate_tok) > 0) parse_hokenjo_number(rate_tok[1]) else 0
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

# 長野県「定点把握感染症（五類（定点））届出状況」PDF（通常の週報、月報合併号ではない）
# https://www.pref.nagano.lg.jp/shippei-kansen/kenko/kenko/kansensho/joho/documents/{YEAR}-{WEEK}w.pdf 型
#
# 保健所別の表には報告数(count)のみが載っており、定点当たり報告数(rate)は
# 表中に無い。代わりに「届出定点数」（保健所ごとの定点数、疾患カテゴリ別に
# ｲﾝﾌﾙ/COVID-19・小児・眼科・基幹の4種）が同じ表内にあるので、
# rate = count / 該当カテゴリの定点数 で算出する。
# 列はx座標で判定（定点の無い保健所は列ごと欠落するため、位置基準の方が
# トークン数基準より安全）。
.nagano_weekly_sentinel_cols <- c("infl_covid" = 70, "ped" = 83, "eye" = 96, "core" = 108)
.nagano_weekly_diseases <- data.frame(
  name = c("インフルエンザ", "COVID-19", "RSウイルス感染症", "咽頭結膜熱",
           "Ａ群溶血性レンサ球菌咽頭炎", "感染性胃腸炎", "水痘", "手足口病",
           "伝染性紅斑", "突発性発しん", "ヘルパンギーナ", "流行性耳下腺炎",
           "急性出血性結膜炎", "流行性角結膜炎", "細菌性髄膜炎", "無菌性髄膜炎",
           "マイコプラズマ肺炎", "クラミジア肺炎(オウム病除く)", "感染性胃腸炎(ロタウイルス)"),
  x = c(130, 158, 186, 211, 235, 256, 284, 308, 333, 357, 382, 406, 431, 452, 480, 504, 529, 553, 578),
  sentinel_x = c(70, 70, 83, 83, 83, 83, 83, 83, 83, 83, 83, 83, 96, 96, 108, 108, 108, 108, 108),
  stringsAsFactors = FALSE
)

fetch_nagano_weekly_report <- function(pdf_url, year = NA_integer_, week_num = NA_integer_) {
  if (!exists("pdf_words")) stop("pdf_table_utils.R を先に source してください")

  tmp <- tempfile(fileext = ".pdf")
  download.file(pdf_url, tmp, mode = "wb", quiet = TRUE)
  pages_txt <- pdftools::pdf_text(tmp)
  target_page <- which(grepl("届出定点数", pages_txt))[1]
  if (is.na(target_page)) stop("届出定点数の表が見つかりません（保健所別データなし）")

  hokenjo_order <- c("佐久", "上田", "諏訪", "伊那", "飯田", "木曽", "松本", "大町", "長野", "北信", "長野市", "松本市")

  wm <- regmatches(pages_txt[target_page], regexec("(20[0-9]{2})年.{0,10}第([0-9]+)週", pages_txt[target_page]))[[1]]
  if (length(wm) == 3) {
    week_label <- sprintf("%s年第%s週", wm[2], wm[3])
  } else if (!is.na(year) && !is.na(week_num)) {
    week_label <- sprintf("%d年第%d週", year, week_num)
  } else {
    week_label <- NA_character_
  }

  words <- pdf_words(tmp, page = target_page)

  # 保健所名列(x=39-42)の各行yを取得
  name_hits <- words[words$x >= 38 & words$x <= 45 & words$text %in% hokenjo_order, ]
  name_hits <- name_hits[!duplicated(name_hits$text), ]
  name_hits <- name_hits[match(hokenjo_order, name_hits$text), ]
  if (any(is.na(name_hits$text))) stop("保健所名の行が見つかりません")

  all_x <- sort(c(unname(.nagano_weekly_sentinel_cols), .nagano_weekly_diseases$x))
  n <- length(all_x)
  lo_bound <- c(all_x[1] - 8, (all_x[-n] + all_x[-1]) / 2)
  hi_bound <- c((all_x[-n] + all_x[-1]) / 2, all_x[n] + 12)
  # セル自体が省略されている（トークンが無い）保健所は「報告なし=0件」
  # として扱う（ユーザー指示）。定点数(sentinel_vals)が0/NAのままなら
  # 後段のrate計算で従来通りrate=NAとなるため、この0埋めがrateの
  # 妥当性を損なうことはない
  get_val_at <- function(row_words, target_x) {
    idx <- which(all_x == target_x)
    tok <- row_words$text[row_words$x >= lo_bound[idx] & row_words$x < hi_bound[idx]]
    if (length(tok) == 0) return(0)
    parse_hokenjo_number(tok[1])
  }

  out <- list()
  for (k in seq_along(hokenjo_order)) {
    hy <- name_hits$y[k]
    row_words <- words[words$y >= hy - 3 & words$y <= hy + 3 & words$x > 45, ]
    sentinel_vals <- sapply(.nagano_weekly_sentinel_cols, function(x) get_val_at(row_words, x))
    for (d in seq_len(nrow(.nagano_weekly_diseases))) {
      cnt <- get_val_at(row_words, .nagano_weekly_diseases$x[d])
      sent_x <- .nagano_weekly_diseases$sentinel_x[d]
      sent_n <- sentinel_vals[as.character(names(.nagano_weekly_sentinel_cols)[.nagano_weekly_sentinel_cols == sent_x])]
      rate <- if (!is.na(cnt) && !is.na(sent_n) && sent_n > 0) round(cnt / sent_n, 2) else NA_real_
      out[[length(out) + 1]] <- data.frame(
        pref = "長野県", week_label = week_label, hokenjo = hokenjo_order[k],
        disease = .nagano_weekly_diseases$name[d], count = cnt, rate = rate,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, out)
}
