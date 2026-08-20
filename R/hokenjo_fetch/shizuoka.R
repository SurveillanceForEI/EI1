# 静岡県「定点把握感染症 保健所別状況」PDF（週報）
# https://www.pref.shizuoka.jp/_res/projects/default_project/_page_/001/081/723/{...}idwr{week}-2.pdf 型
#
# レイアウト: 「定点把握感染症 保健所別状況」というタイトルのページ（通常2ページ、
# 全34ページ超のうち p.32-33付近）に、5疾患ずつ横並びの表が複数セクション
# 縦に並ぶ。各疾患は「罹患数（またはヘルパンギーナ列のみ"週計"）/定点当り」の
# 2列。保健所行は「総数（県計、除外）/賀茂/熱海/東部/御殿場/富士/静岡市/中部/
# 西部/浜松市」の順で固定。値が無いセルも "-" や "…"（非定点）で埋められているため
# 欠損セルによる列ズレは起きないが、疾患名が2行に折り返されたり、複数の疾患表
# ブロックが横に並ぶため、pdf_data() の座標(x,y)で列範囲・行範囲を特定して抽出する。

fetch_shizuoka <- function(pdf_url = "https://www.pref.shizuoka.jp/_res/projects/default_project/_page_/001/081/723/2026idwr31-2.pdf") {
  if (!exists("pdf_words")) stop("pdf_table_utils.R を先に source してください")

  tmp <- tempfile(fileext = ".pdf")
  download.file(pdf_url, tmp, mode = "wb", quiet = TRUE)
  pages_txt <- pdftools::pdf_text(tmp)
  target_pages <- which(grepl("定点把握感染症\\s*保健所別状況", pages_txt))
  if (length(target_pages) == 0) stop("保健所別状況ページが見つかりません")

  row_names <- c("賀茂", "熱海", "東部", "御殿場", "富士", "静岡市", "中部", "西部", "浜松市")

  # 表紙ページ等に全角数字で「２０２５年第３０週」、または「2025年第 20 週」
  # のように空白が入って書かれていることがある。以前は「第」と週番号の間
  # にのみ空白を許容していたが、実際には「20 26 年第 3 1週」のように年・
  # 週番号の桁の間（例:「2026」が「20」「26」に、「31」が「3」「1」に
  # 分割される）にもpdftools側のフォント都合で空白が入ることがあり、
  # その場合は表紙ページで一致せず、本文中の無関係な週表記（過去5年比較の
  # グラフ凡例等）を誤って拾ってしまうバグがあった（例: 2026年第31週号の
  # 表紙が「2026年第25週」と誤認識される）。空白の入り方を個別に想定する
  # のではなく、比較前にテキストから空白を全て除去することで確実に
  # 対応する
  pages_txt_norm <- gsub("[ \t]+", "", chartr("０１２３４５６７８９", "0123456789", pages_txt))
  week_pattern <- "20[0-9]{2}年第[0-9]+週"
  extract_week_label <- function(txt) {
    wm <- regmatches(txt, regexpr(week_pattern, txt))
    if (length(wm) > 0 && nzchar(wm)) wm else NA_character_
  }
  # 表紙(1ページ目)で見つからない場合のみ2ページ目も試す。全文書検索は
  # 本文中の過去比較グラフ等から誤った年を拾うリスクが高いため行わない
  # （見つからなければNAのまま=このPDFからは取得不可として扱う）
  week_label <- extract_week_label(pages_txt_norm[1])
  if (is.na(week_label) && length(pages_txt_norm) >= 2) {
    week_label <- extract_week_label(pages_txt_norm[2])
  }

  exclude_labels <- c("保健所名", "第", "週", "定点把握感染症", "保健所別状況", "指定届出機関", "（定点）数")

  out <- list()

  for (page in target_pages) {
    words <- pdf_words(tmp, page = page)

    rate_hits <- words[words$text == "定点当り", ]
    if (nrow(rate_hits) == 0) next
    header_ys <- sort(unique(rate_hits$y))

    for (hy in header_ys) {
      rr <- rate_hits[rate_hits$y == hy, ]
      rr <- rr[order(rr$x), ]
      rate_xs <- rr$x
      count_xs <- rate_xs - 40

      # データ開始行（"総数"）のy
      total_hits <- words[words$text == "総数" & words$y > hy & words$y < hy + 25, ]
      if (nrow(total_hits) == 0) next
      total_y <- min(total_hits$y)

      # 各保健所行のyを順に連鎖的に特定
      row_ys <- numeric(length(row_names))
      cur_y <- total_y
      ok <- TRUE
      for (i in seq_along(row_names)) {
        cand <- words[words$text == row_names[i] & words$x < 90 & words$y > cur_y & words$y < cur_y + 25, ]
        if (nrow(cand) == 0) { ok <- FALSE; break }
        cand_y <- cand$y[which.min(cand$y)]
        row_ys[i] <- cand_y
        cur_y <- cand_y
      }
      if (!ok) next

      # 疾患名: ヘッダー行(hy)より上、hy-30～hy-1 の範囲のテキストを列範囲ごとに集約
      # 週番号が1桁（第1週〜第9週）のとき、pdftoolsの単語分割で「1週」のように
      # 数字と「週」が1トークンに結合されることがあり、これがexclude_labelsの
      # 完全一致（"週"単体のみ）をすり抜けて疾患名の先頭に混入するバグがあった
      # （例:「ＲＳウイルス感染症」→「1週ＲＳウイルス感染症」）。
      # 「第」「N週」の組み合わせも含めて正規表現で除外する
      name_zone <- words[words$y >= hy - 45 & words$y < hy & !(words$text %in% exclude_labels) &
                           !grepl("^[0-9]+$", words$text) & !grepl("^第?[0-9]*週$", words$text), ]

      n_disease <- length(rate_xs)
      col_lo <- count_xs - 20
      col_hi <- c(count_xs[-1] - 20, rate_xs[n_disease] + 45)

      for (d in seq_len(n_disease)) {
        lo <- col_lo[d]; hi <- col_hi[d]
        nm <- name_zone[name_zone$x >= lo & name_zone$x < hi, ]
        nm <- nm[order(nm$y, nm$x), ]
        disease <- paste(nm$text, collapse = "")
        if (!nzchar(disease)) next

        for (i in seq_along(row_names)) {
          ry <- row_ys[i]
          vals <- words[abs(words$y - ry) <= 3 & words$x >= lo & words$x < hi, ]
          vals <- vals[order(vals$x), ]
          cnt <- if (nrow(vals) >= 1) parse_hokenjo_number(vals$text[1]) else NA_real_
          rt  <- if (nrow(vals) >= 2) parse_hokenjo_number(vals$text[2]) else NA_real_
          out[[length(out) + 1]] <- data.frame(
            pref = "静岡県", week_label = week_label, hokenjo = row_names[i],
            disease = disease, count = cnt, rate = rt,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  do.call(rbind, out)
}
