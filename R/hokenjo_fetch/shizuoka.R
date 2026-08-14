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

  wm <- regmatches(pages_txt[target_pages[1]], regexec("第\\s*([0-9]+)\\s*週", pages_txt[target_pages[1]]))[[1]]
  week_label <- if (length(wm) == 2) sprintf("2026年第%s週", wm[2]) else NA_character_

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
      name_zone <- words[words$y >= hy - 45 & words$y < hy & !(words$text %in% exclude_labels) &
                           !grepl("^[0-9]+$", words$text), ]

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
