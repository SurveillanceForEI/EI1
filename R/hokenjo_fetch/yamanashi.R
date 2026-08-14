# 山梨県「感染症発生動向調査週報（定点把握対象疾患）」PDF
# https://www.pref.yamanashi.jp/documents/101494/{YEAR}{WEEK}w.pdf 型
#
# レイアウト: 疾患ごとに2行1組（「累積」＝報告数、「定当」＝定点当たり報告数）。
# 各行末尾6列が「山梨県, 中北, 峡東, 峡南, 富士・東部, 甲府市」の値
# （数値の無いセルも "－"/"…" で埋められているため pdf_text() の空白区切りでも
# 列ズレが起きない）。疾患名は「累積」行の直前の行にある。

fetch_yamanashi <- function(pdf_url = "https://www.pref.yamanashi.jp/documents/101494/202631w.pdf") {
  if (!requireNamespace("pdftools", quietly = TRUE)) stop("pdftools パッケージが必要です")

  tmp <- tempfile(fileext = ".pdf")
  download.file(pdf_url, tmp, mode = "wb", quiet = TRUE)
  page_txt <- pdftools::pdf_text(tmp)[1]
  lines <- strsplit(page_txt, "\n")[[1]]
  # ページ末尾の「上位５疾患」サマリー欄にも"定当"の文字列が出るため、
  # 主表の終わり（急性呼吸器感染症の直後、コメント欄の手前）で打ち切る
  cutoff <- grep("コメント|上位５疾患", lines)
  if (length(cutoff) > 0) lines <- lines[seq_len(min(cutoff) - 1)]

  hokenjo_cols <- c("山梨県", "中北", "峡東", "峡南", "富士・東部", "甲府市")

  wm <- regmatches(page_txt, regexec("(20[0-9]{2})年([0-9]+)週", page_txt))[[1]]
  week_label <- if (length(wm) == 3) sprintf("%s年第%s週", wm[2], wm[3]) else NA_character_

  out <- list()
  for (i in seq_along(lines)) {
    # ブロック構成は「累積(報告数)行 → 疾患名行 → 定当(定点当たり)行」の順
    if (!grepl("定当", lines[i])) next
    disease <- if (i - 1 >= 1) trimws(lines[i - 1]) else NA_character_
    if (is.na(disease) || !nzchar(disease)) next
    count_line <- if (i - 2 >= 1) lines[i - 2] else ""
    count_toks <- strsplit(trimws(count_line), "\\s+")[[1]]
    count_toks <- tail(count_toks, length(hokenjo_cols))
    rate_toks <- strsplit(trimws(lines[i]), "\\s+")[[1]]
    rate_toks <- tail(rate_toks, length(hokenjo_cols))
    if (length(count_toks) != length(hokenjo_cols) || length(rate_toks) != length(hokenjo_cols)) next

    for (k in seq_along(hokenjo_cols)) {
      hj <- hokenjo_cols[k]
      if (hj == "山梨県") next
      hj_norm <- gsub("・", "", hj)
      out[[length(out) + 1]] <- data.frame(
        pref = "山梨県", week_label = week_label, hokenjo = hj_norm,
        disease = disease,
        count = parse_hokenjo_number(count_toks[k]),
        rate  = parse_hokenjo_number(rate_toks[k]),
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, out)
}
