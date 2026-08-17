# 和歌山県「和歌山県感染症週報」PDF
# https://www.pref.wakayama.lg.jp/prefg/031801/idsw/khdc/d00153694_d/fil/WIDR{YEAR}{WEEK}.pdf
#
# p.11付近「＜保健所別の患者報告数（和歌山県）＞」に9保健所（和歌山市/
# 海南/岩出/橋本/湯浅/御坊/田辺/新宮/串本）×19疾患の「報告」(count)・
# 「定当」(rate)が2行1組（間に疾患名の行）で並ぶ。pdf_text()の出力が
# 各行きれいに整列しているため、テキスト行ベースで解析する。
# 「…」は「保健所管内に定点が存在しない」を意味しNAとする。

WAKAYAMA_HOKENJO <- c("和歌山市", "海南", "岩出", "橋本", "湯浅", "御坊", "田辺", "新宮", "串本")

fetch_wakayama <- function(pdf_url, page = NULL) {
  if (missing(pdf_url) || is.null(pdf_url)) stop("pdf_url を指定してください")
  if (!requireNamespace("pdftools", quietly = TRUE)) stop("pdftools が必要です")
  tf <- tempfile(fileext = ".pdf")
  download.file(pdf_url, tf, mode = "wb", quiet = TRUE)
  full <- pdftools::pdf_text(tf)

  if (is.null(page)) {
    # 本文中の注目トピック解説（1ページ目等）にも「保健所別」の文言が
    # 現れることがあるため、実際の表がある最後の一致ページを使う
    hits <- which(grepl("＜保健所別の患者報告数", full))
    if (length(hits) == 0) hits <- which(grepl("保健所別の患者報告数", full))
    if (length(hits) == 0) stop("「保健所別の患者報告数」ページが見つかりません")
    page <- hits[length(hits)]
  }
  txt <- full[page]
  lines <- strsplit(txt, "\n")[[1]]

  head_txt <- paste(full[1:2], collapse = " ")
  week_m <- regmatches(head_txt, regexpr("20[0-9]{2} ?年第 ?[0-9]+ ?週", head_txt))
  week_label <- if (length(week_m) > 0) gsub(" ", "", week_m[1]) else NA_character_

  out <- list()
  i <- 1
  n <- length(lines)
  while (i <= n) {
    ln <- lines[i]
    if (grepl("^\\s*報告\\s", ln)) {
      report_toks <- strsplit(trimws(sub("^報告", "", trimws(ln))), "\\s+")[[1]]
      # 次の行が疾患名、その次が定当行のはず
      name_line <- if (i + 1 <= n) trimws(lines[i + 1]) else ""
      rate_line <- if (i + 2 <= n) lines[i + 2] else ""
      if (grepl("^\\s*定当\\s", rate_line) && nzchar(name_line) && !grepl("^(報告|定当)", name_line)) {
        rate_toks <- strsplit(trimws(sub("^定当", "", trimws(rate_line))), "\\s+")[[1]]
        disease <- gsub("\\s+", "", name_line)
        if (length(report_toks) == 9 && length(rate_toks) == 9) {
          for (k in seq_along(WAKAYAMA_HOKENJO)) {
            rv <- report_toks[k]; ratev <- rate_toks[k]
            cnt <- if (rv %in% c("…")) NA_real_ else parse_hokenjo_number(rv)
            rate <- if (ratev %in% c("…")) NA_real_ else parse_hokenjo_number(ratev)
            if (is.na(cnt) && is.na(rate)) next
            out[[length(out) + 1]] <- data.frame(
              pref = "和歌山県", week_label = week_label, hokenjo = WAKAYAMA_HOKENJO[k],
              disease = disease, count = cnt, rate = rate,
              stringsAsFactors = FALSE
            )
          }
        }
        i <- i + 3
        next
      }
    }
    i <- i + 1
  }
  do.call(rbind, out)
}
