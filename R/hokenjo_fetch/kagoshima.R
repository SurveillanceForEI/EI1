# 鹿児島県「疾病別保健所別患者報告数及び定点当たり報告数（男女合計）」PDF（週報 p.5）
# https://www.pref.kagoshima.jp/ae06/kenko-fukushi/kenko-iryo/kansen/hasseidoko/week/documents/<id>.pdf 型
# （添付ファイルIDが週ごとに変わるため、呼び出し側で最新PDFのURLを解決すること。
#  旧ドメイン kagoshima-pref.jp は不可、pref.kagoshima.jp を使用）
#
# レイアウト: p.5に14保健所×20疾患（報告数・定点当たりペア）の表が2ブロック
# （各10疾患）に分かれて並ぶ。0件セルは「-」、一部「…」（判読不能/省略）で
# 表される。pdf_text()の空白区切りで抽出できる（保健所名にスペースなし）。

.KAGOSHIMA_HOKENJO <- c("鹿児島市", "指宿", "加世田", "伊集院", "川薩", "出水", "大口",
                         "姶良", "志布志", "鹿屋", "西之表", "屋久島", "名瀬", "徳之島")

.KAGOSHIMA_BLOCK1 <- c("急性呼吸器感染症(ARI)", "インフルエンザ", "COVID-19", "ＲＳウイルス感染症",
                        "咽頭結膜熱", "Ａ群溶血性レンサ球菌咽頭炎", "感染性胃腸炎", "水痘",
                        "手足口病", "伝染性紅斑")
.KAGOSHIMA_BLOCK2 <- c("突発性発しん", "ヘルパンギーナ", "流行性耳下腺炎", "急性出血性結膜炎",
                        "流行性角結膜炎", "細菌性髄膜炎(真菌性を含む)", "無菌性髄膜炎",
                        "マイコプラズマ肺炎", "クラミジア肺炎(オウム病は除く)", "感染性胃腸炎(ロタウイルス)")

.kagoshima_parse_block <- function(lines, diseases, hokenjo_list) {
  hokenjo_re <- paste0("^(", paste(hokenjo_list, collapse = "|"), ")\\s+(.+)$")
  out <- list()
  for (ln in lines) {
    m <- regmatches(ln, regexec(hokenjo_re, ln))[[1]]
    if (length(m) < 3) next
    hokenjo <- m[2]
    toks <- strsplit(trimws(m[3]), "\\s+")[[1]]
    toks[toks == "…"] <- NA
    need <- length(diseases) * 2
    if (length(toks) < need) next
    toks <- toks[1:need]
    for (i in seq_along(diseases)) {
      cnt <- parse_hokenjo_number(toks[(i - 1) * 2 + 1])
      rate <- parse_hokenjo_number(toks[(i - 1) * 2 + 2])
      out[[length(out) + 1]] <- data.frame(hokenjo = hokenjo, disease = diseases[i], count = cnt, rate = rate, stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, out)
}

fetch_kagoshima <- function(pdf_url) {
  is_url <- grepl("^https?://", pdf_url)
  tmp <- pdf_url
  if (is_url) {
    tmp <- tempfile(fileext = ".pdf")
    download.file(pdf_url, tmp, mode = "wb", quiet = TRUE)
  }
  txt <- pdftools::pdf_text(tmp)
  p5 <- NA_integer_
  for (p in seq_along(txt)) if (grepl("疾病別保健所別患者報告数", txt[p])) { p5 <- p; break }
  if (is.na(p5)) stop("kagoshima: 保健所別ページが見つかりません")
  page_txt <- txt[p5]
  lines <- strsplit(page_txt, "\n")[[1]]

  wk_m <- regmatches(page_txt, regexpr("[0-9]{4}年[0-9]+週", page_txt))
  week_label <- if (length(wk_m) > 0) sub("週$", "週", sub("年", "年第", wk_m)) else NA_character_

  block_hdr <- grep("報告数\\s+定点当り", lines)
  if (length(block_hdr) < 2) stop("kagoshima: ブロック見出しが見つかりません")
  blk1 <- lines[block_hdr[1]:(block_hdr[2] - 1)]
  blk2 <- lines[block_hdr[2]:length(lines)]

  df1 <- .kagoshima_parse_block(blk1, .KAGOSHIMA_BLOCK1, .KAGOSHIMA_HOKENJO)
  df2 <- .kagoshima_parse_block(blk2, .KAGOSHIMA_BLOCK2, .KAGOSHIMA_HOKENJO)
  all_df <- rbind(df1, df2)

  data.frame(
    pref = "鹿児島県", week_label = week_label,
    hokenjo = all_df$hokenjo, disease = all_df$disease,
    count = all_df$count, rate = all_df$rate,
    stringsAsFactors = FALSE
  )
}
