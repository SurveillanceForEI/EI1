# 大分県「報告数・定点当たり報告数、疾病・保健所(総数)」PDF（週報 p.3, p.6）
# https://www.pref.oita.jp/uploaded/attachment/<id>.pdf 型
# （添付ファイルIDが週ごとに変わるため、呼び出し側で最新PDFのURLを解決すること）
#
# レイアウト: p.3に7保健所(東部/中部/南部/豊肥/西部/北部/大分市)×
# 疾病(報告数・定点当たりのペア)の表が3ブロックに分かれて並ぶ（1ブロック目
# 8疾患、2・3ブロック目は6疾患＋全数報告の別表が右側に続く）。
# 急性呼吸器感染症(ARI)はp.6の「疾病・保健所・性別(総数)」表（総数列）に
# 別掲。pdf_text()の空白区切りで十分安定（保健所名にスペースを含まない）。

.OITA_HOKENJO <- c("東部", "中部", "南部", "豊肥", "西部", "北部", "大分市")

.OITA_BLOCK1 <- c("インフルエンザ", "COVID-19", "ＲＳウイルス感染症", "咽頭結膜熱",
                   "Ａ群溶血性レンサ球菌咽頭炎", "感染性胃腸炎", "水痘", "手足口病")
.OITA_BLOCK2 <- c("伝染性紅斑", "突発性発疹", "ヘルパンギーナ", "流行性耳下腺炎",
                   "急性出血性結膜炎", "流行性角結膜炎")
.OITA_BLOCK3 <- c("細菌性髄膜炎", "無菌性髄膜炎", "マイコプラズマ肺炎", "クラミジア肺炎",
                   "感染性胃腸炎(ロタウイルス)", "マイコプラズマ肺炎(小児科)県独自")

.oita_parse_block <- function(lines, diseases) {
  hokenjo_re <- paste0("^(", paste(.OITA_HOKENJO, collapse = "|"), ")\\s+(.+)$")
  out <- list()
  for (ln in lines) {
    m <- regmatches(ln, regexec(hokenjo_re, ln))[[1]]
    if (length(m) < 3) next
    hokenjo <- m[2]
    toks <- strsplit(trimws(m[3]), "\\s+")[[1]]
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

fetch_oita <- function(pdf_url) {
  is_url <- grepl("^https?://", pdf_url)
  tmp <- pdf_url
  if (is_url) {
    tmp <- tempfile(fileext = ".pdf")
    download.file(pdf_url, tmp, mode = "wb", quiet = TRUE)
  }
  txt <- pdftools::pdf_text(tmp)
  p3 <- NA_integer_; p6 <- NA_integer_
  for (p in seq_along(txt)) {
    if (grepl("疾病・保健所\\(総数\\)", txt[p])) p3 <- p
    if (grepl("疾病・保健所・性別\\(総数\\)", txt[p])) p6 <- p
  }
  if (is.na(p3)) stop("oita: 保健所別ページが見つかりません")

  week_m <- regmatches(txt[p3], regexpr("[0-9]{4}年[0-9]+週", txt[p3]))
  week_label <- if (length(week_m) > 0) sub("年", "年第", sub("週$", "週", week_m)) else NA_character_

  lines3 <- strsplit(txt[p3], "\n")[[1]]
  # 3ブロックへ分割（"大分県" ヘッダーが各ブロックの開始）
  block_starts <- grep("^大分県", lines3)
  block_starts <- c(block_starts, length(lines3) + 1)

  blk1 <- lines3[block_starts[1]:(block_starts[2] - 1)]
  df1 <- .oita_parse_block(blk1, .OITA_BLOCK1)

  df2 <- NULL; df3 <- NULL
  if (length(block_starts) >= 3) {
    blk2 <- lines3[block_starts[2]:(block_starts[3] - 1)]
    df2 <- .oita_parse_block(blk2, .OITA_BLOCK2)
  }
  if (length(block_starts) >= 4) {
    blk3 <- lines3[block_starts[3]:(block_starts[4] - 1)]
    df3 <- .oita_parse_block(blk3, .OITA_BLOCK3)
  }

  df_ari <- NULL
  if (!is.na(p6)) {
    lines6 <- strsplit(txt[p6], "\n")[[1]]
    hokenjo_re <- paste0("^(", paste(.OITA_HOKENJO, collapse = "|"), ")\\s+(.+)$")
    out <- list()
    for (ln in lines6) {
      m <- regmatches(ln, regexec(hokenjo_re, ln))[[1]]
      if (length(m) < 3) next
      hokenjo <- m[2]
      toks <- strsplit(trimws(m[3]), "\\s+")[[1]]
      if (length(toks) < 2) next
      out[[length(out) + 1]] <- data.frame(
        hokenjo = hokenjo, disease = "急性呼吸器感染症(ARI)",
        count = parse_hokenjo_number(toks[1]), rate = parse_hokenjo_number(toks[2]),
        stringsAsFactors = FALSE
      )
    }
    df_ari <- do.call(rbind, out)
  }

  all_df <- do.call(rbind, Filter(Negate(is.null), list(df1, df2, df3, df_ari)))
  data.frame(
    pref = "大分県", week_label = week_label,
    hokenjo = all_df$hokenjo, disease = all_df$disease,
    count = all_df$count, rate = all_df$rate,
    stringsAsFactors = FALSE
  )
}
