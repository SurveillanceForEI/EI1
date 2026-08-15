# 熊本県「保健所別発生状況」PDF（週報 p.6）
# https://www.pref.kumamoto.jp/uploaded/attachment/<id>.pdf 型
# （添付ファイルIDが週ごとに変わるため、呼び出し側で最新PDFのURLを解決すること）
#
# レイアウト: p.6に「保健所別発生状況」（報告数、上段）と
# 「保健所別 一定点当り患者報告数」（下段）の2表があり、11保健所×20疾患の
# 数値がスペース区切りの行として並ぶ（各行「番号 保健所名 値×20」）。
# pdf_text()の単純な空白分割で十分に安定して読み取れる（保健所名にスペースを
# 含まないため）。疾患名は列見出しが複数行に折り返されるため座標抽出が
# 煩雑なので、固定順の疾患名リストを用いる。

.KUMAMOTO_HOKENJO <- c("熊本市", "山鹿", "菊池", "阿蘇", "御船", "八代", "水俣", "人吉", "有明", "宇城", "天草")

.KUMAMOTO_DISEASES <- c(
  "インフルエンザ", "新型コロナウイルス感染症(COVID-19)", "急性呼吸器感染症(ARI)",
  "RSウイルス感染症", "咽頭結膜熱", "A群溶血性レンサ球菌咽頭炎", "感染性胃腸炎",
  "水痘", "手足口病", "伝染性紅斑(リンゴ病)", "突発性発しん", "ヘルパンギーナ",
  "流行性耳下腺炎", "急性出血性結膜炎", "流行性角結膜炎", "細菌性髄膜炎",
  "無菌性髄膜炎", "マイコプラズマ肺炎", "クラミジア肺炎", "感染性胃腸炎(ロタウイルス)"
)

fetch_kumamoto <- function(pdf_url) {
  tmp <- pdf_url
  is_url <- grepl("^https?://", pdf_url)
  if (is_url) {
    tmp <- tempfile(fileext = ".pdf")
    download.file(pdf_url, tmp, mode = "wb", quiet = TRUE)
  }
  txt <- pdftools::pdf_text(tmp)
  page6 <- NA_integer_
  for (p in seq_along(txt)) if (grepl("保健所別発生状況", txt[p])) { page6 <- p; break }
  if (is.na(page6)) stop("kumamoto: 保健所別発生状況ページが見つかりません")
  page_txt <- txt[page6]
  lines <- strsplit(page_txt, "\n")[[1]]

  full_text_norm <- chartr("０１２３４５６７８９", "0123456789", paste(txt, collapse = " "))
  wm <- regmatches(full_text_norm, regexec("令和\\s*([0-9]+)\\s*年.{0,20}第\\s*([0-9]+)\\s*週", full_text_norm))[[1]]
  week_label <- if (length(wm) == 3) sprintf("%d年第%s週", as.integer(wm[2]) + 2018, wm[3]) else NA_character_

  data_line_re <- paste0("^\\s*[0-9]+\\s+(", paste(.KUMAMOTO_HOKENJO, collapse = "|"), ")保健所\\s+(.+)$")

  split_idx <- grep("一定点当り患者報告数", lines)
  boundary <- if (length(split_idx) > 0) split_idx[1] else floor(length(lines) / 2)

  parse_block <- function(block_lines) {
    out <- list()
    for (ln in block_lines) {
      m <- regmatches(ln, regexec(data_line_re, ln))[[1]]
      if (length(m) < 3) next
      hokenjo <- m[2]
      rest <- trimws(m[3])
      toks <- strsplit(rest, "\\s+")[[1]]
      n <- min(length(toks), length(.KUMAMOTO_DISEASES))
      for (k in seq_len(n)) {
        out[[length(out) + 1]] <- data.frame(
          hokenjo = hokenjo, disease = .KUMAMOTO_DISEASES[k],
          value = parse_hokenjo_number(toks[k]), stringsAsFactors = FALSE
        )
      }
    }
    do.call(rbind, out)
  }

  df_count <- parse_block(lines[1:boundary])
  df_rate <- parse_block(lines[(boundary + 1):length(lines)])

  merged <- merge(df_count, df_rate, by = c("hokenjo", "disease"), suffixes = c("_count", "_rate"), all = TRUE)
  data.frame(
    pref = "熊本県", week_label = week_label,
    hokenjo = merged$hokenjo, disease = merged$disease,
    count = merged$value_count, rate = merged$value_rate,
    stringsAsFactors = FALSE
  )
}
