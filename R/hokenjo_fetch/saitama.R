# 埼玉県「感染症発生状況（定点把握対象疾患）報告患者数 保健所別」PDF
# https://www.pref.saitama.lg.jp/documents/277313/{YEAR}_{WEEK}w.pdf
#
# 該当ページ（8ページ構成中、通常5ページ目）は疾患名が縦書き1文字ずつ
# 複数行に折り返して配置されているため、疾患名は列順が固定であることを
# 利用してハードコードする（page6の年齢階級別表と合計値が一致することで
# 順序を確認済み）。各保健所は「報告数」行→保健所名行→「定点当たり」行
# の3行1組で並ぶ。

.saitama_diseases <- c(
  "インフルエンザ", "新型コロナウイルス感染症", "急性呼吸器感染症",
  "RSウイルス感染症", "咽頭結膜熱", "Ａ群溶血性レンサ球菌咽頭炎",
  "感染性胃腸炎", "水痘", "手足口病", "伝染性紅斑", "突発性発しん",
  "ヘルパンギーナ", "流行性耳下腺炎", "急性出血性結膜炎", "流行性角結膜炎",
  "細菌性髄膜炎", "無菌性髄膜炎", "マイコプラズマ肺炎",
  "クラミジア肺炎（オウム病を除く）", "感染性胃腸炎（ロタウイルスに限る）",
  "インフルエンザ（入院）", "新型コロナウイルス感染症（入院）"
)

fetch_saitama <- function(pdf_url = NULL) {
  if (!exists("pdf_words")) stop("pdf_table_utils.R を先に source してください")
  if (is.null(pdf_url)) {
    pdf_url <- "https://www.pref.saitama.lg.jp/documents/277313/2026_31w.pdf"
  }

  path <- tempfile(fileext = ".pdf")
  download.file(pdf_url, path, mode = "wb", quiet = TRUE)
  n_pages <- length(pdftools::pdf_data(path))

  target_page <- NA_integer_
  for (p in seq_len(n_pages)) {
    w <- pdftools::pdf_data(path)[[p]]
    if ("定点当たり" %in% w$text && sum(w$text == "保") >= 1 && sum(w$text == "健") >= 1) {
      target_page <- p
      break
    }
  }
  if (is.na(target_page)) stop("保健所別データのページが見つかりません")
  w <- pdftools::pdf_data(path)[[target_page]]

  wl <- w$text[grepl("^\\(20[0-9]{2}年第[0-9]+週$", w$text)]
  week_label <- if (length(wl) > 0) sub("^\\(", "", wl[1]) else NA_character_

  rows <- group_words_into_rows(w, y_tol = 3)

  out <- list()
  cur_counts <- NULL
  cur_hokenjo <- NULL

  for (i in seq_along(rows)) {
    rr <- rows[[i]]
    rr <- rr[order(rr$x), ]
    txt <- rr$text

    if (any(txt == "報") && any(txt == "告") && any(txt == "数")) {
      nums <- txt[!(txt %in% c("報", "告", "数"))]
      if (length(nums) >= 15) {
        cur_counts <- nums
        cur_hokenjo <- NULL
      }
      next
    }
    if (any(txt == "定点当たり")) {
      nums <- txt[txt != "定点当たり"]
      if (!is.null(cur_counts) && length(nums) >= 15) {
        m <- min(length(.saitama_diseases), length(cur_counts), length(nums))
        hj <- gsub("\\s+", "", paste(cur_hokenjo, collapse = ""))
        if (!is.null(hj) && nchar(hj) > 0 && hj != "全県") {
          out[[length(out) + 1]] <- data.frame(
            pref = "埼玉県", week_label = week_label,
            hokenjo = hj,
            disease = .saitama_diseases[seq_len(m)],
            count = parse_hokenjo_number(cur_counts[seq_len(m)]),
            rate = parse_hokenjo_number(nums[seq_len(m)]),
            stringsAsFactors = FALSE
          )
        }
      }
      cur_counts <- NULL
      cur_hokenjo <- NULL
      next
    }
    if (!is.null(cur_counts) && is.null(cur_hokenjo) && nrow(rr) <= 6 &&
        all(grepl("^[\\p{Han}\\p{Hiragana}\\p{Katakana}ー]+$", txt, perl = TRUE))) {
      cur_hokenjo <- txt
    }
  }

  df <- do.call(rbind, out)
  rownames(df) <- NULL
  df
}
