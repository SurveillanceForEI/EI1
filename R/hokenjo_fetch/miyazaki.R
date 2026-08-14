# 宮崎県「感染症情報」PDF（最終ページ、9保健所×全疾患の表）
# https://www.pref.miyazaki.lg.jp/contents/org/fukushi/eikanken/center/infectious/pdf/{YEAR}{WEEK}.pdf
#
# レイアウト: 最終ページ（通常p.5）に「疾病名 | 前週 | 今週 | 宮崎市 | 都城 |
# 延岡 | 日南 | 小林 | 高鍋 | 高千穂 | 日向 | 中央」の11列表が「報告数」行と
# 「定点当り」行のペアで疾患ごとに並ぶ。疾患は固定順（19種＋ARI）。
# 一部の疾患（急性出血性結膜炎以降）でPDF内部の省略表記(…)により
# 高千穂等一部保健所の値が読み取れない場合がある（NAとする）。
# 表末尾に急性呼吸器感染症(ARI)の同形式の表が別途続く。

.MIYAZAKI_HOKENJO <- c("宮崎市", "都城", "延岡", "日南", "小林", "高鍋", "高千穂", "日向", "中央")
.MIYAZAKI_X_BINS <- c(209.5, 245.5, 286, 323, 359.5, 395.5, 428, 465, 507, 567)

.MIYAZAKI_DISEASES <- c(
  "インフルエンザ", "新型コロナウイルス感染症", "RSウイルス感染症", "咽頭結膜熱",
  "Ａ群溶血性レンサ球菌咽頭炎", "感染性胃腸炎", "水痘", "手足口病", "伝染性紅斑",
  "突発性発しん", "ヘルパンギーナ", "流行性耳下腺炎", "急性出血性結膜炎",
  "流行性角結膜炎", "細菌性髄膜炎", "無菌性髄膜炎", "マイコプラズマ肺炎",
  "クラミジア肺炎", "感染性胃腸炎(ロタウイルス)", "急性呼吸器感染症(ARI)"
)

fetch_miyazaki <- function(pdf_url) {
  if (!exists("pdf_words")) stop("pdf_table_utils.R を先に source してください")
  is_url <- grepl("^https?://", pdf_url)
  local_path <- pdf_url
  if (is_url) {
    local_path <- tempfile(fileext = ".pdf")
    download.file(pdf_url, local_path, mode = "wb", quiet = TRUE)
  }
  npages <- length(pdftools::pdf_text(local_path))

  words <- pdf_words(pdf_url, page = npages)
  rows <- group_words_into_rows(words, y_tol = 3)

  wk_m <- regmatches(row_text(rows[[1]]), regexpr("第[0-9]+週", row_text(rows[[1]])))
  week_label <- if (length(wk_m) > 0) paste0("2026年", wk_m) else NA_character_

  bins <- .MIYAZAKI_X_BINS
  hokenjo <- .MIYAZAKI_HOKENJO
  is_num <- function(s) grepl("^[0-9,]+(\\.[0-9]+)?$|^…$", s)

  report_idx <- which(sapply(rows, function(r) any(r$text == "報告数")))
  rate_idx <- which(sapply(rows, function(r) any(r$text == "定点当り")))
  n <- min(length(report_idx), length(rate_idx), length(.MIYAZAKI_DISEASES))

  out <- list()
  for (i in seq_len(n)) {
    r_cnt <- rows[[report_idx[i]]]
    r_rate <- rows[[rate_idx[i]]]
    dname <- .MIYAZAKI_DISEASES[i]
    for (k in seq_along(hokenjo)) {
      cell_c <- r_cnt[r_cnt$x >= bins[k] & r_cnt$x < bins[k + 1], ]
      cell_r <- r_rate[r_rate$x >= bins[k] & r_rate$x < bins[k + 1], ]
      ctok <- if (nrow(cell_c) > 0) cell_c$text[vapply(cell_c$text, is_num, logical(1))] else character(0)
      rtok <- if (nrow(cell_r) > 0) cell_r$text[vapply(cell_r$text, is_num, logical(1))] else character(0)
      cnt <- if (length(ctok) >= 1 && ctok[1] != "…") parse_hokenjo_number(ctok[1]) else NA_real_
      rate <- if (length(rtok) >= 1 && rtok[1] != "…") parse_hokenjo_number(rtok[1]) else NA_real_
      out[[length(out) + 1]] <- data.frame(
        pref = "宮崎県", week_label = week_label, hokenjo = hokenjo[k],
        disease = dname, count = cnt, rate = rate, stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, out)
}
