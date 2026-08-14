# 福島県「IDWR福島県 グラフ総覧」PDF（週報の一部、疾患ごとに1〜2疾患/ページの
# 「定点当り報告数グラフ」＋保健所別の直近4週間の報告数・定点当たり報告数表）
# https://www.pref.fukushima.lg.jp/uploaded/attachment/<id>.pdf
# （添付IDは週ごとに変わるため、呼び出し側で最新PDFのURLを解決すること）
#
# 各疾患ページはグラフの下に「福島市/県北/郡山市/県中/県南/会津/南会津/相双/
# いわき市（＋県内総数）」の10行×直近4週間（本レポート週を含む）の定点当たり
# 報告数（上段）と()内の報告数（下段）の表がある。1ページに疾患が2つ（左右に
# 並ぶ）または1つの場合がある。座標ベースで、各保健所ラベルの直近（数px上）に
# ある6トークン（4週分＋累計2列）のうち最新週（4番目）を取得する。

.fukushima_diseases <- c(
  "新型コロナウイルス感染症（COVID-19）", "インフルエンザ", "咽頭結膜熱", "ＲＳウイルス感染症",
  "Ａ群溶血性レンサ球菌咽頭炎", "感染性胃腸炎（ロタウイルス）", "感染性胃腸炎", "水痘", "手足口病",
  "伝染性紅斑", "突発性発しん", "ヘルパンギーナ", "流行性耳下腺炎", "急性出血性結膜炎",
  "流行性角結膜炎", "クラミジア肺炎", "細菌性髄膜炎", "マイコプラズマ肺炎", "無菌性髄膜炎",
  "新型コロナウイルス感染症（入院）", "急性呼吸器感染症（ARI）"
)

.fukushima_hokenjo <- c("福島市", "県北", "郡山市", "県中", "県南", "会津", "南会津", "相双", "いわき市")

.fukushima_page_table <- function(words, week_label) {
  # ページ内の疾患名（タイトル、y<200の最小y出現位置）を左右で判定
  disease_hits <- list()
  for (d in .fukushima_diseases) {
    hit <- words[words$text == d & words$y < 200, ]
    if (nrow(hit) == 0) next
    hit <- hit[order(hit$y), ][1, ]
    side <- if (hit$x < 250) "left" else "right"
    disease_hits[[side]] <- list(name = d, x = hit$x, y = hit$y)
  }
  if (length(disease_hits) == 0) return(NULL)

  out <- list()
  for (side in names(disease_hits)) {
    d <- disease_hits[[side]]$name
    hlabels <- words[words$text %in% .fukushima_hokenjo, ]
    if (side == "left") {
      hlabels <- hlabels[hlabels$x < 250, ]
    } else {
      hlabels <- hlabels[hlabels$x >= 250, ]
    }
    for (i in seq_len(nrow(hlabels))) {
      hx <- hlabels$x[i]; hy <- hlabels$y[i]; hname <- hlabels$text[i]
      rate_toks <- words[words$y >= (hy - 10) & words$y <= (hy - 1) &
                          words$x >= (hx - 70) & words$x <= (hx + 260) &
                          grepl("^([0-9]+(\\.[0-9]+)?|-)$", words$text), ]
      cnt_toks <- words[words$y >= (hy + 1) & words$y <= (hy + 12) &
                         words$x >= (hx - 70) & words$x <= (hx + 260) &
                         grepl("^(\\([0-9,]+\\)|-)$", words$text), ]
      rate_toks <- rate_toks[order(rate_toks$x), ]
      cnt_toks <- cnt_toks[order(cnt_toks$x), ]
      rte <- if (nrow(rate_toks) >= 4) rate_toks$text[4] else NA_character_
      cnt <- if (nrow(cnt_toks) >= 4) gsub("[()]", "", cnt_toks$text[4]) else NA_character_
      out[[length(out) + 1]] <- data.frame(
        pref = "福島県", week_label = week_label, hokenjo = hname, disease = d,
        count = parse_hokenjo_number(cnt), rate = parse_hokenjo_number(rte),
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, out)
}

fetch_fukushima <- function(pdf_url) {
  if (!exists("pdf_words")) stop("pdf_table_utils.R を先に source してください")

  tmp <- tempfile(fileext = ".pdf")
  download.file(pdf_url, tmp, mode = "wb", quiet = TRUE)
  n_pages <- pdftools::pdf_info(tmp)$pages

  words1 <- pdf_words(tmp, page = 1)
  rows1 <- group_words_into_rows(words1, y_tol = 3)
  tx2 <- paste(sapply(rows1[1:3], row_text), collapse = " ")
  wm <- regmatches(tx2, regexec("(20[0-9]{2})年第([0-9]+)週", tx2))[[1]]
  week_label <- if (length(wm) == 3) paste0(wm[2], "年第", wm[3], "週") else NA_character_

  out <- list()
  for (p in 3:n_pages) {
    w <- pdf_words(tmp, page = p)
    res <- tryCatch(.fukushima_page_table(w, week_label), error = function(e) NULL)
    if (!is.null(res)) out[[length(out) + 1]] <- res
  }
  do.call(rbind, out)
}
