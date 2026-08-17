# 滋賀県「感染症発生動向調査週報」PDF
# https://www.pref.shiga.lg.jp/file/attachment/<添付ID>.pdf （IDは週ごとに変わる）
#
# p.3「２．定点把握疾患（五類感染症）の定点当たりの報告数」に
# 7保健所（大津市/草津/甲賀/東近江/彦根/長浜/高島）×22疾患の
# 定点当たり報告数（rate）が掲載されている。このページには報告数
# （count）そのものは含まれない（p.4に一部疾患のみ年齢階級別の
# 総数=countがあるが全疾患を網羅しないため、ここでは rate のみ
# 取得しcountはNAとする）。

SHIGA_HOKENJO <- c("大津市", "草津", "甲賀", "東近江", "彦根", "長浜", "高島")

SHIGA_DISEASES <- c(
  "急性呼吸器感染症（ARI）", "インフルエンザ", "新型コロナウイルス感染症",
  "ＲＳウイルス感染症", "咽頭結膜熱", "Ａ群溶血性レンサ球菌咽頭炎",
  "感染性胃腸炎", "水痘", "手足口病", "伝染性紅斑（リンゴ病）",
  "突発性発しん", "ヘルパンギーナ", "流行性耳下腺炎(おたふくかぜ)",
  "急性出血性結膜炎", "流行性角結膜炎", "細菌性髄膜炎", "無菌性髄膜炎",
  "マイコプラズマ肺炎", "クラミジア肺炎（オウム病を除く）",
  "感染性胃腸炎(ロタウイルス)", "インフルエンザ入院", "COVID-19入院"
)

# p.4「３-1．定点把握疾患の年齢階級別報告数」には、インフルエンザ・
# 新型コロナウイルス感染症・急性呼吸器感染症(ARI)の3疾患に限り
# 「＜保健所名＞保健所 総数 ...」という形で保健所別の報告数(count)
# が掲載されている（他の19疾患は保健所別countがこの週報には
# 含まれず、県全体の総数のみp.5にある）。ここで取得できた3疾患分は
# rateデータとマージしてcountを埋める。
fetch_shiga_page4_counts <- function(pdf_url, page = 4) {
  words <- pdf_words(pdf_url, page = page)
  sub <- words[words$x < 200, ]
  rows <- group_words_into_rows(sub, y_tol = 3)
  disease_map <- c("インフルエンザ" = "インフルエンザ",
                    "新型コロナウイルス" = "新型コロナウイルス感染症",
                    "（ＡＲＩ）" = "急性呼吸器感染症（ARI）",
                    "（ARI）" = "急性呼吸器感染症（ARI）")
  cur_disease <- NA_character_
  out <- list()
  for (i in seq_along(rows)) {
    rdf <- rows[[i]]
    txt <- row_text(rdf)
    if (grepl("^インフルエンザ ", txt) || txt == "インフルエンザ") cur_disease <- "インフルエンザ"
    if (grepl("新型コロナウイルス", txt)) cur_disease <- "新型コロナウイルス感染症"
    if (grepl("^急性呼吸器感染症$", txt) &&
        i + 1 <= length(rows) && grepl("^[0-9]", rows[[i + 1]]$text[1])) cur_disease <- "急性呼吸器感染症（ARI）"
    hj <- rdf$text[grepl("保健所$", rdf$text)]
    if (length(hj) > 0 && !is.na(cur_disease)) {
      hokenjo <- sub("保健所$", "", hj[1])
      nums <- rdf$text[grepl("^[0-9]+$", rdf$text)]
      if (length(nums) > 0) {
        out[[length(out) + 1]] <- data.frame(hokenjo = hokenjo, disease = cur_disease,
                                              count = parse_hokenjo_number(nums[1]),
                                              stringsAsFactors = FALSE)
      }
    }
  }
  do.call(rbind, out)
}

fetch_shiga <- function(pdf_url, page = NULL) {
  if (missing(pdf_url) || is.null(pdf_url)) stop("pdf_url を指定してください（滋賀県の添付IDは週ごとに変わるため事前確認が必要）")
  if (!exists("pdf_words")) stop("pdf_table_utils.R を先に source してください")

  if (is.null(page)) {
    # ページ構成は号によって前後する（ARI解説ページの有無等）ため、
    # 「定点当たりの報告数」の見出しを含むページを都度検出する
    tmp <- tempfile(fileext = ".pdf")
    download.file(pdf_url, tmp, mode = "wb", quiet = TRUE)
    pages_txt <- pdftools::pdf_text(tmp)
    # 表紙や目次にも同じ文言が現れることがあるため、最後の一致（実際の表）を採用する
    hit <- which(grepl("定点当たりの報告数", pages_txt))
    if (length(hit) == 0) stop("shiga: 「定点当たりの報告数」のページが見つかりません")
    page <- hit[length(hit)]
  }
  words <- pdf_words(pdf_url, page = page)

  full_text <- paste(words$text, collapse = " ")
  week_m <- regmatches(full_text, regexpr("令和[0-9０-９]+年第 ?[0-9０-９]+ ?週", full_text))
  week_label <- if (length(week_m) > 0) week_m[1] else NA_character_

  hdr <- words[words$y > 100 & words$y < 116 & words$x > 280, ]
  # 7列のx中心を決め打ち検出（大津市/草津/甲賀/東近江/彦根/長浜/高島の順で概ね等間隔）
  col_x <- c("大津市" = 296, "草津" = 321, "甲賀" = 347, "東近江" = 373,
             "彦根" = 401, "長浜" = 430, "高島" = 457)

  is_num_tok <- function(x) grepl("^([0-9]+\\.[0-9]+|[0-9]+)$", x)

  out <- list()
  for (disease in SHIGA_DISEASES) {
    # 疾患名は改行で分割され複数トークンのことがあるため、先頭語で検索
    key <- strsplit(disease, "")[[1]][1]
    cand <- words[words$x < 260 & words$y > 118, ]
    # 疾患名テキストの完全一致は難しいため、既知の代表語で行を特定
    disease_search <- switch(disease,
      "急性呼吸器感染症（ARI）" = "（ARI）",
      "新型コロナウイルス感染症" = "新型コロナウイルス",
      "Ａ群溶血性レンサ球菌咽頭炎" = "球菌咽頭炎",
      "伝染性紅斑（リンゴ病）" = "（リンゴ病）",
      "流行性耳下腺炎(おたふくかぜ)" = "(おたふくかぜ)",
      "クラミジア肺炎（オウム病を除く）" = "（オウム病を除く）",
      "感染性胃腸炎(ロタウイルス)" = "(ロタウイルス)",
      disease
    )
    hits <- words[words$text == disease_search, ]
    hits <- hits[hits$x < 280, ]
    hit <- hits
    if (nrow(hit) == 0) next
    row_y <- hit$y[1]
    rowband <- words[words$y > row_y - 4 & words$y < row_y + 4 & words$x > 280 & words$x < 480, ]
    numtoks <- rowband[is_num_tok(rowband$text), ]
    for (i in seq_len(nrow(numtoks))) {
      w <- numtoks[i, ]
      nearest <- names(which.min(abs(col_x - w$x)))
      if (abs(col_x[nearest] - w$x) > 15) next
      out[[length(out) + 1]] <- data.frame(
        pref = "滋賀県", week_label = week_label, hokenjo = nearest,
        disease = disease,
        count = NA_real_,
        rate = parse_hokenjo_number(w$text),
        stringsAsFactors = FALSE
      )
    }
  }
  df <- do.call(rbind, out)

  cnt <- tryCatch(fetch_shiga_page4_counts(pdf_url, page = page + 1), error = function(e) NULL)
  if (!is.null(cnt) && nrow(cnt) > 0) {
    for (i in seq_len(nrow(cnt))) {
      idx <- which(df$hokenjo == cnt$hokenjo[i] & df$disease == cnt$disease[i])
      if (length(idx) > 0) df$count[idx] <- cnt$count[i]
    }
  }
  df
}
