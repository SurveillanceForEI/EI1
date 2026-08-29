# 福岡県 保健所別 感染症週報（HTML表）
# https://www.fihes.pref.fukuoka.jp/~idsc_fukuoka/idwr/table_t01/{NN}.html
# NN=01..12 が 北九州市/福岡市/久留米市/筑紫/粕屋/糸島/宗像・遠賀/
# 嘉穂・鞍手/田川/北筑後/南筑後/京築 の順（2026年8月確認済み）。
# 各ページに直近5週分×全疾患の表がある（1行目=保健所名、2-3行目=見出し、
# 4行目以降=定点種別/疾患名/(報告数,定当)×5週）。
# 文字コードは従来ISO-2022-JPだったが、2026-08-30頃にサイト側がShift_JISに
# 変更したことを確認。またサイト側の文字コードが今後も予告なく変わりうるため、
# 固定エンコーディングを指定せず、候補を順に試して正しくデコードできたものを
# 採用する（.fukuoka_decode参照）。

.FUKUOKA_PAGES <- c("北九州市", "福岡市", "久留米市", "筑紫", "粕屋", "糸島",
                     "宗像・遠賀", "嘉穂・鞍手", "田川", "北筑後", "南筑後", "京築")

# 候補エンコーディングを順に試し、NAにならず有効なHTMLとして解析できた
# 最初の結果を返す（サイト側の文字コード変更に自動追従するためのフォールバック）
.fukuoka_decode <- function(raw) {
  for (enc in c("Shift_JIS", "ISO-2022-JP", "UTF-8")) {
    txt <- tryCatch(httr::content(raw, "text", encoding = enc), error = function(e) NA)
    if (is.na(txt)) next
    doc <- tryCatch(xml2::read_html(txt), error = function(e) NULL)
    if (!is.null(doc)) return(doc)
  }
  stop("福岡県: いずれのエンコーディングでもHTMLを解析できませんでした（サイト側の文字コード変更の可能性）")
}

fetch_fukuoka <- function(week_col = NULL) {
  if (!requireNamespace("rvest", quietly = TRUE)) stop("rvest パッケージが必要です")
  if (!requireNamespace("httr", quietly = TRUE)) stop("httr パッケージが必要です")

  out <- list()
  for (i in seq_along(.FUKUOKA_PAGES)) {
    nn <- sprintf("%02d", i)
    url <- paste0("https://www.fihes.pref.fukuoka.jp/~idsc_fukuoka/idwr/table_t01/", nn, ".html")
    raw <- httr::GET(url)
    doc <- .fukuoka_decode(raw)
    tabs <- rvest::html_table(doc, fill = TRUE)
    t1 <- as.data.frame(tabs[[1]], stringsAsFactors = FALSE)

    hokenjo <- as.character(t1[1, 1])
    week_header <- as.character(unlist(t1[2, ]))
    data_rows <- t1[4:nrow(t1), ]
    data_rows <- data_rows[data_rows[, 2] != "" & !is.na(data_rows[, 2]), ]

    # 最新週（最後の報告数/定当ペア）の列を選ぶ。デフォルトは表の最終ペア
    n_col <- ncol(t1)
    if (is.null(week_col)) {
      count_col <- n_col - 1
      rate_col <- n_col
    } else {
      count_col <- week_col
      rate_col <- week_col + 1
    }
    week_label <- week_header[count_col]

    for (r in seq_len(nrow(data_rows))) {
      disease <- as.character(data_rows[r, 2])
      if (disease == "" || is.na(disease)) next
      cnt <- parse_hokenjo_number(data_rows[r, count_col])
      rate <- parse_hokenjo_number(data_rows[r, rate_col])
      out[[length(out) + 1]] <- data.frame(
        pref = "福岡県", week_label = week_label, hokenjo = hokenjo,
        disease = disease, count = cnt, rate = rate, stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, out)
}

# fetch_fukuoka()は表の最終列（最新週）のみを採用するが、実際には
# 各保健所ページに直近5週分（報告数,定当のペアが5組）が横に並んで
# 掲載されているため、バックフィル用にその5週すべてを返すバリアント
fetch_fukuoka_history <- function() {
  if (!requireNamespace("rvest", quietly = TRUE)) stop("rvest パッケージが必要です")
  if (!requireNamespace("httr", quietly = TRUE)) stop("httr パッケージが必要です")

  out <- list()
  for (i in seq_along(.FUKUOKA_PAGES)) {
    nn <- sprintf("%02d", i)
    url <- paste0("https://www.fihes.pref.fukuoka.jp/~idsc_fukuoka/idwr/table_t01/", nn, ".html")
    raw <- httr::GET(url)
    doc <- .fukuoka_decode(raw)
    tabs <- rvest::html_table(doc, fill = TRUE)
    t1 <- as.data.frame(tabs[[1]], stringsAsFactors = FALSE)

    hokenjo <- as.character(t1[1, 1])
    week_header <- as.character(unlist(t1[2, ]))
    data_rows <- t1[4:nrow(t1), ]
    data_rows <- data_rows[data_rows[, 2] != "" & !is.na(data_rows[, 2]), ]

    n_col <- ncol(t1)
    # 3列目以降が(報告数,定当)のペアで週ごとに並ぶ
    pair_starts <- seq(3, n_col - 1, by = 2)
    for (count_col in pair_starts) {
      rate_col <- count_col + 1
      week_label <- week_header[count_col]
      week_m <- regmatches(week_label, regexpr("第[0-9]+週", week_label))
      week_num <- if (length(week_m) > 0) as.integer(gsub("[^0-9]", "", week_m)) else NA_integer_
      for (r in seq_len(nrow(data_rows))) {
        disease <- as.character(data_rows[r, 2])
        if (disease == "" || is.na(disease)) next
        cnt <- parse_hokenjo_number(data_rows[r, count_col])
        rate <- parse_hokenjo_number(data_rows[r, rate_col])
        out[[length(out) + 1]] <- data.frame(
          pref = "福岡県", week_label = week_label, week_num = week_num, hokenjo = hokenjo,
          disease = disease, count = cnt, rate = rate, stringsAsFactors = FALSE
        )
      }
    }
  }
  do.call(rbind, out)
}
