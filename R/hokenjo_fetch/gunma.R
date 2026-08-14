# 群馬県「報告数・定点当たり報告数、疾病・管轄保健所別」PDF
# https://www.pref.gunma.jp/uploaded/attachment/<id>.pdf
# （URLは週ごとに変わる添付ファイルIDのため、呼び出し側で最新PDFのURLを解決すること）
#
# レイアウト: 1ページ内に「主表（ARI・小児科等の疾病×地域）」と、右側に
# 「ARI定点のミニ表」が横並びで配置されている。pdf_data()のx座標を使い、
# 2つ目の"保健所"/"管轄"トークンの出現位置を境界として主表のみを抽出する。

fetch_gunma <- function(pdf_url) {
  source_dir <- dirname(sys.frame(1)$ofile %||% "R/hokenjo_fetch/gunma.R")
  if (!exists("pdf_words")) stop("pdf_table_utils.R を先に source してください")

  # 保健所別の主表は通常1ページ目だが、お知らせ文が長い週は2ページ目に
  # ずれることがあるため、「保健所」と「管轄」を両方含むページを探す
  tmp <- tempfile(fileext = ".pdf")
  download.file(pdf_url, tmp, mode = "wb", quiet = TRUE)
  txt <- pdftools::pdf_text(tmp)
  cand <- which(grepl("保健所", txt) & grepl("管轄", txt))
  target_page <- if (length(cand) > 0) cand[1] else 1

  words <- pdf_words(pdf_url, page = target_page)
  rows <- group_words_into_rows(words, y_tol = 3)

  week_line <- Filter(function(r) grepl("20[0-9]{2}年第[0-9]+週", row_text(r)), rows)
  week_label <- if (length(week_line) > 0) {
    regmatches(row_text(week_line[[1]]), regexpr("20[0-9]{2}年第[0-9]+週", row_text(week_line[[1]])))
  } else NA_character_

  regions <- c("県全体", "北毛", "西毛", "中毛", "東毛")

  # ヘッダー行（"保健所"を含む行）の中で、"保健所"の直後から
  # (報告数,定当) が連続して交互に並ぶ個数を数える。
  # 表の右にARIミニ表や脚注・凡例テキストが同じ行に混ざっていても、
  # パターンが崩れた時点で数え終わるため頑健（ブロックごとに疾患数が
  # 異なる＝ARIサブ表の有無や眼科ブロックの列数の違いにも自動対応する）。
  count_diseases_in_header <- function(row_df) {
    toks <- row_df[order(row_df$x), ]
    idx <- which(toks$text == "保健所")[1]
    if (is.na(idx)) return(0L)
    n <- 0L
    k <- idx + 1
    while (k + 1 <= nrow(toks) && toks$text[k] == "報告数" && toks$text[k + 1] == "定当") {
      n <- n + 1L
      k <- k + 2L
    }
    n
  }

  # ヘッダー行を上に遡り、"管轄"を含む行（＝疾病名の行）を探す。
  # ブロックによっては疾病名の行とヘッダー行の間に脚注等の行が
  # 挟まっていることがあるため、直上1行決め打ちにせず数行遡って探す。
  find_disease_row <- function(rows, header_idx, max_back = 5) {
    for (back in 1:max_back) {
      idx <- header_idx - back
      if (idx < 1) break
      r <- rows[[idx]]
      if (any(r$text == "管轄")) return(r)
    }
    NULL
  }

  out <- list()
  i <- 1
  while (i <= length(rows)) {
    row_df <- rows[[i]]
    if (any(row_df$text == "保健所") && any(row_df$text == "報告数")) {
      n_disease <- count_diseases_in_header(row_df)
      disease_row <- find_disease_row(rows, i)
      diseases <- character(0)
      if (!is.null(disease_row) && n_disease > 0) {
        drow <- disease_row[order(disease_row$x), ]
        kanaku_idx <- which(drow$text == "管轄")[1]
        cand <- drow$text[(kanaku_idx + 1):nrow(drow)]
        cand <- cand[!(cand %in% c("管轄", "保健所")) & !grepl("^【", cand)]
        diseases <- utils::head(cand, n_disease)
      }

      # 続く地域行を読む
      j <- i + 1
      while (j <= length(rows) && j <= i + 12) {
        rr <- rows[[j]]
        region_hit <- regions[sapply(regions, function(rg) any(rr$text == rg))]
        if (length(region_hit) == 0) { j <- j + 1; next }
        region <- region_hit[1]
        main_part <- rr[order(rr$x), ]
        toks <- main_part$text[main_part$text != region]
        n <- min(length(diseases), n_disease, floor(length(toks) / 2))
        for (k in seq_len(n)) {
          out[[length(out) + 1]] <- data.frame(
            pref = "群馬県", week_label = week_label, hokenjo = region,
            disease = diseases[k],
            count = parse_hokenjo_number(toks[(k - 1) * 2 + 1]),
            rate  = parse_hokenjo_number(toks[(k - 1) * 2 + 2]),
            stringsAsFactors = FALSE
          )
        }
        j <- j + 1
        if (region == "東毛") break
      }
      i <- j
    } else {
      i <- i + 1
    }
  }
  do.call(rbind, out)
}

`%||%` <- function(a, b) if (is.null(a)) b else a
