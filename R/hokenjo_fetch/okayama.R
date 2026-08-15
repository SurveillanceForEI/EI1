# 岡山県「保健所別報告患者数」PDF（定点把握、週報）
# 例: https://www.pref.okayama.jp/uploaded/life/1050510_10173723_misc.pdf
# 全15ページ中、通常7ページ目に「保健所別報告患者数」表がある（ページはやや前後する場合あり）。
# 列: 全県,岡山市,倉敷市,備前,備中,備北,真庭,美作 の8地域 x (報告数,定点当たり)
# 全県列は7保健所の合計のため除外する。
#
# 実装メモ：
# 表の値は「－」（報告ゼロ・非公表）で埋まっている疾患行があり、桁数が短くなるため
# 固定幅の正規表現（1行あたりの数値トークン数を数える方式）では該当行がまるごと
# スキップされてしまっていた。pdf_words()/group_words_into_rows() で単語の(x,y)座標
# を取得し、ヘッダー行（報告数/定点当の16列）の右端x座標を列アンカーとして、
# 各データ行の単語をその右端座標が最も近い列に割り当てる方式に変更した。
# （PDF内の数値は右詰め表示されるため、単語の右端 x+width は同じ列であれば
#   桁数によらずほぼ一定になる。左端xだけを見ると桁数で位置がずれてしまう。）

fetch_okayama <- function(pdf_url, page = NULL) {
  if (!requireNamespace("pdftools", quietly = TRUE)) stop("pdftools が必要です")

  is_url <- grepl("^https?://", pdf_url)
  path <- pdf_url
  if (is_url) {
    path <- tempfile(fileext = ".pdf")
    download.file(pdf_url, path, mode = "wb", quiet = TRUE)
  }

  txt <- pdftools::pdf_text(path)

  target_page <- page
  if (is.null(target_page)) {
    hit <- which(grepl("保健所別報告患者数", txt))
    target_page <- if (length(hit) > 0) hit[1] else 7
  }
  page_txt <- txt[[target_page]]

  week_label <- NA_character_
  wl <- regmatches(page_txt, regexpr("20[0-9]{2}年\\s*第?[0-9]+週", page_txt))
  if (length(wl) > 0) week_label <- gsub("\\s", "", wl)

  regions_full <- c("全県", "岡山市", "倉敷市", "備前", "備中", "備北", "真庭", "美作")
  regions_use  <- c("岡山市", "倉敷市", "備前", "備中", "備北", "真庭", "美作")

  diseases <- c("インフルエンザ", "COVID-19", "急性呼吸器感染症", "ＲＳウイルス感染症",
                "咽頭結膜熱", "Ａ群溶血性レンサ球菌咽頭炎", "感染性胃腸炎", "水痘",
                "手足口病", "伝染性紅斑", "突発性発しん", "ヘルパンギーナ",
                "流行性耳下腺炎", "急性出血性結膜炎", "流行性角結膜炎",
                "細菌性髄膜炎", "無菌性髄膜炎", "マイコプラズマ肺炎", "クラミジア肺炎",
                "感染性胃腸炎（ロタウイルス）")

  # --- 座標ベースで表を読み直す ---
  words <- pdf_words(path, page = target_page)
  rows <- group_words_into_rows(words, y_tol = 3)
  # 各行を x 昇順に整列（group_words_into_rows は y優先ソートのため x順が崩れる場合がある）
  rows <- lapply(rows, function(r) r[order(r$x), ])

  # ヘッダー行（"報告数"/"定点当" が並ぶ行）を特定し、列アンカー（右端x座標）を作る
  header_row <- NULL
  for (r in rows) {
    if (sum(r$text %in% c("報告数", "定点当")) >= 14) {
      header_row <- r
      break
    }
  }
  if (is.null(header_row)) stop("岡山県: 表ヘッダー行（報告数/定点当）が見つかりません")

  hdr <- header_row[header_row$text %in% c("報告数", "定点当"), ]
  hdr <- hdr[order(hdr$x), ]
  if (nrow(hdr) != length(regions_full) * 2) {
    stop(sprintf("岡山県: ヘッダー列数が想定と異なります（%d列, 期待%d列）",
                  nrow(hdr), length(regions_full) * 2))
  }
  col_edge <- hdr$x + hdr$width          # 各列の右端アンカー
  col_region <- rep(regions_full, each = 2)
  col_type   <- rep(c("count", "rate"), times = length(regions_full))

  # データ行（先頭語が疾患名と一致する行）を抽出
  out <- list()
  for (r in rows) {
    if (nrow(r) == 0) next
    disease <- r$text[1]
    if (!(disease %in% diseases)) next

    data_words <- r[-1, , drop = FALSE]
    data_words <- data_words[data_words$x > 150, , drop = FALSE]  # 疾患名以外
    if (nrow(data_words) == 0) next

    vals <- setNames(rep(NA_character_, nrow(hdr)), NULL)
    for (i in seq_len(nrow(data_words))) {
      w <- data_words[i, ]
      edge <- w$x + w$width
      j <- which.min(abs(edge - col_edge))
      vals[j] <- w$text
    }

    for (i in seq_along(regions_full)) {
      region <- regions_full[i]
      if (!(region %in% regions_use)) next
      count_idx <- which(col_region == region & col_type == "count")
      rate_idx  <- which(col_region == region & col_type == "rate")
      count <- parse_hokenjo_number(vals[count_idx])
      rate  <- parse_hokenjo_number(vals[rate_idx])
      out[[length(out) + 1]] <- data.frame(
        pref = "岡山県", week_label = week_label, hokenjo = region,
        disease = disease, count = count, rate = rate,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, out)
}
