# 岐阜県「感染症発生動向調査週報（GIDWR） データ・グラフ編」PDF
# https://www.pref.gifu.lg.jp/uploaded/attachment/<id>.pdf
# （URLは週ごとに変わる添付ファイルIDのため、呼び出し側で最新PDFのURLを解決すること）
#
# レイアウト: 1ページに疾患ブロックが2列×2段（最大4疾患）で並び、各ブロックは
# 「◆ 疾患名」見出し→「定点当たり報告数」→「保健所別」表（県全体+岐阜市/岐阜/
# 西濃/関/可茂/東濃/恵那/飛騨の3週間分）という構成。count（報告数）は掲載されて
# おらず rate（定点当たり報告数）のみ。「医療圏別」（岐阜/西濃/中濃/東濃/飛騨の
# 5区分）表は保健所境界と一致しないため対象外とする。
# ブロックが横に2つ並ぶため pdf_text() の単純な行分割では左右の数値が同一行に
# 混在してしまう。pdf_data() の座標(x,y)を使い、◆マーカーごとにブロックのx範囲を
# 決定してから抽出する。

fetch_gifu <- function(pdf_url = "https://www.pref.gifu.lg.jp/uploaded/attachment/509493.pdf") {
  if (!exists("pdf_words")) stop("pdf_table_utils.R を先に source してください")

  tmp <- tempfile(fileext = ".pdf")
  download.file(pdf_url, tmp, mode = "wb", quiet = TRUE)
  pages_txt <- pdftools::pdf_text(tmp)
  npages <- length(pages_txt)

  hokenjo_order <- c("岐阜市", "岐阜", "西濃", "関", "可茂", "東濃", "恵那", "飛騨")

  # 週ラベル（ページ1のタイトルから取得）
  wm <- regmatches(pages_txt[1], regexec("(20[0-9]{2})年第([0-9]+)週", pages_txt[1]))[[1]]
  week_label <- if (length(wm) == 3) sprintf("%s年第%s週", wm[2], wm[3]) else NA_character_

  out <- list()

  for (page in seq_len(npages)) {
    if (!grepl("保健所別", pages_txt[page])) next
    words <- pdf_words(tmp, page = page)

    diamonds <- words[words$text == "◆", ]
    if (nrow(diamonds) == 0) next

    for (di in seq_len(nrow(diamonds))) {
      x0 <- diamonds$x[di]; y0 <- diamonds$y[di]
      others_right <- diamonds$x[abs(diamonds$y - y0) <= 3 & diamonds$x > x0]
      block_x_max <- if (length(others_right) > 0) min(others_right) - 5 else x0 + 300

      # 疾患名（◆と同じy、xが◆より右）
      name_toks <- words[abs(words$y - y0) <= 3 & words$x > x0 & words$x < block_x_max & words$text != "◆", ]
      disease <- paste(name_toks$text[order(name_toks$x)], collapse = "")
      if (!nzchar(disease)) next

      # 週ラベル行（"29週","30週","31週"等）で最新（最大週）を採用
      wk <- words[grepl("^[0-9]+週$", words$text) & words$x >= x0 - 5 & words$x < x0 + 30 & words$y > y0, ]
      if (nrow(wk) == 0) next
      wk$num <- as.integer(gsub("週$", "", wk$text))
      target <- wk[which.max(wk$num), ]
      y_target <- target$y

      # 保健所ヘッダー（8保健所名）をこのブロックのx範囲内・yがdiamondより下・週行より上で探す
      hdr <- words[words$text %in% hokenjo_order & words$x >= x0 & words$x < block_x_max &
                     words$y > y0 & words$y < y_target, ]
      hdr <- hdr[!duplicated(hdr$text), ]
      if (nrow(hdr) != length(hokenjo_order)) next  # 医療圏別など列構成が違うブロックはスキップ
      hdr <- hdr[match(hokenjo_order, hdr$text), ]
      xs <- hdr$x
      bounds <- c(xs[1] - (xs[2] - xs[1]) / 2, (xs[1:(length(xs) - 1)] + xs[2:length(xs)]) / 2,
                  xs[length(xs)] + (xs[length(xs)] - xs[length(xs) - 1]) / 2)

      # 「-」（数値なし＝0）は文字幅が狭く、列の境界線ちょうど付近に
      # 描画されることがあり、上記boundsによる半開区間判定だと隣の列に
      # 誤って割り当てられる（岐阜県で実際に発生：西濃列の「-」が
      # 境界値と一致し関列に混入し、本来の関の値を上書きしていた）。
      # 保健所別の値は常に8列ぶん（「-」も1トークンとして）揃うため、
      # block内のx範囲でトークンをx昇順に並べ、8個ちょうど揃えば
      # 位置ではなく出現順で直接対応させる方が確実
      # 下限をbounds[1]にするのは、◆直下のブロック内には保健所別8列の
      # 手前に「県全体」の値が1つあり（x0側に近い位置）、これを取り込むと
      # 9個になって位置対応がずれるため。上限はboundsの右端(bounds[9])だと
      # 一番右の列（飛騨）の値がわずかに外側にはみ出て漏れることがあるため、
      # ブロックの右端(block_x_max)まで広げる
      data_toks <- words[abs(words$y - y_target) <= 3 & words$x >= bounds[1] & words$x < block_x_max, ]
      data_toks <- data_toks[order(data_toks$x), ]

      if (nrow(data_toks) == length(hokenjo_order)) {
        for (k in seq_along(hokenjo_order)) {
          rate_val <- parse_hokenjo_number(data_toks$text[k])
          out[[length(out) + 1]] <- data.frame(
            pref = "岐阜県", week_label = week_label, hokenjo = hokenjo_order[k],
            disease = disease, count = NA_real_, rate = rate_val,
            stringsAsFactors = FALSE
          )
        }
      } else if (nrow(data_toks) > 0) {
        # トークン数が8個に揃わない想定外のレイアウトの場合のみ、
        # 従来通り列境界での割り当てにフォールバックする
        for (k in seq_along(hokenjo_order)) {
          lo <- bounds[k]; hi <- bounds[k + 1]
          v <- data_toks$text[data_toks$x >= lo & data_toks$x < hi]
          rate_val <- if (length(v) > 0) parse_hokenjo_number(v[1]) else NA_real_
          out[[length(out) + 1]] <- data.frame(
            pref = "岐阜県", week_label = week_label, hokenjo = hokenjo_order[k],
            disease = disease, count = NA_real_, rate = rate_val,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  do.call(rbind, out)
}

# 岐阜県の週報バックナンバーページは表構成が「週・月|情報編|
# データ・グラフ編（週報）（月報）|トピックス」で、リンクテキストが
# 「[PDFファイル／xxxKB]」のみ（週番号を含まない）ため、汎用の
# resolve_hokenjo_pdf_url()（リンクテキストのパターンマッチ）では
# 拾えない。当年分の表（最新週が先頭行、"New!"付き）を直接たどり、
# 各行の3列目（データ・グラフ編＝保健所別表付き）のリンクを取得する
resolve_gifu_data_url <- function() {
  if (!requireNamespace("rvest", quietly = TRUE)) stop("rvest パッケージが必要です")
  landing <- "https://www.pref.gifu.lg.jp/page/107799.html"
  doc <- rvest::read_html(landing, encoding = "UTF-8")
  tbls <- rvest::html_elements(doc, "table")
  for (tbl in tbls) {
    rows <- rvest::html_elements(tbl, "tr")
    for (r in rows) {
      week_txt <- rvest::html_text(rvest::html_elements(r, "td")[1])
      if (length(week_txt) == 0) next
      if (!grepl("New!|New！", week_txt)) next
      cells <- rvest::html_elements(r, "td")
      if (length(cells) < 3) next
      a3 <- rvest::html_elements(cells[[3]], "a")
      if (length(a3) == 0) next
      return(xml2::url_absolute(rvest::html_attr(a3[[1]], "href"), landing))
    }
  }
  stop("岐阜県: 最新週（New!マーク付き）のリンクが見つかりません")
}
