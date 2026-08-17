# 愛媛県「定点把握五類感染症」PDF（週報、2ページ）
# 例: https://www.pref.ehime.jp/uploaded/attachment/187603.pdf
# レイアウトが特殊で、疾患名が「1列に1文字ずつ縦書き」の見出しとして各列の上部に
# 配置され、保健所（行）×疾患（列）の交差点に数値がある。■患者報告数、
# ■定点当たり報告数 の2ブロックがページごとにあり、それぞれ座標ベースで
# 列（疾患）のx中心・行（保健所）のy中心を推定し、最も近いセルに数値を割り当てる。
# 保健所: 四国中央,西条,今治,松山市,中予,八幡浜,宇和島

fetch_ehime <- function(pdf_url) {
  if (!requireNamespace("pdftools", quietly = TRUE)) stop("pdftools が必要です")
  path <- pdf_url
  if (grepl("^https?://", pdf_url)) {
    path <- tempfile(fileext = ".pdf")
    download.file(pdf_url, path, mode = "wb", quiet = TRUE)
  }
  if (!exists("pdf_words")) stop("pdf_table_utils.R を先に source してください")

  hokenjo_order <- c("四国中央", "西条", "今治", "松山市", "中予", "八幡浜", "宇和島")

  parse_page <- function(page_no) {
    w <- pdf_words(path, page = page_no)
    w <- w[order(w$y, w$x), ]

    week_label <- NA_character_
    yr <- w$text[grepl("^20[0-9]{2}年$", w$text)]
    wk <- w$text[grepl("^第?[0-9]+$", w$text) & w$y < 40]
    # week_label はページ全文から正規表現抽出する方が確実
    full_txt <- paste(w$text, collapse = "")

    markers <- w[w$text %in% c("■患者報告数", "■定点当たり報告数"), c("text", "y")]
    markers <- markers[order(markers$y), ]
    if (nrow(markers) < 2) return(NULL)
    y_count_top <- markers$y[markers$text == "■患者報告数"][1]
    y_rate_top  <- markers$y[markers$text == "■定点当たり報告数"][1]

    # --- 列（疾患）のx中心をヘッダー領域（y方向）から推定 ---
    # 見出しは1文字ずつ縦書きで、以下の3パターンがある:
    #   (a) 通常の1列縦書き（例: 手足口病）
    #   (b) 疾患名が長く1列に収まらず、隣の列（xがより大きい＝右側）に
    #       折り返して続く2列構成（例: 新型コロナウイルス感染症 → 右列に
    #       「新型コロナウイルス」、左列に「感染症」。縦書きは右→左に読むため
    #       右（x大）の列を先に読む）
    #   (c) 括弧書きなどの短い注記が主要列の隣にx方向に少しずれて配置される
    #       （例: インフルエンザ（入院）の「（」「）」。この注記列は主要列の
    #       途中/下部からしか始まらず文字数も少ない）
    # (b)と(c)はどちらも隣接列とのxギャップが小さい点は共通だが、(b)は両列と
    # も見出し先頭(y方向)から始まり文字数が多い「主要列」同士である点で、
    # (c)の「主要列＋短い注記列」と区別できる。
    # 【注意】本ロジックはこのPDF（週報）の実測レイアウトに基づく閾値
    # （列間ギャップ14pt、主要列とみなす最小文字数3）を使っている。将来週で
    # 列間隔やフォントサイズが変わった場合はこれらの定数の再調整が必要。
    hdr <- w[w$y > 65 & w$y < y_count_top & w$x > 100 & w$x < 700 &
               nchar(w$text) == 1 & !grepl("[0-9]", w$text), ]
    hdr <- hdr[order(hdr$x), ]
    if (nrow(hdr) == 0) return(NULL)
    ux <- sort(unique(hdr$x))
    gap_threshold <- 14  # これより大きいxギャップは別の疾患列とみなす
    grp <- cumsum(c(1, diff(ux) > gap_threshold))
    col_map <- data.frame(x = ux, grp = grp)
    hdr <- merge(hdr, col_map, by = "x")

    # 1クラスタ（近接するx位置の集合）内の文字列を正しい読み順で連結する
    build_group_name <- function(d) {
      sub_x <- sort(unique(d$x))
      counts <- sapply(sub_x, function(xx) sum(d$x == xx))
      main_min_chars <- 3
      mains <- sub_x[counts >= main_min_chars]
      if (length(mains) == 0) mains <- sub_x  # 単独の短い列はそのままmain扱い
      others <- setdiff(sub_x, mains)
      attach <- sapply(others, function(xx) mains[which.min(abs(mains - xx))])
      parts <- character(0)
      for (mx in sort(mains, decreasing = TRUE)) {  # 右の列（x大）から先に読む
        xs <- c(mx, others[attach == mx])
        sub <- d[d$x %in% xs, ]
        sub <- sub[order(sub$y, sub$x), ]  # 同一論理列内は上から下、注記はx昇順
        parts <- c(parts, paste(sub$text, collapse = ""))
      }
      paste(parts, collapse = "")
    }

    cols <- do.call(rbind, lapply(split(hdr, hdr$grp), function(d) {
      data.frame(x_center = mean(d$x), name = build_group_name(d), stringsAsFactors = FALSE)
    }))
    cols$name <- gsub("[*]", "", cols$name)
    cols <- cols[nchar(cols$name) >= 2, ]
    # 「迅速検査Ａ型/Ｂ型」列はインフルエンザ患者のうち迅速検査結果が
    # 判明したものの内訳（脚注に "*インフルエンザ患者のうち..." とあり、
    # 独立した疾患ではない）、「(入院)」列は基幹定点による入院ベースの
    # 別統計（通常の定点報告とは異なる指標で、画面のプルダウンにも無い）
    # のため、いずれも疾患として扱わず除外する。縦書きの「︵入院︶」
    # （通常の括弧ではなく縦書き用の異体字）が列クラスタリングの誤りで
    # 別の列名に混入し「新型コロナウイルス感染症迅速検査Ｂ型」のような
    # 結合ミス名が生じることがあるため、「入院」「迅速検査」いずれかを
    # 含む列名は丸ごと除外する
    cols <- cols[!grepl("入院|迅速検査|︵|︶", cols$name), ]

    # --- 行（保健所）のy中心を左端ラベルから推定（患者報告数/定点当たり報告数の各ブロックで別々に） ---
    y_rate_bottom0 <- max(w$y[w$y > y_rate_top]) + 5
    get_row_y <- function(y_lo, y_hi) {
      lbl <- w[w$x > 55 & w$x < 120 & w$y > y_lo & w$y < y_hi & !grepl("[0-9]", w$text), ]
      lbl <- lbl[order(lbl$y), ]
      if (nrow(lbl) == 0) return(numeric(0))
      uy <- sort(unique(lbl$y))
      grpy <- cumsum(c(1, diff(uy) > 6))
      row_map <- data.frame(y = uy, grpy = grpy)
      lbl <- merge(lbl, row_map, by = "y")
      ry <- sort(sapply(split(lbl, lbl$grpy), function(d) mean(d$y)))
      ry[seq_len(min(length(ry), length(hokenjo_order)))]  # 愛媛県計は除く
    }
    row_y_count <- get_row_y(y_count_top - 5, y_rate_top - 5)
    row_y_rate  <- get_row_y(y_rate_top - 5, y_rate_bottom0)

    assign_section <- function(y_lo, y_hi, row_y) {
      dat <- w[w$y > y_lo & w$y < y_hi & w$x > 100 & w$x < 500 &
                 (grepl("^[0-9.]+$", w$text) | w$text == "-"), ]
      out <- list()
      for (ri in seq_along(row_y)) {
        ry <- row_y[ri]
        drow <- dat[abs(dat$y - ry) < 6, ]
        for (ci in seq_len(nrow(cols))) {
          cx <- cols$x_center[ci]
          dcell <- drow[abs(drow$x - cx) < 12, ]
          val <- if (nrow(dcell) > 0) dcell$text[1] else NA_character_
          out[[length(out) + 1]] <- data.frame(
            hokenjo = hokenjo_order[ri], disease = cols$name[ci],
            value = val, stringsAsFactors = FALSE
          )
        }
      }
      do.call(rbind, out)
    }

    counts <- assign_section(y_count_top + 3, y_rate_top - 3, row_y_count)
    rates  <- assign_section(y_rate_top + 3, y_rate_bottom0, row_y_rate)

    merged <- merge(counts, rates, by = c("hokenjo", "disease"), suffixes = c("_count", "_rate"))
    merged$count <- parse_hokenjo_number(merged$value_count)
    merged$rate  <- parse_hokenjo_number(merged$value_rate)
    merged$value_count <- NULL
    merged$value_rate <- NULL
    merged$week_label <- week_label
    merged
  }

  p1 <- tryCatch(parse_page(1), error = function(e) NULL)
  p2 <- tryCatch(parse_page(2), error = function(e) NULL)
  all_df <- do.call(rbind, Filter(Negate(is.null), list(p1, p2)))
  if (is.null(all_df) || nrow(all_df) == 0) {
    return(data.frame(pref = character(0), week_label = character(0), hokenjo = character(0),
                       disease = character(0), count = numeric(0), rate = numeric(0)))
  }

  # 週ラベルはページ全体テキストから取得
  txt <- pdftools::pdf_text(path)
  # 「〜」(波ダッシュ U+301C) と「～」(全角チルダ U+FF5E) の両方に対応する
  wl <- regmatches(txt[[1]], regexpr("第\\s*[0-9]+\\s*週\\s*[\\(（][0-9.]+[～~〜][0-9.]+[\\)）]", txt[[1]]))
  week_label <- if (length(wl) > 0) gsub("\\s", "", wl) else NA_character_

  result <- data.frame(
    pref = "愛媛県", week_label = week_label, hokenjo = all_df$hokenjo,
    disease = all_df$disease, count = all_df$count, rate = all_df$rate,
    stringsAsFactors = FALSE
  )
  # 注: 以前は「値が全てNAの疾患列を除去」する後処理があったが、これは
  # 列クラスタリングの誤分裂で生じる空きダミー列を消すための応急処置だった。
  # 列名復元ロジックを修正した現在は、全保健所で報告数0（＝全て"-"→NA）の
  # 疾患も正しいデータとして残すべきなのでこの除去処理は行わない。
  result
}
