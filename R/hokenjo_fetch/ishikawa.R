# 石川県「2026年第◯週の感染症別保健所別届出数及び週推移表」PDF
# https://www.pref.ishikawa.lg.jp/hokan/kansenjoho/top/patients/documents/{YEAR}-{WEEK}.pdf
#
# 【重要】p.4〜18の疾患別ページには保健所別（金沢市/南加賀/石川中央/
# 能登中部/能登北部）の「報告数」「定点あたり報告数」の数値表があるが、
# これは画像として埋め込まれておりpdftools::pdf_text()ではテキスト
# 抽出できない（表以外の本文は通常のテキストとして抽出可能）。
# ARI（p.3）のみテキストの表（rateのみ、countなし）。
#
# 各ページの表は直近5週間分の推移（報告数/定点あたり報告数を保健所ごと
# に2行、5週分を横に並べた表）になっている。ARIはpdf_text()でそのまま
# 読めるが、他の13疾患は画像のため、pdf_render_page()で高解像度に
# ラスタライズしてtesseract(英語エンジン)でOCRし、罫線の縦仕切り
# （"|"トークン）のx座標から5週分の列位置を検出、疾患名等の行ラベルは
# 保健所の並び順が固定（ISHIKAWA_HOKENJO_ORDER）であることを利用して
# 「報告数行→定点あたり報告数行」のペアを5保健所分、出現順に割り当てる
# ことで読み取る。OCRのため一部の数字が誤認識される可能性がある点に
# 留意（読み取れなかったセルはNAとする）。

ISHIKAWA_HOKENJO_ORDER <- c("金沢市", "南加賀", "石川中央", "能登中部", "能登北部")

# 全20疾患のうち、急性出血性結膜炎・細菌性髄膜炎・無菌性髄膜炎・
# マイコプラズマ肺炎・クラミジア肺炎・感染性胃腸炎(ロタ)の6疾患は
# 「5週連続して患者発生数が0となった場合、当該感染症のページは省略」
# という石川県の方針により、掲載されない週がある。
ISHIKAWA_DISEASE_ORDER <- c(
  "インフルエンザ", "COVID-19", "RSウイルス感染症", "咽頭結膜熱",
  "Ａ群溶血性レンサ球菌咽頭炎", "感染性胃腸炎", "水痘", "手足口病",
  "伝染性紅斑", "突発性発しん", "ヘルパンギーナ", "流行性耳下腺炎",
  "流行性角結膜炎", "急性出血性結膜炎", "細菌性髄膜炎", "無菌性髄膜炎",
  "マイコプラズマ肺炎", "クラミジア肺炎", "感染性胃腸炎(ロタウイルス)"
)

# ARI（p.3）は直近5週間分の定点あたり報告数（rateのみ、countなし）が
# 通常のテキストとして抽出できる
.ishikawa_parse_ari_trend <- function(pdf_txt_page3, year) {
  lines <- strsplit(pdf_txt_page3, "\n")[[1]]
  wk_line <- lines[grepl("[0-9]+週\\s+[0-9]+週", lines)][1]
  if (is.na(wk_line)) return(NULL)
  weeks <- as.integer(regmatches(wk_line, gregexpr("[0-9]+(?=週)", wk_line, perl = TRUE))[[1]])

  out <- list()
  for (h in ISHIKAWA_HOKENJO_ORDER) {
    idx <- grep(paste0("^\\s*", h, "\\s"), lines)
    if (length(idx) == 0) next
    vals <- suppressWarnings(as.numeric(strsplit(trimws(lines[idx[1]]), "\\s+")[[1]][-1]))
    vals <- utils::tail(vals, length(weeks))
    for (w in seq_along(weeks)) {
      out[[length(out) + 1]] <- data.frame(
        pref = "石川県", week_label = sprintf("%d年第%d週", year, weeks[w]),
        week_num = weeks[w], hokenjo = h, disease = "急性呼吸器感染症(ARI)",
        count = NA_real_, rate = vals[w], stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, out)
}

# 画像埋め込みの疾患別ページ（保健所別×5週間の報告数/定点あたり報告数）
# をOCRで読み取る
.ishikawa_detect_col_centers <- function(d) {
  # 縦罫線("|")のx位置から5週分の列境界を検出。本文以外のグラフ領域
  # (y>1700程度)の罫線は除外する
  bars <- d[d$word == "|" & d$conf > 40 & d$yc < 1000, ]
  if (nrow(bars) < 4) return(NULL)
  bar_x <- sort(unique(round(bars$xc / 20) * 20))
  # 罫線が近接している場合はまとめる
  groups <- split(bar_x, cumsum(c(1, diff(bar_x) > 60)))
  bar_x <- sapply(groups, mean)
  if (length(bar_x) < 4) return(NULL)
  bar_x <- sort(bar_x)[seq_len(min(5, length(bar_x)))]  # 表左端〜4本の仕切り線で5列 (念のため5本目も許容)
  col_centers <- (utils::head(bar_x, -1) + utils::tail(bar_x, -1)) / 2
  if (length(col_centers) < 4) return(NULL)
  # 列は5週分だが検出できる仕切りは4本（境界）のことが多いため、
  # 不足分は等間隔で外挿する
  while (length(col_centers) < 5) {
    gap <- diff(utils::tail(col_centers, 2))
    col_centers <- c(col_centers, utils::tail(col_centers, 1) + gap)
  }
  col_centers[1:5]
}

# 画像埋め込みの疾患別ページ（保健所別×5週間の報告数/定点あたり報告数）
# をOCRで読み取る。col_centers を渡した場合はページごとの罫線検出を
# スキップする（同一PDF内は全疾患ページで列レイアウトが共通なため、
# 罫線検出が失敗しやすい号でも1ページ分の検出結果を使い回せる）
.ishikawa_ocr_disease_page <- function(png_path, disease, year, eng, col_centers = NULL) {
  d <- tesseract::ocr_data(png_path, engine = eng)
  bb <- do.call(rbind, strsplit(d$bbox, ","))
  d$x1 <- as.numeric(bb[, 1]); d$y1 <- as.numeric(bb[, 2])
  d$x2 <- as.numeric(bb[, 3]); d$y2 <- as.numeric(bb[, 4])
  d$xc <- (d$x1 + d$x2) / 2; d$yc <- (d$y1 + d$y2) / 2
  d$conf <- as.numeric(d$confidence)

  # OCRが小数点を読み落とし、"2.64"が"2"と"64"のように2トークンに
  # 分裂して認識されることがある。同じ行(yc近接)かつxが近接する
  # 整数1桁+整数2桁のペアは1つの小数として結合し直す
  d <- d[order(d$yc, d$xc), ]
  merge_idx <- rep(FALSE, nrow(d))
  for (i in seq_len(nrow(d) - 1)) {
    if (merge_idx[i]) next
    if (grepl("^[0-9]$", d$word[i]) && grepl("^[0-9]{2}$", d$word[i + 1]) &&
        abs(d$yc[i] - d$yc[i + 1]) < 15 && (d$xc[i + 1] - d$xc[i]) < 80 && (d$xc[i + 1] - d$xc[i]) > 0) {
      d$word[i] <- paste0(d$word[i], ".", d$word[i + 1])
      d$xc[i] <- (d$xc[i] + d$xc[i + 1]) / 2
      merge_idx[i + 1] <- TRUE
    }
  }
  d <- d[!merge_idx, ]

  if (is.null(col_centers)) col_centers <- .ishikawa_detect_col_centers(d)
  if (is.null(col_centers) || length(col_centers) != 5 || anyNA(col_centers)) return(NULL)

  is_int <- grepl("^[0-9]+$", d$word)
  is_dec <- grepl("^[0-9]+\\.[0-9]+$", d$word)
  # ヘッダー行(週番号)や列見出し（報告数/定点あたり報告数の凡例文字が
  # 誤認識されたノイズ）を除外するため、ヘッダー行よりさらに下
  # (yc>650)かつグラフ領域より上(yc<1700)に限定する
  nums <- d[(is_int | is_dec) & d$conf > 15 & d$yc > 650 & d$yc < 1700, ]
  if (nrow(nums) == 0) return(NULL)
  # y方向にクラスタリングして行を作る
  nums <- nums[order(nums$yc), ]
  row_id <- cumsum(c(1, diff(nums$yc) > 25))
  nums$row <- row_id

  row_summary <- lapply(split(nums, nums$row), function(rr) {
    list(y = mean(rr$yc), is_dec = mean(grepl("^[0-9]+\\.[0-9]+$", rr$word)) > 0.5, rows_df = rr)
  })
  ys <- sapply(row_summary, function(r) r$y)
  is_dec_row <- sapply(row_summary, function(r) r$is_dec)
  ord <- order(ys)
  row_summary <- row_summary[ord]; is_dec_row <- is_dec_row[ord]; ys <- ys[ord]

  # 表は「保健所ごとに 報告数行→定点あたり報告数行」の10行(5保健所×2)が
  # 均等な間隔で並ぶ。「－」のみの行はOCRトークンが無く行ごと消失する
  # ため、出現順に読み進めるのではなく、行間隔から10行分の格子(スロット)
  # を推定し、検出できた行を最も近いスロットへ割り当てることで
  # 欠落行があってもズレないようにする。
  n_row <- length(ys)
  match_col <- function(x) which.min(abs(col_centers - x))

  cnt_vals_all <- matrix(NA_real_, nrow = 5, ncol = 5)   # [hokenjo, week]
  rate_vals_all <- matrix(NA_real_, nrow = 5, ncol = 5)
  rate_no_decimal <- matrix(FALSE, nrow = 5, ncol = 5)

  if (n_row >= 2) {
    gaps <- diff(ys)
    unit_h <- stats::median(gaps[gaps < 1.5 * min(gaps)])
    if (!is.finite(unit_h) || unit_h <= 0) unit_h <- min(gaps)
    slot <- round((ys - ys[1]) / unit_h)
    slot <- slot - min(slot)  # 先頭行が欠落している可能性はここでは考慮しない
    for (i in seq_len(n_row)) {
      s <- slot[i]
      if (s < 0 || s > 9) next
      hj_idx <- floor(s / 2) + 1
      if (hj_idx > 5) next
      # 表内の行順は「定点あたり報告数行→報告数行」（先に率、後に実数）
      is_rate_slot <- (s %% 2) == 0
      rr <- row_summary[[i]]$rows_df
      for (k in seq_len(nrow(rr))) {
        ci <- match_col(rr$xc[k])
        val <- suppressWarnings(as.numeric(rr$word[k]))
        # 小数点付きトークンとして認識されなかった（＝単なる整数として
        # 読まれた）「定点あたり報告数」は、OCRが先頭の「0.」を読み
        # 落として小数第2位までの値がそのまま整数として出力された誤読
        # （例:「0.73」→「73」）の可能性があるため印を付けておき、
        # 同じ疾患の他4保健所と比較して明らかな外れ値の場合のみ後段で
        # NA化する（ARIのように定点あたり報告数が元々2桁になる疾患も
        # あるため、値そのものの大小だけでは判定できない）
        if (is_rate_slot) {
          rate_vals_all[hj_idx, ci] <- val
          if (!is.na(val) && !grepl("\\.", rr$word[k])) rate_no_decimal[hj_idx, ci] <- TRUE
        } else {
          cnt_vals_all[hj_idx, ci] <- val
        }
      }
    }
  }

  # 小数点なしで読まれた「定点あたり報告数」のうち、同じ疾患・同じ週の
  # 他の保健所の値と比べて明らかな外れ値（中央値の5倍超）になっている
  # ものは、OCRが先頭の「0.」を読み落とした誤読とみなしNA化する
  for (ci in 1:5) {
    col_vals <- rate_vals_all[, ci]
    flagged <- rate_no_decimal[, ci] & !is.na(col_vals)
    if (!any(flagged)) next
    others <- col_vals[!flagged & !is.na(col_vals)]
    if (length(others) < 2) next
    med_other <- stats::median(others)
    if (med_other <= 0) next
    bad <- flagged & !is.na(col_vals) & col_vals > med_other * 5
    rate_vals_all[bad, ci] <- NA_real_
  }

  out <- list()
  for (hi in seq_along(ISHIKAWA_HOKENJO_ORDER)) {
    for (w in 1:5) {
      out[[length(out) + 1]] <- data.frame(
        pref = "石川県", week_label = NA_character_, week_num = NA_integer_,
        hokenjo = ISHIKAWA_HOKENJO_ORDER[hi], disease = disease, week_col = w,
        count = cnt_vals_all[hi, w], rate = rate_vals_all[hi, w], stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, out)
}

# PDF1件から、そのPDFに掲載されている直近5週間分の全疾患データを取得する
fetch_ishikawa_history <- function(pdf_url, year = 2026) {
  if (!requireNamespace("pdftools", quietly = TRUE)) stop("pdftools パッケージが必要です")
  if (!requireNamespace("tesseract", quietly = TRUE)) stop("tesseract パッケージが必要です")

  tmp <- tempfile(fileext = ".pdf")
  download.file(pdf_url, tmp, mode = "wb", quiet = TRUE)
  txt <- pdftools::pdf_text(tmp)

  ari_page <- which(grepl("ARI\\s*発生状況|ARI.*保健所別.*定点あたり", txt))[1]
  if (is.na(ari_page)) ari_page <- 3
  ari <- tryCatch(.ishikawa_parse_ari_trend(txt[ari_page], year), error = function(e) NULL)

  # 週番号→week_labelの対応はARI表から取得する（画像ページには週番号の
  # テキストがないため）
  weeks5 <- if (!is.null(ari)) sort(unique(ari$week_num)) else NULL

  # 疾患別ページ(画像)の位置: ページ番号以外にほぼテキストが無い
  # （画像として埋め込まれているため）ページが連続する範囲を探す。
  # ARIページの直後には年齢階級別グラフ等のテキストページが挟まる
  # ことがあるため、ari_pageの次から順に見るのではなく、条件に合う
  # 最初の連続ブロックを探す
  is_blank_ish <- sapply(txt, function(p) nchar(trimws(gsub("[0-9]+", "", p))) < 3)
  candidates <- which(is_blank_ish)
  candidates <- candidates[candidates > ari_page]
  disease_pages <- integer(0)
  if (length(candidates) > 0) {
    grp <- cumsum(c(1, diff(candidates) > 1))
    disease_pages <- candidates[grp == grp[1]]
  }

  eng <- tesseract::tesseract("eng")

  # 罫線検出は号によって一部ページで失敗しやすい（OCRのレイアウト解析が
  # 崩れるページがある）ため、同一PDF内で列レイアウトが共通であることを
  # 利用し、最初に検出に成功したページの列中心を全ページで使い回す
  shared_col_centers <- NULL
  render_cache <- new.env(parent = emptyenv())
  get_png <- function(p) {
    key <- as.character(p)
    if (!is.null(render_cache[[key]])) return(render_cache[[key]])
    png_path <- tempfile(fileext = ".png")
    bmp <- tryCatch(pdftools::pdf_render_page(tmp, page = p, dpi = 300), error = function(e) NULL)
    if (is.null(bmp)) return(NULL)
    png::writePNG(bmp, png_path)
    render_cache[[key]] <- png_path
    png_path
  }
  for (p in disease_pages) {
    png_path <- get_png(p)
    if (is.null(png_path)) next
    d0 <- tryCatch(tesseract::ocr_data(png_path, engine = eng), error = function(e) NULL)
    if (is.null(d0)) next
    bb <- do.call(rbind, strsplit(d0$bbox, ","))
    d0$xc <- (as.numeric(bb[, 1]) + as.numeric(bb[, 3])) / 2
    d0$yc <- (as.numeric(bb[, 2]) + as.numeric(bb[, 4])) / 2
    d0$conf <- as.numeric(d0$confidence)
    cc <- .ishikawa_detect_col_centers(d0)
    if (!is.null(cc) && length(cc) == 5 && !anyNA(cc)) { shared_col_centers <- cc; break }
  }

  out <- list(ari)
  for (i in seq_along(disease_pages)) {
    if (i > length(ISHIKAWA_DISEASE_ORDER)) break
    p <- disease_pages[i]
    disease <- ISHIKAWA_DISEASE_ORDER[i]
    png_path <- get_png(p)
    if (is.null(png_path)) next
    res <- tryCatch(.ishikawa_ocr_disease_page(png_path, disease, year, eng, col_centers = shared_col_centers),
                     error = function(e) { message("[NG] ", disease, ": ", conditionMessage(e)); NULL })
    if (is.null(res) || is.null(weeks5)) next
    # week_col(1..5、表内の左から何番目か)を実際の週番号に変換
    if (length(weeks5) == 5) {
      res$week_num <- weeks5[res$week_col]
      res$week_label <- sprintf("%d年第%d週", year, res$week_num)
      res$week_col <- NULL
      out[[length(out) + 1]] <- res
    }
  }
  for (path in mget(ls(render_cache), envir = render_cache)) unlink(path)
  do.call(rbind, out)
}

# 2026年第32週データ（画像から目視抽出、出典: 2026-32.pdf p.3-18）
# fetch_ishikawa_history()による自動取得の検証用に残している
.ISHIKAWA_WEEK32_DATA <- list(
  "急性呼吸器感染症(ARI)" = list(rate = c(45.56, 52.50, 58.45, 51.83, 13.00), count = rep(NA_real_, 5)),
  "インフルエンザ"        = list(count = c(1, 0, 0, 0, 0),   rate = c(0.06, 0.00, 0.00, 0.00, 0.00)),
  "COVID-19"               = list(count = c(28, 12, 29, 20, 1), rate = c(1.75, 1.20, 2.64, 3.33, 0.25)),
  "RSウイルス感染症"      = list(count = c(12, 24, 3, 0, 0),   rate = c(1.20, 4.00, 0.50, 0.00, 0.00)),
  "咽頭結膜熱"            = list(count = c(1, 0, 3, 1, 0),     rate = c(0.10, 0.00, 0.50, 0.25, 0.00)),
  "Ａ群溶血性レンサ球菌咽頭炎" = list(count = c(9, 17, 5, 8, 0), rate = c(0.90, 2.83, 0.83, 2.00, 0.00)),
  "感染性胃腸炎"          = list(count = c(69, 24, 88, 16, 0), rate = c(6.90, 4.00, 14.67, 4.00, 0.00)),
  "水痘"                  = list(count = c(0, 0, 2, 1, 0),     rate = c(0.00, 0.00, 0.33, 0.25, 0.00)),
  "手足口病"              = list(count = c(13, 2, 5, 6, 0),    rate = c(1.30, 0.33, 0.83, 1.50, 0.00)),
  "伝染性紅斑"            = list(count = c(0, 0, 0, 0, 0),     rate = c(0.00, 0.00, 0.00, 0.00, 0.00)),
  "突発性発しん"          = list(count = c(2, 2, 3, 1, 1),     rate = c(0.20, 0.33, 0.50, 0.25, 0.50)),
  "ヘルパンギーナ"        = list(count = c(6, 11, 1, 2, 1),    rate = c(0.60, 1.83, 0.17, 0.50, 0.50)),
  "流行性耳下腺炎"        = list(count = c(0, 0, 0, 0, 0),     rate = c(0.00, 0.00, 0.00, 0.00, 0.00)),
  "流行性角結膜炎"        = list(count = c(9, 1, 3, 0, 0),     rate = c(3.00, 1.00, 3.00, 0.00, 0.00))
  # 急性出血性結膜炎・細菌性髄膜炎・無菌性髄膜炎・マイコプラズマ肺炎・
  # クラミジア肺炎・感染性胃腸炎(ロタ)：5週連続報告0のためページ省略、データなし
)

fetch_ishikawa <- function(week_label = "2026年第32週") {
  out <- list()
  for (disease in names(.ISHIKAWA_WEEK32_DATA)) {
    d <- .ISHIKAWA_WEEK32_DATA[[disease]]
    for (i in seq_along(ISHIKAWA_HOKENJO_ORDER)) {
      out[[length(out) + 1]] <- data.frame(
        pref = "石川県", week_label = week_label,
        hokenjo = ISHIKAWA_HOKENJO_ORDER[i], disease = disease,
        count = d$count[i], rate = d$rate[i],
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, out)
}
