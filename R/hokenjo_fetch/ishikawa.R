source("R/hokenjo_fetch/ishikawa_glyph.R")

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
.ishikawa_ocr_disease_page <- function(png_path, disease, year, eng = NULL, col_centers = NULL) {
  # 表は「石川県(県全体)→金沢市→南加賀→石川中央→能登中部→能登北部」の6行×直近5週。
  # 低解像度の画像表なので、tesseractではなく固定フォントのテンプレート照合で読む
  # （詳細は R/hokenjo_fetch/ishikawa_glyph.R）。県全体の行は保健所別データではないため捨てる。
  g <- png::readPNG(png_path)
  if (length(dim(g)) == 3) g <- g[, , 1]
  pg <- isk_read_page(g)
  cnt <- pg$count[2:6, , drop = FALSE]; rate <- pg$rate[2:6, , drop = FALSE]

  # 報告数と定点あたり報告数の整合（報告数 = 定点あたり × 定点数）による補正。
  # 定点数は保健所×疾患ごとに一定なので、5週のうち両方読めた週の 報告数/定点あたり の
  # 最頻値から推定する。定点あたり(太字)の方が読み取りが安定しているため、不一致なら
  # 定点あたりを優先して報告数を直し、片方しか読めなければもう片方を補う。
  for (e in 1:5) {
    both <- !is.na(cnt[e, ]) & !is.na(rate[e, ]) & cnt[e, ] > 0 & rate[e, ] > 0
    n_site <- NA_real_
    if (any(both)) {
      cand <- round(cnt[e, both] / rate[e, both])
      n_site <- as.numeric(names(sort(table(cand), decreasing = TRUE))[1])
    }
    for (w in 1:5) {
      if (!is.na(rate[e, w]) && rate[e, w] == 0) { cnt[e, w] <- 0; next }
      if (!is.na(cnt[e, w]) && cnt[e, w] == 0 && is.na(rate[e, w])) { rate[e, w] <- 0; next }
      if (is.na(n_site)) next
      if (!is.na(rate[e, w])) {
        est <- round(rate[e, w] * n_site)
        if (is.na(cnt[e, w]) || cnt[e, w] != est) cnt[e, w] <- est
      } else if (!is.na(cnt[e, w])) {
        rate[e, w] <- round(cnt[e, w] / n_site, 2)
      }
    }
  }

  out <- list()
  for (hi in seq_along(ISHIKAWA_HOKENJO_ORDER)) {
    for (w in 1:5) {
      out[[length(out) + 1]] <- data.frame(
        pref = "石川県", week_label = NA_character_, week_num = NA_integer_,
        hokenjo = ISHIKAWA_HOKENJO_ORDER[hi], disease = disease, week_col = w,
        count = cnt[hi, w], rate = rate[hi, w], stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, out)
}

# 各疾患ページのタイトルをjpn OCRで読み、ISHIKAWA_DISEASE_ORDERの部分列に対応づける。
# OCRはかなり崩れる（例:「流行性耳下腺炎」→「之行性耳下肛炎」）ため、完全一致ではなく
# 編集距離＋順序制約（ページ順は疾患順の単調増加）で決める。jpnモデルが無い環境では
# 従来どおりページ順で割り当てる（省略疾患があるとずれるため警告）。
.ishikawa_map_page_titles <- function(png_paths) {
  n <- length(png_paths); cand <- ISHIKAWA_DISEASE_ORDER; m <- length(cand)
  fallback <- function() { warning("石川県: ページ見出しを読めないためページ順で疾患を割り当てます"); ifelse(seq_len(n) <= m, cand[pmin(seq_len(n), m)], NA_character_) }
  if (!"jpn" %in% tesseract::tesseract_info()$available) return(fallback())
  jpn <- tesseract::tesseract("jpn", options = list(tessedit_pageseg_mode = "7"))
  titles <- vapply(png_paths, function(p) {
    if (is.null(p)) return("")
    tryCatch({
      g <- magick::image_crop(magick::image_read(p), "1700x200+300+180")
      trimws(tesseract::ocr(g, engine = jpn))
    }, error = function(e) "")
  }, character(1))
  titles <- gsub("[[:space:]ー_|]+", "", titles)
  norm <- function(s) gsub("[()（）\\-]|ウイルス", "", s)
  cost <- matrix(0, n, m)
  for (i in seq_len(n)) for (j in seq_len(m)) {
    a <- norm(titles[i]); b <- norm(cand[j])
    cost[i, j] <- adist(a, b)[1, 1] / max(nchar(b), 1)
  }
  # DP: 単調増加な割り当て（各ページ→疾患、疾患は高々1回）
  best <- matrix(Inf, n + 1, m + 1); best[1, ] <- 0; from <- matrix(0L, n + 1, m + 1)
  for (i in 1:n) for (j in 1:m) {
    for (k in 0:(j - 1)) {
      v <- best[i, k + 1] + cost[i, j]
      if (v < best[i + 1, j + 1]) { best[i + 1, j + 1] <- v; from[i + 1, j + 1] <- k }
    }
  }
  j <- which.min(best[n + 1, 2:(m + 1)]); out <- rep(NA_character_, n)
  if (!is.finite(best[n + 1, j + 1])) return(fallback())
  for (i in n:1) { out[i] <- cand[j]; j <- from[i + 1, j + 1] }
  message("石川県ページ見出し対応: ", paste(out, collapse = " / "))
  out
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

  # セル位置は300dpi描画上の固定座標（ishikawa_glyph.R）なので罫線検出は不要
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

  # 「5週連続0件」の疾患はページが省略されるため、ページ順＝疾患順とは限らない。
  # ページ見出し（大きな日本語タイトル）をOCRし、疾患順を保ったまま
  # 編集距離が最小になる対応づけ（DP）で疾患名を決める
  disease_names <- .ishikawa_map_page_titles(lapply(disease_pages, get_png))

  out <- list(ari)
  for (i in seq_along(disease_pages)) {
    p <- disease_pages[i]
    disease <- disease_names[i]
    if (is.na(disease)) next
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
