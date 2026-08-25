# 千葉県「第◯週 保健所別、年齢群別報告数（男女合計）」PDF（週報後半、
# 疾患別・保健所別・年齢階級別集計表のページ群）
# https://www.pref.chiba.lg.jp/eiken/c-idsc/documents/wr{YEAR}{WEEK}.pdf
#
# 各ページに複数疾患が縦に並び、疾患ごとに年齢階級別の内訳＋「合計」行
# （保健所別合計数）が続く。疾患名は表左端(x~60-85)に1文字ずつ縦書きで
# 配置される。定点当たり報告数の列は無いため、区分（小児科定点数/ARI
# 定点数/眼科定点数/基幹定点数）行の定点数で count を割って rate を算出する。

.chiba_hokenjo_order <- c("野田", "柏市", "松戸", "市川", "船橋市", "習志野",
                           "千葉市", "印旛", "香取", "海匝", "山武", "長生",
                           "夷隅", "安房", "君津", "市原", "合計")

fetch_chiba <- function(pdf_url = NULL) {
  if (!exists("pdf_words")) stop("pdf_table_utils.R を先に source してください")
  if (is.null(pdf_url)) {
    pdf_url <- "https://www.pref.chiba.lg.jp/eiken/c-idsc/documents/wr2631.pdf"
  }

  path <- tempfile(fileext = ".pdf")
  download.file(pdf_url, path, mode = "wb", quiet = TRUE)
  n_pages <- length(pdftools::pdf_data(path))

  target_pages <- integer(0)
  for (p in seq_len(n_pages)) {
    w <- pdftools::pdf_data(path)[[p]]
    # タイトル文言「保健所別、年齢群別報告数（男女合計）」はy=80付近に
    # 印字されることがあり、以前のy<75という閾値では週によって取りこぼし、
    # 該当ページが対象外扱いになって保健所別データが丸ごと欠落するバグが
    # あった（2026年第20週等で確認）。y<90に広げて確実に含める
    hdr_txt <- paste(w$text[w$y < 90], collapse = "")
    if (any(grepl("定点数", w$text)) && any(w$text == "合計") && grepl("年齢群別報告数", hdr_txt)) {
      target_pages <- c(target_pages, p)
    }
  }
  if (length(target_pages) == 0) stop("保健所別年齢群別報告数のページが見つかりません")

  out <- list()
  week_label <- NA_character_

  for (p in target_pages) {
    w <- pdftools::pdf_data(path)[[p]]

    wl <- w$text[grepl("^[0-9]+$", w$text)]
    wk <- w$text[w$text == "週" ]
    page_full_text <- paste(w$text, collapse = "")
    m <- regmatches(page_full_text, regexpr("20[0-9]{2}年第[0-9]+週", page_full_text))
    if (length(m) > 0 && nchar(m) > 0) week_label <- m

    # 列順（x座標）を検出。保健所名見出し行のyは週・ページによって70〜93付近を
    # ばらつくため（「保健所別、年齢群別報告数」というタイトル行の直後のy位置が
    # 一定でない）、以前のy<=84固定では週によってタイトル文言の残骸（「第」
    # 「週」や週番号の数字）だけを拾って本来の保健所名列を取りこぼし、
    # 該当ページ全体がデータなし扱いになるバグがあった（2026年第20週等で確認）。
    # y範囲を広げつつ、タイトル文言由来のトークン（「第」「週」や数字）を
    # 明示的に除外することで、両者を区別する
    hdr <- w[w$y >= 70 & w$y <= 100 & nchar(w$text) == 1 &
               !(w$text %in% c("第", "週")) & !grepl("^[0-9]+$", w$text), ]
    hdr <- hdr[order(hdr$x), ]
    col_x <- unique(hdr$x)
    col_x <- sort(col_x)
    if (length(col_x) < 16) next  # ヘッダーが無い（表の続きページ）→列位置は前ページのものを流用
    x_centers <- col_x

    rows <- group_words_into_rows(w, y_tol = 2)

    teiten <- NULL
    name_buf <- character(0)
    pending_teiten <- FALSE

    match_col <- function(x) which.min(abs(x_centers - x))

    extract_num_vec <- function(rr, exclude_idx) {
      num_rows <- rr[!exclude_idx, ]
      num_rows <- num_rows[grepl("^[0-9,.]+$", num_rows$text) & num_rows$x >= min(x_centers) - 15, ]
      vec <- rep(NA_real_, length(x_centers))
      for (k in seq_len(nrow(num_rows))) {
        ci <- match_col(num_rows$x[k])
        vec[ci] <- parse_hokenjo_number(num_rows$text[k])
      }
      vec
    }

    for (i in seq_along(rows)) {
      rr <- rows[[i]]
      rr <- rr[order(rr$x), ]
      txt <- rr$text

      is_teiten_label <- any(grepl("定点数", txt))
      n_numeric_here <- sum(grepl("^[0-9,.]+$", txt) & rr$x >= min(x_centers) - 15)
      is_total_row <- any(txt == "合計") && any(rr$x[txt == "合計"] < 100)

      if (is_teiten_label) {
        lbl_idx <- grepl("定点数", txt) | !grepl("^[0-9,.]+$", txt)
        if (n_numeric_here >= 8) {
          teiten <- extract_num_vec(rr, lbl_idx)
          pending_teiten <- FALSE
        } else {
          pending_teiten <- TRUE
        }
        name_buf <- character(0)
        next
      }
      if (pending_teiten && n_numeric_here >= 8) {
        teiten <- extract_num_vec(rr, !grepl("^[0-9,.]+$", txt))
        pending_teiten <- FALSE
        next
      }
      if (pending_teiten) {
        # ラベルの続き行（数値なし）。読み飛ばす
        next
      }

      if (is_total_row) {
        num_rows <- rr[rr$text != "合計", ]
        # 報告数0件のセルはPDF上で完全な空欄（トークンなし）になるため、
        # NAでなく0で初期化する（他県と同様の慣習。ユーザー指摘、2026-08-24）
        vec <- rep(0, length(x_centers))
        for (k in seq_len(nrow(num_rows))) {
          ci <- match_col(num_rows$x[k])
          vec[ci] <- parse_hokenjo_number(num_rows$text[k])
        }
        disease <- trimws(paste(name_buf, collapse = ""))
        if (disease != "") {
          for (ci in seq_along(.chiba_hokenjo_order)) {
            if (.chiba_hokenjo_order[ci] == "合計") next
            cnt <- vec[ci]
            rt <- if (!is.null(teiten) && !is.na(teiten[ci]) && teiten[ci] > 0 && !is.na(cnt)) {
              round(cnt / teiten[ci], 2)
            } else NA_real_
            out[[length(out) + 1]] <- data.frame(
              pref = "千葉県", week_label = week_label,
              hokenjo = .chiba_hokenjo_order[ci],
              disease = disease,
              count = ifelse(is.na(cnt), NA_real_, cnt),
              rate = rt,
              stringsAsFactors = FALSE
            )
          }
        }
        name_buf <- character(0)
        next
      }

      # 疾患名候補文字（左端の縦書き列）。1文字トークンのみ採用
      # （定点数ラベルの折返し残骸などの複数文字トークンは除外）
      name_chars <- rr$text[rr$x >= 60 & rr$x <= 85 & nchar(rr$text) == 1]
      if (length(name_chars) > 0) name_buf <- c(name_buf, name_chars)
    }
  }

  df <- do.call(rbind, out)
  rownames(df) <- NULL
  df
}

# ============================================================
# 千葉県「保健所別グラフ」週報（棒グラフのみ、数値ラベルなし）
# 例: https://www.pref.chiba.lg.jp/eiken/c-idsc/documents/5wg-2632.pdf
# ------------------------------------------------------------
# fetch_chiba()が対象とする「年齢群別報告数」表形式のPDFが提供されない
# 週は、この棒グラフ形式のPDFのみが公表される。数値ラベルが無いため、
# グラフの棒の高さ（ピクセル位置）を軸目盛から換算して読み取った、
# あくまで【近似値】である（正確なテーブル値ではない）。
# 第32週（2026年）分をこの方法で目視・画像解析により読み取り、
# 手動で書き起こしたもの。将来週に自動対応するには、pdf_render_page()で
# 画像化した上で棒の色（紺色=第32週相当）のピクセル高さを検出する
# 処理を実装する必要がある。
#
# 値は全て「定点当たり報告数」(rate)。count（実数）はこの形式からは
# 読み取れないためNA。
# ============================================================

.CHIBA_GRAPH_WEEK32_RATE <- list(
  "急性呼吸器感染症(ARI)" = c(野田=24, 柏市=43, 松戸=74, 市川=38, 船橋市=48, 習志野=43, 千葉市=46,
    印旛=56, 香取=38, 海匝=51, 山武=29, 長生=50, 夷隅=66, 安房=15, 君津=41, 市原=57),
  "インフルエンザ" = c(野田=0.87, 柏市=0.51, 松戸=0.70, 市川=0.70, 船橋市=0.46, 習志野=0.24, 千葉市=0.68,
    印旛=1.10, 香取=0.20, 海匝=1.00, 山武=0, 長生=0.19, 夷隅=0.51, 安房=0, 君津=0.10, 市原=0.29),
  "新型コロナウイルス感染症" = c(野田=1.65, 柏市=2.05, 松戸=1.15, 市川=0.15, 船橋市=1.00, 習志野=2.15, 千葉市=0.85,
    印旛=1.00, 香取=4.40, 海匝=0.90, 山武=1.10, 長生=5.15, 夷隅=6.00, 安房=0.60, 君津=0.85, 市原=1.05),
  "RSウイルス感染症" = c(野田=0.75, 柏市=0.28, 松戸=1.10, 市川=0.50, 船橋市=1.50, 習志野=0.55, 千葉市=0.13,
    印旛=0.22, 香取=0, 海匝=1.00, 山武=0, 長生=0, 夷隅=1.00, 安房=0, 君津=0, 市原=0),
  "咽頭結膜熱" = c(野田=0, 柏市=0.29, 松戸=0.28, 市川=0.10, 船橋市=0.12, 習志野=0.22, 千葉市=0.14,
    印旛=0.07, 香取=0, 海匝=0, 山武=0, 長生=0, 夷隅=0, 安房=0.25, 君津=0.14, 市原=0),
  "A群溶血性レンサ球菌咽頭炎" = c(野田=0.50, 柏市=1.15, 松戸=1.35, 市川=0.15, 船橋市=1.65, 習志野=1.85, 千葉市=1.20,
    印旛=1.05, 香取=0.55, 海匝=2.05, 山武=1.35, 長生=6.30, 夷隅=2.35, 安房=0.55, 君津=0.30, 市原=1.15),
  "感染性胃腸炎" = c(野田=1.30, 柏市=2.60, 松戸=4.90, 市川=3.70, 船橋市=1.65, 習志野=2.90, 千葉市=2.55,
    印旛=2.50, 香取=3.65, 海匝=1.00, 山武=0.60, 長生=0.55, 夷隅=0, 安房=0, 君津=3.30, 市原=2.80),
  "水痘" = c(野田=0.27, 柏市=0.28, 松戸=0.20, 市川=0.20, 船橋市=0.63, 習志野=0.10, 千葉市=0.13,
    印旛=0.22, 香取=0, 海匝=0.51, 山武=0, 長生=0.34, 夷隅=0, 安房=0, 君津=0, 市原=0.51),
  "手足口病" = c(野田=2.7, 柏市=2.0, 松戸=4.0, 市川=2.5, 船橋市=4.0, 習志野=5.5, 千葉市=6.5,
    印旛=3.5, 香取=2.5, 海匝=18.5, 山武=6.5, 長生=12.0, 夷隅=0.5, 安房=0, 君津=3.5, 市原=2.0),
  "伝染性紅斑" = c(野田=0, 柏市=0.28, 松戸=0.09, 市川=0.20, 船橋市=0, 習志野=0, 千葉市=0.07,
    印旛=0, 香取=0, 海匝=0, 山武=0, 長生=0, 夷隅=0, 安房=0, 君津=0, 市原=0),
  "突発性発しん" = c(野田=0, 柏市=0.29, 松戸=0.18, 市川=0.30, 船橋市=0.25, 習志野=0.11, 千葉市=0.60,
    印旛=0.21, 香取=0, 海匝=0.50, 山武=0, 長生=0.33, 夷隅=0, 安房=0, 君津=0.13, 市原=0),
  "ヘルパンギーナ" = c(野田=0, 柏市=0.45, 松戸=0.45, 市川=1.10, 船橋市=0.35, 習志野=1.60, 千葉市=1.15,
    印旛=0.75, 香取=0, 海匝=1.50, 山武=0.35, 長生=0, 夷隅=0, 安房=0.20, 君津=0.10, 市原=0.60),
  "流行性耳下腺炎" = c(野田=0.14, 柏市=0.09, 松戸=0, 市川=0, 船橋市=0, 習志野=0.13, 千葉市=0.21,
    印旛=0.21, 香取=0, 海匝=0, 山武=0, 長生=0, 夷隅=0, 安房=0, 君津=0.14, 市原=0),
  "急性出血性結膜炎" = c(野田=0, 柏市=0, 松戸=0, 市川=0, 船橋市=0, 習志野=0, 千葉市=0,
    印旛=0, 香取=0, 海匝=0, 山武=0, 長生=0, 夷隅=0, 安房=0, 君津=0, 市原=0),
  "流行性角結膜炎" = c(野田=0, 柏市=0, 松戸=0.21, 市川=0.34, 船橋市=0, 習志野=0.34, 千葉市=0,
    印旛=0.34, 香取=0, 海匝=0, 山武=0, 長生=0, 夷隅=0, 安房=0, 君津=0, 市原=0)
)

fetch_chiba_graph <- function(week_label = "2026年第32週") {
  out <- list()
  for (disease in names(.CHIBA_GRAPH_WEEK32_RATE)) {
    v <- .CHIBA_GRAPH_WEEK32_RATE[[disease]]
    for (h in names(v)) {
      out[[length(out) + 1]] <- data.frame(
        pref = "千葉県", week_label = week_label, hokenjo = h,
        disease = disease, count = NA_real_, rate = unname(v[h]),
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, out)
}
