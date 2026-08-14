# 東京都「定点把握対象疾患 報告数／定点医療機関当たり報告数【保健所別】」PDF
# https://idsc.tmiph.metro.tokyo.lg.jp/assets/weekly/{YEAR}/{WEEK}.pdf
#
# 週報後半に4ページ1組で「小児科」系（報告数ページ・率ページ）と
# 「急性呼吸器感染症／眼科／基幹」系（報告数ページ・率ページ）が
# それぞれ配置される。ページ順は週によって前後する可能性があるため、
# ヘッダー文言（"定点把握対象疾患"）と "報告数"/"当たり報告数" の
# 有無、および見出し行の疾患集合（小児科 or 急性呼吸器感染症）で
# 判定する。保健所名はページ左端(x<110)、データはx>=110に位置する。

.tokyo_kodomo_diseases <- c(
  "RSウイルス感染症", "咽頭結膜熱", "Ａ群溶血性レンサ球菌咽頭炎",
  "感染性胃腸炎", "水痘", "手足口病", "伝染性紅斑", "突発性発しん",
  "ヘルパンギーナ", "流行性耳下腺炎", "川崎病", "不明発しん症"
)
.tokyo_ari_diseases <- c(
  "インフルエンザ", "新型コロナウイルス感染症（COVID-19）",
  "急性呼吸器感染症（ARI）", "急性出血性結膜炎", "流行性角結膜炎",
  "細菌性髄膜炎", "無菌性髄膜炎", "マイコプラズマ肺炎", "クラミジア肺炎",
  "感染性胃腸炎（ロタウイルス）", "インフルエンザ（入院）", "COVID-19（入院）"
)

.tokyo_exclude_hokenjo <- c("東京都合計", "東京都", "島しょ")

fetch_tokyo <- function(pdf_url = NULL) {
  if (!exists("pdf_words")) stop("pdf_table_utils.R を先に source してください")
  if (is.null(pdf_url)) {
    pdf_url <- "https://idsc.tmiph.metro.tokyo.lg.jp/assets/weekly/2026/31.pdf"
  }

  path <- tempfile(fileext = ".pdf")
  download.file(pdf_url, path, mode = "wb", quiet = TRUE)
  n_pages <- length(pdftools::pdf_data(path))

  page_info <- list()
  for (p in seq_len(n_pages)) {
    w <- pdftools::pdf_data(path)[[p]]
    is_age_page <- any(w$text == "【年齢階級別】")
    is_hokenjo_kodomo <- any(w$text == "【保健所別】") && any(w$text == "小児科")
    is_ari_kikan <- !is_age_page && any(w$text == "急性呼吸器感染症") && any(w$text == "基幹") && any(w$text == "眼科")
    if (!is_hokenjo_kodomo && !is_ari_kikan) next
    # 小数点を含む数値トークンがあれば率ページ、無ければ報告数ページ
    is_rate <- any(grepl("^[0-9]+\\.[0-9]+$", w$text))
    page_info[[length(page_info) + 1]] <- list(page = p, is_rate = is_rate,
                                                 group = if (is_hokenjo_kodomo) "kodomo" else "ari")
  }
  if (length(page_info) == 0) stop("保健所別データのページが見つかりません")

  week_label <- NA_character_

  parse_table <- function(p, diseases) {
    w <- pdftools::pdf_data(path)[[p]]
    wl <- w$text[grepl("^20[0-9]{2}年[0-9]+週$", w$text)]
    if (length(wl) > 0) week_label <<- wl[1]

    rows <- group_words_into_rows(w, y_tol = 3)
    out <- list()
    for (i in seq_along(rows)) {
      rr <- rows[[i]]
      rr <- rr[order(rr$x), ]
      name_tok <- rr$text[rr$x < 110 & !grepl("^[0-9,.]+$", rr$text)]
      if (length(name_tok) == 0) next
      hj <- gsub("\\s+", "", paste(name_tok, collapse = ""))
      if (hj %in% .tokyo_exclude_hokenjo || nchar(hj) == 0) next
      data_tok <- rr[rr$x >= 110, ]
      if (nrow(data_tok) == 0) next
      list(hj = hj, xs = data_tok$x, vals = data_tok$text, y = rr$y[1])
      out[[length(out) + 1]] <- list(hj = hj, xs = data_tok$x, vals = data_tok$text)
    }
    out
  }

  # 列のx中心は数値の桁数（"1"と"97"等）により開始位置がばらつき、
  # データ行だけからの自動クラスタリングでは列数を安定検出できない
  # ため、ヘッダー文字（複数行に折り返した疾患名の先頭文字）のx座標
  # から列位置を検出する。ヘッダーは3〜4行に渡って文字が分割される
  # ため、y方向に幅を取って1文字トークンを集め、x方向にクラスタリング
  # する。
  build_col_centers_from_header <- function(page) {
    w <- pdftools::pdf_data(path)[[page]]
    hdr <- w[w$y >= 70 & w$y <= 112 & w$x >= 100, ]
    hdr <- hdr[order(hdr$x), ]
    clusters <- list()
    for (i in seq_len(nrow(hdr))) {
      x <- hdr$x[i]
      matched <- FALSE
      for (ci in seq_along(clusters)) {
        if (abs(median(clusters[[ci]]) - x) <= 18) {
          clusters[[ci]] <- c(clusters[[ci]], x)
          matched <- TRUE
          break
        }
      }
      if (!matched) clusters[[length(clusters) + 1]] <- x
    }
    sort(sapply(clusters, median))
  }

  build_df <- function(group_name, diseases) {
    info_rate <- Filter(function(pi) pi$group == group_name && pi$is_rate, page_info)
    info_cnt <- Filter(function(pi) pi$group == group_name && !pi$is_rate, page_info)
    if (length(info_cnt) == 0) return(NULL)
    rows_cnt <- parse_table(info_cnt[[1]]$page, diseases)
    rows_rate <- if (length(info_rate) > 0) parse_table(info_rate[[1]]$page, diseases) else list()

    col_x <- build_col_centers_from_header(info_cnt[[1]]$page)
    if (length(col_x) != length(diseases)) {
      # ヘッダー検出が疾患数と一致しない場合は率ページのヘッダーで再試行
      if (length(info_rate) > 0) col_x <- build_col_centers_from_header(info_rate[[1]]$page)
    }
    if (length(col_x) == 0) return(NULL)
    match_col <- function(x) which.min(abs(col_x - x))

    rate_lookup <- new.env()
    for (r in rows_rate) {
      vec <- rep(NA_real_, length(col_x))
      for (k in seq_along(r$xs)) vec[match_col(r$xs[k])] <- parse_hokenjo_number(r$vals[k])
      assign(r$hj, vec, envir = rate_lookup)
    }

    out <- list()
    for (r in rows_cnt) {
      vec <- rep(NA_real_, length(col_x))
      for (k in seq_along(r$xs)) vec[match_col(r$xs[k])] <- parse_hokenjo_number(r$vals[k])
      rt <- if (exists(r$hj, envir = rate_lookup)) get(r$hj, envir = rate_lookup) else rep(NA_real_, length(col_x))
      m <- min(length(diseases), length(vec))
      for (ci in seq_len(m)) {
        out[[length(out) + 1]] <- data.frame(
          pref = "東京都", week_label = NA_character_,
          hokenjo = r$hj, disease = diseases[ci],
          count = vec[ci], rate = rt[ci],
          stringsAsFactors = FALSE
        )
      }
    }
    do.call(rbind, out)
  }

  df1 <- build_df("kodomo", .tokyo_kodomo_diseases)
  df2 <- build_df("ari", .tokyo_ari_diseases)
  df <- do.call(rbind, list(df1, df2))
  df$week_label <- week_label
  rownames(df) <- NULL
  df
}
