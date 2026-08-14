# 青森県「第◯週五類定点把握対象疾患」表（週報PDF 1ページ目）
# https://www.pref.aomori.lg.jp/soshiki/kenko/hoken/files/wr{YEAR}w{WEEK}.pdf
# 保健所: 東津軽・青森市(東青)、中南、三戸・八戸市(三八)、西北、上北、下北
# （「青森県計」列は保健所別ではないため除外する。「前週からの増減」列も除外）
# 表は1ページ目に座標ベースで配置されており、疾患名は行の左端(x=55-130付近)、
# 各保健所の「数」「人/定点」がペアで並ぶ。

fetch_aomori <- function(pdf_url) {
  if (!exists("pdf_words")) stop("pdf_table_utils.R を先に source してください")

  words <- pdf_words(pdf_url, page = 1)
  rows <- group_words_into_rows(words, y_tol = 3)

  # 週番号・年はURL（wr{YEAR}w{WEEK}.pdf）から確実に取得する
  url_m <- regmatches(pdf_url, regexec("wr(20[0-9]{2})w([0-9]+)\\.pdf", pdf_url))[[1]]
  week_label <- if (length(url_m) == 3) paste0(url_m[2], "年第", as.integer(url_m[3]), "週") else NA_character_

  # 保健所名の行（東青 中南 三八 西北 上北 下北）を探す
  region_row_idx <- NA_integer_
  for (i in seq_along(rows)) {
    tx <- rows[[i]]$text
    if (all(c("東青", "中南", "西北", "下北") %in% tx)) { region_row_idx <- i; break }
  }
  if (is.na(region_row_idx)) stop("保健所名の行が見つかりません")
  region_row <- rows[[region_row_idx]]
  region_row <- region_row[order(region_row$x), ]
  region_order <- region_row$text[region_row$text %in% c("東青", "中南", "三八", "西北", "上北", "下北")]
  region_full <- c(
    "東青" = "東津軽・青森市", "中南" = "中南", "三八" = "三戸・八戸市",
    "西北" = "西北", "上北" = "上北", "下北" = "下北"
  )

  # ヘッダー行（"数" "人/定点" の繰り返し）を探す
  header_row_idx <- NA_integer_
  for (i in (region_row_idx):(region_row_idx + 4)) {
    tx <- rows[[i]]$text
    if (sum(tx == "数") >= 7 && sum(tx == "人/定点") >= 6) { header_row_idx <- i; break }
  }
  if (is.na(header_row_idx)) stop("列ヘッダー行が見つかりません")
  hdr <- rows[[header_row_idx]]
  hdr <- hdr[order(hdr$x), ]
  count_x <- hdr$x[hdr$text == "数"]
  rate_x  <- hdr$x[hdr$text == "人/定点"]
  # 最後の「数」は前週差分列なので除外（7個中先頭6区分+計=7、7番目までがcount/rateペア）
  n_regions <- length(region_order)
  count_x <- sort(count_x)[1:(n_regions + 1)]  # 6保健所 + 計
  rate_x  <- sort(rate_x)[1:(n_regions + 1)]

  data_start <- header_row_idx + 1

  out <- list()
  for (i in data_start:length(rows)) {
    r <- rows[[i]]
    if (grepl("^Ⅲ ", row_text(r))) break
    r <- r[order(r$x), ]
    # 疾患名: x < count_x[1]-5 の語のうち最もxが大きいもの（カテゴリラベルより右）
    label_words <- r[r$x < (count_x[1] - 10), ]
    if (nrow(label_words) == 0) next
    disease <- label_words$text[which.max(label_words$x)]
    if (disease %in% c("小", "児", "科", "眼", "基", "幹")) next
    if (!nzchar(disease)) next

    for (k in seq_len(n_regions)) {
      cx <- count_x[k]; rx <- rate_x[k]
      cnt_word <- r[r$x >= (cx - 8) & r$x < (cx + 15), ]
      rate_word <- r[r$x >= (rx - 8) & r$x < (rx + 18), ]
      cnt <- if (nrow(cnt_word) > 0) cnt_word$text[1] else NA_character_
      rte <- if (nrow(rate_word) > 0) rate_word$text[1] else NA_character_
      out[[length(out) + 1]] <- data.frame(
        pref = "青森県", week_label = week_label,
        hokenjo = region_full[region_order[k]],
        disease = disease,
        count = parse_hokenjo_number(cnt),
        rate = parse_hokenjo_number(rte),
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, out)
}
