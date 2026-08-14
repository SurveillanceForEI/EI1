# 秋田県「RAPIDS週報」PDF 2ページ目「第◯週の保健所別報告数」表
# https://idsc.pref.akita.jp/kss/RAPIDS.pdf （固定URL、内容は毎週更新）
# 保健所: 秋田市, 大館, 北秋田, 能代, 秋田中央, 由利本荘, 大仙, 横手, 湯沢
# （先頭列「秋田県」は県全体集計のため除外）
# 疾患名は x≈95 の列、各保健所は「患者報告数」「定点あたり患者報告数」の
# ペアが横に並ぶ。「＊」（非定点）と空欄（報告なし=0扱いだが未記載）がある。

fetch_akita <- function(pdf_url = "https://idsc.pref.akita.jp/kss/RAPIDS.pdf") {
  if (!exists("pdf_words")) stop("pdf_table_utils.R を先に source してください")

  words1 <- pdf_words(pdf_url, page = 1)
  rows1 <- group_words_into_rows(words1, y_tol = 3)
  title_tx <- paste(sapply(rows1[1:2], row_text), collapse = " ")
  wm <- regmatches(title_tx, regexec("(20[0-9]{2})年第([0-9]+)週", title_tx))[[1]]
  week_label <- if (length(wm) == 3) paste0(wm[2], "年第", wm[3], "週") else NA_character_

  words <- pdf_words(pdf_url, page = 2)
  rows <- group_words_into_rows(words, y_tol = 3)

  # 保健所名の行（秋田県 秋田市 大館 ... 湯沢）
  region_row_idx <- NA_integer_
  for (i in seq_along(rows)) {
    tx <- rows[[i]]$text
    if (all(c("秋田市", "大館", "湯沢") %in% tx)) { region_row_idx <- i; break }
  }
  if (is.na(region_row_idx)) stop("保健所名の行が見つかりません")
  region_row <- rows[[region_row_idx]][order(rows[[region_row_idx]]$x), ]
  region_names_all <- region_row$text  # 「秋田県」（全体集計）を含む

  # 「患者報告数」「定点あたり患者報告数」のペア列x（ヘッダー2行下）
  header_row_idx <- region_row_idx + 3  # 患者/定点あたり(148) -> 報告数/患者報告数(155)
  hdr <- rows[[header_row_idx]]
  hdr <- hdr[order(hdr$x), ]
  cnt_x_all <- sort(hdr$x[hdr$text == "報告数"])
  rate_x_all <- sort(hdr$x[hdr$text == "患者報告数"])
  n_all <- length(region_names_all)
  cnt_x_all <- cnt_x_all[seq_len(n_all)]
  rate_x_all <- rate_x_all[seq_len(n_all)]

  # 保健所別のみ抽出（「秋田県」全体集計はスロットとして残しつつ最終出力からは除く）
  region_names <- region_names_all[region_names_all != "秋田県"]
  n_regions <- length(region_names)

  out <- list()
  for (i in (header_row_idx + 1):length(rows)) {
    r <- rows[[i]]
    tx_all <- row_text(r)
    if (grepl("定点あたり患者報告数は|＊」印は|定点医療機関数", tx_all)) break
    name_word <- r[r$x >= 85 & r$x <= 112, ]
    if (nrow(name_word) == 0) next
    disease <- name_word$text[1]
    if (!nzchar(disease)) next

    # 数値/ダッシュ/アスタリスクのトークンを2個1組（報告数,定点あたり）とみなし、
    # 組の先頭(報告数)のxを「報告数」列の位置と最近傍照合して保健所を決める
    val_words <- r[grepl("^([0-9]+(\\.[0-9]+)?|-|\\*)$", r$text) & r$x > 150, ]
    val_words <- val_words[order(val_words$x), ]
    vals <- setNames(rep(NA_character_, n_all * 2),
                      paste(rep(region_names_all, each = 2), c("count", "rate"), sep = "|"))
    n_pairs <- floor(nrow(val_words) / 2)
    if (n_pairs > 0) {
      for (j in seq_len(n_pairs)) {
        cx <- val_words$x[(j - 1) * 2 + 1]
        rxv <- val_words$x[(j - 1) * 2 + 2]
        nearest <- which.min(abs(cnt_x_all - cx))
        reg <- region_names_all[nearest]
        vals[paste(reg, "count", sep = "|")] <- val_words$text[(j - 1) * 2 + 1]
        vals[paste(reg, "rate", sep = "|")] <- val_words$text[(j - 1) * 2 + 2]
      }
    }

    for (k in seq_len(n_regions)) {
      cnt <- vals[paste(region_names[k], "count", sep = "|")]
      rte <- vals[paste(region_names[k], "rate", sep = "|")]
      out[[length(out) + 1]] <- data.frame(
        pref = "秋田県", week_label = week_label, hokenjo = region_names[k],
        disease = disease,
        count = parse_hokenjo_number(cnt),
        rate = parse_hokenjo_number(rte),
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, out)
}
