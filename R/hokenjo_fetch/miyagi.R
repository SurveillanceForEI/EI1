# 宮城県「感染症発生動向調査情報」週報PDF 1ページ目の集計表
# https://www.pref.miyagi.jp/documents/1967/syuho{YEAR}{WEEK}w.pdf
# 保健所: 仙南、塩釜、大崎、石巻、気仙沼、仙台市
# （HOKENJO_DATA_SOURCESには「グラフのみで数値表なし」とあったが、
#   実際には1ページ目に患者報告数（上段）・定点当たり報告数（下段）の
#   数値表が存在することを確認した。列順は左から 仙南,塩釜,大崎,石巻,
#   気仙沼,仙台市,患者数(県計),累計 で、"仙台市"が末尾側にあることに注意）
#
# 疾患名は「報告数+疾患名」の行として1行にまとまり（y_tol=3で結合される）、
# その直後の行が「定点当たり報告数のみ」の行になる。値が0またはデータなし
# の保健所は空欄（トークンなし）で省略されるため、座標（x）による
# 最近傍列マッチングで保健所を特定する。

fetch_miyagi <- function(pdf_url) {
  if (!exists("pdf_words")) stop("pdf_table_utils.R を先に source してください")

  words <- pdf_words(pdf_url, page = 1)
  rows <- group_words_into_rows(words, y_tol = 3)

  title_tx <- paste(sapply(rows[1:3], row_text), collapse = " ")
  wm <- regmatches(title_tx, regexec("第([0-9]+)週", title_tx))[[1]]
  ym <- regmatches(title_tx, regexpr("20[0-9]{2}(?=\\.[0-9]+\\.[0-9]+)", title_tx, perl = TRUE))
  week_label <- if (length(wm) == 2 && length(ym) > 0) paste0(ym, "年第", wm[2], "週") else NA_character_

  hokenjo_order <- c("仙南", "塩釜", "大崎", "石巻", "気仙沼", "仙台市")
  col_order <- c(hokenjo_order, "患者数", "累計")

  is_number <- function(s) grepl("^[0-9,]+(\\.[0-9]+)?$", s)

  # 列のx中心は、ヘッダーのラベル位置ではなく実データ行から校正する
  # （"気仙沼"ラベルの文字位置と実際の数値セルの中心がずれているため、
  #   ラベル基準の最近傍判定では気仙沼の値が仙台市に誤って割り当てられる
  #   ことがある）。最初に見つかる「8列すべて値がある行」（通常は
  # 「急性呼吸器感染症」）を基準行として、その数値トークンのx座標を
  # そのまま列中心として採用する。
  col_x <- NULL
  for (k in seq_along(rows)) {
    r <- rows[[k]][order(rows[[k]]$x), ]
    nums <- r[is_number(r$text), ]
    if (nrow(nums) == 8) { col_x <- nums$x; break }
  }
  if (is.null(col_x)) stop("列位置校正用の基準行（8列すべて値がある行）が見つかりません")

  # 表本体の開始行（"患者数"と"累計"を含む見出し行の次）から、
  # 脚注段落（"＊1 急性呼吸器感染症は、..."）の手前までを対象とする
  start_idx <- NA_integer_
  end_idx <- length(rows)
  for (k in seq_along(rows)) {
    tx <- row_text(rows[[k]])
    if (is.na(start_idx) && grepl("患者数", tx) && grepl("累計", tx)) start_idx <- k + 1
    if (grepl("急性呼吸器感染症は、急性の上気道炎", tx)) { end_idx <- k - 1; break }
  }
  if (is.na(start_idx)) stop("表の開始位置が見つかりません")

  out <- list()
  i <- start_idx
  while (i <= end_idx) {
    r <- rows[[i]]
    r <- r[order(r$x), ]
    # 「急性呼吸器感染症定点」「小児科定点」「眼科定点」「基幹定点」等の
    # 定点種別カテゴリラベル（縦書きで疾患名の前後に混入）を除外する
    category_labels <- c(
      "急性呼吸器", "感染症定点", "小児科定点", "眼科定点", "基幹定点",
      "拡張", "疾病", "感染症", "定点"
    )
    name_toks <- r$text[!is_number(r$text)]
    name_toks <- gsub("＊[0-9０-９]$", "", name_toks)
    name_toks <- name_toks[nzchar(name_toks)]
    name_toks <- name_toks[!name_toks %in% category_labels]
    num_toks <- r[is_number(r$text), ]
    if (length(name_toks) > 0 && any(nchar(name_toks) >= 2) && nrow(num_toks) >= 1) {
      disease <- paste(name_toks, collapse = "")
      # 各数値トークンをx最近傍の列（保健所6列＋患者数＋累計の8列）に割当てる
      cnt <- setNames(rep(NA_character_, 8), col_order)
      for (j in seq_len(nrow(num_toks))) {
        nearest <- which.min(abs(col_x - num_toks$x[j]))
        cnt[col_order[nearest]] <- num_toks$text[j]
      }
      # 次の行が定点当たり報告数のみの行かどうかを確認
      rte <- setNames(rep(NA_character_, 8), col_order)
      if (i + 1 <= end_idx) {
        r2 <- rows[[i + 1]]
        r2 <- r2[order(r2$x), ]
        if (nrow(r2) > 0 && all(grepl("^[0-9]+\\.[0-9]+$", r2$text))) {
          for (j in seq_len(nrow(r2))) {
            nearest <- which.min(abs(col_x - r2$x[j]))
            rte[col_order[nearest]] <- r2$text[j]
          }
          i <- i + 1
        }
      }
      for (h in hokenjo_order) {
        out[[length(out) + 1]] <- data.frame(
          pref = "宮城県", week_label = week_label, hokenjo = h, disease = disease,
          count = parse_hokenjo_number(cnt[h]), rate = parse_hokenjo_number(rte[h]),
          stringsAsFactors = FALSE
        )
      }
    }
    i <- i + 1
  }
  do.call(rbind, out)
}
