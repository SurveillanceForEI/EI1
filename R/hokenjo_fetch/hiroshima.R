# 広島県「感染症発生動向週報」PDF（1ページ完結）
# https://www.pref.hiroshima.lg.jp/uploaded/attachment/<id>.pdf
#
# 【2026-09-16 レイアウト変更対応】以前は警報・注意報対象の1疾患のみ
# 「保健所別の流行状況（定点当たり）」表に掲載される形式だったが、
# 現在は「類別／報告数／疾患名／計／西部／西部東／東部／北部／広島市／
# 呉市／福山市」という1〜4類感染症の全数把握疾患を保健所別の報告数
# （count、定点当たりではない）で列挙する表に変わっている。空欄セルは
# 「報告なし＝0件」として扱う（実例: 2026-09-16 ユーザー指摘で発覚。
# 旧ロジックでは「対象疾患名」ラベル行と保健所名ヘッダー行が同じy座標に
# あるため`words$y > anchor_y`の厳密不等号がヘッダー行自体を除外してしまい
# 常にエラーになっていた）。

.HIROSHIMA_HOKENJO <- c("西部", "西部東", "東部", "北部", "広島市", "呉市", "福山市")

fetch_hiroshima <- function(pdf_url) {
  if (!exists("pdf_words")) stop("pdf_table_utils.R を先に source してください")

  tmp <- tempfile(fileext = ".pdf")
  download.file(pdf_url, tmp, mode = "wb", quiet = TRUE)
  txt <- pdftools::pdf_text(tmp)[1]

  # 週ラベル（"令和８年第３２週(令和8年8月3日～8月9日)"のような表記、
  # 全角数字・空白混じりのため正規化してから抽出）
  s <- chartr("０１２３４５６７８９", "0123456789", txt)
  s_flat <- gsub("\\s+", "", s)
  wm <- regmatches(s_flat, regexec("令和([0-9]+)年第([0-9]+)週", s_flat))[[1]]
  week_label <- if (length(wm) == 3) sprintf("%d年第%s週", as.integer(wm[2]) + 2018L, wm[3]) else NA_character_

  words <- pdf_words(tmp, page = 1)

  # 保健所名ヘッダー行（西部/西部東/.../福山市が横に並ぶ行）を直接検出する。
  # 「計」列は保健所別内訳の対象外のため、ヘッダー・データ抽出のどちらにも
  # 含めない
  hdr <- words[words$text %in% .HIROSHIMA_HOKENJO, ]
  hdr <- hdr[!duplicated(hdr$text), ]
  if (nrow(hdr) != length(.HIROSHIMA_HOKENJO)) stop("hiroshima: 保健所別ヘッダー行が見つかりません")
  hdr <- hdr[match(.HIROSHIMA_HOKENJO, hdr$text), ]
  header_y <- stats::median(hdr$y)
  xs <- hdr$x
  bounds <- c(xs[1] - (xs[2] - xs[1]) / 2, (xs[1:(length(xs) - 1)] + xs[2:length(xs)]) / 2,
              xs[length(xs)] + (xs[length(xs)] - xs[length(xs) - 1]) / 2)

  # 疾患名列: ヘッダー行より下、「西部」列より手前（＝類別・報告数・疾患名列）
  # にある非数値トークン。類別（一類/二類…）の1文字ラベル列(x<90)は除外する
  below <- words[words$y > header_y + 3, ]
  is_num_tok <- grepl("^[0-9]+(\\.[0-9]+)?$", below$text)
  name_toks_all <- below[!is_num_tok & below$x >= 90 & below$x < bounds[1] - 10, ]
  if (nrow(name_toks_all) == 0) stop("hiroshima: 疾患名が見つかりません")

  # 同じ行(y近接)ごとに疾患名トークンを連結し、疾患ごとの行を作る
  name_toks_all <- name_toks_all[order(name_toks_all$y, name_toks_all$x), ]
  row_id <- cumsum(c(1, diff(name_toks_all$y) > 4))
  out <- list()
  for (rid in unique(row_id)) {
    rr <- name_toks_all[row_id == rid, ]
    disease <- gsub("[[:space:]]", "", paste(rr$text, collapse = ""))
    # 「発生なし」はその類の感染症が0件であることを示すプレースホルダーで
    # 実際の疾患名ではないため除外する
    if (!nzchar(disease) || disease == "発生なし") next
    y0 <- mean(rr$y)
    data_row <- words[abs(words$y - y0) <= 4 & words$x >= bounds[1] - 5, ]
    for (k in seq_along(.HIROSHIMA_HOKENJO)) {
      lo <- bounds[k]; hi <- bounds[k + 1]
      v <- data_row$text[data_row$x >= lo & data_row$x < hi]
      cnt <- if (length(v) > 0) parse_hokenjo_number(v[1]) else 0
      out[[length(out) + 1]] <- data.frame(
        pref = "広島県", week_label = week_label, hokenjo = .HIROSHIMA_HOKENJO[k],
        disease = disease, count = cnt, rate = NA_real_,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, out)
}
