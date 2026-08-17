# 広島県「感染症発生動向週報」PDF（1ページ完結）
# https://www.pref.hiroshima.lg.jp/uploaded/attachment/<id>.pdf
#
# 全疾患・全保健所の内訳は無く、「保健所別の流行状況（定点当たり）」という
# 1つの表に、その週警報・注意報の対象となった1疾患のみ、7保健所
# （西部/西部東/東部/北部/広島市/呉市/福山市）別の定点当たり報告数が
# 掲載される（対象疾患は週によって変わる）。それ以外の疾患は県全体の
# 数値のみで保健所別内訳が無いため対象外。

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

  # 「対象疾患名」ラベルの行を見つけ、その少し下にある保健所名ヘッダー行
  # （西部/西部東/.../福山市が横に並ぶ行のうち、"対象疾患名"より下にある方）
  # を特定する。「疾」と「患」が隣接して現れるyを探すことで、他の場所に
  # 単独で現れうる「対」「象」等の文字と誤認しないようにする
  # ページ上部の疾患一覧表にも「疾患名」という見出しが出るため、それより
  # 下（中段の「対象疾患名」欄）に絞る
  shi <- words[words$text == "疾" & words$y > 300, ]
  kan <- words[words$text == "患" & words$y > 300, ]
  shi <- shi[order(shi$y), ]
  anchor_y <- NA_real_
  for (i in seq_len(nrow(shi))) {
    near_kan <- kan[abs(kan$y - shi$y[i]) <= 2 & kan$x > shi$x[i] & kan$x < shi$x[i] + 20, ]
    if (nrow(near_kan) > 0) { anchor_y <- shi$y[i]; break }
  }
  if (is.na(anchor_y)) stop("hiroshima: 「対象疾患名」ラベルが見つかりません")

  hdr <- words[words$text %in% .HIROSHIMA_HOKENJO & words$y > anchor_y & words$y < anchor_y + 40, ]
  hdr <- hdr[!duplicated(hdr$text), ]
  if (nrow(hdr) != length(.HIROSHIMA_HOKENJO)) stop("hiroshima: 保健所別ヘッダー行が見つかりません")
  hdr <- hdr[match(.HIROSHIMA_HOKENJO, hdr$text), ]
  xs <- hdr$x
  bounds <- c(xs[1] - (xs[2] - xs[1]) / 2, (xs[1:(length(xs) - 1)] + xs[2:length(xs)]) / 2,
              xs[length(xs)] + (xs[length(xs)] - xs[length(xs) - 1]) / 2)
  header_y <- hdr$y[1]

  # ヘッダー行のすぐ下（同じ表の最初のデータ行）に疾患名と定点あたり数値が
  # 並ぶが、その間に「警報基準／注意報基準」という別の小さな見出し行が
  # 挟まることがあるため、実際に保健所列(bounds内)へ数値が入っている行だけを
  # 対象にする
  cand <- words[words$y > header_y & words$y < header_y + 30 &
                  words$x >= bounds[1] & words$x < bounds[length(bounds)], ]
  if (nrow(cand) == 0) stop("hiroshima: データ行が見つかりません")
  data_y <- min(cand$y)
  data_row <- words[abs(words$y - data_y) <= 3, ]

  # 疾患名: 同じ行の先頭付近（警報基準等の数値列より手前）のトークンを連結。
  # 「警報基準／継続基準／注意報基準」の数値もこの行のbounds[1]より左側に
  # あるため、単純にbounds[1]で区切ると疾患名にこれらの数値が混入する。
  # 疾患名の直後（警報基準列の手前）は目視で確認した固定の空白があるため、
  # x<150の範囲に限定する
  name_toks <- data_row[data_row$x < 150, ]
  disease <- gsub("[[:space:]]", "", paste(name_toks$text[order(name_toks$x)], collapse = ""))
  if (!nzchar(disease)) stop("hiroshima: 疾患名が取得できません")

  out <- list()
  for (k in seq_along(.HIROSHIMA_HOKENJO)) {
    lo <- bounds[k]; hi <- bounds[k + 1]
    v <- data_row$text[data_row$x >= lo & data_row$x < hi]
    rate_val <- if (length(v) > 0) parse_hokenjo_number(v[1]) else NA_real_
    out[[length(out) + 1]] <- data.frame(
      pref = "広島県", week_label = week_label, hokenjo = .HIROSHIMA_HOKENJO[k],
      disease = disease, count = NA_real_, rate = rate_val,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, out)
}
