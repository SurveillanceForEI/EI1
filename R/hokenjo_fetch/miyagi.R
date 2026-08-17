# 宮城県「感染症発生動向調査情報」週報PDF
# https://www.pref.miyagi.jp/documents/1967/<filename>.pdf
# （URLファイル名が週ごとに命名規則がバラバラなため、呼び出し側で解決すること）
#
# レイアウト: p.1の表に、疾患ごとに「患者数」行→「定点あたり」行のペアが
# 縦に並ぶ。列は 仙南/塩釜/大崎/石巻/気仙沼/仙台市 の6保健所＋
# 県計/累計の集計列。疾患名は固定19種（表記も安定）で、単一トークンとして
# 抽出できるため、固定リストを座標のyアンカーとして使う。値が0件の保健所は
# 空欄（トークン無し）になるため、インフルエンザ行（全保健所で必ず値がある）の
# 実測x座標を列アンカーとして使い、最近傍トークンを割り当てる。
# 空欄は0件として扱う（ユーザー指示）。

.MIYAGI_HOKENJO <- c("仙南", "塩釜", "大崎", "石巻", "気仙沼", "仙台市")

.MIYAGI_DISEASES <- c(
  "インフルエンザ", "新型コロナウイルス感染症", "ＲＳウイルス感染症", "咽頭結膜熱",
  "Ａ群溶血性レンサ球菌咽頭炎", "感染性胃腸炎", "水痘", "手足口病", "伝染性紅斑",
  "突発性発しん", "ヘルパンギーナ", "流行性耳下腺炎", "急性出血性結膜炎",
  "流行性角結膜炎", "感染性胃腸炎（ロタウイルス）", "ｸﾗﾐｼﾞｱ肺炎(ｵｳﾑ病は除く)",
  "細菌性髄膜炎(真菌性を含む)", "ﾏｲｺﾌﾟﾗｽﾞﾏ肺炎", "無菌性髄膜炎"
)

fetch_miyagi <- function(pdf_url) {
  if (!exists("pdf_words")) stop("pdf_table_utils.R を先に source してください")

  tmp <- tempfile(fileext = ".pdf")
  download.file(pdf_url, tmp, mode = "wb", quiet = TRUE)
  txt <- pdftools::pdf_text(tmp)[1]

  # 週番号は本文中の「－ 第N週 －」表記、年は発行日の令和表記から取る
  # （日付範囲は期間開始日が年またぎのことがあり、発行年とは限らないため）
  s <- chartr("０１２３４５６７８９", "0123456789", txt)
  s_flat <- gsub("\\s+", "", s)
  week_label <- NA_character_
  wnum_m <- regmatches(s_flat, regexec("第([0-9]+)週", s_flat))[[1]]
  yr_m <- regmatches(s_flat, regexec("令和([0-9]+)年", s_flat))[[1]]
  if (length(yr_m) == 2 && length(wnum_m) == 2) {
    week_label <- sprintf("%d年第%s週", as.integer(yr_m[2]) + 2018L, wnum_m[2])
  }

  words <- pdf_words(tmp, page = 1)

  hdr <- words[words$text %in% .MIYAGI_HOKENJO & words$y < 100, ]
  hdr <- hdr[!duplicated(hdr$text), ]
  if (nrow(hdr) != length(.MIYAGI_HOKENJO)) stop("miyagi: 保健所別ヘッダー行が見つかりません")
  hdr <- hdr[match(.MIYAGI_HOKENJO, hdr$text), ]
  xs <- hdr$x
  n <- length(.MIYAGI_HOKENJO)

  is_num <- function(t) grepl("^[0-9,.]+$", t)

  # 個々の疾患行だけでは欠測（0件で空欄）がありうるため、ページ全体の数値トークンの
  # x座標をクラスタリングして列アンカーを求める（値が多い週ほど密になり、
  # どの週でも安定して6保健所+集計2列分のクラスタが得られる）
  all_num <- words[words$x >= xs[1] - 30 & is_num(words$text), ]
  if (nrow(all_num) < n) stop("miyagi: 数値トークンが不足しています（アンカー計算不可）")
  ux <- sort(unique(round(all_num$x)))
  clusters <- list()
  cur <- ux[1]
  for (v in ux[-1]) {
    if (v - cur[length(cur)] <= 8) cur <- c(cur, v) else { clusters[[length(clusters) + 1]] <- cur; cur <- v }
  }
  clusters[[length(clusters) + 1]] <- cur
  cluster_centers <- sapply(clusters, function(cl) {
    toks <- all_num[round(all_num$x) %in% cl, ]
    weighted.mean(toks$x, w = rep(1, nrow(toks)))
  })
  cluster_centers <- sort(cluster_centers)
  if (length(cluster_centers) < n) stop("miyagi: 列クラスタ数が不足しています")
  cnt_anchor_xs <- cluster_centers[seq_len(n)]
  rate_anchor_xs <- cnt_anchor_xs

  nearest_match <- function(row, anchor_xs) {
    vals <- rep(NA_character_, length(anchor_xs))
    if (nrow(row) == 0) return(vals)
    for (t in seq_len(nrow(row))) {
      d <- abs(anchor_xs - row$x[t])
      k <- which.min(d)
      if (d[k] <= 15 && is.na(vals[k])) vals[k] <- row$text[t]
    }
    vals
  }

  out <- list()
  for (disease in .MIYAGI_DISEASES) {
    hit <- words[words$text == disease & words$x < 150, ]
    if (nrow(hit) == 0) next
    dy <- hit$y[1]

    # 疾患名のyは「患者数行」と「定点あたり行」の間に位置する
    # （患者数がやや上、定点あたりがやや下）
    cnt_row <- words[words$y >= dy - 6 & words$y < dy & words$x >= xs[1] - 30 & is_num(words$text), ]
    rate_row <- words[words$y > dy & words$y <= dy + 8 & words$x >= xs[1] - 30 & is_num(words$text), ]
    cnt_vals <- nearest_match(cnt_row, cnt_anchor_xs)
    rate_vals <- nearest_match(rate_row, rate_anchor_xs)

    # 空欄（値トークンなし）は0件と判断する（ユーザー指示）
    for (k in seq_along(.MIYAGI_HOKENJO)) {
      cnt_val <- if (!is.na(cnt_vals[k])) parse_hokenjo_number(cnt_vals[k]) else 0
      rate_val <- if (!is.na(rate_vals[k])) parse_hokenjo_number(rate_vals[k]) else 0
      out[[length(out) + 1]] <- data.frame(
        pref = "宮城県", week_label = week_label, hokenjo = .MIYAGI_HOKENJO[k],
        disease = disease, count = cnt_val, rate = rate_val,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, out)
}
