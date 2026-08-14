# 三重県感染症情報センター「定点あたり保健所管内別患者報告数」HTML表
# https://www.kenkou.pref.mie.jp/weekly_fp_new.html
#
# ページには「定点あたり報告数（rate）」と「定点数（カテゴリ別の定点医療
# 機関数）」の両方が掲載されているが、報告実数（count）は直接載っていない。
# count = round(rate × 該当カテゴリの定点数) で算出する。
# rvest::html_table(fill=TRUE) はrowspanを自動展開してくれるため、
# カテゴリ列(X1)が各行に伝播した状態で取得できる。

MIE_HOKENJO_ORDER <- c("桑名", "四日市市", "鈴鹿", "津", "松阪", "伊勢", "伊賀", "尾鷲", "熊野")

# 定点数の行の疾患ラベル → データ行のカテゴリラベルへの対応
# 「小児科（含独自）」は小児科定点・独自定点の両カテゴリ分の定点数を兼ねる
.mie_teiten_category_map <- list(
  "急性呼吸器感染症" = "ARI",
  "小児科（含独自）" = c("小児科", "独自"),
  "眼　　　　　科"   = "眼科",
  "基　　　　　幹"   = "基幹"
)

fetch_mie <- function(url = "https://www.kenkou.pref.mie.jp/weekly_fp_new.html") {
  if (!requireNamespace("rvest", quietly = TRUE)) stop("rvest パッケージが必要です")
  page <- rvest::read_html(url, encoding = "UTF-8")
  tabs <- rvest::html_table(page, fill = TRUE)

  target <- NULL
  for (t in tabs) {
    if (any(grepl("定点あたり保健所管内別患者報告数", t[[1]]))) { target <- t; break }
  }
  if (is.null(target)) stop("対象の表が見つかりません")
  target <- as.data.frame(target, stringsAsFactors = FALSE)
  names(target) <- paste0("V", seq_len(ncol(target)))

  week_m <- regmatches(rvest::html_text(page), regexpr("20[0-9]{2}年第[0-9]+週", rvest::html_text(page)))
  week_label <- if (length(week_m) > 0) week_m[1] else NA_character_

  hokenjo_cols <- setNames(3:11, MIE_HOKENJO_ORDER)  # V3..V11 = 9保健所

  # 定点数テーブルを作る: teiten[[category]][[hokenjo]] = n
  teiten <- list()
  teiten_rows <- which(target$V1 == "定点数")
  for (r in teiten_rows) {
    label <- trimws(target$V2[r])
    cats <- .mie_teiten_category_map[[label]]
    if (is.null(cats)) next
    for (cat in cats) {
      vals <- setNames(parse_hokenjo_number(target[r, hokenjo_cols]), names(hokenjo_cols))
      teiten[[cat]] <- vals
    }
  }

  data_rows <- which(!(target$V1 %in% c("定点あたり保健所管内別患者報告数", "定点数")) &
                        nchar(trimws(target$V1)) > 0)
  out <- list()
  for (r in data_rows) {
    cat_name <- trimws(target$V1[r])
    disease <- trimws(target$V2[r])
    if (nchar(disease) == 0) next
    teiten_vals <- teiten[[cat_name]]
    for (h in names(hokenjo_cols)) {
      rate_val <- parse_hokenjo_number(target[r, hokenjo_cols[[h]]])
      n_teiten <- if (!is.null(teiten_vals)) teiten_vals[[h]] else NA_real_
      count_val <- if (!is.na(rate_val) && !is.na(n_teiten)) round(rate_val * n_teiten) else NA_real_
      out[[length(out) + 1]] <- data.frame(
        pref = "三重県", week_label = week_label, hokenjo = h,
        disease = disease, count = count_val, rate = rate_val,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, out)
}
