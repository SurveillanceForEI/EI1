# 愛知県「愛知県感染症情報」週報PDF（保健所別・定点把握感染症報告数）
# https://www.pref.aichi.jp/eiseiken/kansen/{YEAR}/{YEAR}{WEEK}.pdf
#
# レイアウト: 「愛知県(保健所別)」という見出しの表があり、列は
# 「定点数(ARI/小児科/眼科/STD/基幹の5列)」＋「疾患別報告数（22列）」
# の合計27列。ヘッダーは1文字ずつ縦書きで並んでおり文字抽出が困難な
# ため、疾患名は国基準の定点把握対象疾患の標準順序をハードコードして
# 対応する（列位置はx座標で判定するため数値の列ズレ（空欄）には対応
# できる）。
#
# rate（定点当たり報告数）は表に直接記載がないため、count÷該当カテゴリ
# の定点数で算出する。STDカテゴリの定点数は本リストの22疾患には
# 対応する疾患がないため使用しない。

AICHI_DISEASE_COLS <- c(
  "急性呼吸器感染症(ARI)", "インフルエンザ", "COVID-19", "RSウイルス感染症",
  "咽頭結膜熱", "Ａ群溶血性レンサ球菌咽頭炎", "感染性胃腸炎", "水痘",
  "手足口病", "伝染性紅斑", "突発性発疹", "ヘルパンギーナ",
  "流行性耳下腺炎", "急性出血性結膜炎", "流行性角結膜炎", "細菌性髄膜炎",
  "無菌性髄膜炎", "マイコプラズマ肺炎", "クラミジア肺炎(オウム病を除く。)",
  "感染性胃腸炎(ロタウイルスによるものに限る。)",
  "インフルエンザ菌感染症(髄膜炎に限る。)", "COVID-19(入院患者に限る。)"
)

# 各疾患が属する定点カテゴリ（定点数列の対応付け用）
AICHI_DISEASE_CATEGORY <- c(
  "急性呼吸器感染症(ARI)" = "ARI", "インフルエンザ" = "ARI", "COVID-19" = "ARI",
  "RSウイルス感染症" = "小児科", "咽頭結膜熱" = "小児科",
  "Ａ群溶血性レンサ球菌咽頭炎" = "小児科", "感染性胃腸炎" = "小児科",
  "水痘" = "小児科", "手足口病" = "小児科", "伝染性紅斑" = "小児科",
  "突発性発疹" = "小児科", "ヘルパンギーナ" = "小児科", "流行性耳下腺炎" = "小児科",
  "急性出血性結膜炎" = "眼科", "流行性角結膜炎" = "眼科",
  "細菌性髄膜炎" = "基幹", "無菌性髄膜炎" = "基幹", "マイコプラズマ肺炎" = "基幹",
  "クラミジア肺炎(オウム病を除く。)" = "基幹",
  "感染性胃腸炎(ロタウイルスによるものに限る。)" = "基幹",
  "インフルエンザ菌感染症(髄膜炎に限る。)" = "基幹",
  "COVID-19(入院患者に限る。)" = "基幹"
)

# 定点数5列の並び順（PDF内での実際の列順。x座標昇順で確認済み）
AICHI_TEITEN_CATEGORY_ORDER <- c("ARI", "小児科", "眼科", "STD", "基幹")

AICHI_HOKENJO_ORDER <- c("名古屋市", "瀬戸", "津島", "清須", "一宮市", "春日井",
                          "江南", "半田", "知多", "岡崎市", "衣浦東部", "西尾",
                          "豊田市", "豊橋市", "豊川", "新城")

fetch_aichi <- function(pdf_url = NULL, year = NULL, week = NULL, page = NULL) {
  if (is.null(pdf_url)) {
    if (is.null(year) || is.null(week)) stop("pdf_url または year/week を指定してください")
    pdf_url <- sprintf("https://www.pref.aichi.jp/eiseiken/kansen/%d/%d%02d.pdf", year, year, week)
  }
  if (!exists("pdf_words")) stop("pdf_table_utils.R を先に source してください")

  # ページ番号未指定なら「定点数」を含むページを自動検出
  if (is.null(page)) {
    tmp <- tempfile(fileext = ".pdf")
    # 愛知県のサーバーはUser-Agent未指定のリクエストを403で拒否するため付与する
    download.file(pdf_url, tmp, mode = "wb", quiet = TRUE,
                  headers = c(`User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
    txt <- pdftools::pdf_text(tmp)
    cand <- which(grepl("定点数", txt) & grepl("愛知県全体", txt))
    if (length(cand) == 0) cand <- which(grepl("定点数", txt))
    if (length(cand) == 0) stop("「定点数」を含むページが見つかりません")
    page <- cand[1]
  }

  words <- pdf_words(pdf_url, page = page)

  week_m <- regmatches(paste(words$text, collapse = " "),
                        regexpr("20[0-9]{2}年[0-9]+週", paste(words$text, collapse = " ")))
  week_label <- if (length(week_m) > 0) week_m[1] else NA_character_

  # 「愛知県全体」行のy座標を基準行として使う
  base_hits <- words[words$text == "愛知県全体", ]
  if (nrow(base_hits) == 0) stop("基準行(愛知県全体)が見つかりません。ページ番号を確認してください")
  base_row_y <- base_hits$y[1]
  base_row <- words[words$y > base_row_y - 4 & words$y < base_row_y + 4 & words$x > base_hits$x[1] + 5, ]
  base_row <- base_row[order(base_row$x), ]
  col_x <- base_row$x
  if (length(col_x) != 27) {
    warning(sprintf("想定列数27に対し%d列検出。ズレの可能性あり", length(col_x)))
  }

  teiten_col_x <- col_x[1:min(5, length(col_x))]                # 定点数5列
  disease_col_x <- col_x[6:min(27, length(col_x))]               # 疾患別報告数列
  diseases <- AICHI_DISEASE_COLS[seq_along(disease_col_x)]

  assign_col <- function(x, ref_x) which.min(abs(ref_x - x))

  out <- list()
  for (hokenjo in AICHI_HOKENJO_ORDER) {
    ch1 <- substr(hokenjo, 1, 1)
    name_hits <- words[words$x < 100 & words$text == ch1, ]
    if (nrow(name_hits) == 0) next
    row_y <- name_hits$y[1]
    rowband <- words[words$y > row_y - 5 & words$y < row_y + 5 & words$x >= teiten_col_x[1] - 15, ]
    rowband <- rowband[grepl("^[0-9,．.０-９-]+$", rowband$text) | rowband$text %in% c("-", "－"), ]
    if (nrow(rowband) == 0) next

    # まず定点数5列を取得（カテゴリ名→値）
    teiten_vals <- setNames(rep(NA_real_, length(AICHI_TEITEN_CATEGORY_ORDER)), AICHI_TEITEN_CATEGORY_ORDER)
    teiten_band <- rowband[rowband$x < disease_col_x[1] - 15, ]
    for (i in seq_len(nrow(teiten_band))) {
      w <- teiten_band[i, ]
      ci <- assign_col(w$x, teiten_col_x)
      if (ci <= length(AICHI_TEITEN_CATEGORY_ORDER)) {
        teiten_vals[AICHI_TEITEN_CATEGORY_ORDER[ci]] <- parse_hokenjo_number(w$text)
      }
    }

    disease_band <- rowband[rowband$x >= disease_col_x[1] - 15 & rowband$x <= disease_col_x[length(disease_col_x)] + 20, ]
    for (i in seq_len(nrow(disease_band))) {
      w <- disease_band[i, ]
      ci <- assign_col(w$x, disease_col_x)
      disease <- diseases[ci]
      cnt <- parse_hokenjo_number(w$text)
      cat_name <- AICHI_DISEASE_CATEGORY[[disease]]
      teiten_n <- teiten_vals[[cat_name]]
      rate_val <- if (!is.na(cnt) && !is.na(teiten_n) && teiten_n > 0) round(cnt / teiten_n, 2) else NA_real_
      out[[length(out) + 1]] <- data.frame(
        pref = "愛知県", week_label = week_label, hokenjo = hokenjo,
        disease = disease,
        count = cnt,
        rate = rate_val,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, out)
}
