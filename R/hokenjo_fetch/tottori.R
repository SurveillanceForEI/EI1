# 鳥取県「感染症流行情報」週報PDF p.3（地区別）
# https://www.pref.tottori.lg.jp/secure/519458/R8W{WEEK}_hp.pdf
#
# p.1の記号表（×△○◎★★）はサマリーに過ぎず、p.3に東部/中部/西部の
# 全20疾患の実数（報告数）と前週比のテキスト表がある。定点数は
# ARI/小児科/眼科/基幹の4カテゴリ別に東部/中部/西部で示されており、
# rate（定点あたり報告数）は直接記載がないため count÷該当カテゴリの
# 定点数 で算出する。

TOTTORI_HOKENJO_ORDER <- c("東部", "中部", "西部")

TOTTORI_DISEASE_LIST <- c(
  "インフルエンザ", "新型コロナウイルス感染症", "咽頭結膜熱",
  "A群溶血性レンサ球菌咽頭炎", "感染性胃腸炎", "水痘", "手足口病",
  "伝染性紅斑", "突発性発疹", "ヘルパンギーナ", "流行性耳下腺炎",
  "RSウイルス感染症", "急性出血性結膜炎", "流行性角結膜炎",
  "細菌性髄膜炎", "無菌性髄膜炎", "マイコプラズマ肺炎",
  "クラミジア肺炎（オウム病を除く）", "感染性胃腸炎（病原体がロタウイルスであるものに限る。）",
  "急性呼吸器感染症(ARI)"
)

TOTTORI_DISEASE_CATEGORY <- c(
  "インフルエンザ" = "ARI", "新型コロナウイルス感染症" = "ARI",
  "咽頭結膜熱" = "小児科", "A群溶血性レンサ球菌咽頭炎" = "小児科",
  "感染性胃腸炎" = "小児科", "水痘" = "小児科", "手足口病" = "小児科",
  "伝染性紅斑" = "小児科", "突発性発疹" = "小児科", "ヘルパンギーナ" = "小児科",
  "流行性耳下腺炎" = "小児科", "RSウイルス感染症" = "小児科",
  "急性出血性結膜炎" = "眼科", "流行性角結膜炎" = "眼科",
  "細菌性髄膜炎" = "基幹", "無菌性髄膜炎" = "基幹", "マイコプラズマ肺炎" = "基幹",
  "クラミジア肺炎（オウム病を除く）" = "基幹",
  "感染性胃腸炎（病原体がロタウイルスであるものに限る。）" = "基幹",
  "急性呼吸器感染症(ARI)" = "ARI"
)

.tottori_num_tokens <- function(line) {
  toks <- strsplit(trimws(line), "\\s+")[[1]]
  toks <- toks[!grepl("%$", toks)]
  # 「－」「-」は報告数0を表すため、列位置を保つためにトークンとして残したまま0に置換する
  toks[toks %in% c("－", "-")] <- "0"
  toks <- gsub(",", "", toks)
  suppressWarnings(as.numeric(toks[grepl("^[0-9]+(\\.[0-9]+)?$", toks)]))
}

fetch_tottori <- function(pdf_url) {
  if (!requireNamespace("pdftools", quietly = TRUE)) stop("pdftools パッケージが必要です")
  tmp <- tempfile(fileext = ".pdf")
  download.file(pdf_url, tmp, mode = "wb", quiet = TRUE)
  txt <- pdftools::pdf_text(tmp)

  page3 <- txt[grepl("地区別", txt) & grepl("定点数", txt)][1]
  if (is.na(page3)) stop("地区別データのページが見つかりません")
  lines <- strsplit(page3, "\n")[[1]]

  week_m <- regmatches(page3, regexpr("第[0-9]+週", page3))
  week_label <- if (length(week_m) > 0) paste0("2026年", week_m[1]) else NA_character_

  # カテゴリ別定点数の行を取得（1回目に出現するものを採用）
  teiten <- list()
  cat_defs <- list("ARI" = "急性呼吸器感染症\\(ARI\\)定点数",
                    "小児科" = "小児科定点数", "眼科" = "眼科定点数", "基幹" = "基幹定点数")
  for (cat in names(cat_defs)) {
    idx <- grep(cat_defs[[cat]], lines)[1]
    if (is.na(idx)) next
    vals <- .tottori_num_tokens(lines[idx])
    if (length(vals) >= 3) teiten[[cat]] <- setNames(vals[1:3], TOTTORI_HOKENJO_ORDER)
  }

  out <- list()
  for (disease in TOTTORI_DISEASE_LIST) {
    esc <- gsub("([()])", "\\\\\\1", disease)
    idx <- grep(paste0("^\\s*[0-9]+\\s*", esc), lines)
    if (length(idx) == 0) idx <- grep(esc, lines, fixed = FALSE)
    if (length(idx) == 0) next
    # 行頭の連番（1〜20）と疾患名を取り除いてから数値トークンを抽出する
    line_body <- sub(paste0("^\\s*[0-9]+\\s*", esc), "", lines[idx[1]])
    if (identical(line_body, lines[idx[1]])) line_body <- sub(esc, "", lines[idx[1]])
    vals <- .tottori_num_tokens(line_body)
    if (length(vals) < 3) next
    counts <- vals[1:3]
    cat_name <- TOTTORI_DISEASE_CATEGORY[[disease]]
    teiten_vals <- teiten[[cat_name]]
    for (i in seq_along(TOTTORI_HOKENJO_ORDER)) {
      n_teiten <- if (!is.null(teiten_vals)) teiten_vals[[i]] else NA_real_
      rate_val <- if (!is.na(counts[i]) && !is.na(n_teiten) && n_teiten > 0) round(counts[i] / n_teiten, 2) else NA_real_
      out[[length(out) + 1]] <- data.frame(
        pref = "鳥取県", week_label = week_label,
        hokenjo = TOTTORI_HOKENJO_ORDER[i], disease = disease,
        count = counts[i], rate = rate_val,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, out)
}
