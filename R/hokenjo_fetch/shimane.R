# 島根県感染症情報提供システム（DIDSS）週報PDF
# 例: https://pref.shimane.didss.dsvc.jp/files/report/week/weeklyreport_y2026w32.pdf
# 2ページ目=定点あたり報告数（直近3週）、3ページ目=報告実数（直近3週）
# 医療圏域: 松江,雲南,出雲,大田,浜田,益田,隠岐（保健所境界データの圏域名に対応）
# 各行は 地域(松江,雲南,出雲,大田,浜田,益田,隠岐,島根県) x 3週分の値がこの順で並ぶ。
# 症状の増減記号（○×△◎）はデータ抽出前に除去する。

fetch_shimane <- function(pdf_url) {
  if (!requireNamespace("pdftools", quietly = TRUE)) stop("pdftools が必要です")
  path <- pdf_url
  if (grepl("^https?://", pdf_url)) {
    path <- tempfile(fileext = ".pdf")
    download.file(pdf_url, path, mode = "wb", quiet = TRUE)
  }
  txt <- pdftools::pdf_text(path)
  if (length(txt) < 3) stop("島根県: ページ数が想定と異なります(3ページ以上を想定)")

  # 定点あたり報告数/報告実数の表は常に2,3ページ目とは限らない
  # （概況ページが挟まり後ろにずれることがある）ため、タイトル文言で
  # ページを探す
  # 概況ページの本文にも「定点あたり報告数」という語句が出現することが
  # あるため、実際の集計表ページであることを示す「感染症発生動向調査
  # 情報」というタイトル文言も併せて要求する
  rate_page_idx <- which(grepl("感染症発生動向調査情報", txt) & grepl("定点あたり報告数", txt) & !grepl("報告実数", txt))[1]
  count_page_idx <- which(grepl("感染症発生動向調査情報", txt) & grepl("報告実数", txt))[1]
  if (is.na(rate_page_idx) || is.na(count_page_idx)) stop("島根県: 定点あたり報告数/報告実数のページが見つかりません")

  page_rate  <- txt[[rate_page_idx]]
  page_count <- txt[[count_page_idx]]

  week_label <- NA_character_
  wl <- regmatches(page_rate, regexpr("[0-9]{4}年\\s*第[0-9]+週", page_rate))
  if (length(wl) > 0) week_label <- gsub("\\s", "", wl)

  regions <- c("松江", "雲南", "出雲", "大田", "浜田", "益田", "隠岐")

  disease_patterns <- c(
    "インフルエンザ", "新型コロナ感染症", "急性呼吸器感染症", "RSウイルス感染症",
    "咽頭結膜熱", "Ａ群溶連菌咽頭炎", "感染性胃腸炎(ロタ)", "感染性胃腸炎",
    "水\\s*痘", "手足口病", "伝染性紅斑", "突発性発しん", "ヘルパンギーナ",
    "流行性耳下腺炎", "急性出血性結膜炎", "流行性角結膜炎", "細菌性髄膜炎",
    "無菌性髄膜炎", "マイコプラズマ肺炎", "クラミジア肺炎"
  )
  disease_names <- c(
    "インフルエンザ", "新型コロナ感染症", "急性呼吸器感染症", "RSウイルス感染症",
    "咽頭結膜熱", "Ａ群溶連菌咽頭炎", "感染性胃腸炎(ロタ)", "感染性胃腸炎",
    "水痘", "手足口病", "伝染性紅斑", "突発性発しん", "ヘルパンギーナ",
    "流行性耳下腺炎", "急性出血性結膜炎", "流行性角結膜炎", "細菌性髄膜炎",
    "無菌性髄膜炎", "マイコプラズマ肺炎", "クラミジア肺炎"
  )

  extract_week3 <- function(page_txt) {
    lines <- strsplit(page_txt, "\n")[[1]]
    # 増減記号を除去し、数値(または-)のみのトークン列を得る
    res <- list()
    used <- rep(FALSE, length(lines))
    for (di in seq_along(disease_patterns)) {
      pat <- disease_patterns[di]
      idx <- which(!used & grepl(pat, lines))
      if (length(idx) == 0) next
      i <- idx[1]
      used[i] <- TRUE
      ln <- lines[i]
      ln_clean <- gsub("[○×△◎]", "", ln)
      # 病名以降を切り出す
      after <- sub(paste0(".*?", pat), "", ln_clean, perl = TRUE)
      toks <- strsplit(trimws(after), "\\s+")[[1]]
      toks <- toks[toks != ""]
      toks <- toks[grepl("^-$|^[0-9]+(\\.[0-9]+)?$", toks)]
      if (length(toks) < 21) next
      # 7地域 x 3週 (先頭21個) + 島根県計3個
      vals <- list()
      for (r in seq_along(regions)) {
        w3 <- toks[(r - 1) * 3 + 3]  # 当該週（3週分のうち3番目）
        vals[[regions[r]]] <- w3
      }
      res[[disease_names[di]]] <- vals
    }
    res
  }

  rate_data  <- extract_week3(page_rate)
  count_data <- extract_week3(page_count)

  out <- list()
  for (dz in names(rate_data)) {
    for (region in regions) {
      rate_v  <- rate_data[[dz]][[region]]
      count_v <- if (!is.null(count_data[[dz]])) count_data[[dz]][[region]] else NA
      out[[length(out) + 1]] <- data.frame(
        pref = "島根県", week_label = week_label, hokenjo = region,
        disease = dz, count = parse_hokenjo_number(count_v), rate = parse_hokenjo_number(rate_v),
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, out)
}
