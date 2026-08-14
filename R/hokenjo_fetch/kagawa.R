# 香川県「保健所別報告数内訳」PDF（感染症週報、通常2ページ目）
# 例: https://www.pref.kagawa.lg.jp/documents/7135/2026syuuhou31.pdf
# 各疾患行の末尾10トークンが 高松市,小豆,東讃,中讃,西讃 の(人数,定点)ペア。

fetch_kagawa <- function(pdf_url, page = NULL) {
  if (!requireNamespace("pdftools", quietly = TRUE)) stop("pdftools が必要です")
  path <- pdf_url
  if (grepl("^https?://", pdf_url)) {
    path <- tempfile(fileext = ".pdf")
    download.file(pdf_url, path, mode = "wb", quiet = TRUE)
  }
  txt <- pdftools::pdf_text(path)

  target_page <- page
  if (is.null(target_page)) {
    hit <- which(grepl("保健所別報告数内訳", txt))
    target_page <- if (length(hit) > 0) hit[1] else 2
  }
  page_txt <- txt[[target_page]]
  lines <- strsplit(page_txt, "\n")[[1]]

  week_label <- NA_character_
  wl <- regmatches(page_txt, regexpr("20[0-9]{2}\\s*年\\s*[0-9]+/[0-9]+[～~][0-9]+/[0-9]+", page_txt))
  if (length(wl) > 0) week_label <- gsub("\\s", "", wl)

  hokenjo_order <- c("高松市", "小豆", "東讃", "中讃", "西讃")

  disease_patterns <- c(
    "急性呼吸器感染症", "RSウイルス感染症", "咽頭結膜熱", "Ａ群溶血性レンサ球菌咽頭炎",
    "感染性胃腸炎（ロタウイルス）",
    "感染性胃腸炎", "○ ウイルス性", "○ 細菌性", "水痘", "手足口病", "伝染性紅斑",
    "突発性発しん", "へルパンギーナ", "流行性耳下腺炎",
    "細菌性髄膜炎", "無菌性髄膜炎", "マイコプラズマ肺炎", "クラミジア肺炎（ｵｳﾑ病を除く）",
    "ヒトメタニューモウイルス感染症", "急性出血性結膜炎", "流行性角結膜炎",
    "インフルエンザ", "新型コロナウイルス感染症"
  )

  out <- list()
  used_lines <- rep(FALSE, length(lines))
  stop_idx <- which(grepl("年齢別報告状況", lines))
  max_line <- if (length(stop_idx) > 0) stop_idx[1] - 1 else length(lines)

  for (dz in disease_patterns) {
    candidates <- which(!used_lines[1:max_line] & grepl(dz, lines[1:max_line], fixed = TRUE))
    if (length(candidates) == 0) next
    i <- candidates[1]
    used_lines[i] <- TRUE
    ln <- lines[i]
    after <- sub(paste0(".*", gsub("([().])", "\\\\\\1", dz)), "", ln)
    toks <- strsplit(trimws(after), "\\s+")[[1]]
    toks <- toks[toks != ""]
    if (length(toks) < 10) next
    tail10 <- utils::tail(toks, 10)
    for (j in seq_along(hokenjo_order)) {
      cnt <- tail10[(j - 1) * 2 + 1]
      rt  <- tail10[(j - 1) * 2 + 2]
      out[[length(out) + 1]] <- data.frame(
        pref = "香川県", week_label = week_label, hokenjo = hokenjo_order[j],
        disease = dz, count = parse_hokenjo_number(cnt), rate = parse_hokenjo_number(rt),
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, out)
}
