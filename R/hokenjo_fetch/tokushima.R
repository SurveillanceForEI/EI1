# 徳島県「定点把握対象疾患 報告数」PDF（週報、通常5ページ目）
# 例: https://www.pref.tokushima.lg.jp/file/attachment/1008847.pdf
# 保健所別（徳島,阿南,美波,吉野川,美馬,三好）の「報告数」（人数のみ、定点当たりなし）
# 行末は [徳島,阿南,美波,吉野川,美馬,三好, 全国前週, 全国累計] の順で8トークンなので、
# 行内の数値トークンの末尾8個のうち先頭6個を保健所別報告数として使う。

fetch_tokushima <- function(pdf_url, page = NULL) {
  if (!requireNamespace("pdftools", quietly = TRUE)) stop("pdftools が必要です")
  path <- pdf_url
  if (grepl("^https?://", pdf_url)) {
    path <- tempfile(fileext = ".pdf")
    download.file(pdf_url, path, mode = "wb", quiet = TRUE)
  }
  txt <- pdftools::pdf_text(path)

  target_page <- page
  if (is.null(target_page)) {
    hit <- which(grepl("定点把握対象疾患\\s*報告数", txt) & !grepl("週別報告数|年齢階級別", txt))
    target_page <- if (length(hit) > 0) hit[1] else 5
  }
  page_txt <- txt[[target_page]]
  lines <- strsplit(page_txt, "\n")[[1]]

  week_label <- NA_character_
  wl <- regmatches(page_txt, regexpr("20[0-9]{2}年第[0-9]+週", page_txt))
  if (length(wl) > 0) week_label <- wl

  hokenjo_order <- c("徳島", "阿南", "美波", "吉野川", "美馬", "三好")

  disease_patterns <- c(
    "急性呼吸器感染症", "インフルエンザ", "新型コロナウイルス感染症",
    "RSウイルス感染症", "咽頭結膜熱", "A群溶血性レンサ球菌咽頭炎",
    "感染性胃腸炎（ロタウイルス）", "感染性胃腸炎", "水痘", "手足口病",
    "伝染性紅斑", "突発性発しん", "ヘルパンギーナ", "流行性耳下腺炎",
    "急性出血性結膜炎", "流行性角結膜炎", "細菌性髄膜炎", "無菌性髄膜炎",
    "マイコプラズマ肺炎", "クラミジア肺炎"
  )

  out <- list()
  used_lines <- rep(FALSE, length(lines))
  for (dz in disease_patterns) {
    idx <- which(!used_lines & grepl(dz, lines, fixed = TRUE))
    if (length(idx) == 0) next
    i <- idx[1]
    used_lines[i] <- TRUE
    ln <- lines[i]
    # 病名以降の部分を取り出す
    after <- sub(paste0(".*", gsub("([().])", "\\\\\\1", dz)), "", ln)
    toks <- strsplit(trimws(after), "\\s+")[[1]]
    toks <- toks[toks != ""]
    if (length(toks) < 8) next
    tail8 <- utils::tail(toks, 8)
    hk_counts <- tail8[1:6]
    for (j in seq_along(hokenjo_order)) {
      out[[length(out) + 1]] <- data.frame(
        pref = "徳島県", week_label = week_label, hokenjo = hokenjo_order[j],
        disease = dz, count = parse_hokenjo_number(hk_counts[j]), rate = NA_real_,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, out)
}
