# 山口県感染症情報システム（DIDSS/NESID）週報PDF
# 例: https://pref.yamaguchi.didss.dsvc.jp/files/report/week/weeklyreport_y2026w32.pdf
# 2ページ目「【保健所別】定点あたりの報告数」表を使用（定点当たりのみ、報告数（人数）はなし）
# 保健所: 下関,岩国,柳井,周南,防府,山口,宇部,長門,萩

fetch_yamaguchi <- function(pdf_url, page = 2) {
  if (!requireNamespace("pdftools", quietly = TRUE)) stop("pdftools が必要です")
  path <- pdf_url
  if (grepl("^https?://", pdf_url)) {
    path <- tempfile(fileext = ".pdf")
    download.file(pdf_url, path, mode = "wb", quiet = TRUE)
  }
  txt <- pdftools::pdf_text(path)
  page_txt <- txt[[page]]
  lines <- strsplit(page_txt, "\n")[[1]]

  week_label <- NA_character_
  wl <- regmatches(page_txt, regexpr("[0-9]{4}年第[0-9]+週", page_txt))
  if (length(wl) > 0) week_label <- wl

  hokenjo_order <- c("下関", "岩国", "柳井", "周南", "防府", "山口", "宇部", "長門", "萩")

  disease_patterns <- c(
    "インフルエンザ", "新型コロナウイルス感染症", "RSウイルス感染症", "咽頭結膜熱",
    "A群溶血性レンサ球菌咽頭炎", "感染性胃腸炎（ロタウイルス）", "感染性胃腸炎",
    "水痘", "手足口病", "伝染性紅斑", "突発性発疹", "ヘルパンギーナ",
    "流行性耳下腺炎", "急性出血性結膜炎", "流行性角結膜炎", "クラミジア肺炎",
    "細菌性髄膜炎", "マイコプラズマ肺炎", "無菌性髄膜炎", "急性呼吸器感染症（ARI）"
  )

  # 【保健所別】表の範囲に限定（年齢階級別セクションの前まで）
  start_idx <- which(grepl("【保健所別】.*定点あたりの報告数", lines))
  s <- if (length(start_idx) > 0) start_idx[1] else 1
  end_idx <- which(grepl("○ 【年齢階級別】", lines) & seq_along(lines) > s)
  e <- if (length(end_idx) > 0) end_idx[1] - 1 else length(lines)
  block <- lines[s:e]

  out <- list()
  used <- rep(FALSE, length(block))
  for (dz in disease_patterns) {
    idx <- which(!used & grepl(dz, block, fixed = TRUE))
    if (length(idx) == 0) next
    i <- idx[1]
    used[i] <- TRUE
    ln <- block[i]
    after <- sub(paste0(".*", gsub("([().])", "\\\\\\1", dz)), "", ln)
    toks <- strsplit(trimws(after), "\\s+")[[1]]
    toks <- toks[toks != ""]
    # 数値（0や小数含む）のみを残し、右側の別表見出し等の混入を除去
    toks <- toks[grepl("^[0-9]+(\\.[0-9]+)?$", toks)]
    if (length(toks) < 10) next
    # 先頭が「山口県」全体値、続く9個が保健所別（下関,岩国,柳井,周南,防府,山口,宇部,長門,萩）
    hk9 <- toks[2:10]
    for (j in seq_along(hokenjo_order)) {
      out[[length(out) + 1]] <- data.frame(
        pref = "山口県", week_label = week_label, hokenjo = hokenjo_order[j],
        disease = dz, count = NA_real_, rate = parse_hokenjo_number(hk9[j]),
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, out)
}
