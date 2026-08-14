# 広島県「感染症発生動向週報」PDF
# 例: https://www.pref.hiroshima.lg.jp/uploaded/attachment/678623.pdf
# 注意: このPDFの「保健所別の流行状況（定点当たり）」表は、その週の警報・注意報
# 対象疾患（1疾患のみ）についてしか地区別内訳を掲載していない。
# （他の疾患は県全体の値のみで、地区別分解はPDFに含まれない）
# そのため本パーサーが返すのは基本的に1疾患 x 7地区のみとなる。
# 地区: 西部,西部東,東部,北部,広島市,呉市,福山市

fetch_hiroshima <- function(pdf_url, page = 1) {
  if (!requireNamespace("pdftools", quietly = TRUE)) stop("pdftools が必要です")
  path <- pdf_url
  if (grepl("^https?://", pdf_url)) {
    path <- tempfile(fileext = ".pdf")
    download.file(pdf_url, path, mode = "wb", quiet = TRUE)
  }
  txt <- pdftools::pdf_text(path)
  page_txt <- txt[[page]]
  lines <- strsplit(page_txt, "\n")[[1]]

  page_txt_ascii <- chartr("０１２３４５６７８９", "0123456789", page_txt)
  week_label <- NA_character_
  wl <- regmatches(page_txt_ascii, regexpr("令和[0-9]+年第[0-9]+週", page_txt_ascii))
  if (length(wl) > 0) week_label <- wl

  regions <- c("西部", "西部東", "東部", "北部", "広島市", "呉市", "福山市")

  # 「対象疾患名」ブロック内の、1文字ずつスペースで区切られた疾患名行
  # （例: "手 足 口 病"）を探す。地域名や「基準」等を含む行は除外する。
  known_diseases <- c(
    "手足口病", "インフルエンザ", "新型コロナウイルス感染症", "感染性胃腸炎",
    "ヘルパンギーナ", "伝染性紅斑", "咽頭結膜熱", "RSウイルス感染症", "水痘",
    "Ａ群溶血性レンサ球菌咽頭炎", "A群溶血性レンサ球菌咽頭炎", "流行性耳下腺炎",
    "突発性発しん", "急性出血性結膜炎", "流行性角結膜炎", "細菌性髄膜炎",
    "無菌性髄膜炎", "マイコプラズマ肺炎", "クラミジア肺炎", "急性呼吸器感染症"
  )
  disease <- NA_character_
  idx_disease <- which(grepl("対象疾患名", lines))
  if (length(idx_disease) > 0) {
    for (k in (idx_disease[1] + 1):min(idx_disease[1] + 6, length(lines))) {
      ln_k <- gsub("\\s+", "", lines[k])
      for (dz in known_diseases) {
        if (grepl(dz, ln_k, fixed = TRUE)) { disease <- dz; break }
      }
      if (!is.na(disease)) break
    }
  }

  # 7地区分の定点当たり数値が並ぶ行を探す（小数点付き数値が7個連続する行）
  num_pat <- "([0-9]+\\.[0-9]+|-)"
  rate_line <- NULL
  for (ln in lines) {
    m <- gregexpr(num_pat, ln)[[1]]
    vals <- regmatches(ln, gregexpr(num_pat, ln))[[1]]
    if (length(vals) == 7) { rate_line <- vals; break }
  }

  if (is.null(rate_line) || is.na(disease)) {
    warning("広島県: 地区別データ行または疾患名が見つかりませんでした")
    return(data.frame(pref = character(0), week_label = character(0), hokenjo = character(0),
                       disease = character(0), count = numeric(0), rate = numeric(0)))
  }

  out <- list()
  for (i in seq_along(regions)) {
    out[[i]] <- data.frame(
      pref = "広島県", week_label = week_label, hokenjo = regions[i],
      disease = disease, count = NA_real_, rate = parse_hokenjo_number(rate_line[i]),
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, out)
}
