# 富山県「定点把握感染症発生状況」PDF（厚生センター別）
# https://www.pref.toyama.jp/documents/32640/teiten_hc_2632w.pdf 型
# レイアウト: 疾患ごとに1ブロック（1ページに1～2疾患）。データ行は
# "No 地域名 定点当たり報告数(週27..32) 対前週差 報告数(週27..32) 定点数(週27..32)" の
# 固定幅レイアウトで、pdftools::pdf_text() が列位置を保った1行テキストを返すため、
# ここでは pdf_text() の行を正規表現で解析する（pdf_data()の単語クラスタリングでは
# グラフ注釈と表データの y 座標が近接し行が混線するため、本PDFに限り pdf_text を使用）。
# 全11ページ、1ページに1～2疾患ブロック、最新週（末尾の週）の値のみ採用する。

fetch_toyama <- function(pdf_url = "https://www.pref.toyama.jp/documents/32640/teiten_hc_2632w.pdf") {
  if (!requireNamespace("pdftools", quietly = TRUE)) stop("pdftools パッケージが必要です")

  tmp <- tempfile(fileext = ".pdf")
  download.file(pdf_url, tmp, mode = "wb", quiet = TRUE)
  pages <- pdftools::pdf_text(tmp)

  exclude_regions <- c("富山県")
  out <- list()

  for (page_txt in pages) {
    lines <- strsplit(page_txt, "\n")[[1]]

    # ブロックの先頭（ヘッダー行）を探す
    header_idx <- grep("集計区分.*第[0-9]+週", lines)
    for (h in header_idx) {
      weeks <- regmatches(lines[h], gregexpr("第[0-9]+週", lines[h]))[[1]]
      # ヘッダー行には[定点当たり報告数]/[報告数]/[定点数]の3ブロック分、
      # 同じ週番号が繰り返し出現するため重複を除く
      week_nums <- unique(as.integer(gsub("[^0-9]", "", weeks)))
      if (length(week_nums) == 0) next
      latest_week <- max(week_nums)
      week_label <- sprintf("2026年第%d週", latest_week)

      # 疾患名: ヘッダー行より上で直近の非空行のうち、"定点"で終わるページタイトル行や
      # "["を含む行、"公開日"を含む行を除いたもの
      disease <- NA_character_
      k <- h - 1
      while (k >= 1) {
        t <- trimws(lines[k])
        if (nzchar(t) && !grepl("\\[|公開日|定点$|定点当たり報告数|報告数|定点数", t)) {
          # 同一行に別要素（グラフ軸ラベル等）が2つ以上のスペースで並ぶ場合は先頭のみ採用
          disease <- strsplit(t, "\\s{2,}")[[1]][1]
          break
        }
        k <- k - 1
      }
      if (is.na(disease)) next

      # データ行: "No 地域名 数値..." の形式が続く限り読む
      j <- h + 1
      while (j <= length(lines)) {
        ln <- lines[j]
        if (!nzchar(trimws(ln))) { j <- j + 1; next }
        m <- regmatches(ln, regexec("^\\s*([0-9]+)\\s+(\\S+)\\s+(.*)$", ln))[[1]]
        if (length(m) == 0) break
        region <- m[3]
        rest <- trimws(m[4])
        nums <- strsplit(rest, "\\s+")[[1]]
        if (length(nums) < 13) break
        if (!(region %in% exclude_regions)) {
          latest_rate  <- parse_hokenjo_number(nums[6])
          latest_count <- parse_hokenjo_number(nums[13])
          out[[length(out) + 1]] <- data.frame(
            pref = "富山県", week_label = week_label, hokenjo = region,
            disease = disease, count = latest_count, rate = latest_rate,
            stringsAsFactors = FALSE
          )
        }
        j <- j + 1
      }
    }
  }
  do.call(rbind, out)
}

# fetch_toyama()は表内の最新週（右端列）のみを採用するが、実際には
# 表に直近6週間分（同ページ内に横並び）が掲載されているため、
# 過去分バックフィル用にその6週すべてを返すバリアント
fetch_toyama_history <- function(pdf_url = "https://www.pref.toyama.jp/documents/32640/teiten_hc_2632w.pdf", year = 2026) {
  if (!requireNamespace("pdftools", quietly = TRUE)) stop("pdftools パッケージが必要です")
  tmp <- tempfile(fileext = ".pdf")
  download.file(pdf_url, tmp, mode = "wb", quiet = TRUE)
  pages <- pdftools::pdf_text(tmp)

  exclude_regions <- c("富山県")
  out <- list()

  for (page_txt in pages) {
    lines <- strsplit(page_txt, "\n")[[1]]
    header_idx <- grep("集計区分.*第[0-9]+週", lines)
    for (h in header_idx) {
      weeks <- regmatches(lines[h], gregexpr("第[0-9]+週", lines[h]))[[1]]
      # ヘッダー行には[定点当たり報告数]/[報告数]/[定点数]の3ブロック分、
      # 同じ週番号が繰り返し出現するため重複を除く
      week_nums <- unique(as.integer(gsub("[^0-9]", "", weeks)))
      n_wk <- length(week_nums)
      if (n_wk == 0) next

      disease <- NA_character_
      k <- h - 1
      while (k >= 1) {
        t <- trimws(lines[k])
        if (nzchar(t) && !grepl("\\[|公開日|定点$|定点当たり報告数|報告数|定点数", t)) {
          disease <- strsplit(t, "\\s{2,}")[[1]][1]
          break
        }
        k <- k - 1
      }
      if (is.na(disease)) next

      j <- h + 1
      while (j <= length(lines)) {
        ln <- lines[j]
        if (!nzchar(trimws(ln))) { j <- j + 1; next }
        m <- regmatches(ln, regexec("^\\s*([0-9]+)\\s+(\\S+)\\s+(.*)$", ln))[[1]]
        if (length(m) == 0) break
        region <- m[3]
        rest <- trimws(m[4])
        nums <- strsplit(rest, "\\s+")[[1]]
        if (length(nums) < n_wk * 2 + 1) { j <- j + 1; next }
        if (!(region %in% exclude_regions)) {
          rates  <- parse_hokenjo_number(nums[1:n_wk])
          counts <- parse_hokenjo_number(nums[(n_wk + 2):(n_wk * 2 + 1)])
          for (wi in seq_len(n_wk)) {
            out[[length(out) + 1]] <- data.frame(
              pref = "富山県", week_label = sprintf("%d年第%d週", year, week_nums[wi]),
              week_num = week_nums[wi], hokenjo = region,
              disease = disease, count = counts[wi], rate = rates[wi],
              stringsAsFactors = FALSE
            )
          }
        }
        j <- j + 1
      }
    }
  }
  do.call(rbind, out)
}
