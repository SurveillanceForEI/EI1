# 北海道「定点把握感染症（週報）」CSV
# https://www.iph.pref.hokkaido.jp/kansen/weekunitdata.csv
# 固定URL（内容は毎週更新される）。Shift-JISエンコード。
# 構造:
#   1行目: "2026年 第31週" のような年週見出し
#   2行目: 先頭列空 + 疾病名（2列ずつ: 報告数, 定点当たり）
#   3行目: 先頭列空 + "報告数","定点当たり" の繰り返し
#   4行目以降: 地域名（全国, 北海道, 札幌市, 小樽市, ... 各保健所）+ 数値

fetch_hokkaido <- function(csv_url = "https://www.iph.pref.hokkaido.jp/kansen/weekunitdata.csv") {
  tmp <- tempfile(fileext = ".csv")
  download.file(csv_url, tmp, mode = "wb", quiet = TRUE)
  raw_lines <- readLines(tmp, encoding = "CP932", warn = FALSE)
  raw_lines <- iconv(raw_lines, from = "CP932", to = "UTF-8")

  week_label <- gsub("\\s+", "", raw_lines[1])
  week_label <- sub("^(\\d{4}年)第", "\\1第", week_label)

  header_disease <- strsplit(raw_lines[2], ",", fixed = TRUE)[[1]]
  # 疾病名は2列ごとに1つ、空文字を後方に埋める
  diseases <- character(length(header_disease))
  last <- NA_character_
  for (i in seq_along(header_disease)) {
    v <- trimws(header_disease[i])
    if (nzchar(v)) last <- v
    diseases[i] <- last
  }
  # 1列目（地域名列）は除く
  diseases <- diseases[-1]

  data_lines <- raw_lines[4:length(raw_lines)]
  data_lines <- data_lines[nzchar(trimws(data_lines))]

  # 除外する集計行（全国・北海道全体）。保健所別のみ残す
  exclude_regions <- c("全国", "北海道")

  out <- list()
  for (ln in data_lines) {
    toks <- strsplit(ln, ",", fixed = TRUE)[[1]]
    region <- trimws(toks[1])
    if (!nzchar(region) || region %in% exclude_regions) next
    vals <- toks[-1]
    n <- min(length(diseases), floor(length(vals) / 2))
    for (k in seq_len(n)) {
      out[[length(out) + 1]] <- data.frame(
        pref = "北海道", week_label = week_label, hokenjo = region,
        disease = diseases[(k - 1) * 2 + 1],
        count = parse_hokenjo_number(vals[(k - 1) * 2 + 1]),
        rate  = parse_hokenjo_number(vals[(k - 1) * 2 + 2]),
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, out)
}
