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

# ============================================================
# 北海道 保健所別 年間バックナンバーCSV
# https://www.iph.pref.hokkaido.jp/kansen/{slug}/weekunitdata{YEAR}.csv
# 保健所ごとのページ（例: otaru/index.html）に「定点把握感染症(週単位報告)」
# セクションがあり、年別のCSVリンクがある（1999年〜現在まで遡及可能）。
# 1ファイルに対象年の全週（定点当たり報告数の表＋報告数の表）が
# 含まれるため、年を指定すればその年の全週を一度に取得できる。
# ============================================================

.HOKKAIDO_HOKENJO_SLUGS <- c(
  sapporo = "札幌市", otaru = "小樽市", hakodate = "市立函館", asahikawa = "旭川市",
  ebetsu = "江別", chitose = "千歳", iwamizawa = "岩見沢", takikawa = "滝川",
  fukagawa = "深川", furano = "富良野", nayoro = "名寄", iwanai = "岩内",
  kucchan = "倶知安", esashi = "江差", oshima = "渡島", yakumo = "八雲",
  muroran = "室蘭", tomakomai = "苫小牧", urakawa = "浦河", shizunai = "静内",
  obihiro = "帯広", kushiro = "釧路", nemuro = "根室", nakashibetsu = "中標津",
  abashiri = "網走", kitami = "北見", monbetsu = "紋別", wakkanai = "稚内",
  rumoi = "留萌", kamikawa = "上川"
)

.hokkaido_parse_weekunit_csv <- function(lines, hokenjo, year) {
  block_starts <- grep("^(定点当たり報告数|報告数)-", lines)
  if (length(block_starts) < 2) return(NULL)

  parse_block <- function(start_idx, metric) {
    header <- strsplit(lines[start_idx + 1], ",", fixed = TRUE)[[1]]
    week_cols <- header[-1]
    week_cols <- week_cols[grepl("第[0-9]+週", week_cols)]
    n_week <- length(week_cols)
    week_nums <- as.integer(gsub(".*第([0-9]+)週.*", "\\1", week_cols))

    out <- list()
    i <- start_idx + 2
    while (i <= length(lines) && nzchar(lines[i]) && !grepl("^(定点当たり報告数|報告数)-", lines[i])) {
      toks <- strsplit(lines[i], ",", fixed = TRUE)[[1]]
      disease <- trimws(toks[1])
      if (nzchar(disease)) {
        vals <- toks[2:(1 + n_week)]
        for (w in seq_len(n_week)) {
          out[[length(out) + 1]] <- data.frame(
            pref = "北海道", week_label = sprintf("%d年第%02d週", year, week_nums[w]),
            week_num = week_nums[w], hokenjo = hokenjo, disease = disease,
            metric = metric, value = parse_hokenjo_number(vals[w]),
            stringsAsFactors = FALSE
          )
        }
      }
      i <- i + 1
    }
    do.call(rbind, out)
  }

  rate_df <- parse_block(block_starts[1], "rate")
  count_df <- parse_block(block_starts[2], "count")
  merged <- merge(count_df[, c("week_num", "hokenjo", "disease", "value")],
                   rate_df[, c("week_num", "hokenjo", "disease", "value")],
                   by = c("week_num", "hokenjo", "disease"), suffixes = c("_count", "_rate"))
  wl <- unique(rate_df[, c("week_num", "week_label")])
  merged <- merge(merged, wl, by = "week_num")
  data.frame(
    pref = "北海道", week_label = merged$week_label, week_num = merged$week_num,
    hokenjo = merged$hokenjo, disease = merged$disease,
    count = merged$value_count, rate = merged$value_rate,
    stringsAsFactors = FALSE
  )
}

fetch_hokkaido_history <- function(year = 2026, slugs = names(.HOKKAIDO_HOKENJO_SLUGS)) {
  out <- list()
  for (slug in slugs) {
    hokenjo <- .HOKKAIDO_HOKENJO_SLUGS[[slug]]
    url <- sprintf("https://www.iph.pref.hokkaido.jp/kansen/%s/weekunitdata%d.csv", slug, year)
    res <- tryCatch({
      tmp <- tempfile(fileext = ".csv")
      download.file(url, tmp, mode = "wb", quiet = TRUE)
      raw_lines <- readLines(tmp, encoding = "CP932", warn = FALSE)
      raw_lines <- iconv(raw_lines, from = "CP932", to = "UTF-8")
      .hokkaido_parse_weekunit_csv(raw_lines, hokenjo, year)
    }, error = function(e) {
      message(sprintf("[NG] 北海道 %s(%s) %d年: %s", hokenjo, slug, year, conditionMessage(e)))
      NULL
    })
    if (!is.null(res) && nrow(res) > 0) out[[length(out) + 1]] <- res
  }
  do.call(rbind, out)
}
