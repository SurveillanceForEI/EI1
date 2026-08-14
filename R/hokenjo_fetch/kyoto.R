# 京都府 感染症情報センター「地域別集計【週報（定点把握）】」CSV
# https://www.pref.kyoto.jp/idsc/data/week/area-table/{YEAR}/documents/{YEAR}{WEEK}_2-2-2.csv

fetch_kyoto <- function(year = NULL, week = NULL, week_url = NULL) {
  if (is.null(week_url)) {
    if (is.null(year) || is.null(week)) stop("year/week または week_url を指定してください")
    week_url <- sprintf("https://www.pref.kyoto.jp/idsc/data/week/area-table/%d/documents/%d%02d_2-2-2.csv",
                         year, year, week)
  }
  tmp <- tempfile(fileext = ".csv")
  download.file(week_url, tmp, mode = "wb", quiet = TRUE)
  raw_bytes <- readLines(tmp, warn = FALSE, encoding = "unknown")
  raw <- iconv(raw_bytes, from = "shift-jis", to = "UTF-8", sub = "")
  con <- textConnection(raw)
  d <- read.csv(con, header = FALSE, stringsAsFactors = FALSE, skip = 3)
  close(con)

  # 1行目に週表示（"2026年31週　定点報告（週）地域別集計"）
  week_label <- sub("　.*$", "", raw[1])

  # 列構成: col1=性別タイプ, col2=疾患名, col3-19=報告数（12地域＋京都市/京都市以外/京都府/近畿/全国）
  #         col20-36=定点あたり（同じ地域順）
  area_names <- c("北・左京", "上京・中京・下京", "東山・山科", "南・伏見", "右京・西京",
                   "乙訓", "山城北", "山城南", "南丹", "中丹西", "中丹東", "丹後")
  # 実列位置はヘッダー行（skip対象外の3行目）から本来検出すべきだが、
  # 京都府のCSVは列構成が固定のため、確認済みの列番号を直接使用する
  count_cols <- 3:14   # 12地域の報告数列
  rate_cols  <- 20:31  # 12地域の定点あたり列（都市/府/近畿/全国列を挟んだ後）

  d <- d[d[[1]] == 0, ]  # 性別タイプ=0（総数）のみ使用
  out <- list()
  for (i in seq_len(nrow(d))) {
    disease <- d[i, 2]
    for (j in seq_along(area_names)) {
      out[[length(out) + 1]] <- data.frame(
        pref = "京都府", week_label = week_label, hokenjo = area_names[j],
        disease = disease,
        count = parse_hokenjo_number(d[i, count_cols[j]]),
        rate  = parse_hokenjo_number(d[i, rate_cols[j]]),
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, out)
}
