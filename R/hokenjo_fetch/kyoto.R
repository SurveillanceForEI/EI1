# 京都府 感染症情報センター「地域別集計【週報（定点把握）】」CSV
# https://www.pref.kyoto.jp/idsc/data/week/area-table/{YEAR}/documents/{YEAR}{WEEK}_2-2-2.csv
#
# 列構成が時期によって2パターンある（性別タイプ,疾患名 の後）:
#   (A) 36列形式: 境界データ(kyoto.geojson)と同じ12保健所単位で既に統合済み
#       （北・左京,上京・中京・下京,東山・山科,南・伏見,右京・西京,乙訓,
#         山城北,山城南,南丹,中丹西,中丹東,丹後）+ 5集計列(京都市/京都市
#         以外/京都府/近畿/全国) を報告数・定点あたりそれぞれに持つ
#   (B) 48列形式: 12保健所へ統合される前の生の18保健所単位
#       （北,上京,左京,中京,東山,山科,下京,南,右京,伏見,西京,乙訓,山城北,
#         山城南,南丹,中丹西,中丹東,丹後）+ 同じ5集計列を報告数・定点あたり
#       それぞれに持つ。この場合、統合対象は報告数を合算し、定点あたりは
#       各エリアの定点数が個別に取得できないため単純平均を近似値として使う
# 列数(ncol)でどちらの形式か判定する。

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

  hokenjo_names <- c("北・左京", "上京・中京・下京", "東山・山科", "南・伏見",
                     "右京・西京", "乙訓", "山城北", "山城南", "南丹",
                     "中丹西", "中丹東", "丹後")

  d <- d[d[[1]] == 0, ]  # 性別タイプ=0（総数）のみ使用
  out <- list()

  if (ncol(d) == 36) {
    # (A) 既に12保健所単位で統合済み
    count_cols <- 3:14
    rate_cols  <- 20:31
    for (i in seq_len(nrow(d))) {
      disease <- d[i, 2]
      for (j in seq_along(hokenjo_names)) {
        out[[length(out) + 1]] <- data.frame(
          pref = "京都府", week_label = week_label, hokenjo = hokenjo_names[j],
          disease = disease,
          count = parse_hokenjo_number(d[i, count_cols[j]]),
          rate  = parse_hokenjo_number(d[i, rate_cols[j]]),
          stringsAsFactors = FALSE
        )
      }
    }
  } else {
    # (B) 生の18保健所単位（列3〜20=報告数、列26〜43=定点あたり）
    raw_areas <- c("北", "上京", "左京", "中京", "東山", "山科", "下京", "南",
                   "右京", "伏見", "西京", "乙訓", "山城北", "山城南", "南丹",
                   "中丹西", "中丹東", "丹後")
    raw_count_cols <- setNames(3:20, raw_areas)
    raw_rate_cols  <- setNames(26:43, raw_areas)
    hokenjo_group <- list(
      "北・左京"         = c("北", "左京"),
      "上京・中京・下京" = c("上京", "中京", "下京"),
      "東山・山科"       = c("東山", "山科"),
      "南・伏見"         = c("南", "伏見"),
      "右京・西京"       = c("右京", "西京"),
      "乙訓"             = c("乙訓"),
      "山城北"           = c("山城北"),
      "山城南"           = c("山城南"),
      "南丹"             = c("南丹"),
      "中丹西"           = c("中丹西"),
      "中丹東"           = c("中丹東"),
      "丹後"             = c("丹後")
    )
    for (i in seq_len(nrow(d))) {
      disease <- d[i, 2]
      for (hokenjo_name in names(hokenjo_group)) {
        members <- hokenjo_group[[hokenjo_name]]
        cnts <- vapply(members, function(m) parse_hokenjo_number(d[i, raw_count_cols[[m]]]), numeric(1))
        rates <- vapply(members, function(m) parse_hokenjo_number(d[i, raw_rate_cols[[m]]]), numeric(1))
        out[[length(out) + 1]] <- data.frame(
          pref = "京都府", week_label = week_label, hokenjo = hokenjo_name,
          disease = disease,
          count = if (all(is.na(cnts))) NA_real_ else sum(cnts, na.rm = TRUE),
          rate  = if (all(is.na(rates))) NA_real_ else mean(rates, na.rm = TRUE),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  do.call(rbind, out)
}
