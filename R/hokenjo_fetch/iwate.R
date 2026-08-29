# 岩手県 保健所別感染症情報（速報Excel、「保健所」シート）
# https://www2.pref.iwate.jp/~hp1353/kansen/image/sokuhou.xlsx
# 以前は分布図（GIF画像に実数値を描画したもの）を画像解析する方針だったが、
# 同じサイト内に保健所別データを機械可読（xlsx）で提供しているシートが
# あることが判明したため、そちらを直接パースする（2026-08-29実装）。
#
# 「保健所」シートの構成（ヘッダー1行+疾患ごとに1行、最新週のみ）:
#   列1-3: 疾病コード, 報告週(週番号のみ、年の記載なし), 疾病名
#   列4-15: 定点あたり報告数（岩手県計, 盛岡市, 県央, 中部, [空列],
#           奥州, 一関, 大船渡, 釜石, 宮古, 久慈, 二戸）
#   列16-27: 実数（同じ並び順）
# 年の記載がないため、取得時点のシステム年を用いる
# （本ファイルの最新週のみを提供する仕様のため、実運用上は問題にならない）。

IWATE_SOKUHOU_URL <- "https://www2.pref.iwate.jp/~hp1353/kansen/image/sokuhou.xlsx"

.IWATE_HOKENJO_COLS <- c(
  "盛岡市" = 5, "県央" = 6, "中部" = 7, "奥州" = 9, "一関" = 10,
  "大船渡" = 11, "釜石" = 12, "宮古" = 13, "久慈" = 14, "二戸" = 15
)

fetch_iwate <- function(url = IWATE_SOKUHOU_URL) {
  if (!requireNamespace("readxl", quietly = TRUE)) stop("readxl パッケージが必要です")

  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)
  utils::download.file(url, tmp, mode = "wb", quiet = TRUE)

  d <- suppressMessages(readxl::read_excel(tmp, sheet = "保健所", col_names = FALSE))
  if (nrow(d) < 2) stop("岩手県: 保健所シートにデータ行がありません")

  hdr <- as.character(d[1, ])
  if (!identical(hdr[3], "疾病名") || !identical(hdr[2], "報告週")) {
    stop("岩手県: 保健所シートの列構成が想定と異なります（サイト側の様式変更の可能性）")
  }

  data_rows <- d[-1, ]
  data_rows <- data_rows[!is.na(data_rows[[3]]), ]
  if (nrow(data_rows) == 0) stop("岩手県: 疾患データ行が見つかりません")

  week_num <- suppressWarnings(as.integer(data_rows[[2]][1]))
  if (is.na(week_num)) stop("岩手県: 報告週番号を取得できませんでした")
  year <- as.integer(format(Sys.Date(), "%Y"))
  week_label <- sprintf("%d年第%d週", year, week_num)

  out <- list()
  for (r in seq_len(nrow(data_rows))) {
    disease <- as.character(data_rows[[3]][r])
    if (is.na(disease) || !nzchar(disease)) next
    for (hokenjo in names(.IWATE_HOKENJO_COLS)) {
      rate_col  <- .IWATE_HOKENJO_COLS[[hokenjo]]
      count_col <- rate_col + 12L  # 実数列は定点あたり列の12列右
      rate  <- suppressWarnings(as.numeric(data_rows[[rate_col]][r]))
      count <- suppressWarnings(as.numeric(data_rows[[count_col]][r]))
      out[[length(out) + 1]] <- data.frame(
        pref = "岩手県", week_label = week_label, hokenjo = hokenjo,
        disease = disease, count = count, rate = rate,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, out)
}
