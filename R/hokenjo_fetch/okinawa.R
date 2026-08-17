# 沖縄県「週報 保健所毎集計」Excel（シート「令和{N}年各保健所毎集計」）
# https://www.pref.okinawa.jp/_res/projects/default_project/_page_/001/006/484/syuuho{MMDD}.xlsx
#
# シート構成: 疾患ごとに「報告数」8行ブロック＋「定点あたり報告数」8行
# ブロックが連続して並ぶ。各ブロックの8行は 北部/中部/那覇市/南部/宮古/
# 八重山/沖縄県(計)/全国。列は週番号（4行目ヘッダー）で、右端の非空
# 列が最新週。

OKINAWA_HOKENJO_ORDER <- c("北部", "中部", "那覇市", "南部", "宮古", "八重山")

.OKINAWA_CURRENT_YEAR <- 2026

fetch_okinawa <- function(url, sheet = NULL, year = .OKINAWA_CURRENT_YEAR) {
  if (!requireNamespace("readxl", quietly = TRUE)) stop("readxl パッケージが必要です")
  tmp <- tempfile(fileext = ".xlsx")
  download.file(url, tmp, mode = "wb", quiet = TRUE)

  if (is.null(sheet)) {
    sheets <- readxl::excel_sheets(tmp)
    sheet <- sheets[grepl("各保健所毎集計", sheets)][1]
    if (is.na(sheet)) stop("「各保健所毎集計」シートが見つかりません")
  }
  d <- suppressMessages(as.data.frame(readxl::read_excel(tmp, sheet = sheet, col_names = FALSE)))

  # 疾患名の行（col1が非NAで、"報告数"/"警報"/"注意報"を含まない行）
  is_label <- !is.na(d[[1]]) & grepl("報告数|警報|注意報", d[[1]])
  disease_name_rows <- which(!is.na(d[[1]]) & !is_label & !grepl("週別疾病別|疾病名", d[[1]]))

  if (length(disease_name_rows) %% 2 != 0) {
    warning("疾患ブロックの行数が偶数になっていません。データがずれている可能性があります")
  }

  # 週ヘッダー行（4行目）から最新の非NA週列を特定
  week_hdr <- suppressWarnings(as.numeric(d[4, 3:ncol(d)]))
  valid_cols <- which(!is.na(week_hdr)) + 2  # 元のd内の列番号に戻す

  out <- list()
  n_pairs <- length(disease_name_rows) %/% 2
  for (p in seq_len(n_pairs)) {
    count_start <- disease_name_rows[(p - 1) * 2 + 1]
    rate_start  <- disease_name_rows[(p - 1) * 2 + 2]
    disease <- trimws(as.character(d[count_start, 1]))

    for (blk_start in c(count_start, rate_start)) {
      is_rate_block <- (blk_start == rate_start)
      for (k in seq_along(OKINAWA_HOKENJO_ORDER)) {
        row_i <- blk_start + k - 1
        if (row_i > nrow(d)) next
        hokenjo_label <- trimws(as.character(d[row_i, 2]))
        if (!identical(hokenjo_label, OKINAWA_HOKENJO_ORDER[k])) next  # 行ズレ検知
        # 最新週＝valid_colsの中で最後に値が入っている列
        vals <- suppressWarnings(as.numeric(d[row_i, valid_cols]))
        filled <- which(!is.na(vals))
        if (length(filled) == 0) next
        last_col <- valid_cols[max(filled)]
        val <- suppressWarnings(as.numeric(d[row_i, last_col]))
        week_num <- suppressWarnings(as.numeric(d[4, last_col]))

        key <- paste(disease, OKINAWA_HOKENJO_ORDER[k])
        idx <- Find(function(i) identical(attr(out[[i]], "key"), key), seq_along(out))
        if (is.null(idx)) {
          row_df <- data.frame(
            pref = "沖縄県", week_label = sprintf("%d年第%d週", year, week_num),
            hokenjo = OKINAWA_HOKENJO_ORDER[k], disease = disease,
            count = if (is_rate_block) NA_real_ else val,
            rate  = if (is_rate_block) val else NA_real_,
            stringsAsFactors = FALSE
          )
          attr(row_df, "key") <- key
          out[[length(out) + 1]] <- row_df
        } else {
          if (is_rate_block) out[[idx]]$rate <- val else out[[idx]]$count <- val
        }
      }
    }
  }
  do.call(rbind, lapply(out, function(x) { attr(x, "key") <- NULL; x }))
}

# fetch_okinawa()は最新週(右端の非NA列)のみを採用するが、シート自体は
# 前年からの連続した週データを含んでいる。バックフィル用に、右端から
# 週番号が1つずつ減っている連続区間（＝当年の第1週〜最新週）を
# すべて抽出するバリアント
fetch_okinawa_history <- function(url, sheet = NULL, current_week = NULL, max_week = 53, year = .OKINAWA_CURRENT_YEAR) {
  if (!requireNamespace("readxl", quietly = TRUE)) stop("readxl パッケージが必要です")
  tmp <- tempfile(fileext = ".xlsx")
  download.file(url, tmp, mode = "wb", quiet = TRUE)

  if (is.null(sheet)) {
    sheets <- readxl::excel_sheets(tmp)
    sheet <- sheets[grepl("各保健所毎集計", sheets)][1]
    if (is.na(sheet)) stop("「各保健所毎集計」シートが見つかりません")
  }
  d <- suppressMessages(as.data.frame(readxl::read_excel(tmp, sheet = sheet, col_names = FALSE)))

  is_label <- !is.na(d[[1]]) & grepl("報告数|警報|注意報", d[[1]])
  disease_name_rows <- which(!is.na(d[[1]]) & !is_label & !grepl("週別疾病別|疾病名", d[[1]]))

  week_hdr <- suppressWarnings(as.numeric(d[4, 3:ncol(d)]))
  valid_cols <- which(!is.na(week_hdr)) + 2

  # 右端(最新週)から左へ、週番号が1ずつ減る連続区間だけを当年分として使う
  ordered <- valid_cols[order(valid_cols)]
  weeks_at <- suppressWarnings(as.numeric(d[4, ordered]))
  n <- length(ordered)
  keep_from <- n
  for (i in seq(n - 1, 1)) {
    if (!is.na(weeks_at[i]) && !is.na(weeks_at[i + 1]) && weeks_at[i] == weeks_at[i + 1] - 1) {
      keep_from <- i
    } else break
  }
  this_year_cols <- ordered[keep_from:n]
  this_year_weeks <- weeks_at[keep_from:n]

  out <- list()
  n_pairs <- length(disease_name_rows) %/% 2
  for (p in seq_len(n_pairs)) {
    count_start <- disease_name_rows[(p - 1) * 2 + 1]
    rate_start  <- disease_name_rows[(p - 1) * 2 + 2]
    disease <- trimws(as.character(d[count_start, 1]))

    for (blk_start in c(count_start, rate_start)) {
      is_rate_block <- (blk_start == rate_start)
      for (k in seq_along(OKINAWA_HOKENJO_ORDER)) {
        row_i <- blk_start + k - 1
        if (row_i > nrow(d)) next
        hokenjo_label <- trimws(as.character(d[row_i, 2]))
        if (!identical(hokenjo_label, OKINAWA_HOKENJO_ORDER[k])) next

        for (ci in seq_along(this_year_cols)) {
          col <- this_year_cols[ci]
          wk <- this_year_weeks[ci]
          val <- suppressWarnings(as.numeric(d[row_i, col]))
          key <- paste(disease, OKINAWA_HOKENJO_ORDER[k], wk)
          idx <- Find(function(i) identical(attr(out[[i]], "key"), key), seq_along(out))
          if (is.null(idx)) {
            row_df <- data.frame(
              pref = "沖縄県", week_label = sprintf("%d年第%d週", year, wk), week_num = wk,
              hokenjo = OKINAWA_HOKENJO_ORDER[k], disease = disease,
              count = if (is_rate_block) NA_real_ else val,
              rate  = if (is_rate_block) val else NA_real_,
              stringsAsFactors = FALSE
            )
            attr(row_df, "key") <- key
            out[[length(out) + 1]] <- row_df
          } else {
            if (is_rate_block) out[[idx]]$rate <- val else out[[idx]]$count <- val
          }
        }
      }
    }
  }
  df <- do.call(rbind, lapply(out, function(x) { attr(x, "key") <- NULL; x }))
  # 週番号の折り返し検出に失敗した場合の保険として、当年の週範囲外
  # (0以下や52を超えるもの)は除外する
  df[df$week_num >= 1 & df$week_num <= max_week, ]
}
