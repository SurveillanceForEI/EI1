source("R/hokenjo_fetch/hokenjo_fetch_schema.R")
OKINAWA_HOKENJO_ORDER <- c("北部", "中部", "那覇市", "南部", "宮古", "八重山")

url <- "https://www.pref.okinawa.jp/_res/projects/default_project/_page_/001/006/484/syuuho0832.xlsx"
tmp <- tempfile(fileext = ".xlsx")
download.file(url, tmp, mode = "wb", quiet = TRUE)
d <- suppressMessages(as.data.frame(readxl::read_excel(tmp, sheet = "令和8年各保健所毎集計", col_names = FALSE)))

is_label <- !is.na(d[[1]]) & grepl("報告数|警報|注意報", d[[1]])
disease_name_rows <- which(!is.na(d[[1]]) & !is_label & !grepl("週別疾病別|疾病名", d[[1]]))

# 年ラベル("2025年"/"2026年")の列位置から2025年の列範囲を特定
yr_row <- as.character(unlist(d[1, ]))
start_col <- which(yr_row == "2025年")
end_marker <- which(yr_row == "2026年")
stopifnot(length(start_col) == 1, length(end_marker) == 1)
this_year_cols <- start_col:(end_marker - 1)
this_year_weeks <- suppressWarnings(as.numeric(d[4, this_year_cols]))
stopifnot(all(this_year_weeks == 1:52))

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
            pref = "沖縄県", week_label = sprintf("2025年第%d週", wk), week_num = wk,
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
cat("rows:", nrow(df), "weeks:", paste(sort(unique(df$week_num)), collapse=","), "\n")

h <- readRDS("data/hokenjo_history.rds")
df$fetched_at <- as.character(Sys.time())
common_cols <- intersect(names(h), names(df))
combined <- rbind(h[, common_cols], df[, common_cols])
saveRDS(combined, "data/hokenjo_history.rds")
cat("saved. total:", nrow(combined), "\n")
