setwd(if (basename(getwd()) == "scripts") ".." else ".")
source("R/hokenjo_fetch/hokenjo_fetch_schema.R")
source("R/hokenjo_fetch/pdf_table_utils.R")
row_text <- function(rdf) paste(rdf$text, collapse = " ")
source("R/hokenjo_fetch/shiga.R")
HISTORY_PATH <- "data/hokenjo_history.rds"
h <- readRDS(HISTORY_PATH)
new_rows <- list()
status_log <- character(0)

shiga_ids <- c(
  5512737,5513249,5514894,5516859,5518603,5519552,5520964,5522540,5523978,5525784,
  5527901,5530147,5536985,5541931,5536989,5536990,5539334,5541932,5541812,5543690,
  5543692,5544899,5547541,5547542,5551055,5551056,5552412,5553932,5555536,5556675,
  5557826,5558282,5559341,5560635,5563208,5563213,5564416,5565515,5566963,5570204,
  5570206,5571740,5572692,5573665,5574813,5576055,5576938,5578165,5579161,5580470,
  5582187,5583519
)
for (i in seq_along(shiga_ids)) {
  week <- i
  u <- sprintf("https://www.pref.shiga.lg.jp/file/attachment/%d.pdf", shiga_ids[i])
  res <- tryCatch(fetch_shiga(u, page = 3), error = function(e) {
    tryCatch(fetch_shiga(u, page = 4), error = function(e2) {
      status_log <<- c(status_log, sprintf("[NG] 滋賀県 第%d週: p3=%s / p4=%s", week, conditionMessage(e), conditionMessage(e2)))
      NULL
    })
  })
  if (is.null(res) || nrow(res) == 0) next
  res$week_num <- week
  res$fetched_at <- as.character(Sys.time())
  new_rows[[length(new_rows) + 1]] <- res
  status_log <- c(status_log, sprintf("[OK] 滋賀県 第%d週 (%d行) label=%s", week, nrow(res), unique(res$week_label)[1]))
}

cat("\n=== 実行ログ（2025年 第22弾：滋賀県） ===\n")
cat(paste(status_log, collapse = "\n"), "\n")

if (length(new_rows) > 0) {
  added <- do.call(rbind, new_rows)
  common_cols <- intersect(names(h), names(added))
  combined <- rbind(h[, common_cols], added[, common_cols])
  saveRDS(combined, HISTORY_PATH)
  cat(sprintf("\n追記: %d行（新規） / 累計: %d行\n", nrow(added), nrow(combined)))
}
