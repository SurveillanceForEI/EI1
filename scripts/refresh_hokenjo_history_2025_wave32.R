setwd(if (basename(getwd()) == "scripts") ".." else ".")
source("R/hokenjo_fetch/hokenjo_fetch_schema.R")
source("R/hokenjo_fetch/pdf_table_utils.R")
source("R/hokenjo_fetch/miyazaki.R")
HISTORY_PATH <- "data/hokenjo_history.rds"
h <- readRDS(HISTORY_PATH)

u <- "https://www.pref.miyazaki.lg.jp/contents/org/fukushi/eikanken/center/infectious/pdf/202452-202501.pdf"
res <- fetch_miyazaki(u)
res$week_num <- 1
res$fetched_at <- as.character(Sys.time())
cat("[OK] 第1週 (", nrow(res), "行) label=", unique(res$week_label)[1], "\n")

common_cols <- intersect(names(h), names(res))
combined <- rbind(h[, common_cols], res[, common_cols])
saveRDS(combined, HISTORY_PATH)
cat("追記:", nrow(res), "行(新規) / 累計:", nrow(combined), "行\n")
