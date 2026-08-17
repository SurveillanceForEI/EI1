setwd(if (basename(getwd()) == "scripts") ".." else ".")
source("R/hokenjo_fetch/hokenjo_fetch_schema.R")
source("R/hokenjo_fetch/pdf_table_utils.R")
row_text <- function(rdf) paste(rdf$text, collapse = " ")
source("R/hokenjo_fetch/chiba.R")
source("R/hokenjo_fetch/nagasaki.R")
HISTORY_PATH <- "data/hokenjo_history.rds"
h <- readRDS(HISTORY_PATH)

# 千葉県: 第8週のみ古いロジック時点のデータがweek_label=NAのまま残っていた
before_chiba <- sum(h$pref == "千葉県" & is.na(h$week_label))
h <- h[!(h$pref == "千葉県" & is.na(h$week_label)), ]
res_chiba <- tryCatch(fetch_chiba("https://www.pref.chiba.lg.jp/eiken/c-idsc/documents/wr2508.pdf"), error = function(e) NULL)
new_rows <- list()
status_log <- character(0)
if (!is.null(res_chiba) && nrow(res_chiba) > 0) {
  res_chiba$week_num <- 8
  res_chiba$fetched_at <- as.character(Sys.time())
  new_rows[[length(new_rows) + 1]] <- res_chiba
  status_log <- c(status_log, sprintf("[OK] 千葉県 第8週 (%d行) label=%s (旧NA行%d件を置換)", nrow(res_chiba), unique(res_chiba$week_label)[1], before_chiba))
}

# 長崎県: 表紙(p.1)からも年を拾うようnagasaki.Rを修正したので、
# 孤立していた既存のNA行(200行)を削除し、最新版で再取得
before_nagasaki <- sum(h$pref == "長崎県" & is.na(h$week_label))
h <- h[!(h$pref == "長崎県" & is.na(h$week_label)), ]
res_naga <- tryCatch(fetch_nagasaki("https://www.pref.nagasaki.jp/fs/3/3/4/7/0/_/2026__31__7_27___8_2______.pdf"), error = function(e) NULL)
if (!is.null(res_naga) && nrow(res_naga) > 0) {
  res_naga$week_num <- 31
  res_naga$fetched_at <- as.character(Sys.time())
  new_rows[[length(new_rows) + 1]] <- res_naga
  status_log <- c(status_log, sprintf("[OK] 長崎県 第31週 (%d行) label=%s (旧NA行%d件を置換)", nrow(res_naga), unique(res_naga$week_label)[1], before_nagasaki))
}

cat("\n=== 実行ログ(2025年 第28弾:千葉県・長崎県 NA行修正) ===\n")
cat(paste(status_log, collapse = "\n"), "\n")

if (length(new_rows) > 0) {
  added <- do.call(rbind, new_rows)
  common_cols <- intersect(names(h), names(added))
  combined <- rbind(h[, common_cols], added[, common_cols])
  saveRDS(combined, HISTORY_PATH)
  cat(sprintf("\n追記: %d行(新規) / 累計: %d行\n", nrow(added), nrow(combined)))
}
