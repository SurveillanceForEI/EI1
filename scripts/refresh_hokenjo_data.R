# ============================================================
# 保健所別データ 一括更新スクリプト
# ------------------------------------------------------------
# R/hokenjo_fetch/*.R の各県パーサーを呼び出し、直近週の
# 「保健所別×疾患別」count/rateを取得して1つのdata.frameにまとめ、
# data/hokenjo_current.rds に保存する。
#
# 各県のfetch_*()関数は引数の取り方が異なる（PDF URLを1つ取るもの、
# year/weekを取るもの、固定URLで呼べるものなど）ため、都道府県ごとに
# 「引数なしで直近週を取得する」ラッパー関数を定義している。
#
# 実行方法: Rscript scripts/refresh_hokenjo_data.R
# ============================================================

setwd(if (basename(getwd()) == "scripts") ".." else ".")
source("R/hokenjo_fetch/hokenjo_fetch_schema.R")
source("R/hokenjo_fetch/pdf_table_utils.R")
source("R/hokenjo_data_sources.R")

FETCH_DIR <- "R/hokenjo_fetch"
for (f in list.files(FETCH_DIR, pattern = "\\.R$", full.names = TRUE)) {
  if (basename(f) %in% c("hokenjo_fetch_schema.R", "pdf_table_utils.R")) next
  source(f)
}

# 都道府県名 → 引数なしラッパー関数（直近週の週報URLは基本的に
# hokenjo_data_sources.R の sample_url をそのまま使う。CSV/Excel/
# 固定URL系はfetch_*()自体が引数なしで最新週を返す設計のため
# そのまま呼ぶ）。scripts/refresh_hokenjo_history.R（夜間自動実行）
# とも共有するため、定義自体はR/hokenjo_refresh_dispatch.Rに集約している
# （2県別々にメンテすると、片方だけ直してもう片方がstaleなURLの
# ままになる事故が起きるため）
source("R/hokenjo_refresh_dispatch.R")

results <- list()
status <- list()
for (pref in names(HOKENJO_REFRESH_DISPATCH)) {
  res <- tryCatch(HOKENJO_REFRESH_DISPATCH[[pref]](), error = function(e) {
    message(sprintf("[NG] %s: %s", pref, conditionMessage(e)))
    NULL
  })
  if (!is.null(res) && nrow(res) > 0) {
    results[[pref]] <- res
    status[[pref]] <- sprintf("OK (%d行)", nrow(res))
  } else {
    status[[pref]] <- "NG (0行 or エラー)"
  }
}

cat("\n=== 更新結果 ===\n")
for (pref in names(status)) cat(sprintf("%-8s %s\n", pref, status[[pref]]))

all_data <- do.call(rbind, results)
if (!is.null(all_data)) {
  saveRDS(all_data, "data/hokenjo_current.rds")
  cat(sprintf("\n保存完了: data/hokenjo_current.rds （%d県分、%d行）\n", length(results), nrow(all_data)))
} else {
  cat("\n取得できたデータがありませんでした\n")
}
