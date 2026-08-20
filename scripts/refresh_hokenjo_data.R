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

.sample_url <- function(pref) {
  src <- Find(function(x) x$pref == pref, HOKENJO_DATA_SOURCES)
  if (is.null(src)) NA_character_ else src$sample_url
}

# 都道府県名 → 引数なしラッパー関数（直近週の週報URLは基本的に
# hokenjo_data_sources.R の sample_url をそのまま使う。CSV/Excel/
# 固定URL系はfetch_*()自体が引数なしで最新週を返す設計のため
# そのまま呼ぶ）
HOKENJO_REFRESH_DISPATCH <- list(
  "北海道"   = function() fetch_hokkaido(),
  "青森県"   = function() fetch_aomori(.sample_url("青森県")),
  "秋田県"   = function() fetch_akita(),
  "山形県"   = function() fetch_yamagata(ari_pdf_url = "https://www.eiken.yamagata.yamagata.jp/pdfshuho/2026/202632.pdf"),
  "福島県"   = function() fetch_fukushima(.sample_url("福島県")),
  "宮城県"   = function() fetch_miyagi(resolve_hokenjo_pdf_url_for_pref("宮城県")),
  "茨城県"   = function() fetch_ibaraki(.sample_url("茨城県")),
  "栃木県"   = function() fetch_tochigi(.sample_url("栃木県")),
  "群馬県"   = function() fetch_gunma(resolve_hokenjo_pdf_url_for_pref("群馬県")),
  "埼玉県"   = function() fetch_saitama(.sample_url("埼玉県")),
  "千葉県"   = function() fetch_chiba(resolve_hokenjo_pdf_url_for_pref("千葉県")),
  "東京都"   = function() fetch_tokyo(.sample_url("東京都")),
  "神奈川県" = function() fetch_kanagawa(.sample_url("神奈川県")),
  "新潟県"   = function() fetch_niigata(.sample_url("新潟県")),
  "富山県"   = function() {
    d <- fetch_toyama_zip(resolve_hokenjo_pdf_url_for_pref("富山県"))
    d[d$week_num == max(d$week_num), ]  # currentは最新週のみでよい（履歴側は別途一括反映）
  },
  "石川県"   = function() fetch_ishikawa(),
  "福井県"   = function() fetch_fukui(),
  "山梨県"   = function() fetch_yamanashi(.sample_url("山梨県")),
  "長野県"   = function() fetch_nagano(.sample_url("長野県")),
  "岐阜県"   = function() fetch_gifu(.sample_url("岐阜県")),
  "静岡県"   = function() fetch_shizuoka(.sample_url("静岡県")),
  "愛知県"   = function() fetch_aichi(.sample_url("愛知県")),
  "三重県"   = function() fetch_mie(),
  "滋賀県"   = function() fetch_shiga(.sample_url("滋賀県")),
  "京都府"   = function() fetch_kyoto(2026, 32),
  "大阪府"   = function() fetch_osaka(.sample_url("大阪府")),
  "兵庫県"   = function() fetch_hyogo(2026, 32),
  "奈良県"   = function() fetch_nara(.sample_url("奈良県")),
  "和歌山県" = function() fetch_wakayama(.sample_url("和歌山県")),
  "鳥取県"   = function() fetch_tottori(.sample_url("鳥取県")),
  "島根県"   = function() fetch_shimane(.sample_url("島根県")),
  "岡山県"   = function() fetch_okayama(.sample_url("岡山県")),
  "山口県"   = function() fetch_yamaguchi(.sample_url("山口県")),
  "徳島県"   = function() fetch_tokushima(.sample_url("徳島県")),
  "香川県"   = function() fetch_kagawa(.sample_url("香川県")),
  "愛媛県"   = function() fetch_ehime(.sample_url("愛媛県")),
  "高知県"   = function() fetch_kochi(.sample_url("高知県")),
  "福岡県"   = function() fetch_fukuoka(),
  "佐賀県"   = function() fetch_saga(yw = "202632"),
  "長崎県"   = function() fetch_nagasaki(.sample_url("長崎県")),
  "熊本県"   = function() fetch_kumamoto(.sample_url("熊本県")),
  "大分県"   = function() fetch_oita(.sample_url("大分県")),
  "宮崎県"   = function() fetch_miyazaki(.sample_url("宮崎県")),
  "鹿児島県" = function() fetch_kagoshima(.sample_url("鹿児島県")),
  "沖縄県"   = function() fetch_okinawa("https://www.pref.okinawa.jp/_res/projects/default_project/_page_/001/006/484/syuuho0831.xlsx"),
  "石川県"   = function() fetch_ishikawa()
)

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
