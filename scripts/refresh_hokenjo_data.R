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
  "青森県"   = function() probe_latest_week_fetch(function(y, w)
    fetch_aomori(sprintf("https://www.pref.aomori.lg.jp/soshiki/kenko/hoken/files/wr%dw%02d.pdf", y, w))),
  "秋田県"   = function() fetch_akita(),
  "山形県"   = function() {
    # 主要疾患データはfetch_yamagata()内部で年別CSV（週指定不要・常に最新）
    # を参照するため自己解決するが、ARI（急性呼吸器感染症）だけは週報PDFの
    # URLに週番号が必要なため、計算上の直近週から遡ってPDFが存在する
    # 週を探す（見つからなければARI無しでも他疾患は取得できる設計）
    yw <- current_iso_year_week()
    ari_url <- NA_character_
    for (back in 0:3) {
      wk <- yw$week - back
      cand <- sprintf("https://www.eiken.yamagata.yamagata.jp/pdfshuho/%d/%d%02d.pdf", yw$year, yw$year, wk)
      if (!is.na(tryCatch({ download.file(cand, tempfile(), mode="wb", quiet=TRUE); cand }, error=function(e) NA))) {
        ari_url <- cand
        break
      }
    }
    fetch_yamagata(year = yw$year, ari_pdf_url = if (is.na(ari_url)) NULL else ari_url)
  },
  "福島県"   = function() fetch_fukushima(.sample_url("福島県")),
  "宮城県"   = function() fetch_miyagi(resolve_hokenjo_pdf_url_for_pref("宮城県")),
  "茨城県"   = function() probe_latest_week_fetch(function(y, w)
    fetch_ibaraki(sprintf("https://www.pref.ibaraki.jp/hokenfukushi/eiken/idwr/weekly/documents/%didwr%02d.pdf", y, w))),
  "栃木県"   = function() probe_latest_week_fetch(function(y, w)
    fetch_tochigi(sprintf("https://www.pref.tochigi.lg.jp/e60/tidc/documents/intidwr%d%02d.pdf", y, w))),
  "群馬県"   = function() fetch_gunma(resolve_hokenjo_pdf_url_for_pref("群馬県")),
  "埼玉県"   = function() probe_latest_week_fetch(function(y, w)
    fetch_saitama(sprintf("https://www.pref.saitama.lg.jp/documents/277313/%d_%dw.pdf", y, w))),
  "千葉県"   = function() fetch_chiba(resolve_hokenjo_pdf_url_for_pref("千葉県")),
  "東京都"   = function() probe_latest_week_fetch(function(y, w)
    fetch_tokyo(sprintf("https://idsc.tmiph.metro.tokyo.lg.jp/assets/weekly/%d/%d.pdf", y, w))),
  "神奈川県" = function() probe_latest_week_fetch(function(y, w)
    fetch_kanagawa(sprintf("https://www.pref.kanagawa.jp/sys/eiken/003_center/0001_weekly/pdf/wrR%02d_%d.pdf", y - 2018, w))),
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
  "静岡県"   = function() probe_latest_week_fetch(function(y, w)
    fetch_shizuoka(sprintf("https://www.pref.shizuoka.jp/_res/projects/default_project/_page_/001/081/723/%didwr%d-2.pdf", y, w))),
  "愛知県"   = function() fetch_aichi(.sample_url("愛知県")),
  "三重県"   = function() fetch_mie(),
  "滋賀県"   = function() fetch_shiga(.sample_url("滋賀県")),
  "京都府"   = function() probe_latest_week_fetch(function(y, w) fetch_kyoto(y, w)),
  "大阪府"   = function() probe_latest_week_fetch(function(y, w)
    fetch_osaka(sprintf("https://www.iph.pref.osaka.jp/infection/surv%02d/surv%dt.html", y %% 100, w))),
  "兵庫県"   = function() fetch_hyogo(2026, 32),
  "奈良県"   = function() fetch_nara(.sample_url("奈良県")),
  "和歌山県" = function() probe_latest_week_fetch(function(y, w)
    fetch_wakayama(sprintf("https://www.pref.wakayama.lg.jp/prefg/031801/idsw/khdc/d00153694_d/fil/WIDR%d%02d.pdf", y, w))),
  "鳥取県"   = function() fetch_tottori(.sample_url("鳥取県")),
  "島根県"   = function() fetch_shimane(.sample_url("島根県")),
  "岡山県"   = function() fetch_okayama(resolve_hokenjo_pdf_url_for_pref("岡山県")),
  "広島県"   = function() fetch_hiroshima(resolve_hokenjo_pdf_url_for_pref("広島県")),
  "山口県"   = function() fetch_yamaguchi(.sample_url("山口県")),
  "徳島県"   = function() fetch_tokushima(.sample_url("徳島県")),
  "香川県"   = function() probe_latest_week_fetch(function(y, w)
    fetch_kagawa(sprintf("https://www.pref.kagawa.lg.jp/documents/7135/%dsyuuhou%d.pdf", y, w))),
  "愛媛県"   = function() fetch_ehime(.sample_url("愛媛県")),
  "高知県"   = function() fetch_kochi(.sample_url("高知県")),
  "福岡県"   = function() fetch_fukuoka(),
  "佐賀県"   = function() fetch_saga(yw = resolve_saga_latest_yw()),
  "長崎県"   = function() fetch_nagasaki(.sample_url("長崎県")),
  "熊本県"   = function() fetch_kumamoto(.sample_url("熊本県")),
  "大分県"   = function() fetch_oita(.sample_url("大分県")),
  "宮崎県"   = function() probe_latest_week_fetch(function(y, w)
    fetch_miyazaki(sprintf("https://www.pref.miyazaki.lg.jp/contents/org/fukushi/eikanken/center/infectious/pdf/%d%02d.pdf", y, w))),
  "鹿児島県" = function() fetch_kagoshima(.sample_url("鹿児島県")),
  "沖縄県"   = function() fetch_okinawa(resolve_hokenjo_pdf_url_for_pref("沖縄県")),
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
