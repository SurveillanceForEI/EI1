# ============================================================
# 保健所別データ「直近週を引数なしで取得する」都道府県別ラッパー関数
# ------------------------------------------------------------
# scripts/refresh_hokenjo_data.R（手動一括更新／current.rds生成）と
# scripts/refresh_hokenjo_history.R（夜間自動実行のバックフィル/週次
# 追記）の両方から共有で使う。以前はこの2つのスクリプトがそれぞれ
# 独自にURL解決ロジックを持っており、片方だけ修正すると夜間の自動
# 実行がstaleなURL（沖縄県の固定xlsxが404になる等）に取り残されて
# しまう事故が起きたため、1箇所にまとめてある。
#
# 前提: 呼び出し側で以下を先にsourceしておくこと
#   R/hokenjo_fetch/hokenjo_fetch_schema.R
#   R/hokenjo_fetch/pdf_table_utils.R
#   R/hokenjo_data_sources.R （HOKENJO_DATA_SOURCES, resolve_hokenjo_pdf_url_for_pref,
#                              current_iso_year_week, probe_latest_week_fetch）
#   R/hokenjo_fetch/*.R （各県のfetch_*()関数）
# ============================================================

.sample_url <- function(pref) {
  src <- Find(function(x) x$pref == pref, HOKENJO_DATA_SOURCES)
  if (is.null(src)) NA_character_ else src$sample_url
}

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
  "福島県"   = function() fetch_fukushima(resolve_hokenjo_pdf_url_for_pref("福島県")),
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
  "新潟県"   = function() probe_latest_week_fetch(function(y, w)
    fetch_niigata_xlsx(resolve_niigata_xlsx_url(y - 2018L, w))),
  "岩手県"   = function() fetch_iwate(),
  "富山県"   = function() {
    d <- fetch_toyama_zip(resolve_hokenjo_pdf_url_for_pref("富山県"))
    d[d$week_num == max(d$week_num), ]  # currentは最新週のみでよい（履歴側は別途一括反映）
  },
  # 石川県: fetch_ishikawa()は常に2026年第32週の固定値（.ISHIKAWA_WEEK32_DATA、
  # 検証用に手動抽出したもの）を返すだけで実際にはサイトから取得していない
  # ため、OCRベースの実装であるfetch_ishikawa_history()を使う。1つのPDFに
  # 直近5週分の推移表が載っているため、直近週から遡ってPDFが存在する週を
  # 探し、その週のデータだけを取り出す。OCR特有の誤読で稀に桁が飛んだ
  # 異常値（定点あたり報告数が数百等）が出ることがあるため、500超は
  # 誤読とみなしNA化する
  "石川県"   = function() {
    yw <- current_iso_year_week()
    for (back in 0:3) {
      wk <- yw$week - back
      if (wk < 1) break
      url <- sprintf("https://www.pref.ishikawa.lg.jp/hokan/kansenjoho/stock/%d/documents/%d-%d.pdf", yw$year, yw$year, wk)
      d <- tryCatch(fetch_ishikawa_history(url, year = yw$year), error = function(e) NULL)
      if (is.null(d) || nrow(d) == 0) next
      d2 <- d[!is.na(d$week_num) & d$week_num == wk, ]
      if (nrow(d2) == 0) next
      d2$rate[!is.na(d2$rate) & d2$rate > 500] <- NA
      return(d2)
    }
    stop("石川県: 直近4週分とも取得できませんでした")
  },
  "福井県"   = function() fetch_fukui(),
  "山梨県"   = function() probe_latest_week_fetch(function(y, w)
    fetch_yamanashi(sprintf("https://www.pref.yamanashi.jp/documents/101494/%d%02dw.pdf", y, w))),
  "長野県"   = function() probe_latest_week_fetch(function(y, w)
    fetch_nagano(resolve_nagano_data_url(y, w))),
  "岐阜県"   = function() fetch_gifu(resolve_gifu_data_url()),
  "静岡県"   = function() probe_latest_week_fetch(function(y, w)
    fetch_shizuoka(sprintf("https://www.pref.shizuoka.jp/_res/projects/default_project/_page_/001/081/723/%didwr%d-2.pdf", y, w))),
  # 愛知県: サイト全体がリダイレクトループ中で現状取得不可（2026-08確認）。
  # URLパターン自体は分かっているので復旧後はprobe_latest_week_fetchに変更可能
  "愛知県"   = function() fetch_aichi(.sample_url("愛知県")),
  "三重県"   = function() fetch_mie(),
  "滋賀県"   = function() fetch_shiga(resolve_hokenjo_pdf_url_for_pref("滋賀県")),
  "京都府"   = function() probe_latest_week_fetch(function(y, w) fetch_kyoto(y, w)),
  "大阪府"   = function() probe_latest_week_fetch(function(y, w)
    fetch_osaka(sprintf("https://www.iph.pref.osaka.jp/infection/surv%02d/surv%dt.html", y %% 100, w))),
  "兵庫県"   = function() probe_latest_week_fetch(function(y, w) fetch_hyogo(y, w)),
  "奈良県"   = function() probe_latest_week_fetch(function(y, w)
    fetch_nara(sprintf("https://www.pref.nara.lg.jp/documents/4352/08%02d.pdf", w))),
  "和歌山県" = function() probe_latest_week_fetch(function(y, w)
    fetch_wakayama(sprintf("https://www.pref.wakayama.lg.jp/prefg/031801/idsw/khdc/d00153694_d/fil/WIDR%d%02d.pdf", y, w))),
  "鳥取県"   = function() probe_latest_week_fetch(function(y, w)
    fetch_tottori(sprintf("https://www.pref.tottori.lg.jp/secure/519458/R%dW%02d_hp.pdf", y - 2018L, w))),
  "島根県"   = function() probe_latest_week_fetch(function(y, w)
    fetch_shimane(sprintf("https://pref.shimane.didss.dsvc.jp/files/report/week/weeklyreport_y%dw%02d.pdf", y, w))),
  "岡山県"   = function() fetch_okayama(resolve_hokenjo_pdf_url_for_pref("岡山県")),
  "広島県"   = function() fetch_hiroshima(resolve_hokenjo_pdf_url_for_pref("広島県")),
  "山口県"   = function() probe_latest_week_fetch(function(y, w)
    fetch_yamaguchi(sprintf("https://pref.yamaguchi.didss.dsvc.jp/files/report/week/weeklyreport_y%dw%02d.pdf", y, w))),
  "徳島県"   = function() fetch_tokushima(resolve_hokenjo_pdf_url_for_pref("徳島県")),
  "香川県"   = function() probe_latest_week_fetch(function(y, w)
    fetch_kagawa(sprintf("https://www.pref.kagawa.lg.jp/documents/7135/%dsyuuhou%d.pdf", y, w))),
  "愛媛県"   = function() fetch_ehime(resolve_hokenjo_pdf_url_for_pref("愛媛県")),
  "高知県"   = function() fetch_kochi(resolve_hokenjo_pdf_url_for_pref("高知県")),
  "福岡県"   = function() fetch_fukuoka(),
  "佐賀県"   = function() fetch_saga(yw = resolve_saga_latest_yw()),
  "長崎県"   = function() fetch_nagasaki(resolve_hokenjo_pdf_url_for_pref("長崎県")),
  "熊本県"   = function() fetch_kumamoto(resolve_hokenjo_pdf_url_for_pref("熊本県")),
  "大分県"   = function() fetch_oita(resolve_hokenjo_pdf_url_for_pref("大分県")),
  "宮崎県"   = function() probe_latest_week_fetch(function(y, w)
    fetch_miyazaki(sprintf("https://www.pref.miyazaki.lg.jp/contents/org/fukushi/eikanken/center/infectious/pdf/%d%02d.pdf", y, w))),
  "鹿児島県" = function() fetch_kagoshima(resolve_hokenjo_pdf_url_for_pref("鹿児島県")),
  "沖縄県"   = function() fetch_okinawa(resolve_hokenjo_pdf_url_for_pref("沖縄県"))
)
