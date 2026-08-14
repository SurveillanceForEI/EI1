# 石川県「2026年第◯週の感染症別保健所別届出数及び週推移表」PDF
# https://www.pref.ishikawa.lg.jp/hokan/kansenjoho/top/patients/documents/{YEAR}-{WEEK}.pdf
#
# 【重要】p.4〜18の疾患別ページには保健所別（金沢市/南加賀/石川中央/
# 能登中部/能登北部）の「報告数」「定点あたり報告数」の数値表があるが、
# これは画像として埋め込まれておりpdftools::pdf_text()ではテキスト
# 抽出できない（表以外の本文は通常のテキストとして抽出可能）。
# ARI（p.3）のみテキストの表（rateのみ、countなし）。
#
# 現状の実装は「2026年第32週」時点でPDFページを画像化し、目視（Claude
# の画像認識）で読み取った値をハードコードしたものである。今後の週次
# 更新を自動化するには、pdftools::pdf_render_page()で該当ページを
# 画像化した上でOCR（例: tesseract Rパッケージ）にかける処理を追加
# する必要がある。
#
# 全20疾患のうち、急性出血性結膜炎・細菌性髄膜炎・無菌性髄膜炎・
# マイコプラズマ肺炎・クラミジア肺炎・感染性胃腸炎(ロタ)の6疾患は
# 「5週連続して患者発生数が0となった場合、当該感染症のページは省略」
# という石川県の方針により、この週はページ自体が存在せずデータなし
# （NA）。

ISHIKAWA_HOKENJO_ORDER <- c("金沢市", "南加賀", "石川中央", "能登中部", "能登北部")

# 2026年第32週データ（画像から目視抽出、出典: 2026-32.pdf p.3-18）
.ISHIKAWA_WEEK32_DATA <- list(
  "急性呼吸器感染症(ARI)" = list(rate = c(45.56, 52.50, 58.45, 51.83, 13.00), count = rep(NA_real_, 5)),
  "インフルエンザ"        = list(count = c(1, 0, 0, 0, 0),   rate = c(0.06, 0.00, 0.00, 0.00, 0.00)),
  "COVID-19"               = list(count = c(28, 12, 29, 20, 1), rate = c(1.75, 1.20, 2.64, 3.33, 0.25)),
  "RSウイルス感染症"      = list(count = c(12, 24, 3, 0, 0),   rate = c(1.20, 4.00, 0.50, 0.00, 0.00)),
  "咽頭結膜熱"            = list(count = c(1, 0, 3, 1, 0),     rate = c(0.10, 0.00, 0.50, 0.25, 0.00)),
  "Ａ群溶血性レンサ球菌咽頭炎" = list(count = c(9, 17, 5, 8, 0), rate = c(0.90, 2.83, 0.83, 2.00, 0.00)),
  "感染性胃腸炎"          = list(count = c(69, 24, 88, 16, 0), rate = c(6.90, 4.00, 14.67, 4.00, 0.00)),
  "水痘"                  = list(count = c(0, 0, 2, 1, 0),     rate = c(0.00, 0.00, 0.33, 0.25, 0.00)),
  "手足口病"              = list(count = c(13, 2, 5, 6, 0),    rate = c(1.30, 0.33, 0.83, 1.50, 0.00)),
  "伝染性紅斑"            = list(count = c(0, 0, 0, 0, 0),     rate = c(0.00, 0.00, 0.00, 0.00, 0.00)),
  "突発性発しん"          = list(count = c(2, 2, 3, 1, 1),     rate = c(0.20, 0.33, 0.50, 0.25, 0.50)),
  "ヘルパンギーナ"        = list(count = c(6, 11, 1, 2, 1),    rate = c(0.60, 1.83, 0.17, 0.50, 0.50)),
  "流行性耳下腺炎"        = list(count = c(0, 0, 0, 0, 0),     rate = c(0.00, 0.00, 0.00, 0.00, 0.00)),
  "流行性角結膜炎"        = list(count = c(9, 1, 3, 0, 0),     rate = c(3.00, 1.00, 3.00, 0.00, 0.00))
  # 急性出血性結膜炎・細菌性髄膜炎・無菌性髄膜炎・マイコプラズマ肺炎・
  # クラミジア肺炎・感染性胃腸炎(ロタ)：5週連続報告0のためページ省略、データなし
)

fetch_ishikawa <- function(week_label = "2026年第32週") {
  out <- list()
  for (disease in names(.ISHIKAWA_WEEK32_DATA)) {
    d <- .ISHIKAWA_WEEK32_DATA[[disease]]
    for (i in seq_along(ISHIKAWA_HOKENJO_ORDER)) {
      out[[length(out) + 1]] <- data.frame(
        pref = "石川県", week_label = week_label,
        hokenjo = ISHIKAWA_HOKENJO_ORDER[i], disease = disease,
        count = d$count[i], rate = d$rate[i],
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, out)
}
