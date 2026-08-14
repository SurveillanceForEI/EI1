# ============================================================
# 保健所別データ 時系列バックフィル／週次追記スクリプト
# ------------------------------------------------------------
# data/hokenjo_history.rds に「都道府県×週×保健所×疾患」の時系列データを
# 蓄積する。既に取得済みの(都道府県,週)は再取得しないため、
# 週報が新しく発行されるたびに本スクリプトを再実行すれば、
# 新規に公開された週だけを追記できる（＝毎週の定期更新にも使える）。
#
# 週ごとのURLが規則的なパターンで自動生成できる県（9県）は
# 2026年第1週〜現在までの全週を遡って取得を試みる。
# それ以外の県はURLパターンが不明（週ごとに固有の添付ID/固定URLの
# みが公開される、専用システムから毎回URLを取得する必要がある等）
# のため、現在公開されている最新週のみを追記する。
#
# 実行方法: Rscript scripts/refresh_hokenjo_history.R
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

HISTORY_PATH <- "data/hokenjo_history.rds"
CURRENT_YEAR <- 2026
# 実行時点で何週まで発行されていそうか（安全側に最新既知週+1まで試す）
MAX_WEEK <- 33

# week_label文字列から週番号を抜き出す。県によって表記が異なるため
# 複数パターンを順に試す（例: "2026年第32週", "令和８年第 32 週",
# "令和８年第３１週"(全角数字), "2026年31週", "32 (R. 8. 8. 3 ～ R. 8. 8. 9)"）。
# "2026年7/27～8/2"のように週番号自体が書かれていない県はNAのままとする
# （4桁の年を週番号と誤認しないよう、週候補は1〜2桁に限定する）。
extract_week_num <- function(week_label) {
  if (is.na(week_label)) return(NA_integer_)
  # 全角数字はASCIIに正規化してから照合する
  wl <- chartr("０１２３４５６７８９", "0123456789", week_label)
  patterns <- c("第\\s*([0-9]{1,2})\\s*週", "[0-9]{4}年\\s*([0-9]{1,2})\\s*週", "^\\s*([0-9]{1,2})\\s*[年(（]")
  for (pat in patterns) {
    m <- regmatches(wl, regexec(pat, wl))[[1]]
    if (length(m) >= 2) return(as.integer(m[2]))
  }
  NA_integer_
}

# 週番号が判別できない場合のフォールバックキー（週ラベル文字列そのもの
# を使うことで、次回実行時も同じ行を正しく「取得済み」として判定できる）
make_key <- function(pref, week_num, week_label) {
  if (!is.na(week_num)) paste(pref, week_num) else paste(pref, "label:", week_label)
}

# ---- 既存の履歴を読み込み、(pref, week_num) の取得済みキーを把握 ----
history <- if (file.exists(HISTORY_PATH)) readRDS(HISTORY_PATH) else NULL
existing_keys <- if (!is.null(history) && nrow(history) > 0) {
  mapply(make_key, history$pref, history$week_num, history$week_label)
} else character(0)

# ---- ①週ごとのURLパターンが既知で、遡及取得を試みる9県 ----
# 各要素: function(week) -> fetch結果 data.frame（失敗時はerrorをthrow）
PATTERN_DISPATCH <- list(
  "青森県" = function(week) fetch_aomori(
    sprintf("https://www.pref.aomori.lg.jp/soshiki/kenko/hoken/files/wr%dw%02d.pdf", CURRENT_YEAR, week)),
  "茨城県" = function(week) fetch_ibaraki(
    sprintf("https://www.pref.ibaraki.jp/hokenfukushi/eiken/idwr/weekly/documents/%didwr%02d.pdf", CURRENT_YEAR, week)),
  "東京都" = function(week) fetch_tokyo(
    sprintf("https://idsc.tmiph.metro.tokyo.lg.jp/assets/weekly/%d/%d.pdf", CURRENT_YEAR, week)),
  "愛知県" = function(week) fetch_aichi(year = CURRENT_YEAR, week = week),
  "京都府" = function(week) fetch_kyoto(year = CURRENT_YEAR, week = week),
  "大阪府" = function(week) fetch_osaka(
    sprintf("https://www.iph.pref.osaka.jp/infection/surv%02d/surv%02dt.html", CURRENT_YEAR %% 100, week)),
  "兵庫県" = function(week) fetch_hyogo(year = CURRENT_YEAR, week = week),
  "佐賀県" = function(week) fetch_saga(yw = sprintf("%d%02d", CURRENT_YEAR, week)),
  "宮崎県" = function(week) fetch_miyazaki(
    sprintf("https://www.pref.miyazaki.lg.jp/contents/org/fukushi/eikanken/center/infectious/pdf/%d%02d.pdf", CURRENT_YEAR, week))
)

new_rows <- list()
status_log <- character(0)

# ---- 北海道: 保健所別ページの年間バックナンバーCSV
#      （weekunitdata{YEAR}.csv、1999年〜）は1回の呼び出しでその年の
#      全週が返るため、週ループではなく年単位で呼び、未取得の週だけ追加する ----
hokkaido_res <- tryCatch(fetch_hokkaido_history(year = CURRENT_YEAR), error = function(e) {
  status_log <<- c(status_log, sprintf("[NG] 北海道: %s", conditionMessage(e)))
  NULL
})
if (!is.null(hokkaido_res) && nrow(hokkaido_res) > 0) {
  hokkaido_res$key <- paste(hokkaido_res$pref, hokkaido_res$week_num)
  new_hokkaido <- hokkaido_res[!(hokkaido_res$key %in% existing_keys), setdiff(names(hokkaido_res), "key")]
  if (nrow(new_hokkaido) > 0) {
    new_hokkaido$fetched_at <- as.character(Sys.time())
    new_rows[[length(new_rows) + 1]] <- new_hokkaido
    for (wk in sort(unique(new_hokkaido$week_num))) {
      status_log <- c(status_log, sprintf("[OK] 北海道 第%d週 (%d行)", wk, sum(new_hokkaido$week_num == wk)))
    }
  } else {
    status_log <- c(status_log, "[==] 北海道 (新規週なし、取得済み)")
  }
}

for (pref in names(PATTERN_DISPATCH)) {
  for (week in 1:MAX_WEEK) {
    key <- paste(pref, week)
    if (key %in% existing_keys) next  # 取得済みはスキップ
    res <- tryCatch(PATTERN_DISPATCH[[pref]](week), error = function(e) NULL)
    if (!is.null(res) && is.data.frame(res) && nrow(res) > 0) {
      res$week_num <- week
      res$fetched_at <- as.character(Sys.time())
      new_rows[[length(new_rows) + 1]] <- res
      status_log <- c(status_log, sprintf("[OK] %s 第%d週 (%d行)", pref, week, nrow(res)))
    } else {
      status_log <- c(status_log, sprintf("[--] %s 第%d週 (未公開/取得不可)", pref, week))
    }
  }
}

# ---- ②それ以外の県：現状の「最新週のみ」ラッパーを流用し、
#      まだ履歴に無い週であれば1件だけ追記する ----
.sample_url <- function(pref) {
  src <- Find(function(x) x$pref == pref, HOKENJO_DATA_SOURCES)
  if (is.null(src)) NA_character_ else src$sample_url
}

OTHER_DISPATCH <- list(
  "秋田県"   = function() fetch_akita(),
  "山形県"   = function() fetch_yamagata(ari_pdf_url = "https://www.eiken.yamagata.yamagata.jp/pdfshuho/2026/202632.pdf"),
  "福島県"   = function() fetch_fukushima(.sample_url("福島県")),
  "宮城県"   = function() fetch_miyagi(.sample_url("宮城県")),
  "栃木県"   = function() fetch_tochigi(.sample_url("栃木県")),
  "群馬県"   = function() fetch_gunma(.sample_url("群馬県")),
  "埼玉県"   = function() fetch_saitama(.sample_url("埼玉県")),
  "千葉県"   = function() fetch_chiba_graph(),
  "神奈川県" = function() fetch_kanagawa(.sample_url("神奈川県")),
  "新潟県"   = function() fetch_niigata(.sample_url("新潟県")),
  "富山県"   = function() fetch_toyama(.sample_url("富山県")),
  "石川県"   = function() fetch_ishikawa(),
  "福井県"   = function() fetch_fukui(),
  "山梨県"   = function() fetch_yamanashi(.sample_url("山梨県")),
  "長野県"   = function() fetch_nagano(.sample_url("長野県")),
  "岐阜県"   = function() fetch_gifu(.sample_url("岐阜県")),
  "静岡県"   = function() fetch_shizuoka(.sample_url("静岡県")),
  "三重県"   = function() fetch_mie(),
  "滋賀県"   = function() fetch_shiga(.sample_url("滋賀県")),
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
  "長崎県"   = function() fetch_nagasaki(.sample_url("長崎県")),
  "熊本県"   = function() fetch_kumamoto(.sample_url("熊本県")),
  "大分県"   = function() fetch_oita(.sample_url("大分県")),
  "鹿児島県" = function() fetch_kagoshima(.sample_url("鹿児島県")),
  "沖縄県"   = function() fetch_okinawa("https://www.pref.okinawa.jp/_res/projects/default_project/_page_/001/006/484/syuuho0831.xlsx")
)

for (pref in names(OTHER_DISPATCH)) {
  res <- tryCatch(OTHER_DISPATCH[[pref]](), error = function(e) {
    status_log <<- c(status_log, sprintf("[NG] %s: %s", pref, conditionMessage(e)))
    NULL
  })
  if (is.null(res) || !is.data.frame(res) || nrow(res) == 0) next
  wk <- extract_week_num(unique(res$week_label)[1])
  key <- make_key(pref, wk, unique(res$week_label)[1])
  if (key %in% existing_keys) {
    status_log <- c(status_log, sprintf("[==] %s 第%s週 (取得済みのためスキップ)", pref, ifelse(is.na(wk), "?", wk)))
    next
  }
  res$week_num <- wk
  res$fetched_at <- as.character(Sys.time())
  new_rows[[length(new_rows) + 1]] <- res
  status_log <- c(status_log, sprintf("[OK] %s 第%s週 (%d行)", pref, ifelse(is.na(wk), "?", wk), nrow(res)))
}

cat("\n=== 実行ログ ===\n")
cat(paste(status_log, collapse = "\n"), "\n")

if (length(new_rows) > 0) {
  added <- do.call(rbind, new_rows)
  combined <- if (!is.null(history)) rbind(history, added) else added
  saveRDS(combined, HISTORY_PATH)
  cat(sprintf("\n追記: %d行（新規） / 累計: %d行\n", nrow(added), nrow(combined)))
} else {
  cat("\n新規追加データはありませんでした（全て取得済み、または全て失敗）\n")
}

# 都道府県×週のカバレッジ一覧を表示
if (file.exists(HISTORY_PATH)) {
  h <- readRDS(HISTORY_PATH)
  cov <- unique(h[, c("pref", "week_num")])
  cov <- cov[order(cov$pref, cov$week_num), ]
  cat("\n=== 都道府県別カバレッジ（週番号） ===\n")
  for (p in unique(cov$pref)) {
    wks <- sort(cov$week_num[cov$pref == p])
    cat(sprintf("%-8s %s\n", p, paste(wks, collapse=",")))
  }
}
