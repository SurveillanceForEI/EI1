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
CURRENT_YEAR <- as.integer(format(Sys.Date(), "%Y"))
# 実行時点で何週まで発行されていそうかを実行日から動的に算出する
# （固定値だと新しい週が公開されても永久に取得されなくなるバグの元になるため、
# 2026-08-30に固定値33からの動的算出に変更した）。
# ISO週番号（月曜始まり）+1週分のバッファを持たせ、まだ公開されていない週の
# 空振り試行はエラーとして無視される想定
MAX_WEEK <- as.integer(format(Sys.Date(), "%V")) + 1L

# week_label文字列から週番号を抜き出す。県によって表記が異なるため
# 複数パターンを順に試す（例: "2026年第32週", "令和８年第 32 週",
# "令和８年第３１週"(全角数字), "2026年31週", "32 (R. 8. 8. 3 ～ R. 8. 8. 9)"）。
# 香川県の"2026年7/27～8/2"のように週番号自体が書かれていない場合は、
# 開始日からISO週番号（月曜始まり、%V）を逆算する
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
  m <- regmatches(wl, regexec("(20[0-9]{2})年\\s*([0-9]{1,2})/([0-9]{1,2})", wl))[[1]]
  if (length(m) == 4) {
    d <- tryCatch(as.Date(sprintf("%s-%s-%s", m[2], m[3], m[4])), error = function(e) NA)
    if (!is.na(d)) return(as.integer(format(d, "%V")))
  }
  NA_integer_
}

# 週番号が判別できない場合のフォールバックキー（週ラベル文字列そのもの
# を使うことで、次回実行時も同じ行を正しく「取得済み」として判定できる）。
# week_numだけでは年をまたいで再利用される値のため、年も含めないと
# 「2025年に同じ週番号のデータが既にある」場合に当年の週を誤ってスキップ
# してしまう（逆に、年が不明な場合は従来通りweek_numのみで判定する）
make_key <- function(pref, week_num, week_label) {
  yr <- .hokenjo_extract_year_local(week_label)
  if (!is.na(week_num) && !is.na(yr)) paste(pref, yr, week_num)
  else if (!is.na(week_num)) paste(pref, week_num)
  else paste(pref, "label:", week_label)
}

.hokenjo_extract_year_local <- function(label) {
  if (is.na(label)) return(NA_integer_)
  s <- chartr("０１２３４５６７８９", "0123456789", label)
  m <- regmatches(s, regexpr("(20[0-9]{2})年", s))
  if (length(m) > 0 && nzchar(m)) return(as.integer(sub("年", "", m)))
  m <- regmatches(s, regexec("令和\\s*([0-9]+)\\s*年", s))[[1]]
  if (length(m) == 2) return(as.integer(m[2]) + 2018L)
  # 福井県「R. 7. 8.18」のような、漢字なしの略式和暦表記
  m <- regmatches(s, regexec("R\\.?\\s*([0-9]+)\\s*\\.", s))[[1]]
  if (length(m) == 2) return(as.integer(m[2]) + 2018L)
  # 愛媛県「2025.8.18」のような、「年」の付かない西暦表記
  # （末尾に週の日付範囲があるため、文中どこにあっても検出する）
  m <- regmatches(s, regexpr("(?<![0-9])(20[0-9]{2})\\.[0-9]{1,2}\\.[0-9]{1,2}", s, perl = TRUE))
  if (length(m) > 0 && nzchar(m)) return(as.integer(sub("\\..*", "", m)))
  NA_integer_
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
  "茨城県" = function(week) {
    # 通常は2桁ゼロ埋め（idwr02.pdf等）だが、第1週のみ非ゼロ埋め（idwr1.pdf）、
    # 第26週のみ"_ver2"サフィックス付きだったりと、週によってURLの細部が
    # 異なることがあるため、候補を順に試す
    candidates <- c(
      sprintf("https://www.pref.ibaraki.jp/hokenfukushi/eiken/idwr/weekly/documents/%didwr%02d.pdf", CURRENT_YEAR, week),
      sprintf("https://www.pref.ibaraki.jp/hokenfukushi/eiken/idwr/weekly/documents/%didwr%d.pdf", CURRENT_YEAR, week),
      sprintf("https://www.pref.ibaraki.jp/hokenfukushi/eiken/idwr/weekly/documents/%didwr%02d_ver2.pdf", CURRENT_YEAR, week)
    )
    last_err <- NULL
    for (u in candidates) {
      res <- tryCatch(fetch_ibaraki(u), error = function(e) { last_err <<- e; NULL })
      if (!is.null(res)) return(res)
    }
    stop(conditionMessage(last_err))
  },
  "東京都" = function(week) fetch_tokyo(
    sprintf("https://idsc.tmiph.metro.tokyo.lg.jp/assets/weekly/%d/%02d.pdf", CURRENT_YEAR, week)),
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
    key <- paste(pref, CURRENT_YEAR, week)
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
# 以前はここに①（PATTERN_DISPATCH）と重複する独自のstaleな
# URL定義を持っていたが、片方だけ修正するともう片方が古いURL
# （沖縄県の固定xlsx等、実際に404化していた）のまま取り残される
# 事故が起きたため、scripts/refresh_hokenjo_data.Rと共有の
# R/hokenjo_refresh_dispatch.R（HOKENJO_REFRESH_DISPATCH）を使う
source("R/hokenjo_refresh_dispatch.R")
OTHER_DISPATCH <- HOKENJO_REFRESH_DISPATCH[
  setdiff(names(HOKENJO_REFRESH_DISPATCH), c(names(PATTERN_DISPATCH), "北海道"))
]

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
  # hokenjo_year列は起動時の再計算コスト削減のためdata/hokenjo_history.rds側に
  # 保存している（R/hokenjo_map_module.R参照）。新規追記行のみ計算して既存の
  # historyと列を揃える（この列がないとrbindで列数不一致エラーになる）。
  added$hokenjo_year <- vapply(added$week_label, .hokenjo_extract_year_local, integer(1))
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
