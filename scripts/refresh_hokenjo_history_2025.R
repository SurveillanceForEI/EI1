# ============================================================
# 保健所別データ 2025年分バックフィル
# refresh_hokenjo_history.R の2025年版（CURRENT_YEARのみ変更）
# URLパターンが年を含む9県は自動的に2025年の週を試す。
# それ以外の県はsample_url（2026年時点の最新週URL）しか分からないため、
# このスクリプトでは取得できない（ユーザーからの2025年アーカイブURL提供が必要）。
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
CURRENT_YEAR <- 2025
MAX_WEEK <- 53

extract_week_num <- function(week_label) {
  if (is.na(week_label)) return(NA_integer_)
  wl <- chartr("０１２３４５６７８９", "0123456789", week_label)
  patterns <- c("第\\s*([0-9]{1,2})\\s*週", "[0-9]{4}年\\s*([0-9]{1,2})\\s*週", "^\\s*([0-9]{1,2})\\s*[年(（]")
  for (pat in patterns) {
    m <- regmatches(wl, regexec(pat, wl))[[1]]
    if (length(m) >= 2) return(as.integer(m[2]))
  }
  NA_integer_
}

# 既存のmake_key(pref, week_num)は年を区別しないため、2026年データと
# 2025年データで同じ週番号のキーが衝突してしまう。2025年専用の本スクリプトでは
# week_labelから西暦年を抽出し、年を含めたキーで既存データを判定する
extract_year_num <- function(week_label) {
  if (is.na(week_label)) return(NA_integer_)
  wl <- chartr("０１２３４５６７８９", "0123456789", week_label)
  m <- regmatches(wl, regexec("([0-9]{4})年", wl))[[1]]
  if (length(m) >= 2) return(as.integer(m[2]))
  m <- regmatches(wl, regexec("令和\\s*([0-9]{1,2})\\s*年", wl))[[1]]
  if (length(m) >= 2) return(as.integer(m[2]) + 2018)
  NA_integer_
}

make_key <- function(pref, week_num, week_label) {
  yr <- extract_year_num(week_label)
  if (!is.na(week_num) && !is.na(yr)) paste(pref, yr, week_num)
  else if (!is.na(week_num)) paste(pref, "unkyear", week_num)
  else paste(pref, "label:", week_label)
}

history <- if (file.exists(HISTORY_PATH)) readRDS(HISTORY_PATH) else NULL
existing_keys <- if (!is.null(history) && nrow(history) > 0) {
  mapply(make_key, history$pref, history$week_num, history$week_label)
} else character(0)

PATTERN_DISPATCH <- list(
  "青森県" = function(week) fetch_aomori(
    sprintf("https://www.pref.aomori.lg.jp/soshiki/kenko/hoken/files/wr%dw%02d.pdf", CURRENT_YEAR, week)),
  "茨城県" = function(week) {
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

hokkaido_res <- tryCatch(fetch_hokkaido_history(year = CURRENT_YEAR), error = function(e) {
  status_log <<- c(status_log, sprintf("[NG] 北海道: %s", conditionMessage(e)))
  NULL
})
if (!is.null(hokkaido_res) && nrow(hokkaido_res) > 0) {
  hokkaido_res$key <- mapply(make_key, hokkaido_res$pref, hokkaido_res$week_num, hokkaido_res$week_label)
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
    if (key %in% existing_keys) next
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

cat("\n=== 実行ログ（2025年） ===\n")
cat(paste(status_log, collapse = "\n"), "\n")

if (length(new_rows) > 0) {
  added <- do.call(rbind, new_rows)
  combined <- if (!is.null(history)) rbind(history, added) else added
  saveRDS(combined, HISTORY_PATH)
  cat(sprintf("\n追記: %d行（新規） / 累計: %d行\n", nrow(added), nrow(combined)))
} else {
  cat("\n新規追加データはありませんでした\n")
}

cat("\n=== 2025年 カバレッジ（自動取得できた9県+北海道のみ） ===\n")
if (file.exists(HISTORY_PATH)) {
  h <- readRDS(HISTORY_PATH)
  target_prefs <- c("北海道", names(PATTERN_DISPATCH))
  for (p in target_prefs) {
    wks <- sort(unique(h$week_num[h$pref == p & !is.na(h$week_num) &
                                    grepl("^2025|令和7", h$week_label)]))
    cat(sprintf("%-8s %s\n", p, paste(wks, collapse=",")))
  }
}
