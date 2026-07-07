# ============================================================
# change_tracker.R
# 取得元データに実質的な変化があったかどうかを検知し、
# 「最終的に更新があったと判断した日時」を記録・参照するための仕組み。
#
# 単純なファイル取得日時（fetch/mtime）は、内容が変わっていなくても
# 定期実行のたびに更新されてしまう。ここでは取得したデータの内容から
# シグネチャ（ハッシュ）を計算し、前回保存したシグネチャと異なる場合のみ
# 「更新検知日時」を現在時刻に更新する。内容が同じ場合は前回の日時を維持する。
# ============================================================

library(digest)

CHANGE_LOG_PATH <- "data/data_change_log.rds"

.read_change_log <- function(log_path = CHANGE_LOG_PATH) {
  tryCatch({
    if (file.exists(log_path)) readRDS(log_path) else list()
  }, error = function(e) list())
}

# source_idのデータに変化があれば changed_at を現在時刻に更新して返す。
# 変化がなければ、前回記録した changed_at をそのまま返す（初回はNAではなく現在時刻）。
record_data_change <- function(source_id, signature, log_path = CHANGE_LOG_PATH) {
  log  <- .read_change_log(log_path)
  now  <- Sys.time()
  prev <- log[[source_id]]

  if (is.null(prev) || is.null(prev$signature) || !identical(prev$signature, signature)) {
    log[[source_id]] <- list(signature = signature, changed_at = now)
    tryCatch({
      dir.create(dirname(log_path), showWarnings = FALSE, recursive = TRUE)
      saveRDS(log, log_path)
    }, error = function(e) message("変更ログ保存エラー: ", e$message))
    return(now)
  }
  prev$changed_at
}

# source_idの最終更新検知日時を返す（記録がなければNA）
get_last_change_time <- function(source_id, log_path = CHANGE_LOG_PATH) {
  log <- .read_change_log(log_path)
  ce  <- log[[source_id]]
  if (is.null(ce) || is.null(ce$changed_at)) return(NA)
  ce$changed_at
}

# データフレームの「直近部分」からシグネチャを計算するヘルパー。
# 新規行の追加だけでなく、既存期間の遡及修正（速報値→確定値の訂正等）も
# 検知できるよう、直近 recent_days 日分のデータを対象にハッシュ化する。
compute_recent_signature <- function(df, date_col = "date", recent_days = 90) {
  if (is.null(df) || nrow(df) == 0) return(NA_character_)
  tryCatch({
    dates <- df[[date_col]]
    cutoff <- max(dates, na.rm = TRUE) - recent_days
    recent <- df[!is.na(dates) & dates >= cutoff, , drop = FALSE]
    digest::digest(recent, algo = "xxhash64")
  }, error = function(e) NA_character_)
}
