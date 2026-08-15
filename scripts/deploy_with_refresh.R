# ============================================================
# deploy_with_refresh.R — 最新データ取得 → GitHub push → shinyapps.io デプロイ
# 「デプロイして」と指示された際は、このスクリプトを実行する。
# ============================================================

setwd("C:/Users/kobayashi/Documents/R/japan_surveillance")

# ① デプロイ前に最新データを取得（定点・全数・EBSニュース）
source("scripts/auto_update.R")

# ② GitHubへpush（Connect Cloud側のデータ鮮度を保つため）
# shinyapps.ioへは直接ファイル一式をデプロイするが、Posit Connect Cloud版は
# GitHubリポジトリの中身からビルドする方式のため、pushしないとConnect Cloud
# 側だけデータが古いまま取り残される（2026-08-12、ユーザー指摘により発覚・
# 修正）。ebs_startup_cache.rds・last_update.txtだけでなく、週次データ
# （data/cache等）一式を必ずforce-addしてpushする。
tryCatch({
  data_paths <- c(
    "data/cache", "data/cache_zensu", "data/cache_hosp",
    "data/cache_iasr", "data/cache_ari",
    "data/japan_map.rds", "data/last_update.txt",
    "data/ebs_startup_cache.rds", "data/gtrends_cache_JP.rds",
    "data/data_change_log.rds",
    "data/hokenjo_current.rds", "data/hokenjo_history.rds"
  )
  data_paths <- data_paths[file.exists(data_paths)]
  # バックグラウンド実行環境ではgitのglobal設定(HOME解決)が正しく引き継がれず
  # "unable to auto-detect email address"で失敗することがあるため、
  # -cでuser.name/user.emailを明示的に指定する（auto_update_deploy.Rと同じ対策）
  git_id <- c("-c", "user.name=japan_surveillance-auto-update",
              "-c", "user.email=kobayashi.yus@jihs.go.jp")
  system2("git", c("add", "-f", shQuote(data_paths)))
  status_out <- system2("git", c("status", "--porcelain"), stdout = TRUE)
  if (length(status_out) > 0) {
    commit_msg <- paste0("デプロイ前データ更新 ", format(Sys.time(), "%Y-%m-%d %H:%M"))
    commit_result <- system2("git", c(git_id, "commit", "-m", shQuote(commit_msg)), stdout = TRUE, stderr = TRUE)
    cat("GitHub commit結果: ", paste(tail(commit_result, 2), collapse = " / "), "\n")
    push_result <- system2("git", c("push", "origin", "main"), stdout = TRUE, stderr = TRUE)
    cat("GitHub push完了: ", paste(tail(push_result, 3), collapse = " / "), "\n")
    cat("※ Posit Connect Cloud側は自動反映されません。ダッシュボードで手動Republishが必要です。\n")
  } else {
    cat("GitHub: コミット対象の変更なし\n")
  }
}, error = function(e) {
  cat("GitHub pushエラー: ", e$message, "\n")
})

# ③ renvスナップショットの依存パッケージ検証を無効化
#    （ローカルのcurl/httr2等のバージョン差でデプロイが失敗するのを防ぐ）
options(renv.config.snapshot.validate = FALSE)

# ④ shinyapps.io デプロイ
rsconnect::deployApp(
  appDir      = ".",
  forceUpdate = TRUE,
  logLevel    = "verbose"
)
