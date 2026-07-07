# ============================================================
# deploy_with_refresh.R — 最新データ取得 → shinyapps.io デプロイ
# 「デプロイして」と指示された際は、このスクリプトを実行する。
# ============================================================

setwd("C:/Users/kobayashi/Documents/R/japan_surveillance")

# ① デプロイ前に最新データを取得（定点・全数・EBSニュース）
source("scripts/auto_update.R")

# ② renvスナップショットの依存パッケージ検証を無効化
#    （ローカルのcurl/httr2等のバージョン差でデプロイが失敗するのを防ぐ）
options(renv.config.snapshot.validate = FALSE)

# ③ デプロイ
rsconnect::deployApp(
  appDir      = ".",
  forceUpdate = TRUE,
  logLevel    = "verbose"
)
