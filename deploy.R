# shinyapps.io デプロイスクリプト
# 手順:
#   1. shinyapps.io にログイン → 右上アカウント名 → Tokens → Show → コピー
#   2. 下の setAccountInfo を実際の値に書き換えて実行
#   3. deployApp() を実行

# ── Step 1: 認証情報を設定（初回のみ） ──────────────────────
# ※ shinyapps.io の Tokens ページから取得したコードをそのまま貼り付けてください
# rsconnect::setAccountInfo(
#   name   = "YOUR_ACCOUNT_NAME",
#   token  = "YOUR_TOKEN",
#   secret = "YOUR_SECRET"
# )

# ── Step 2: デプロイ ────────────────────────────────────────
rsconnect::deployApp(
  appDir  = "C:/Users/kobayashi/Documents/R/japan_surveillance",
  appName = "japan_surveillance",
  appFiles = c(
    "app.R",
    "R/ebs_loader.R",
    "R/ebs_rule_screening.R",
    "R/ebs_ai_screening.R",
    "R/ebs_screening_module.R",
    "R/data_loader.R",
    "R/rt_estimation.R",
    "R/zensu_loader.R",
    "R/correlation.R",
    "data/"   # キャッシュ以外のdataフォルダ（.rscignoreで除外済み）
  ),
  forceUpdate = TRUE,
  launch.browser = TRUE
)
