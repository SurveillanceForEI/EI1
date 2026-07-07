# EBS AI Screening - Claude APIを使ったニュース記事の自動判定
# 必要パッケージ: httr2, jsonlite, readxl
#
# 使い方:
#   source("ebs_ai_screening.R")
#   result <- screen_ebs_entry(title = "...", summary = "...")
#   print(result)

library(httr2)
library(jsonlite)

# ============================================================
# 設定
# ============================================================

# Claude APIキー（環境変数から取得 or 直接指定）
get_api_key <- function() {
  key <- Sys.getenv("ANTHROPIC_API_KEY")
  if (nchar(key) == 0) stop("環境変数 ANTHROPIC_API_KEY が設定されていません")
  key
}

# 使用するモデル（高速・低コスト版）
CLAUDE_MODEL <- "claude-haiku-4-5-20251001"

# ============================================================
# スクリーニング基準（Screening基準シートより）
# ============================================================

SCREENING_CRITERIA <- list(
  unusual_unexpected = paste0(
    "Unusual/unexpected: 変か？ベースラインと異なる状況か？",
    "（Zoonosisの動物における大規模アウトブレイクも含む）"
  ),
  serious_ph_country = paste0(
    "Serious PH impact in the country: ",
    "その国（地域:その国を含む周辺）にとって、公衆衛生上のインパクトがあるか。",
    "国内の広域事例含む"
  ),
  serious_ph_japan = paste0(
    "Serious PH impact to Japan: ",
    "日本にとって、公衆衛生上のインパクトがあるか"
  ),
  epidemic_prone = paste0(
    "Epidemic-prone: クラスターや地域流行などにつながるもの。",
    "新たなパンデミックとなる可能性のある病原体。",
    "Animal-human interface for health (avian flu, swine flu, MERS)"
  ),
  mass_exposure = "Mass exposure: 集団への曝露の可能性があるか",
  high_profile = paste0(
    "High profile: 日本や国際的視点で関心が高いもの、懸念される事項など",
    "（特に国際機関、所内Directorsの関心など）。",
    "応用疫学研究センターとして対応（リスク評価、アクション等）の可能性があるもの。",
    "原因不明疾患のうち、重症度が高いもの、症例数が多いもの、日本への影響が考えられるもの"
  ),
  special_pathogen = paste0(
    "Special pathogen/bioterrorism agents: ",
    "一類感染症や、バイオテロに用いられる病原体の散発例は該当",
    "（ベースラインとしての発生の把握）。",
    "Bioterrorism agents: 痘そう、炭疽、ボツリヌス、野兎病、鼻疽・類鼻疽等"
  )
)

# ============================================================
# 疾患分類リスト（疾患名・分類リストシートより）
# ============================================================

DISEASE_CATEGORIES <- c(
  "Bioterrorism agents",
  "Viral haemorrhagic fever",
  "Zoonotic diseases",
  "Respiratory diseases",
  "Vaccine-preventable diseases",
  "Food-and-water-borne diseases",
  "Sexually transmitted diseases",
  "Mosquito-borne diseases",
  "Mycosis",
  "Tick-borne diseases",
  "Antimicrobial resistant diseases",
  "Undiagnosed",
  "Disaster",
  "Other"
)

# ============================================================
# プロンプト生成
# ============================================================

build_prompt <- function(title, summary, disease_list = NULL, country_list = NULL) {

  criteria_text <- paste(
    paste0("1. ", SCREENING_CRITERIA$unusual_unexpected),
    paste0("2. ", SCREENING_CRITERIA$serious_ph_country),
    paste0("3. ", SCREENING_CRITERIA$serious_ph_japan),
    paste0("4. ", SCREENING_CRITERIA$epidemic_prone),
    paste0("5. ", SCREENING_CRITERIA$mass_exposure),
    paste0("6. ", SCREENING_CRITERIA$high_profile),
    paste0("7. ", SCREENING_CRITERIA$special_pathogen),
    sep = "\n"
  )

  categories_text <- paste(DISEASE_CATEGORIES, collapse = ", ")

  prompt <- paste0(
    "あなたは感染症のサーベイランス専門家です。\n",
    "以下のニュース記事について、WHO EBS（Event-Based Surveillance）の基準に従って評価してください。\n\n",

    "【ニュースタイトル】\n", title, "\n\n",

    if (!is.null(summary) && nchar(trimws(summary)) > 0)
      paste0("【概要】\n", summary, "\n\n")
    else "",

    "【スクリーニング基準】\n", criteria_text, "\n\n",

    "【Signalの重みづけ基準】\n",
    "- Signal: 即座にリスク評価・対応が必要な重大事例\n",
    "- Event: 対応・モニタリングが必要な事例\n",
    "- FYI: 参考情報として共有すべき事例（対応不要）\n",
    "- Internal FYI: 内部参考情報のみ\n\n",

    "【疾患分類の選択肢】\n", categories_text, "\n\n",

    "以下のJSON形式のみで回答してください（説明文不要）:\n",
    "{\n",
    '  "unusual_unexpected": true/false,\n',
    '  "serious_ph_country": true/false,\n',
    '  "serious_ph_japan": true/false,\n',
    '  "epidemic_prone": true/false,\n',
    '  "mass_exposure": true/false,\n',
    '  "high_profile": true/false,\n',
    '  "special_pathogen": true/false,\n',
    '  "signal_weight": "Signal" or "Event" or "FYI" or "Internal FYI",\n',
    '  "disease_category": "上記疾患分類から1つ選択",\n',
    '  "disease_name_en": "英語疾患名",\n',
    '  "disease_name_ja": "日本語疾患名",\n',
    '  "location": "場所（国・地域名）",\n',
    '  "region": "地域区分（Asia/Africa/Europe/Americas/Middle East/Oceania/Global等）",\n',
    '  "reasoning": "判定理由を2-3文で"\n',
    "}"
  )

  prompt
}

# ============================================================
# Claude API呼び出し
# ============================================================

call_claude <- function(prompt, model = CLAUDE_MODEL, max_tokens = 1024) {

  req <- request("https://api.anthropic.com/v1/messages") |>
    req_headers(
      "x-api-key" = get_api_key(),
      "anthropic-version" = "2023-06-01",
      "content-type" = "application/json"
    ) |>
    req_body_json(list(
      model = model,
      max_tokens = max_tokens,
      messages = list(
        list(role = "user", content = prompt)
      )
    )) |>
    req_timeout(30) |>
    req_retry(max_tries = 3, backoff = ~ 2)

  resp <- req_perform(req)

  body <- resp_body_json(resp)
  content_text <- body$content[[1]]$text

  # JSONを抽出（```json ... ``` で囲まれている場合も対応）
  json_text <- gsub("```json\\s*|```", "", content_text)
  json_text <- trimws(json_text)

  fromJSON(json_text)
}

# ============================================================
# メイン関数: 1記事を評価
# ============================================================

screen_ebs_entry <- function(title, summary = "", model = CLAUDE_MODEL) {

  prompt <- build_prompt(title, summary)
  result <- call_claude(prompt, model = model)

  # 結果を整形
  list(
    # 判定（TRUE/FALSEをチェックマーク変換）
    unusual_unexpected    = if (isTRUE(result$unusual_unexpected)) "✓" else "",
    serious_ph_country    = if (isTRUE(result$serious_ph_country)) "✓" else "",
    serious_ph_japan      = if (isTRUE(result$serious_ph_japan)) "✓" else "",
    epidemic_prone        = if (isTRUE(result$epidemic_prone)) "✓" else "",
    mass_exposure         = if (isTRUE(result$mass_exposure)) "✓" else "",
    high_profile          = if (isTRUE(result$high_profile)) "✓" else "",
    special_pathogen      = if (isTRUE(result$special_pathogen)) "✓" else "",
    signal_weight         = result$signal_weight,
    # 分類
    disease_category      = result$disease_category,
    disease_name_en       = result$disease_name_en,
    disease_name_ja       = result$disease_name_ja,
    location              = result$location,
    region                = result$region,
    # 判定理由
    reasoning             = result$reasoning,
    # 生データ（デバッグ用）
    raw = result
  )
}

# ============================================================
# バッチ処理: 複数記事を一括評価
# ============================================================

screen_ebs_batch <- function(df, title_col = "Signalタイトル", summary_col = "概要",
                              model = CLAUDE_MODEL, sleep_sec = 0.5, verbose = TRUE) {
  results <- vector("list", nrow(df))

  for (i in seq_len(nrow(df))) {
    if (verbose) cat(sprintf("[%d/%d] %s\n", i, nrow(df), substr(df[[title_col]][i], 1, 60)))

    tryCatch({
      results[[i]] <- screen_ebs_entry(
        title   = df[[title_col]][i],
        summary = if (summary_col %in% names(df)) df[[summary_col]][i] else "",
        model   = model
      )
    }, error = function(e) {
      if (verbose) cat("  ERROR:", conditionMessage(e), "\n")
      results[[i]] <<- list(error = conditionMessage(e))
    })

    Sys.sleep(sleep_sec)
  }

  # データフレームに変換
  result_df <- do.call(rbind, lapply(results, function(r) {
    if (!is.null(r$error)) {
      data.frame(
        unusual_unexpected = NA, serious_ph_country = NA, serious_ph_japan = NA,
        epidemic_prone = NA, mass_exposure = NA, high_profile = NA,
        special_pathogen = NA, signal_weight = NA,
        disease_category = NA, disease_name_en = NA, disease_name_ja = NA,
        location = NA, region = NA, reasoning = r$error,
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(
        unusual_unexpected = r$unusual_unexpected,
        serious_ph_country = r$serious_ph_country,
        serious_ph_japan   = r$serious_ph_japan,
        epidemic_prone     = r$epidemic_prone,
        mass_exposure      = r$mass_exposure,
        high_profile       = r$high_profile,
        special_pathogen   = r$special_pathogen,
        signal_weight      = r$signal_weight,
        disease_category   = r$disease_category,
        disease_name_en    = r$disease_name_en,
        disease_name_ja    = r$disease_name_ja,
        location           = r$location,
        region             = r$region,
        reasoning          = r$reasoning,
        stringsAsFactors = FALSE
      )
    }
  }))

  cbind(df, result_df)
}

# ============================================================
# テスト実行（直接実行時のみ）
# ============================================================

if (sys.nframe() == 0) {
  cat("=== EBS AI Screening テスト ===\n\n")

  # テスト記事
  test_title   <- "Cholera – Benin"
  test_summary <- paste0(
    "2021年9月1日から2022年1月16日までに1430例の報告があり、",
    "うち20例が死亡（CFR：1.4％）WASH対策が不十分な国でのアウトブレイク。",
    "日本ではコレラワクチンは任意接種扱い（自費）。"
  )

  cat("タイトル:", test_title, "\n")
  cat("概要:", test_summary, "\n\n")
  cat("評価中...\n")

  result <- screen_ebs_entry(test_title, test_summary)

  cat("\n=== 判定結果 ===\n")
  cat("Unusual/unexpected:           ", result$unusual_unexpected, "\n")
  cat("Serious PH impact (country):  ", result$serious_ph_country, "\n")
  cat("Serious PH impact (Japan):    ", result$serious_ph_japan, "\n")
  cat("Epidemic-prone:               ", result$epidemic_prone, "\n")
  cat("Mass exposure:                ", result$mass_exposure, "\n")
  cat("High profile:                 ", result$high_profile, "\n")
  cat("Special pathogen/bioterrorism:", result$special_pathogen, "\n")
  cat("Signal重みづけ:               ", result$signal_weight, "\n")
  cat("疾患分類:                     ", result$disease_category, "\n")
  cat("疾患名(英):                   ", result$disease_name_en, "\n")
  cat("疾患名(日):                   ", result$disease_name_ja, "\n")
  cat("場所:                         ", result$location, "\n")
  cat("地域:                         ", result$region, "\n")
  cat("判定理由:                     ", result$reasoning, "\n")
}
