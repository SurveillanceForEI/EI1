# ============================================================
# SNS情報（Bluesky）取得
# ------------------------------------------------------------
# EBS（ニュース・行政機関等の一次情報）とは別建てで、Bluesky上の一般ユーザー投稿を
# 感染症サーベイランスの補助情報として表示する。
#
# X（旧Twitter）は2026年2月の料金体系変更で新規開発者向けの無料枠が撤廃され、
# 従量課金（読み取り$0.005/件等）のみとなり運用コストが青天井になりやすいため採用を
# 見送った。Blueskyは検索APIが無料で使えるが、2026年時点で未認証（ログアウト状態）
# での検索がBluesky側の仕様変更で無効化されているため、専用アカウントのアプリ
# パスワードで認証してから検索する。
#
# 認証情報は環境変数で渡す（.Renvironまたは Connect Cloud の環境変数設定）:
#   BLUESKY_IDENTIFIER    … ログイン用メールアドレスまたはハンドル名
#   BLUESKY_APP_PASSWORD  … アプリパスワード（通常のログインパスワードではない）
#
# 投稿は玉石混交（個人の感想・雑談等のノイズも多い）のため、EBSの一次情報とは
# 明確に区別して「参考情報」として別タブに表示する想定。

BLUESKY_SEARCH_KEYWORDS <- c(
  "インフルエンザ", "新型コロナ", "麻しん", "はしか", "風しん",
  "手足口病", "感染性胃腸炎", "ノロウイルス", "食中毒", "百日咳",
  "溶連菌", "RSウイルス", "集団感染", "感染症"
)

.bluesky_login <- function(identifier = Sys.getenv("BLUESKY_IDENTIFIER"),
                            app_password = Sys.getenv("BLUESKY_APP_PASSWORD")) {
  if (!nzchar(identifier) || !nzchar(app_password)) {
    stop("環境変数 BLUESKY_IDENTIFIER / BLUESKY_APP_PASSWORD が設定されていません")
  }
  resp <- httr::POST(
    "https://bsky.social/xrpc/com.atproto.server.createSession",
    body = list(identifier = identifier, password = app_password),
    encode = "json"
  )
  if (httr::status_code(resp) != 200) {
    stop("Bluesky ログイン失敗: HTTP ", httr::status_code(resp))
  }
  httr::content(resp, as = "parsed", simplifyVector = FALSE)$accessJwt
}

# 1キーワードあたりlimit件を検索し、data.frameで返す
.bluesky_search_one <- function(jwt, keyword, limit = 20) {
  resp <- tryCatch(
    httr::GET(
      "https://bsky.social/xrpc/app.bsky.feed.searchPosts",
      query = list(q = keyword, limit = limit, lang = "ja"),
      httr::add_headers(Authorization = paste("Bearer", jwt))
    ),
    error = function(e) NULL
  )
  if (is.null(resp) || httr::status_code(resp) != 200) return(NULL)
  d <- httr::content(resp, as = "parsed", simplifyVector = FALSE)
  posts <- d$posts
  if (length(posts) == 0) return(NULL)

  out <- lapply(posts, function(p) {
    handle_parts <- strsplit(p$uri, "/")[[1]]
    post_id <- handle_parts[length(handle_parts)]
    data.frame(
      keyword    = keyword,
      author     = coalesce_chr(p$author$displayName, p$author$handle),
      handle     = p$author$handle,
      text       = coalesce_chr(p$record$text, ""),
      created_at = coalesce_chr(p$record$createdAt, NA_character_),
      like_count = as.integer(coalesce_num(p$likeCount, 0)),
      repost_count = as.integer(coalesce_num(p$repostCount, 0)),
      uri        = p$uri,
      url        = paste0("https://bsky.app/profile/", p$author$handle, "/post/", post_id),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, out)
}

coalesce_chr <- function(x, default) if (is.null(x) || !nzchar(x)) default else x
coalesce_num <- function(x, default) if (is.null(x)) default else x

# ── 紛れ込み対策 ────────────────────────────────────────────
# 「はしか」等の短い和語キーワードは、疾患名としてではなく文法的な語尾
# （「〜はしなかった」「〜はしません」「〜はしかねない」等の「〜は」＋
# 「し」＋「か」の組み合わせ）としてヒットしてしまうことが多い
# （実例: 2026-09-18 ユーザー指摘。「はしか」検索で無関係な投稿が混入）。
# 疾患名の直後に助詞・句読点等が続く自然な名詞用法のみを許可する
.SNS_KEYWORD_FALSE_POSITIVE_PATTERNS <- list(
  "はしか" = "はし(かね|かった|ません|ない|なかった)"
)

# 「インフルエンザ」等の疾患名キーワードにヒットしても、実際には症状・流行状況等
# 感染症サーベイランスの文脈を伴わない個人の雑談・比喩表現等が大半を占める。
# EBSの一次情報と異なりSNS投稿は文脈情報が乏しいため、キーワードに加えて
# 感染症の発生状況を示唆する語（症状・流行・行政的対応等）が最低1つ共起する
# ことを要求し、雑談・比喩的な言及を減らす
.SNS_CONTEXT_KEYWORDS <- c(
  "感染", "患者", "ウイルス", "流行", "発症", "症状", "ワクチン", "予防接種",
  "陽性", "発熱", "咳", "下痢", "嘔吐", "学級閉鎖", "休校", "休園",
  "集団感染", "保健所", "厚労省", "自治体", "医療機関", "病院", "感染症",
  "定点", "報告数", "警報", "注意報", "食中毒", "検疫"
)

.sns_is_relevant <- function(keyword, text) {
  # Bluesky検索は語形変化・トークン単位の緩いマッチングを行っており、検索語を
  # 文字列として全く含まない投稿（例:「はしか」で検索して「骨しか」がヒット）が
  # 返ってくることがある（実例: 2026-09-18 ユーザー指摘で発覚）。まず検索語を
  # 文字列として実際に含むことを必須にする
  if (!grepl(keyword, text, fixed = TRUE)) return(FALSE)

  fp <- .SNS_KEYWORD_FALSE_POSITIVE_PATTERNS[[keyword]]
  if (!is.null(fp) && grepl(fp, text, perl = TRUE)) return(FALSE)
  # キーワード自体が既に感染症文脈を強く示唆する語（ウイルス名等を含む）の
  # 場合は文脈語の共起を必須にしない
  if (grepl("ウイルス|感染症|食中毒", keyword, fixed = FALSE)) return(TRUE)
  any(vapply(.SNS_CONTEXT_KEYWORDS, function(k) grepl(k, text, fixed = TRUE), logical(1)))
}

# 全キーワードを検索し、重複投稿(uri基準)を除去して返す
fetch_bluesky_posts <- function(keywords = BLUESKY_SEARCH_KEYWORDS, limit_per_keyword = 20) {
  jwt <- .bluesky_login()
  results <- lapply(keywords, function(kw) {
    tryCatch(.bluesky_search_one(jwt, kw, limit_per_keyword), error = function(e) NULL)
  })
  df <- do.call(rbind, Filter(Negate(is.null), results))
  if (is.null(df) || nrow(df) == 0) return(df)

  # 同じ投稿が複数キーワードにヒットした場合はuriで重複排除（最初にヒットしたキーワードを残す）
  df <- df[!duplicated(df$uri), ]

  # キーワードの文法的な誤爆・文脈の乏しい雑談投稿を除外する
  is_relevant <- vapply(seq_len(nrow(df)), function(i) {
    tryCatch(.sns_is_relevant(df$keyword[i], df$text[i]), error = function(e) TRUE)
  }, logical(1))
  df <- df[is_relevant, , drop = FALSE]
  if (nrow(df) == 0) return(df)

  # 既存のEBSノイズ判定ロジックを流用（is_noise_articleはR/ebs_rule_screening.Rで定義）
  if (exists("is_noise_article", mode = "function")) {
    is_noise <- vapply(seq_len(nrow(df)), function(i) {
      tryCatch(isTRUE(is_noise_article(df$text[i], "")), error = function(e) FALSE)
    }, logical(1))
    df <- df[!is_noise, , drop = FALSE]
  }
  if (nrow(df) == 0) return(df)

  df <- df[order(df$created_at, decreasing = TRUE), ]
  df$fetched_at <- as.character(Sys.time())
  df
}

# 取得結果をキャッシュファイルに保存する（scripts/からの定期実行用）。
# Bluesky検索APIは直近の投稿しか返さないため、疾患トレンド（日別件数の推移）を
# 出すには毎回の取得結果を蓄積する必要がある。そのため過去のキャッシュに
# 新規分をマージし、uriで重複排除したうえで保存する（保持期間: keep_days）。
refresh_bluesky_cache <- function(cache_path = "data/sns_bluesky_cache.rds",
                                   keywords = BLUESKY_SEARCH_KEYWORDS,
                                   limit_per_keyword = 20,
                                   keep_days = 90) {
  new_df <- fetch_bluesky_posts(keywords, limit_per_keyword)
  old_df <- if (file.exists(cache_path)) tryCatch(readRDS(cache_path), error = function(e) NULL) else NULL

  if ((is.null(new_df) || nrow(new_df) == 0) && is.null(old_df)) {
    message("Bluesky: 新規投稿なし、またはノイズ除去後0件")
    return(invisible(NULL))
  }

  df <- if (is.null(old_df)) new_df
        else if (is.null(new_df)) old_df
        else rbind(new_df, old_df)
  df <- df[!duplicated(df$uri), ]

  created <- suppressWarnings(as.POSIXct(df$created_at, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
  cutoff <- Sys.time() - as.difftime(keep_days, units = "days")
  df <- df[is.na(created) | created >= cutoff, , drop = FALSE]
  df <- df[order(df$created_at, decreasing = TRUE), ]

  dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(df, cache_path)
  message("Bluesky: 累計", nrow(df), "件のSNS投稿を保存しました（新規取得", if (is.null(new_df)) 0 else nrow(new_df), "件）")
  invisible(df)
}
