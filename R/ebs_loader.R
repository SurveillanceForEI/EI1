# ============================================================
# ebs_loader.R  — Event-Based Surveillance データ取得
# ソース: RSS (厚労省/JIHS/WHO/ProMED/ECDC/NHK) +
#         Google News RSS
# ============================================================

library(httr)
library(xml2)
library(dplyr)
library(lubridate)
library(jsonlite)

# ── 国内・海外判定（app.Rのis_overseas_articleと同じロジック）────────────
.JAPAN_KW_LOADER <- c(
  "日本","japan","tokyo","osaka","kyoto","yokohama","sapporo","fukuoka",
  "nagoya","kobe","sendai","hiroshima","東京","大阪","京都","横浜",
  "札幌","福岡","名古屋","神戸","仙台","広島","沖縄","北海道",
  "都道府県","prefecture","厚生労働省","mhlw","jihs","niid",
  "国内","全国","感染症法","保健所","定点"
)
.OVERSEAS_KW_LOADER <- c(
  # アフリカ
  "africa","アフリカ","congo","コンゴ","drc","nigeria","ナイジェリア",
  "kenya","ethiopia","tanzania","mozambique","somalia","cameroon","zambia",
  "zimbabwe","malawi","guinea","sierra leone","liberia",
  # アジア・中東
  "china","中国","india","インド","vietnam","bangladesh","philippines",
  "sri lanka","pakistan","myanmar","cambodia","thailand","indonesia",
  "malaysia","nepal","afghanistan","iran","iraq","syria","yemen",
  "saudi arabia","turkey","israel","gaza","lebanon",
  # 南米・中米
  "brazil","ブラジル","mexico","メキシコ","venezuela","colombia",
  "peru","ecuador","chile","argentina","bolivia","haiti","cuba",
  "barbados","caribbean","latin america","central america",
  # 欧米・オセアニア
  "europe","ヨーロッパ","欧州","usa","united states","アメリカ","米国",
  "canada","uk ","united kingdom","australia","new zealand",
  "france","germany","spain","italy","ukraine","russia",
  # 広域・国際
  "global","グローバル","worldwide","world:","southern hemisphere",
  "northern hemisphere","international","multinational","cross-border",
  "who outbreak","cdc alert","ecdc","promed","reliefweb",
  "outbreak in","cases in","deaths in","reported in",
  "situation report","sitrep","humanitarian",
  # ReliefWeb形式（"国名: タイトル" or "国名 - タイトル"）
  "dr congo:","drc:","afghanistan -","venezuela -","barbados -",
  "haiti -","myanmar -","somalia -","sudan -","south sudan -",
  # 海外在住日本語コミュニティ向けメディア・海外都市名（本文が日本語要約のため
  # 国名キーワードに一致しないケースの補完）
  "ニューヨーク","マンハッタン","アッパーイーストサイド","japion","ロサンゼルス",
  "ハワイ報知","ブラジル日報","paulista shimbun"
)

# COUNTRY_DB（ebs_rule_screening.R、地域分類用）は英語・カタカナ・漢字表記の国名を
# 網羅的に持っている。.OVERSEAS_KW_LOADERの手書きリストには「香港」「台湾」「スリランカ」
# 「韓国」等のカタカナ/漢字国名が漏れており、国内/海外判定にズレが生じていたため、
# COUNTRY_DBから全国名を取り込んで補完する（COUNTRY_DBはebs_rule_screening.R側で
# ebs_loader.Rより先にsourceされている前提）。
if (exists("COUNTRY_DB")) {
  # Japanエントリ自体は除外（"japan"/"日本"が海外キーワードに混入しないように）
  .country_db_kw <- unique(tolower(unlist(lapply(
    Filter(function(c) !identical(c$en[1], "Japan"), COUNTRY_DB),
    function(c) c(c$en, c$ja)
  ))))
  .OVERSEAS_KW_LOADER <- unique(c(.OVERSEAS_KW_LOADER, .country_db_kw))
}

# is_overseas_article()は記事1件ごとにキーワードリスト（数百件）をsapplyで
# 1件ずつgreplしていたため、記事件数×キーワード数のR関数呼び出しが発生し
# 著しく遅かった（実測: 2859件で75秒）。キーワードリストはこの時点（ソース
# 読み込み時）で固定なので、あらかじめ「(?:kw1|kw2|...)」形式の1本の正規表現に
# まとめておき、判定時は行ごとに1回のgreplで済ませる（判定結果は従来の
# fixed=TRUE照合のOR条件と等価）。
.escape_regex_lit <- function(x) gsub("([.^$|()\\[\\]{}*+?\\\\])", "\\\\\\1", x, perl = TRUE)
.build_kw_pattern <- function(kws) paste0("(?:", paste(.escape_regex_lit(kws), collapse = "|"), ")")
.build_kw_boundary_pattern <- function(kws) paste0("\\b(?:", paste(.escape_regex_lit(trimws(kws)), collapse = "|"), ")\\b")

# 短い（2〜5文字）英数字のみの略語・国名（"UAE","DRC","Mali","Oman"等）は、
# fixed部分一致だと英単語の内部にたまたま出現して誤マッチしやすい
# （例: "Oman" ⊂ "woman"、"Mali" ⊂ "malignant"）。tag_diseases()のkeyword_matches()
# と同じ考え方で、そうした短い英数字トークンだけ単語境界(\b)付きにし、
# それ以外（日本語や3文字超の英語フレーズ）は従来どおり境界なしの部分一致とする
# （日本語はスペース区切りが無く\bが単語内部で成立しないため、日本語キーワードに
# \bを付けると一致しなくなってしまう。実際に検証済み: "\bコンゴ\b"は
# 「コンゴでエボラが流行」のような通常の地の文とは一致しない）。
# 1本の正規表現にまとめる方式は変えない（記事数×キーワード数のループを避けるため）。
.build_kw_pattern_smart <- function(kws) {
  kws <- trimws(kws)
  is_short_token <- grepl("^[A-Za-z0-9-]{2,5}$", kws)
  parts <- ifelse(is_short_token,
                   paste0("\\b", .escape_regex_lit(kws), "\\b"),
                   .escape_regex_lit(kws))
  paste0("(?:", paste(parts, collapse = "|"), ")")
}

.JAPAN_KW_PATTERN            <- .build_kw_pattern_smart(.JAPAN_KW_LOADER)
.OVERSEAS_KW_PATTERN         <- .build_kw_pattern_smart(.OVERSEAS_KW_LOADER)
.OVERSEAS_KW_BOUNDARY_PATTERN <- .build_kw_boundary_pattern(.OVERSEAS_KW_LOADER)
.FP_WORDS_PATTERN <- if (exists(".COUNTRY_MATCH_FALSE_POSITIVE_WORDS"))
  .build_kw_pattern(.COUNTRY_MATCH_FALSE_POSITIVE_WORDS) else NULL

# 国際ソース：日本キーワードがなければ原則海外
# （台湾CDCの中国語記事等、記事本文が「國內」等の現地語表現を使うため地名キーワード判定
# だけでは海外記事と検出できず、誤って国内タブに表示されてしまう問題があった。
# ソース自体が海外の政府機関・国際機関である場合は、記事内容に関わらず原則海外として扱う）
.OVERSEAS_SOURCE_IDS_LOADER <- c(
  "reliefweb", "who_eios", "who_don", "cdc", "ukhsa", "rki", "nicd",
  "taiwan_cdc", "china_cdc", "chp", "spf"
)

# Google Newsは「記事タイトル - メディア名」形式（title）、
# および「記事タイトル &nbsp;&nbsp; メディア名」形式（summary、区切りがダッシュではない）で
# メディア名を末尾に含む。メディア名に「日本経済新聞」「○○日本版」「日本医事新報社」
# 「東京新聞」「北海道新聞」等、記事内容と無関係に「日本」や都道府県名を含むものが多いため、
# 地名・国内外判定の前にこの部分を除去する。
strip_gnews_suffix <- function(title, source_id = "") {
  title <- if (is.na(title)) "" else title
  sid <- tolower(trimws(as.character(source_id)))
  if (!grepl("^gnews", sid)) return(title)
  b <- trimws(gsub("(&nbsp;)+\\s*[^&]+$", "", title, perl = TRUE))
  b <- trimws(gsub("[-–—]\\s*[^-–—]+$", "", b, perl = TRUE))
  if (nchar(b) < 5) title else b
}

# Google Newsのtitleから末尾のメディア名部分（例:「Vietnam.vn」「CGTN Japanese」）を抽出する。
# ebs_loader.Rのsource_name列はGoogle News経由の記事では常に"Google News"固定であり
# 元メディア名を保持していないため、titleサフィックスから直接取り出す必要がある。
extract_gnews_media_name <- function(title, source_id = "") {
  title <- if (is.na(title)) "" else title
  sid <- tolower(trimws(as.character(source_id)))
  if (!grepl("^gnews", sid)) return("")
  stripped <- strip_gnews_suffix(title, source_id)
  if (nchar(stripped) >= nchar(title)) return("")
  trimws(sub("^[-–—]\\s*", "", substr(title, nchar(stripped) + 1, nchar(title))))
}

is_overseas_article <- function(title, summary, ebs_pref = NA, source_id = "", source_name = "") {
  if (!is.na(ebs_pref) && nchar(ebs_pref) > 0) return(FALSE)
  title   <- if (is.na(title))   "" else title
  summary <- if (is.na(summary)) "" else summary
  sid <- tolower(trimws(as.character(source_id)))
  # summaryもGoogle Newsでは末尾にタイトルと同じメディア名サフィックスを含むため同様に除去
  title_body   <- strip_gnews_suffix(title, source_id)
  summary_body <- strip_gnews_suffix(summary, source_id)

  detected <- tryCatch(
    detect_pref(title = title_body, summary = summary_body),
    error = function(e) NA_character_
  )
  if (!is.na(detected)) return(FALSE)

  # タイトルに海外キーワード（国名等）が含まれる場合、要約中の付随的な地名
  # （NPO本部所在地・記者発信地・広告文等、記事本題と無関係な「日本」「京都」等の言及）に
  # 惑わされないよう、判定対象をタイトルのみに限定する（detect_prefと同じ考え方）。
  # 「サーベイランス」等、国名マッチングと衝突しうる頻出語を除去してから判定する
  # （classify_locationと同じ考え方。例:「サーベイランス」→「イラン」を誤検出する）
  strip_fp <- function(s) {
    if (is.null(.FP_WORDS_PATTERN)) return(s)
    gsub(.FP_WORDS_PATTERN, "", s, perl = TRUE)
  }
  title_low <- strip_fp(tolower(title_body))
  title_has_overseas <- grepl(.OVERSEAS_KW_PATTERN, title_low, perl = TRUE)
  txt <- if (title_has_overseas) title_low else strip_fp(tolower(paste(title_body, summary_body)))

  has_japan    <- grepl(.JAPAN_KW_PATTERN,    txt, perl = TRUE)
  has_overseas <- grepl(.OVERSEAS_KW_PATTERN, txt, perl = TRUE)

  # メディア名自体が海外を示す場合（例:「Vietnam.vn」「CGTN」等）も海外シグナルとして扱う。
  # 記事本文が現地語の翻訳等で国名に言及しないケース（Vietnam.vn等の現地メディアがベトナム
  # 国内向け記事をそのまま配信している場合）を拾うため。Google News経由はsource_name列が
  # 常に"Google News"固定なので、titleサフィックスから元メディア名を抽出して判定する。
  # ただしメディア名の「日本」等は（日本経済新聞・ニューズウィーク日本版等と同様）
  # 国内シグナルとしては使わない。
  gnews_media   <- extract_gnews_media_name(title, source_id)
  media_low     <- tolower(paste(coalesce(source_name, ""), gnews_media))

  # *.lg.jp（地方自治体）・*.go.jp（中央省庁）は日本の行政ドメインに限定して
  # 発行されるため、ドメイン自体が国内発信の確実な根拠になる。これを見ないと、
  # 「pref.fukuoka.lg.jp」「city.kawachinagano.lg.jp」等のローマ字表記に
  # "uk"（UK＝英国の略称）や"china"（中国）が偶然部分一致し、国内自治体の
  # お知らせが海外記事に誤分類される事故が起きる
  # （実例: 2026-07-24 ユーザー報告で発覚）。
  media_is_japan_gov_domain <- grepl("\\.(lg|go)\\.jp\\b", media_low)

  # メディア名（ローマ字・欧文表記）に対する国名等キーワード判定は、部分一致
  # (fixed=TRUE)だとドメイン名・地名ローマ字表記中に偶然キーワードが出現して
  # 誤検出しやすい（上記の例）。単語境界(\\b)を要求することで、"uk"が
  # "fukuoka"の一部として誤って一致するのを防ぐ。記号を含むキーワード
  # （"dr congo:"等）は正規表現化に失敗し得るためtryCatchでfixed判定に
  # フォールバックする。
  .kw_matches_word_boundary <- function(k, text) {
    tryCatch(
      grepl(paste0("\\b", gsub("([.^$|()\\[\\]{}*+?\\\\])", "\\\\\\1", trimws(k), perl = TRUE), "\\b"),
            text, perl = TRUE),
      error = function(e) grepl(k, text, fixed = TRUE)
    )
  }
  media_has_overseas <- !media_is_japan_gov_domain && tryCatch(
    grepl(.OVERSEAS_KW_BOUNDARY_PATTERN, media_low, perl = TRUE),
    # 何らかの理由でまとめた正規表現が不正になった場合のみ、従来通り
    # キーワードを1件ずつ照合するフォールバックにする
    error = function(e) any(sapply(.OVERSEAS_KW_LOADER,
      function(k) .kw_matches_word_boundary(k, media_low)))
  )

  # メディア名がキリル文字・ハングル・アラビア文字等、日本語圏で通常使われない文字体系
  # のみで構成される場合（例:「Межа. Новини України.」等、ウクライナ語の現地メディア名）は、
  # 国名キーワードに一致しなくても海外メディアとみなす
  # （タイトルが日本語に翻訳・要約されているため本文からは国名が読み取れないケースの対策）
  media_non_japanese_script <- nchar(gsub("[^Ѐ-ӿ가-힣؀-ۿ]", "",
                                            gnews_media)) > 0

  if (sid %in% .OVERSEAS_SOURCE_IDS_LOADER) return(!has_japan)
  if (sid == "jptimes") return(!has_japan)
  !has_japan && (has_overseas || media_has_overseas || media_non_japanese_script)
}

# ベクトル版 is_overseas_article()。mapply(is_overseas_article, ...)で記事
# 1件ごとに呼び出すと、内部で使うdetect_pref()のCITY_PREF_MAP走査等が
# 「記事数 × 候補数」のR関数呼び出しになり遅い（実測: 2859件で約75秒、
# detect_pref_vec化前で約17秒）。判定ロジックはis_overseas_article()と
# 完全に同一（分岐・優先順位とも変更なし）で、内部の重い判定（都道府県特定・
# キーワード一致）だけをベクトル化している。国内/海外タブの一覧表示・
# トレンド集計等、記事件数分をまとめて判定する箇所ではこちらを使う。
is_overseas_article_vec <- function(titles, summaries, ebs_prefs = NA, source_ids = "", source_names = "") {
  n <- length(titles)
  titles    <- ifelse(is.na(titles), "", titles)
  summaries <- ifelse(is.na(summaries), "", summaries)
  if (length(ebs_prefs) == 1) ebs_prefs <- rep(ebs_prefs, n)
  if (length(source_ids) == 1) source_ids <- rep(source_ids, n)
  if (length(source_names) == 1) source_names <- rep(source_names, n)
  sids <- tolower(trimws(as.character(source_ids)))

  title_body   <- mapply(strip_gnews_suffix, titles, source_ids, USE.NAMES = FALSE)
  summary_body <- mapply(strip_gnews_suffix, summaries, source_ids, USE.NAMES = FALSE)

  detected <- tryCatch(
    detect_pref_vec(title_body, summary_body),
    error = function(e) rep(NA_character_, n)
  )

  strip_fp <- function(s) {
    if (is.null(.FP_WORDS_PATTERN)) return(s)
    gsub(.FP_WORDS_PATTERN, "", s, perl = TRUE)
  }
  title_low <- strip_fp(tolower(title_body))
  title_has_overseas <- grepl(.OVERSEAS_KW_PATTERN, title_low, perl = TRUE)
  txt <- ifelse(title_has_overseas, title_low, strip_fp(tolower(paste(title_body, summary_body))))

  has_japan    <- grepl(.JAPAN_KW_PATTERN,    txt, perl = TRUE)
  has_overseas <- grepl(.OVERSEAS_KW_PATTERN, txt, perl = TRUE)

  gnews_media <- mapply(extract_gnews_media_name, titles, source_ids, USE.NAMES = FALSE)
  media_low   <- tolower(paste(coalesce(source_names, ""), gnews_media))
  media_is_japan_gov_domain <- grepl("\\.(lg|go)\\.jp\\b", media_low)

  media_has_overseas <- !media_is_japan_gov_domain & tryCatch(
    grepl(.OVERSEAS_KW_BOUNDARY_PATTERN, media_low, perl = TRUE),
    error = function(e) rep(FALSE, n)
  )

  media_non_japanese_script <- nchar(gsub("[^Ѐ-ӿ가-힣؀-ۿ]", "", gnews_media)) > 0

  base_result <- ifelse(
    sids %in% .OVERSEAS_SOURCE_IDS_LOADER | sids == "jptimes",
    !has_japan,
    !has_japan & (has_overseas | media_has_overseas | media_non_japanese_script)
  )

  known_pref <- !is.na(ebs_prefs) & nchar(ebs_prefs) > 0
  ifelse(known_pref | !is.na(detected), FALSE, base_result)
}

# ── 公式情報源判定 ────────────────────────────────────────
# 都道府県(pref_*)・政令指定都市/中核市等の自治体(city_*)は全て自治体公式サイトからの
# 直接取得のため一律公式扱いとする。それ以外は行政機関・国際機関・国際保健機関の
# 公式サイト/公式RSSのみを列挙する（報道機関・ニュース集約(Google News)・
# 学術論文(PubMed)・SNSは公式情報源に含めない）。
# who_eiosはWHOのツールだが、世界中の一般メディア記事を自動収集するアグリゲーターであり
# WHO自身が発信した公式情報ではないため、あえて公式情報源には含めない
OFFICIAL_EBS_SOURCE_IDS <- c(
  "mhlw", "jihs", "cdc", "reliefweb", "rki", "ukhsa", "nicd",
  "taiwan_cdc", "china_cdc", "chp", "spf", "who_don"
)

is_official_ebs_source <- function(source_id) {
  sid <- tolower(trimws(as.character(source_id)))
  !is.na(sid) & (grepl("^pref_", sid) | grepl("^city_", sid) | sid %in% OFFICIAL_EBS_SOURCE_IDS)
}

# ── 固定RSSフィード ────────────────────────────────────────
EBS_SOURCES <- list(
  # ── 日本・行政 ─────────────────────────────────────────────
  list(id="mhlw",    name="厚生労働省",             lang="ja", category="行政",
       url="https://www.mhlw.go.jp/stf/news.rdf"),
  # jihsはRSS(https://www.niid.jihs.go.jp/feed/)が廃止(404)されたため、
  # EBS_SOURCESには含めずHTMLスクレイピング経由のfetch_jihs_news()で個別取得する
  # ── 都道府県（保健所設置自治体） ───────────────────────────
  # 東京都・兵庫県は保健医療局/健康カテゴリ専用RSSのため精度が高い。
  # 埼玉県・北海道・愛知県は全庁共通RSSのため、既存のINFECT_FILTER_KEYWORDSによる
  # 感染症関連キーワード絞り込みに依存する
  list(id="pref_tokyo",    name="東京都 保健医療局",  lang="ja", category="行政",
       url="https://www.hokeniryo.metro.tokyo.lg.jp/index.html/-/asset_publisher/fjpa/rss"),
  list(id="pref_hyogo",    name="兵庫県（健康）",      lang="ja", category="行政",
       url="https://web.pref.hyogo.lg.jp/rss/health.xml"),
  list(id="pref_saitama",  name="埼玉県 県政ニュース", lang="ja", category="行政",
       url="https://www.pref.saitama.lg.jp/news/news.xml"),
  list(id="pref_hokkaido", name="北海道 お知らせ",     lang="ja", category="行政",
       url="https://www.pref.hokkaido.lg.jp/news/oshirase/rss.xml"),
  list(id="pref_aichi",    name="愛知県",              lang="ja", category="行政",
       url="https://www.pref.aichi.jp/rss/10/list1.xml"),
  list(id="pref_mie",      name="三重県（報道発表）",  lang="ja", category="行政",
       url="https://www.pref.mie.lg.jp/app/rss/hodo_rss"),
  list(id="pref_shizuoka", name="静岡県",              lang="ja", category="行政",
       url="https://www.pref.shizuoka.jp/news.rss"),
  list(id="pref_fukushima",name="福島県",              lang="ja", category="行政",
       url="https://www.pref.fukushima.lg.jp/rss/10/list1.xml"),
  list(id="pref_gunma",    name="群馬県",              lang="ja", category="行政",
       url="https://www.pref.gunma.jp/rss/10/list1.xml"),
  list(id="pref_niigata",  name="新潟県",              lang="ja", category="行政",
       url="https://www.pref.niigata.lg.jp/rss/10/list1.xml"),
  list(id="pref_gifu",     name="岐阜県",              lang="ja", category="行政",
       url="https://www.pref.gifu.lg.jp/rss/10/list1.xml"),
  list(id="pref_shiga",    name="滋賀県",              lang="ja", category="行政",
       url="https://www.pref.shiga.lg.jp/file/rss/kensei_koho_e-shinbun_oshirase_index.rss"),
  list(id="pref_aomori",   name="青森県",              lang="ja", category="行政",
       url="https://www.pref.aomori.lg.jp/rss/feed.rss"),
  list(id="pref_iwate",    name="岩手県",              lang="ja", category="行政",
       url="https://www.pref.iwate.jp/news.rss"),
  list(id="pref_tottori",  name="鳥取県（報道発表）",  lang="ja", category="行政",
       url="https://www.pref.tottori.lg.jp/xml/pressrelease_RSS/all.xml"),
  list(id="pref_shimane",  name="島根県",              lang="ja", category="行政",
       url="https://www.pref.shimane.lg.jp/top_news.rdf"),
  list(id="pref_okayama",  name="岡山県",              lang="ja", category="行政",
       url="https://www.pref.okayama.jp/rss/10/list1.xml"),
  list(id="pref_hiroshima",name="広島県",              lang="ja", category="行政",
       url="https://www.pref.hiroshima.lg.jp/rss/10/list1.xml"),
  list(id="pref_yamaguchi",name="山口県",              lang="ja", category="行政",
       url="https://www.pref.yamaguchi.lg.jp/rss/10/list6.xml"),
  list(id="pref_ehime",    name="愛媛県",              lang="ja", category="行政",
       url="https://www.pref.ehime.jp/rss/20/list1.xml"),
  list(id="pref_kumamoto", name="熊本県",              lang="ja", category="行政",
       url="https://www.pref.kumamoto.jp/rss/10/list1.xml"),
  list(id="pref_oita",     name="大分県",              lang="ja", category="行政",
       url="https://www.pref.oita.jp/rss/10/list1.xml"),
  list(id="pref_kagoshima",name="鹿児島県",            lang="ja", category="行政",
       url="https://www.pref.kagoshima.jp/saishin/saishin.xml"),
  list(id="pref_okinawa",  name="沖縄県",              lang="ja", category="行政",
       url="https://www.pref.okinawa.lg.jp/news.rss"),
  list(id="pref_chiba",    name="千葉県",              lang="ja", category="行政",
       url="https://www.pref.chiba.lg.jp/homepage/shinchaku/shinchaku.xml"),
  list(id="pref_yamanashi",name="山梨県（報道発表）",  lang="ja", category="行政",
       url="https://www.pref.yamanashi.jp/release/release.xml"),
  list(id="pref_nagano",   name="長野県",              lang="ja", category="行政",
       url="https://www.pref.nagano.lg.jp/chumoku/chumoku.xml"),
  list(id="pref_miyazaki", name="宮崎県（記者発表）",  lang="ja", category="行政",
       url="https://www.pref.miyazaki.lg.jp/kisha/hodo.xml"),
  list(id="pref_miyagi",   name="宮城県（報道発表）",  lang="ja", category="行政",
       url="https://www.pref.miyagi.jp/release/release.xml"),
  list(id="pref_yamagata", name="山形県",              lang="ja", category="行政",
       url="https://www.pref.yamagata.jp/shinchaku/shinchaku.xml"),
  list(id="pref_toyama",   name="富山県",              lang="ja", category="行政",
       url="https://www.pref.toyama.jp/shinchaku/shinchaku.xml"),
  list(id="pref_ishikawa", name="石川県",              lang="ja", category="行政",
       url="https://www.pref.ishikawa.lg.jp/shinchaku/shinchaku.xml"),
  list(id="pref_kyoto",    name="京都府（報道発表）",  lang="ja", category="行政",
       url="https://www.pref.kyoto.jp/press/press.xml"),
  list(id="pref_osaka",    name="大阪府",              lang="ja", category="行政",
       url="https://www.pref.osaka.lg.jp/shinchaku/shinchaku.xml"),
  list(id="pref_kanagawa", name="神奈川県",            lang="ja", category="行政",
       url="https://www.pref.kanagawa.jp/news/news.xml"),
  list(id="pref_tokushima",name="徳島県（健康・感染症）",lang="ja", category="行政",
       url="https://www.pref.tokushima.lg.jp/file/rss/ippannokata_kenko_kansensho_index.rss"),
  list(id="pref_tochigi",  name="栃木県（報道発表）",  lang="ja", category="行政",
       url="https://www.pref.tochigi.lg.jp/kensei/kouhou/houdou/houdou.xml"),
  list(id="pref_ibaraki",  name="茨城県",              lang="ja", category="行政",
       url="https://www.pref.ibaraki.jp/news.xml"),
  list(id="pref_kagawa",   name="香川県",              lang="ja", category="行政",
       url="https://www.pref.kagawa.lg.jp/shinchaku.xml"),
  list(id="pref_kochi",    name="高知県（新着情報）",  lang="ja", category="行政",
       url="https://www.pref.kochi.lg.jp/category/kubun/news/index.atom"),
  # 福井はRSS未提供のためHTMLスクレイピング経由のfetch_fukui_news()で個別取得する
  # ── 政令指定都市（保健所設置自治体） ─────────────────────
  list(id="city_sapporo",    name="札幌市（報道発表資料）", lang="ja", category="行政",
       url="https://www.city.sapporo.jp/somu/koho/hodo/houdou.xml"),
  list(id="city_sendai",     name="仙台市",                 lang="ja", category="行政",
       url="https://www.city.sendai.jp/shinchaku.xml"),
  list(id="city_chiba",      name="千葉市",                 lang="ja", category="行政",
       url="https://www.city.chiba.jp/shinchaku.xml"),
  list(id="city_sagamihara", name="相模原市",               lang="ja", category="行政",
       url="https://www.city.sagamihara.kanagawa.jp/rss.rss"),
  list(id="city_hamamatsu",  name="浜松市",                 lang="ja", category="行政",
       url="https://www.city.hamamatsu.shizuoka.jp/oshirase/oshirase.xml"),
  list(id="city_nagoya",     name="名古屋市",               lang="ja", category="行政",
       url="https://www.city.nagoya.jp/news.rss"),
  list(id="city_kyoto",      name="京都市",                 lang="ja", category="行政",
       url="https://www.city.kyoto.lg.jp/main/rss/rss_new.xml"),
  list(id="city_okayama",    name="岡山市",                 lang="ja", category="行政",
       url="https://www.city.okayama.jp/rss/rss.xml"),
  list(id="city_hiroshima",  name="広島市",                 lang="ja", category="行政",
       url="https://www.city.hiroshima.lg.jp/news.rss"),
  list(id="city_shizuoka",   name="静岡市（お知らせ）", lang="ja", category="行政",
       url="https://www.city.shizuoka.lg.jp/oshirase.xml"),
  # 堺市・川崎市・北九州市・横浜市・神戸市・福岡市・さいたま市・大阪市・新潟市・熊本市は
  # RSS未提供のためHTMLスクレイピング経由の各fetch_*_news()で個別取得する
  # （政令指定都市20/20市 完了）
  # ── 中核市（保健所設置自治体） ─────────────────────────
  list(id="city_takasaki",   name="高崎市",     lang="ja", category="行政",
       url="https://www.city.takasaki.gunma.jp/rss/10/list1.xml"),
  list(id="city_kawagoe",    name="川越市",     lang="ja", category="行政",
       url="https://www.city.kawagoe.saitama.jp/news.rss"),
  # 川口市はRSS(dirId=4850)が特定カテゴリのみ(21件)で限定的だったため、後述の
  # fetch_kawaguchi_news()（index.update.json、100件）に切り替えて個別取得する
  list(id="city_hachioji",   name="八王子市",   lang="ja", category="行政",
       url="https://www.city.hachioji.tokyo.jp/topnewsrss.xml"),
  list(id="city_iwaki",      name="いわき市",   lang="ja", category="行政",
       url="https://www.city.iwaki.lg.jp/www/rss/news.rdf"),
  list(id="city_kofu",       name="甲府市",     lang="ja", category="行政",
       url="https://www.city.kofu.yamanashi.jp/shinchaku.xml"),
  list(id="city_aomori",     name="青森市",     lang="ja", category="行政",
       url="https://www.city.aomori.aomori.jp/news.rss"),
  list(id="city_morioka",    name="盛岡市",     lang="ja", category="行政",
       url="https://www.city.morioka.iwate.jp/news.rss"),
  list(id="city_yokosuka",   name="横須賀市",   lang="ja", category="行政",
       url="https://www.city.yokosuka.kanagawa.jp/shinchaku.xml"),
  list(id="city_matsumoto",  name="松本市",     lang="ja", category="行政",
       url="https://www.city.matsumoto.nagano.jp/rss/10/list1.xml"),
  list(id="city_toyohashi",  name="豊橋市",     lang="ja", category="行政",
       url="https://www.city.toyohashi.lg.jp/services/rdf/rss10/11280.xml"),
  list(id="city_toyota",     name="豊田市",     lang="ja", category="行政",
       url="https://www.city.toyota.aichi.jp/news.rss"),
  list(id="city_gifu",       name="岐阜市",     lang="ja", category="行政",
       url="https://www.city.gifu.lg.jp/news.rss"),
  list(id="city_tottori_c",  name="鳥取市",     lang="ja", category="行政",
       url="https://www.city.tottori.lg.jp/rss/10/list1.xml"),
  list(id="city_kurashiki",  name="倉敷市",     lang="ja", category="行政",
       url="https://www.city.kurashiki.okayama.jp/rss_list/news.rss"),
  list(id="city_kure",       name="呉市",       lang="ja", category="行政",
       url="https://www.city.kure.lg.jp/rss/10/list1.xml"),
  list(id="city_fukuyama",   name="福山市",     lang="ja", category="行政",
       url="https://www.city.fukuyama.hiroshima.jp/rss/10/list1.xml"),
  list(id="city_matsuyama",  name="松山市",     lang="ja", category="行政",
       url="https://www.city.matsuyama.ehime.jp/rss_news.xml"),
  list(id="city_kochi_c",    name="高知市",     lang="ja", category="行政",
       url="https://www.city.kochi.kochi.jp/rss/10/list1.xml"),
  list(id="city_nagasaki_c", name="長崎市",     lang="ja", category="行政",
       url="https://www.city.nagasaki.lg.jp/rss/10/list1.xml"),
  list(id="city_miyazaki_c", name="宮崎市（報道発表）", lang="ja", category="行政",
       url="https://www.city.miyazaki.miyazaki.jp/city/public_relations/press_material/rss.xml"),
  list(id="city_mito",       name="水戸市",     lang="ja", category="行政",
       url="https://www.city.mito.lg.jp/rss/10/list1.xml"),
  list(id="city_wakayama_c", name="和歌山市",   lang="ja", category="行政",
       url="https://www.city.wakayama.wakayama.jp/news.rss"),
  list(id="city_kurume",     name="久留米市",   lang="ja", category="行政",
       url="https://www.city.kurume.fukuoka.jp/rss/feed.rss"),
  list(id="city_naha",       name="那覇市",     lang="ja", category="行政",
       url="https://www.city.naha.okinawa.jp/news.rss"),
  list(id="city_hirakata",   name="枚方市",     lang="ja", category="行政",
       url="https://www.city.hirakata.osaka.jp/rss/rss_new.xml"),
  list(id="city_yao",        name="八尾市",     lang="ja", category="行政",
       url="https://www.city.yao.osaka.jp/1016213/news.rss"),
  list(id="city_higashiosaka",name="東大阪市",  lang="ja", category="行政",
       url="https://www.city.higashiosaka.lg.jp/rss/rss.xml"),
  list(id="city_himeji",     name="姫路市",     lang="ja", category="行政",
       url="https://www.city.himeji.lg.jp/rss/rss.xml"),
  list(id="city_nishinomiya",name="西宮市",     lang="ja", category="行政",
       url="https://www.nishi.or.jp/rss_news.xml"),
  list(id="city_amagasaki",  name="尼崎市",     lang="ja", category="行政",
       url="https://www.city.amagasaki.hyogo.jp/news.rss"),
  list(id="city_toyonaka",   name="豊中市",     lang="ja", category="行政",
       url="https://www.city.toyonaka.osaka.jp/rss_news.xml"),
  list(id="city_takatsuki",  name="高槻市",     lang="ja", category="行政",
       url="https://www.city.takatsuki.osaka.jp/rss/10/list1.xml"),
  list(id="city_nara_c",     name="奈良市",     lang="ja", category="行政",
       url="https://www.city.nara.lg.jp/rss/10/list1.xml"),
  list(id="city_shimonoseki",name="下関市",     lang="ja", category="行政",
       url="https://www.city.shimonoseki.lg.jp/rss/10/list1.xml"),
  list(id="city_oita",       name="大分市",     lang="ja", category="行政",
       url="https://www.city.oita.oita.jp/shinchaku.xml"),
  list(id="city_kagoshima_c",name="鹿児島市",   lang="ja", category="行政",
       url="https://www.city.kagoshima.lg.jp/shinchaku.xml"),
  list(id="city_toyama_c",   name="富山市",     lang="ja", category="行政",
       url="https://www.city.toyama.lg.jp/news.rss"),
  list(id="city_okazaki",    name="岡崎市",     lang="ja", category="行政",
       url="https://www.city.okazaki.lg.jp/news.rss"),
  list(id="city_akashi",     name="明石市",     lang="ja", category="行政",
       url="https://www.city.akashi.lg.jp/oshirase.xml"),
  list(id="city_sasebo",     name="佐世保市",   lang="ja", category="行政",
       url="https://www.city.sasebo.lg.jp/oshirase.xml"),
  list(id="city_ichinomiya", name="一宮市",     lang="ja", category="行政",
       url="https://www.city.ichinomiya.aichi.jp/news.rss"),
  list(id="city_akita",      name="秋田市",     lang="ja", category="行政",
       url="https://www.city.akita.lg.jp/rss.rss"),
  list(id="city_yamagata_c", name="山形市",     lang="ja", category="行政",
       url="https://www.city.yamagata-yamagata.lg.jp/news.rss"),
  list(id="city_fukushima_c",name="福島市",     lang="ja", category="行政",
       url="https://www.city.fukushima.fukushima.jp/cgi-bin/feed.php?siteNew=1&displayRange=90"),
  list(id="city_suita",      name="吹田市",     lang="ja", category="行政",
       url="https://www.city.suita.osaka.jp/news.rss"),
  # 春日井市・鈴鹿市は厚労省「設置主体別保健所数」(令和8年4月1日現在)に記載がなく、
  # 実際には中核市／保健所設置市ではない（保健所は愛知県・三重県が所管）。
  # ニュース取得元としては引き続き有用なため取得は継続するが、「保健所設置自治体」
  # とはみなさない（is_official_ebs_sourceの判定自体には影響しない＝市の公式発表として扱う）
  list(id="city_kasugai",    name="春日井市（保健所設置市ではない）",   lang="ja", category="行政",
       url="https://www.city.kasugai.lg.jp/rss.rss"),
  list(id="city_suzuka",     name="鈴鹿市（保健所設置市ではない）",     lang="ja", category="行政",
       url="https://www.city.suzuka.lg.jp/news.rss"),
  # 旭川・函館・前橋・越谷・柏・金沢・大津・福井・長野・松江・寝屋川・高松・八戸ほか
  # その他多数の保健所設置市は未確認（継続調査中）。
  # つくば市・佐賀市も同様に保健所設置市ではないが、ニュース取得元としては
  # fetch_tsukuba_news()/fetch_saga_news()で継続取得する
  # 郡山市は接続不能だったがユーザー提供URLで解決済み（fetch_koriyama_news参照）、
  # 宇都宮市はSSL証明書期限切れと思われたが実際はブラウザ確認で静的HTML取得可能と判明
  # （fetch_utsunomiya_news参照）
  # ── その他政令市（地域保健法施行令第1条第3号による保健所設置市、5市）─────
  # 藤沢市は一般新着情報のRSS/JSON配信が見つからず未収録（継続調査中）
  list(id="city_otaru",      name="小樽市",     lang="ja", category="行政",
       url="https://www.city.otaru.lg.jp/docs/index.rss"),
  list(id="city_machida",    name="町田市",     lang="ja", category="行政",
       url="https://www.city.machida.tokyo.jp/rss_news.xml"),
  list(id="city_chigasaki",  name="茅ヶ崎市",   lang="ja", category="行政",
       url="https://www.city.chigasaki.kanagawa.jp/news.rss"),
  list(id="city_yokkaichi",  name="四日市市",   lang="ja", category="行政",
       url="https://www.city.yokkaichi.lg.jp/www/rss/news.rdf"),
  # ── 特別区（東京23区、保健所設置自治体）──────────────────────
  # 品川区・渋谷区・荒川区は一般新着情報のRSS/JSON配信が見つからず未収録（継続調査中）
  list(id="city_chiyoda",    name="千代田区",   lang="ja", category="行政",
       url="https://www.city.chiyoda.lg.jp/shinchaku/shinchaku.xml"),
  list(id="city_chuo",       name="中央区",     lang="ja", category="行政",
       url="https://www.city.chuo.lg.jp/shinchaku/shinchaku.xml"),
  list(id="city_minato",     name="港区",       lang="ja", category="行政",
       url="https://www.city.minato.tokyo.jp/shinchaku/shinchaku.xml"),
  list(id="city_shinjuku",   name="新宿区",     lang="ja", category="行政",
       url="https://www.city.shinjuku.lg.jp/top_rss.rdf"),
  list(id="city_bunkyo",     name="文京区",     lang="ja", category="行政",
       url="https://www.city.bunkyo.lg.jp/shinchaku/shinchaku.xml"),
  list(id="city_taito",      name="台東区",     lang="ja", category="行政",
       url="https://www.city.taito.lg.jp/rss_news.xml"),
  list(id="city_sumida",     name="墨田区",     lang="ja", category="行政",
       url="https://www.city.sumida.lg.jp/rss_news.xml"),
  list(id="city_koto",       name="江東区",     lang="ja", category="行政",
       url="https://www.city.koto.lg.jp/shinchaku/shinchaku.xml"),
  list(id="city_meguro",     name="目黒区",     lang="ja", category="行政",
       url="https://www.city.meguro.tokyo.jp/oshirase/rss_news.xml"),
  list(id="city_ota_c",      name="大田区",     lang="ja", category="行政",
       url="https://www.city.ota.tokyo.jp/oshirase/rss_news.xml"),
  list(id="city_setagaya",   name="世田谷区",   lang="ja", category="行政",
       url="https://www.city.setagaya.lg.jp/shinchaku/shinchaku.xml"),
  list(id="city_nakano",     name="中野区",     lang="ja", category="行政",
       url="https://www.city.tokyo-nakano.lg.jp/rss_news.xml"),
  list(id="city_suginami",   name="杉並区",     lang="ja", category="行政",
       url="https://www.city.suginami.tokyo.jp/kenkou/shinchaku/shinchaku.xml"),
  list(id="city_toshima",    name="豊島区",     lang="ja", category="行政",
       url="https://www.city.toshima.lg.jp/oshirase/oshirase.xml"),
  list(id="city_kita",       name="北区",       lang="ja", category="行政",
       url="https://www.city.kita.lg.jp/news.rss"),
  list(id="city_itabashi",   name="板橋区",     lang="ja", category="行政",
       url="https://www.city.itabashi.tokyo.jp/rss/news_release.rss"),
  list(id="city_nerima",     name="練馬区",     lang="ja", category="行政",
       url="https://www.city.nerima.tokyo.jp/rss/oshirase/rss_news.xml"),
  list(id="city_adachi",     name="足立区",     lang="ja", category="行政",
       url="https://www.city.adachi.tokyo.jp/ku/koho/news/news.xml"),
  list(id="city_katsushika", name="葛飾区",     lang="ja", category="行政",
       url="https://www.city.katsushika.lg.jp/news.rss"),
  list(id="city_edogawa",    name="江戸川区",   lang="ja", category="行政",
       url="https://www.city.edogawa.tokyo.jp/news/shinchaku.xml"),
  # ── 国際機関 ───────────────────────────────────────────────
  # who_donはRSS(https://www.who.int/feeds/entity/csr/don/en/rss.xml)が廃止(404)されたため、
  # EBS_SOURCESには含めずJSON API経由のfetch_who_don()で個別取得する（fetch_who_eiosと同様のパターン）
  # promedはサイトリニューアル(Next.js化)によりRSS(https://promedmail.org/feed/)・
  # 静的HTML双方から記事一覧を取得できなくなったため、現状取得手段なし（要継続調査）
  # ecdcはRSS(https://www.ecdc.europa.eu/en/rss.xml)が廃止(404)されたため、
  # EBS_SOURCESには含めずHTMLスクレイピング経由のfetch_ecdc_news()で個別取得する
  list(id="cdc",     name="CDC Health Alerts (US)",  lang="en", category="国際",
       url="https://tools.cdc.gov/api/v2/resources/media/316422.rss"),
  list(id="reliefweb", name="ReliefWeb (Japan)",     lang="en", category="国際",
       url="https://reliefweb.int/updates/rss.xml?primary_country=JPN"),
  # ── G20（欧州） ────────────────────────────────────────────
  list(id="ukhsa",   name="UK Health Security Agency", lang="en", category="国際",
       url="https://www.gov.uk/government/organisations/uk-health-security-agency.atom"),
  list(id="rki",     name="Robert Koch-Institut（独）", lang="de", category="国際",
       url="https://www.rki.de/SiteGlobals/Functions/RSS/RSS-neue-Dokumente.xml?nn=16777276"),
  list(id="nicd",    name="NICD（南アフリカ）",         lang="en", category="国際",
       url="https://www.nicd.ac.za/feed/"),
  # santepubliquefrance（仏）はRSS未提供のため、EBS_SOURCESには含めず
  # fetch_france_spf_news()で個別取得する
  # iss（伊）はWAFにより取得不可（Request Rejected）、brazil/saudi/turkeyも
  # WAFブロック・SharePoint SPA・告知内容が調達/人事中心で信号として不適切等の理由で見送り
  # ── 東・東南アジア ────────────────────────────────────────
  list(id="taiwan_cdc", name="台湾 CDC（衛生福利部疾病管制署）", lang="zh", category="国際",
       url="https://www.cdc.gov.tw/RSS/RssXml/Hh094B49-DRwe2RR4eFfrQ?type=1"),
  # chinacdc・chp（香港）はRSS未提供のため、EBS_SOURCESには含めず
  # fetch_china_cdc_news() / fetch_chp_news()（後者はchromoteによるヘッドレスブラウザ取得）で個別取得する
  # ── 専門メディア ────────────────────────────────────────────
  # CIDRAPは旧来の"/rss.xml"（全記事横断フィードのつもりだったもの）が
  # 実際には更新が止まった古いフィードで、2020〜2022年の記事しか返さず
  # 一度も現在のキャッシュに記事が入っていなかった（2026-08-24 ユーザー
  # 提供の参照ログとの突合で発覚）。CIDRAPは全記事横断のRSSを提供しておらず
  # トピック別フィード（/news/{id}/rss）のみのため、主要な疾患・カテゴリの
  # トピックフィードを個別に登録する
  list(id="cidrap_public_health", name="CIDRAP: Public Health", lang="en", category="研究機関",
       url="https://www.cidrap.umn.edu/news/91/rss"),
  list(id="cidrap_emerging",      name="CIDRAP: Misc Emerging Topics", lang="en", category="研究機関",
       url="https://www.cidrap.umn.edu/news/31175/rss"),
  list(id="cidrap_vhf",           name="CIDRAP: Viral Hemorrhagic Fever", lang="en", category="研究機関",
       url="https://www.cidrap.umn.edu/news/102/rss"),
  list(id="cidrap_ebola",         name="CIDRAP: Ebola", lang="en", category="研究機関",
       url="https://www.cidrap.umn.edu/news/64/rss"),
  list(id="cidrap_marburg",       name="CIDRAP: Marburg", lang="en", category="研究機関",
       url="https://www.cidrap.umn.edu/news/77/rss"),
  list(id="cidrap_avian_flu",     name="CIDRAP: Avian Influenza", lang="en", category="研究機関",
       url="https://www.cidrap.umn.edu/news/49/rss"),
  list(id="cidrap_pandemic_flu",  name="CIDRAP: Pandemic Influenza", lang="en", category="研究機関",
       url="https://www.cidrap.umn.edu/news/86/rss"),
  list(id="cidrap_measles",       name="CIDRAP: Measles", lang="en", category="研究機関",
       url="https://www.cidrap.umn.edu/news/78/rss"),
  list(id="cidrap_cholera",       name="CIDRAP: Cholera", lang="en", category="研究機関",
       url="https://www.cidrap.umn.edu/news/58/rss"),
  list(id="cidrap_mpox",          name="CIDRAP: Mpox", lang="en", category="研究機関",
       url="https://www.cidrap.umn.edu/news/230556/rss"),
  list(id="cidrap_dengue",        name="CIDRAP: Dengue", lang="en", category="研究機関",
       url="https://www.cidrap.umn.edu/news/61/rss"),
  list(id="cidrap_malaria",       name="CIDRAP: Malaria", lang="en", category="研究機関",
       url="https://www.cidrap.umn.edu/news/76/rss"),
  list(id="cidrap_foodborne",     name="CIDRAP: Foodborne Disease", lang="en", category="研究機関",
       url="https://www.cidrap.umn.edu/news/66/rss"),
  list(id="cidrap_legionella",    name="CIDRAP: Legionella", lang="en", category="研究機関",
       url="https://www.cidrap.umn.edu/news/111256/rss"),
  list(id="cidrap_botulism",      name="CIDRAP: Botulism", lang="en", category="研究機関",
       url="https://www.cidrap.umn.edu/news/52/rss"),
  list(id="cidrap_mers",          name="CIDRAP: MERS-CoV", lang="en", category="研究機関",
       url="https://www.cidrap.umn.edu/news/84/rss"),
  list(id="cidrap_polio",         name="CIDRAP: Polio", lang="en", category="研究機関",
       url="https://www.cidrap.umn.edu/news/90/rss"),
  list(id="cidrap_yellow_fever",  name="CIDRAP: Yellow Fever", lang="en", category="研究機関",
       url="https://www.cidrap.umn.edu/news/104/rss"),
  list(id="cidrap_zika",          name="CIDRAP: Zika", lang="en", category="研究機関",
       url="https://www.cidrap.umn.edu/news/100856/rss"),
  list(id="cidrap_plague",        name="CIDRAP: Plague", lang="en", category="研究機関",
       url="https://www.cidrap.umn.edu/news/88/rss"),
  list(id="ont",     name="Outbreak News Today",     lang="en", category="研究機関",
       url="https://outbreaknewstoday.com/feed/"),
  # 米国CDC（参照ログとの突合で未カバーと判明したため追加、2026-08-24）
  list(id="cdc_outbreaks", name="CDC Outbreaks (US Based)", lang="en", category="国際",
       url="https://tools.cdc.gov/api/v2/resources/media/285676.rss"),
  list(id="cdc_newsroom",  name="CDC Online Newsroom",      lang="en", category="国際",
       url="https://tools.cdc.gov/api/v2/resources/media/132608.rss"),
  list(id="afludiary", name="Avian Flu Diary（Crawford Kilian個人ブログ）", lang="en", category="研究機関",
       url="https://afludiary.blogspot.com/feeds/posts/default?alt=rss"),
  # 各国・国際機関の保健当局プレスリリース（参照ログとの突合で未カバーと
  # 判明したため追加、2026-08-24）。韓国KDCA・タイDDC・ギリシャEODYは
  # RSS未提供またはWAFによる直接アクセス拒否（403）のため見送り
  list(id="uganda_moh", name="ウガンダ保健省", lang="en", category="国際",
       url="https://health.go.ug/feed/"),
  list(id="who_afro",   name="WHOアフリカ地域事務局", lang="en", category="国際",
       url="https://www.afro.who.int/rss.xml"),
  list(id="woah",       name="WOAH（世界動物保健機関）", lang="en", category="国際",
       url="https://www.woah.org/en/feed/"),
  # ── 日本メディア ────────────────────────────────────────────
  # 参照ログとの突合で未カバーと判明したため追加（2026-08-24）
  list(id="kumanichi", name="熊本日日新聞",             lang="ja", category="メディア",
       url="https://kumanichi.com/rss.xml"),
  list(id="chiba_shinchaku", name="千葉県 新着情報",     lang="ja", category="自治体",
       url="https://www.pref.chiba.lg.jp/homepage/shinchaku/shinchaku.xml"),
  list(id="nhk",      name="NHK 健康・医療",          lang="ja", category="メディア",
       url="https://www3.nhk.or.jp/rss/news/cat6.xml"),
  list(id="nhk_sci",  name="NHK 科学・文化",          lang="ja", category="メディア",
       url="https://www3.nhk.or.jp/rss/news/cat3.xml"),
  list(id="nhk_soc",  name="NHK 社会",                lang="ja", category="メディア",
       url="https://www3.nhk.or.jp/rss/news/cat1.xml"),
  list(id="nhk_pol",  name="NHK 政治",                lang="ja", category="メディア",
       url="https://www3.nhk.or.jp/rss/news/cat4.xml"),
  list(id="asahi_h",  name="朝日新聞 医療・健康",      lang="ja", category="メディア",
       url="https://www.asahi.com/rss/asahi/health.rdf"),
  list(id="asahi_s",  name="朝日新聞 科学",            lang="ja", category="メディア",
       url="https://www.asahi.com/rss/asahi/science.rdf"),
  list(id="asahi_n",  name="朝日新聞 国内",            lang="ja", category="メディア",
       url="https://www.asahi.com/rss/asahi/national.rdf"),
  list(id="mainichi", name="毎日新聞",                 lang="ja", category="メディア",
       url="https://mainichi.jp/rss/etc/mainichi-flash.rss"),
  list(id="jiji",     name="時事通信",                 lang="ja", category="ニュース",
       url="https://www.jiji.com/rss/ranking.rdf"),
  list(id="yahoo_s",  name="Yahoo!ニュース 科学",      lang="ja", category="ニュース",
       url="https://news.yahoo.co.jp/rss/categories/science.xml"),
  list(id="yahoo_d",  name="Yahoo!ニュース 国内",      lang="ja", category="ニュース",
       url="https://news.yahoo.co.jp/rss/categories/domestic.xml"),
  list(id="nikkei_s", name="日経スタイル 健康",        lang="ja", category="メディア",
       url="https://style.nikkei.com/rss/feed.xml"),
  list(id="jptimes",  name="Japan Times",             lang="en", category="メディア",
       url="https://www.japantimes.co.jp/feed/"),
  # ── 地方紙 ────────────────────────────────────────────────
  list(id="hokkaido", name="北海道新聞",               lang="ja", category="地方紙",
       url="https://www.hokkaido-np.co.jp/output/7/free/index.ad.xml"),
  list(id="miyanichi",name="宮崎日日新聞",             lang="ja", category="地方紙",
       url="https://www.the-miyanichi.co.jp/feed")
)

# ── Google News RSS クエリ定義（疾患別）──────────────────
GNEWS_QUERIES <- list(
  # 定点把握
  flu       = "インフルエンザ 感染 流行",
  covid     = "新型コロナウイルス COVID-19 感染",
  rsv       = "RSウイルス 感染症",
  ari       = "急性呼吸器感染症",
  hfmd      = "手足口病 感染",
  varicella = "水痘 水ぼうそう 感染",
  mumps     = "おたふくかぜ 流行性耳下腺炎",
  mycop     = "マイコプラズマ肺炎",
  gi        = "感染性胃腸炎 ノロウイルス",
  strep     = "溶連菌 咽頭炎",
  # 全数把握
  measles   = "麻疹 はしか 感染",
  rubella   = "風疹 感染",
  pertussis = "百日咳",
  syphilis  = "梅毒 感染",
  ehec      = "O157 腸管出血性大腸菌 食中毒",
  dengue    = "デング熱",
  mpox      = "エムポックス mpox",
  tb        = "結核 感染",
  igas      = "劇症型溶血性レンサ球菌 人食いバクテリア",
  invasive_pneu = "侵襲性肺炎球菌感染症",
  legionella = "レジオネラ症",
  hep_a     = "A型肝炎",
  sfts      = "重症熱性血小板減少症候群 SFTS",
  # 総合
  general   = "感染症 流行 アウトブレイク 公衆衛生"
)

# ── 疾患キーワード辞書 ─────────────────────────────────────
DISEASE_KEYWORDS <- list(
  # 定点把握
  flu           = c("インフルエンザ","influenza","flu season","flu outbreak","H1N1","H3N2",
                    "A型インフルエンザ","B型インフルエンザ","seasonal flu","flu case"),
  covid         = c("COVID","新型コロナ","SARS-CoV-2","コロナウイルス","coronavirus",
                    "omicron","オミクロン","BA.","XBB","JN.","KP.","COVID-19"),
  rsv           = c("RSウイルス","RSV","respiratory syncytial","RSV bronchiolitis"),
  ari           = c("急性呼吸器感染症","acute respiratory infection","acute respiratory illness",
                    "respiratory illness","respiratory disease","ARI感染","SARI"),
  hfmd          = c("手足口病","hand foot mouth","HFMD","enterovirus 71","EV71",
                    "coxsackievirus","foot-and-mouth","hand, foot"),
  varicella     = c("水痘","水ぼうそう","varicella","chickenpox","VZV"),
  mumps         = c("おたふく","流行性耳下腺炎","mumps","parotitis","耳下腺"),
  mycop         = c("マイコプラズマ","mycoplasma","mycoplasma pneumoniae","歩く肺炎","walking pneumonia"),
  gi            = c("感染性胃腸炎","ノロウイルス","norovirus","ロタ","rotavirus",
                    "gastroenteritis","stomach bug","food poisoning","foodborne","noro"),
  strep         = c("溶連菌","溶血性レンサ球菌","streptococcus","strep throat","group A strep",
                    "GAS infection","strep A","scarlet fever","猩紅熱","咽頭炎"),
  # 1類
  ebola         = c("エボラ","Ebola","EVD","ebola virus","ebola outbreak","ebola case",
                    "ebola death","ebola response","ebola hemorrhagic"),
  crimean_congo = c("クリミア・コンゴ","Crimean-Congo","CCHF","crimean congo hemorrhagic"),
  smallpox      = c("痘そう","天然痘","smallpox","variola","vaccinia"),
  south_am_hem  = c("南米出血熱","South American hemorrhagic fever","Junin","Machupo","Guanarito"),
  plague        = c("ペスト","plague","bubonic plague","pneumonic plague","Yersinia pestis"),
  marburg       = c("マールブルグ","Marburg","MVD","marburg virus","marburg outbreak","marburg case"),
  lassa         = c("ラッサ","Lassa","lassa fever","lassa virus","lassa outbreak"),
  # 2類
  polio         = c("ポリオ","急性灰白髄炎","poliovirus","poliomyelitis","polio case",
                    "polio outbreak","vaccine-derived poliovirus","VDPV","cVDPV"),
  tb            = c("結核","tuberculosis","TB case","TB outbreak","MDR-TB","XDR-TB",
                    "抗酸菌","結核菌","latent TB","TB death","mycobacterium tuberculosis"),
  diphtheria    = c("ジフテリア","diphtheria","Corynebacterium diphtheriae","diphtheria case"),
  sars          = c("重症急性呼吸器症候群","SARS","SARS-CoV","severe acute respiratory"),
  mers          = c("中東呼吸器症候群","MERS","MERS-CoV","middle east respiratory"),
  avian_h5n1    = c("鳥インフルエンザ","H5N1","avian influenza H5N1","HPAI H5",
                    "bird flu","highly pathogenic avian","H5 influenza","poultry outbreak",
                    "avian flu","H5N1 human","H5N1 death"),
  avian_h7n9    = c("H7N9","avian influenza H7N9","H7N9 human case"),
  # 3類
  cholera       = c("コレラ","cholera","Vibrio cholerae","cholera outbreak","cholera case",
                    "cholera death","oral rehydration","ORS cholera","acute watery diarrhea"),
  dysentery     = c("細菌性赤痢","bacterial dysentery","Shigella","shigellosis","bacillary dysentery"),
  ehec          = c("O157","腸管出血性大腸菌","EHEC","HUS","溶血性尿毒症",
                    "E. coli O157","enterohemorrhagic","STEC","hemolytic uremic"),
  typhoid       = c("腸チフス","typhoid","typhoid fever","Salmonella typhi","enteric fever"),
  paratyphoid   = c("パラチフス","paratyphoid","Salmonella paratyphi","paratyphoid fever"),
  # 4類
  hep_e         = c("E型肝炎","Ｅ型肝炎","hepatitis E","HEV","hepatitis e virus"),
  west_nile     = c("ウエストナイル","West Nile","WNV","west nile virus","west nile fever",
                    "west nile neuroinvasive"),
  hep_a         = c("A型肝炎","Ａ型肝炎","hepatitis A","HAV","hepatitis a virus",
                    "hepatitis a outbreak"),
  echinococcus  = c("エキノコックス","Echinococcus","包虫","hydatid","echinococcosis"),
  mpox          = c("エムポックス","mpox","monkeypox","サル痘","clade I","clade Ib","clade II",
                    "mpox case","mpox outbreak","mpox death","mpox spread"),
  yellow_fever  = c("黄熱","yellow fever","YFV","yellow fever case","yellow fever outbreak",
                    "yellow fever vaccination","YF vaccine"),
  psittacosis   = c("オウム病","psittacosis","Chlamydia psittaci","ornithosis","parrot fever"),
  omsk_hem      = c("オムスク出血熱","Omsk hemorrhagic fever","OHFV"),
  relapsing_f   = c("回帰熱","relapsing fever","tick-borne relapsing fever","TBRF"),
  kyasanur      = c("キャサヌル","Kyasanur Forest","KFD","kyasanur forest disease"),
  q_fever       = c("Q熱","Ｑ熱","Q fever","Coxiella burnetii","query fever"),
  rabies        = c("狂犬病","rabies","rabid","rabies case","rabies death","rabies exposure",
                    "rabies post-exposure","lyssavirus"),
  coccidioides  = c("コクシジオイデス","coccidioidomycosis","Valley fever","Coccidioides"),
  # "ジカ"（2文字）は「フィジカル」等に埋め込まれて誤マッチするため、
  # より特定的な「ジカ熱」「ジカウイルス」を使用する
  zika          = c("ジカ熱","ジカウイルス","Zika","ZIKV","zika virus","zika outbreak","zika microcephaly"),
  sfts          = c("SFTS","重症熱性血小板減少","severe fever with thrombocytopenia","SFTSV",
                    "thrombocytopenia syndrome","SFTS virus","SFTS case"),
  hfrs          = c("腎症候性出血熱","ハンタウイルス","HFRS","hemorrhagic fever with renal","hantavirus renal",
                    "hantavirus","seoul virus","puumala","hantaan","hantavirus outbreak","hantavirus case"),
  wee           = c("西部ウマ脳炎","Western equine encephalitis","WEE"),
  tick_enceph   = c("ダニ媒介脳炎","tick-borne encephalitis","TBE","tick encephalitis"),
  anthrax       = c("炭疽","anthrax","Bacillus anthracis","cutaneous anthrax","inhalation anthrax"),
  chikungunya   = c("チクングニア","chikungunya","CHIKV","chikungunya virus","chikungunya outbreak",
                    "chikungunya case","chikungunya fever"),
  scrub         = c("つつが虫","scrub typhus","Orientia tsutsugamushi","chigger"),
  dengue        = c("デング","dengue","DENV","dengue fever","dengue hemorrhagic","DHF","dengue case",
                    "dengue outbreak","dengue death","dengue serotype","severe dengue"),
  eee           = c("東部ウマ脳炎","Eastern equine encephalitis","EEE","triple E"),
  avian_other   = c("鳥インフルエンザ(H5N1を除く","avian influenza H5N2","H5N2","H5N8","H5N6",
                    "avian flu non-H5N1","LPAI"),
  nipah         = c("ニパ","Nipah","NiV","nipah virus","nipah outbreak","nipah encephalitis"),
  spotted_f     = c("日本紅斑熱","Japanese spotted fever","Rickettsia japonica","spotted fever"),
  japanese_enc  = c("日本脳炎","Japanese encephalitis","JEV","JE vaccine","JE case",
                    "japanese encephalitis virus"),
  hps           = c("ハンタウイルス肺症候群","ハンタウイルス","Hantavirus pulmonary","HPS","sin nombre virus",
                    "hantavirus pulmonary syndrome","hantavirus case","hantavirus outbreak"),
  b_virus       = c("Bウイルス","B virus","herpes B","Macacine herpesvirus"),
  glanders      = c("鼻疽","glanders","Burkholderia mallei","farcy"),
  brucella      = c("ブルセラ","brucellosis","Brucella","undulant fever","malta fever"),
  vee           = c("ベネズエラウマ脳炎","Venezuelan equine encephalitis","VEE","VEEV"),
  hendra        = c("ヘンドラ","Hendra","HeV","hendra virus"),
  typhus        = c("発しんチフス","typhus","epidemic typhus","Rickettsia prowazekii","louse-borne"),
  botulism      = c("ボツリヌス","botulism","Clostridium botulinum","infant botulism","food botulism"),
  malaria       = c("マラリア","malaria","Plasmodium","falciparum","vivax","artemisinin",
                    "malaria case","malaria death","antimalarial","ACT malaria","malaria outbreak"),
  tularemia     = c("野兎病","tularemia","Francisella tularensis","rabbit fever"),
  lyme          = c("ライム病","Lyme disease","Borrelia burgdorferi","lyme borreliosis",
                    "lyme case","lyme tick"),
  lyssavirus    = c("リッサウイルス","Lyssavirus","Australian bat lyssavirus"),
  rift_valley   = c("リフトバレー","Rift Valley fever","RVF","rift valley virus"),
  melioidosis   = c("類鼻疽","melioidosis","Burkholderia pseudomallei","whitmore"),
  legionella    = c("レジオネラ","legionella","在郷軍人病","Legionnaire","legionellosis",
                    "legionella case","legionella outbreak","pontiac fever"),
  leptospira    = c("レプトスピラ","leptospirosis","Leptospira","weil disease","weil's disease"),
  rocky_mtn     = c("ロッキー山紅斑熱","Rocky Mountain spotted fever","RMSF","Rickettsia rickettsii"),
  # 5類全数
  ameba         = c("アメーバ赤痢","amebiasis","Entamoeba histolytica","amoebic dysentery"),
  hep_viral     = c("ウイルス性肝炎","viral hepatitis","B型肝炎","C型肝炎",
                    "hepatitis B","hepatitis C","HBV","HCV","HBsAg","cirrhosis",
                    "liver cancer hepatitis","hepatitis delta"),
  cre           = c("カルバペネム耐性","CRE","carbapenem-resistant","carbapenemase",
                    "KPC","NDM","OXA-48","CRKP","CRAB"),
  acute_flaccid = c("急性弛緩性麻痺","acute flaccid paralysis","AFP","acute flaccid myelitis","AFM"),
  encephalitis  = c("急性脳炎","acute encephalitis","viral encephalitis","encephalitis case",
                    "brain inflammation","encephalitis outbreak"),
  cryptospor    = c("クリプトスポリジウム","cryptosporidiosis","Cryptosporidium","crypto outbreak"),
  cjd           = c("クロイツフェルト","CJD","Creutzfeldt-Jakob","vCJD","プリオン","prion disease",
                    "variant CJD"),
  igas          = c("劇症型溶血性レンサ球菌","人食いバクテリア","iGAS","invasive GAS",
                    "invasive group A strep","flesh-eating","necrotizing fasciitis",
                    "streptococcal toxic shock","STSS","invasive streptococcal"),
  aids          = c("エイズ","AIDS","HIV","後天性免疫不全","HIV/AIDS","antiretroviral",
                    "ART HIV","HIV case","HIV prevalence","HIV transmission"),
  giardia       = c("ジアルジア","giardiasis","Giardia lamblia","giardia","beaver fever"),
  invasive_hib  = c("侵襲性インフルエンザ菌","invasive Haemophilus influenzae","Hib",
                    "invasive Hib","Haemophilus influenzae type b"),
  invasive_mening = c("侵襲性髄膜炎菌","invasive meningococcal","Neisseria meningitidis",
                      "meningococcal disease","meningococcal meningitis","meningococcemia",
                      "meningococcal outbreak","meningococcal case"),
  invasive_pneu = c("侵襲性肺炎球菌","pneumococcal","肺炎球菌","Streptococcus pneumoniae",
                    "pneumococcal disease","invasive pneumococcal","PCV","pneumococcal vaccine"),
  varicella_hosp= c("水痘（入院","varicella hospitalization","severe varicella","varicella complication"),
  crs           = c("先天性風しん","先天性風疹","congenital rubella","CRS","congenital rubella syndrome"),
  mdra          = c("多剤耐性緑膿菌","MDRP","multidrug-resistant Pseudomonas","MDRP aeruginosa"),
  syphilis      = c("梅毒","syphilis","treponema","congenital syphilis","早期梅毒","潜伏梅毒",
                    "syphilis case","syphilis outbreak","syphilis rise","STI syphilis"),
  crypto_dissem = c("播種性クリプトコックス","disseminated cryptococcosis","Cryptococcus",
                    "cryptococcal meningitis","Cryptococcus neoformans"),
  tetanus       = c("破傷風","tetanus","Clostridium tetani","neonatal tetanus","lockjaw"),
  vrsa          = c("バンコマイシン耐性黄色ブドウ球菌","VRSA","vancomycin-resistant Staphylococcus"),
  vre           = c("バンコマイシン耐性腸球菌","VRE","vancomycin-resistant Enterococcus"),
  pertussis     = c("百日咳","pertussis","whooping cough","bordetella","百日せき",
                    "pertussis outbreak","pertussis case","whooping cough outbreak"),
  rubella       = c("風疹","風しん","rubella","German measles","rubella case","rubella outbreak",
                    "rubella vaccine","rubella immunity"),
  measles       = c("麻疹","麻しん","measles","はしか","rubeola","measles case","measles outbreak",
                    "measles death","measles vaccination","MMR","measles elimination",
                    "measles spread","measles cluster"),
  dra           = c("薬剤耐性アシネトバクター","MDRA","multidrug-resistant Acinetobacter",
                    "Acinetobacter baumannii","CRAB"),
  # ── 月報疾患（性感染症・薬剤耐性菌）─────────────────────
  chlamydia_genital = c("性器クラミジア","クラミジア感染症","chlamydia","genital chlamydia",
                        "Chlamydia trachomatis"),
  herpes_genital    = c("性器ヘルペス","genital herpes","HSV-2","genital HSV"),
  condyloma         = c("尖圭コンジローマ","condyloma","genital warts","HPV genital wart"),
  gonorrhea         = c("淋菌感染症","淋病","gonorrhea","gonorrhoea","Neisseria gonorrhoeae",
                        "drug-resistant gonorrhea"),
  mrsa              = c("メチシリン耐性黄色ブドウ球菌","MRSA","methicillin-resistant Staphylococcus aureus"),
  prsp              = c("ペニシリン耐性肺炎球菌","PRSP","penicillin-resistant Streptococcus pneumoniae",
                        "penicillin-resistant pneumococcus"),
  general       = c("感染症","outbreak","流行","epidemic","pandemic",
                    "クラスター","cluster","疫学","公衆衛生","感染拡大")
)

SIGNAL_KEYWORDS <- list(
  high   = c("緊急","警戒","非常事態","パンデミック","pandemic","emergency",
             "alert","死亡","重症","死者","致死","急増","急拡大","過去最多",
             "outbreak","流行警報","感染爆発"),
  medium = c("増加","拡大","サーベイランス強化","warning","spread","rise",
             "increase","cluster","クラスター","感染拡大","流行中","感染者増",
             "報告数増","定点超","基準超","波","感染が広がって"),
  low    = c("報告","確認","モニタリング","monitoring","report","update",
             "発生","検出","陽性","速報","週報","動向","サーベイランス")
)

# 直近の流行状況を示す時制キーワード（あれば信頼度アップ）
TEMPORAL_KEYWORDS <- c(
  "今週","先週","直近","最新","現在","足元","この週","本週",
  "第\\d+週","week \\d+","最近","ここ数週","ここ数日",
  "2024年","2025年","2026年","january","february","march","april",
  "may","june","july","august","september","october","november","december"
)

# 一般的な健康情報・予防啓発（流行状況を反映しない記事）
GENERIC_HEALTH_KEYWORDS <- c(
  "予防方法","予防のため","予防には","感染予防","予防接種のご案内",
  "手洗い","うがい","マスク着用","換気を","ワクチン接種のお知らせ",
  "基礎知識","とは何か","について知ろう","気をつけましょう",
  "健康情報","健康増進","生活習慣","食事で","栄養","睡眠",
  "what is","how to prevent","tips for","basics of"
)

# ============================================================
# 汎用 RSS パーサー
# ============================================================
parse_rss <- function(content_text, source_def) {
  xml_doc <- tryCatch(read_xml(content_text), error = function(e) NULL)
  if (is.null(xml_doc)) return(NULL)
  # RSS 1.0/RDF系フィード（asahi, mhlw等）はデフォルト名前空間(xmlns="http://purl.org/rss/1.0/")
  # を宣言しており、無プレフィックスのXPath（".//item"等）が一致しない問題があったため、
  # 名前空間を除去してRSS 1.0/2.0/Atom全形式を無プレフィックスXPathで統一的に扱う
  xml_ns_strip(xml_doc)
  root_name <- xml_name(xml_doc)

  items <- if (grepl("feed", root_name, ignore.case = TRUE)) {
    xml_find_all(xml_doc, ".//entry")
  } else {
    xml_find_all(xml_doc, ".//item")
  }
  if (length(items) == 0) return(NULL)

  # 単純なタグ名（例:"date"）を直接XPathで指定すると、一部のフィード（例: 埼玉県の
  # RSS1.0/RDF、dc:date由来）でxml_find_firstが原因不明にNAを返すことがあるため、
  # local-name()ベースのXPathに変換して確実に一致させる（xml_ns_strip後は名前空間が
  # 除去されているのでlocal-name()マッチで安全に子要素を選択できる）
  .to_localname_xpath <- function(tag) {
    if (grepl("/@", tag)) {
      parts <- strsplit(tag, "/@", fixed = TRUE)[[1]]
      paste0("*[local-name()='", parts[1], "']/@", parts[2])
    } else {
      paste0("*[local-name()='", tag, "']")
    }
  }

  get_text <- function(item, ...) {
    for (tag in c(...)) {
      xpath <- .to_localname_xpath(tag)
      v <- tryCatch(xml_text(xml_find_first(item, xpath)), error = function(e) NA)
      if (!is.na(v) && nchar(trimws(v)) > 0) return(trimws(v))
    }
    NA_character_
  }

  rows <- lapply(items, function(item) {
    title    <- get_text(item, "title")
    link     <- get_text(item, "link/@href", "link")
    # 台湾CDCのRSSは<pubDate>が無く、<guid>に"YYYY-MM-DD-HH-MM-SS"形式で日時を格納しているため
    # guidも日付候補として試す（isPermaLink属性の有無に関わらず値自体は日時形式）
    date_str <- get_text(item, "pubDate", "date", "published", "updated", "guid")
    desc     <- get_text(item, "description", "summary", "content")

    pub_date <- tryCatch({
      d <- as.Date(NA)
      if (!is.na(date_str) && nchar(trimws(date_str)) > 0) {
        # タイムゾーン表記・曜日を除去
        ds <- trimws(date_str)
        ds <- gsub("\\s*(GMT|UTC|[+-]\\d{2}:?\\d{2})$", "", ds)
        ds <- gsub("^[A-Za-z]+,\\s*", "", ds)  # "Thu, " を除去
        # 英語月名を数字に置換（日本語Localeでも動作）
        months_en <- c(Jan="01",Feb="02",Mar="03",Apr="04",May="05",Jun="06",
                       Jul="07",Aug="08",Sep="09",Oct="10",Nov="11",Dec="12")
        for (mn in names(months_en)) {
          ds <- gsub(paste0("\\b", mn, "\\b"), months_en[mn], ds, ignore.case = TRUE)
        }
        ds <- trimws(ds)
        fmts <- c(
          "%d %m %Y %H:%M:%S",   # 25 06 2026 07:00:00
          "%Y-%m-%dT%H:%M:%S",   # 2026-06-25T07:00:00
          "%Y-%m-%d %H:%M:%S",   # 2026-06-25 07:00:00
          "%Y-%m-%d-%H-%M-%S",   # 2026-06-25-07-00-00（台湾CDCのguid形式）
          "%Y/%m/%d %H:%M:%S",   # 2026/07/15 00:00:00（鳥取県RSS形式）
          "%Y-%m-%d"             # 2026-06-25
        )
        for (fmt in fmts) {
          pt <- suppressWarnings(as.POSIXct(ds, format = fmt, tz = "UTC"))
          if (!is.na(pt)) { d <- as.Date(pt); break }
        }
      }
      d
    }, error = function(e) as.Date(NA))

    if (!is.na(desc)) {
      desc <- gsub("<[^>]+>", " ", desc)
      desc <- trimws(gsub("\\s+", " ", desc))
      desc <- substr(desc, 1, 500)
    }

    tibble(source_id=source_def$id, source_name=source_def$name,
           category=source_def$category, lang=source_def$lang,
           title=title, link=link, pub_date=pub_date, summary=desc)
  })
  bind_rows(rows)
}

# ============================================================
# 固定RSSソース取得
# ============================================================
fetch_rss_feed <- function(source_def, timeout_sec = 5) {
  tryCatch({
    resp <- GET(source_def$url, timeout(timeout_sec),
                add_headers("User-Agent"="JapanSurveillanceDashboard/1.0"))
    if (status_code(resp) != 200) return(NULL)
    parse_rss(content(resp, "text", encoding="UTF-8"), source_def)
  }, error = function(e) { message(source_def$name, " エラー: ", e$message); NULL })
}

# ============================================================
# Google News RSS 取得（APIキー不要）
# ============================================================
fetch_google_news <- function(query, lang = "ja", n_results = 10) {
  encoded_q <- URLencode(query, reserved = TRUE)
  url <- paste0(
    "https://news.google.com/rss/search?q=", encoded_q,
    "&hl=ja&gl=JP&ceid=JP:ja"
  )
  src <- list(id="gnews", name="Google News", category="ニュース", lang=lang, url=url)

  tryCatch({
    resp <- GET(url, timeout(8),
                add_headers(
                  "User-Agent" = paste0(
                    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) ",
                    "AppleWebKit/537.36 (KHTML, like Gecko) ",
                    "Chrome/120.0.0.0 Safari/537.36"
                  )
                ))
    if (status_code(resp) != 200) return(NULL)
    df <- parse_rss(content(resp, "text", encoding="UTF-8"), src)
    if (!is.null(df)) head(df, n_results) else NULL
  }, error = function(e) { message("Google News エラー: ", e$message); NULL })
}

# 疾患別Google News 全件取得
fetch_google_news_all <- function() {
  bind_rows(lapply(names(GNEWS_QUERIES), function(disease) {
    message("Google News 取得: ", disease)
    df <- fetch_google_news(GNEWS_QUERIES[[disease]])
    if (!is.null(df)) df %>% mutate(disease_hint = disease) else NULL
  }))
}

# ============================================================
# WHO EIOS RSS（専用タイムアウト設定）
# ============================================================
WHO_EIOS_URL <- "https://eios.who.int/portal/api/api/rssfeed/1773793757422?pinned=false&token=C34A025A-E502-432A-9D8F-1470ACCC2C3A"

fetch_who_eios <- function(timeout_sec = 30) {
  message("WHO EIOS 取得中...")
  src <- list(id="who_eios", name="WHO EIOS", lang="en", category="国際")
  tryCatch({
    resp <- GET(WHO_EIOS_URL, timeout(timeout_sec),
                add_headers("User-Agent" = "JapanSurveillanceDashboard/1.0"))
    if (status_code(resp) != 200) {
      message("WHO EIOS: HTTP ", status_code(resp)); return(NULL)
    }
    parse_rss(content(resp, "text", encoding = "UTF-8"), src)
  }, error = function(e) { message("WHO EIOS エラー: ", e$message); NULL })
}

# ============================================================
# WHO Disease Outbreak News（JSON API。旧RSSフィードは廃止済みのため
# who.intのODataベースの公開APIから直接取得する）
# ============================================================
WHO_DON_API_URL <- "https://www.who.int/api/emergencies/diseaseoutbreaknews"

fetch_who_don <- function(timeout_sec = 15, n_results = 30) {
  message("WHO Disease Outbreak News 取得中...")
  tryCatch({
    resp <- GET(WHO_DON_API_URL, timeout(timeout_sec),
                query = list(`$orderby` = "PublicationDate desc", `$top` = n_results),
                add_headers("User-Agent" = "JapanSurveillanceDashboard/1.0",
                            "Accept" = "application/json"))
    if (status_code(resp) != 200) {
      message("WHO DON: HTTP ", status_code(resp)); return(NULL)
    }
    js <- jsonlite::fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyDataFrame = TRUE)
    items <- js$value
    if (is.null(items) || nrow(items) == 0) return(NULL)

    summary_txt <- if (!is.null(items$Overview)) items$Overview else items$Summary
    summary_txt <- gsub("<[^>]+>", " ", summary_txt)
    summary_txt <- trimws(gsub("\\s+", " ", summary_txt))
    summary_txt <- substr(summary_txt, 1, 500)

    tibble(
      source_id   = "who_don",
      source_name = "WHO Disease Outbreak News",
      category    = "国際",
      lang        = "en",
      title       = items$Title,
      link        = paste0("https://www.who.int/emergencies/disease-outbreak-news/item/", items$UrlName),
      pub_date    = as.Date(items$PublicationDate),
      summary     = summary_txt
    )
  }, error = function(e) { message("WHO DON エラー: ", e$message); NULL })
}

# ============================================================
# ECDC ニュース（HTMLスクレイピング。旧RSS(en/rss.xml)は廃止済みのため、
# news-eventsページに実際にサーバーサイドレンダリングされているニュースカードを直接解析する）
# ============================================================
ECDC_NEWS_URL <- "https://www.ecdc.europa.eu/en/news-events"

fetch_ecdc_news <- function(timeout_sec = 15, n_results = 20) {
  message("ECDC ニュース取得中...")
  tryCatch({
    resp <- GET(ECDC_NEWS_URL, timeout(timeout_sec),
                add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
    if (status_code(resp) != 200) {
      message("ECDC: HTTP ", status_code(resp)); return(NULL)
    }
    doc <- read_html(content(resp, "text", encoding = "UTF-8"))
    cards <- xml_find_all(doc,
      "//div[contains(concat(' ', normalize-space(@class), ' '), ' ecdc-card-list-item')]")
    if (length(cards) == 0) return(NULL)

    rows <- lapply(cards, function(card) {
      a <- xml_find_first(card, ".//p[contains(@class,'ecdc-card__title')]/a")
      title <- trimws(xml_text(a))
      href  <- xml_attr(a, "href")
      date_str <- xml_attr(xml_find_first(card, ".//time"), "datetime")
      desc  <- trimws(xml_text(xml_find_first(card, ".//p[contains(@class,'ecdc-card__description')]")))
      if (is.na(title) || nchar(title) == 0 || is.na(date_str)) return(NULL)
      tibble(
        source_id   = "ecdc",
        source_name = "ECDC",
        category    = "国際",
        lang        = "en",
        title       = title,
        link        = if (grepl("^https?://", href)) href else paste0("https://www.ecdc.europa.eu", href),
        pub_date    = as.Date(date_str),
        summary     = substr(desc, 1, 500)
      )
    })
    bind_rows(Filter(Negate(is.null), rows)) %>% head(n_results)
  }, error = function(e) { message("ECDC エラー: ", e$message); NULL })
}

# ============================================================
# WHO Weekly Epidemiological Record（WER）「Highlighted signals and events」
# の「Selected new signals of potential public health events assessed」表
# （HTMLスクレイピング）。WERは毎週金曜発行。最新号のURLはトップページの
# 「Read the full HTML edition」リンクから解決する（号数のURLパターンが
# 巻数・号数依存で単純な連番予測ができないため）。
# ユーザー指示（2026-08-18）: 「Highlighted signals and events」の
# 「Selected new signals of potential public health events assessed」に
# 記載の地域・疾患をEBSの参考情報として毎回確認する。
# ============================================================
WHO_WER_INDEX_URL <- "https://www.who.int/publications/journals/weekly-epidemiological-record"

fetch_who_wer_news <- function(timeout_sec = 15) {
  message("WHO WER（週刊疫学記録）取得中...")
  tryCatch({
    idx_resp <- GET(WHO_WER_INDEX_URL, timeout(timeout_sec),
                     add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
    if (status_code(idx_resp) != 200) { message("WHO WER index: HTTP ", status_code(idx_resp)); return(NULL) }
    idx_doc <- read_html(content(idx_resp, "text", encoding = "UTF-8"))
    latest_link <- xml_find_first(idx_doc,
      "//a[contains(., 'full HTML edition') or contains(., 'Read the full HTML')]")
    issue_href <- if (!is.na(latest_link)) xml_attr(latest_link, "href") else NA_character_
    if (is.na(issue_href) || nchar(issue_href) == 0) {
      message("WHO WER: 最新号リンクが見つかりません"); return(NULL)
    }
    issue_url <- if (grepl("^https?://", issue_href)) issue_href else paste0("https://www.who.int", issue_href)

    issue_resp <- GET(issue_url, timeout(timeout_sec),
                       add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
    if (status_code(issue_resp) != 200) { message("WHO WER issue: HTTP ", status_code(issue_resp)); return(NULL) }
    doc <- read_html(content(issue_resp, "text", encoding = "UTF-8"))

    # 号タイトル・対象週の文言（"Epidemiological Week 32 (3 August - 9 August 2026)"）から
    # 対象週の終了日をpub_dateとして使う。取得できない場合はSys.Date()にフォールバックする
    header_txt <- xml_text(xml_find_first(doc, "//body"))
    wk_m <- regmatches(header_txt, regexpr(
      "Epidemiological Week [0-9]+ ?\\([0-9]{1,2} [A-Za-z]+ ?[–-] ?[0-9]{1,2} [A-Za-z]+ [0-9]{4}\\)",
      header_txt, perl = TRUE))
    pub_date <- Sys.Date()
    # as.Date(format="%B")はロケール依存（実行環境が日本語ロケールだと英語月名を
    # 解釈できずNAになる）ため、ロケールに依存しない月名テーブルで手動変換する
    .EN_MONTHS <- c(January=1, February=2, March=3, April=4, May=5, June=6,
                     July=7, August=8, September=9, October=10, November=11, December=12)
    if (length(wk_m) > 0) {
      end_m <- regmatches(wk_m, regexpr("([0-9]{1,2}) ([A-Za-z]+) ([0-9]{4})\\)$", wk_m, perl = TRUE))
      if (length(end_m) > 0) {
        parts <- regmatches(end_m, regexec("([0-9]{1,2}) ([A-Za-z]+) ([0-9]{4})", end_m))[[1]]
        if (length(parts) == 4 && parts[3] %in% names(.EN_MONTHS)) {
          parsed <- suppressWarnings(as.Date(sprintf("%s-%02d-%02d",
            parts[4], .EN_MONTHS[[parts[3]]], as.integer(parts[2]))))
          if (!is.na(parsed)) pub_date <- parsed
        }
      }
    }

    # 「Region / Hazard」の2列表を、ヘッダーセルの文言で特定する（表の並び順が
    # 号によって変わっても頑健に検出できるようにするため、固定インデックスではなく
    # ヘッダーテキストで探す）
    tables <- xml_find_all(doc, "//table[contains(@class,'wer-table')]")
    target <- NULL
    for (tb in tables) {
      hdr <- xml_find_all(tb, ".//tr[1]/*")
      hdr_txt <- trimws(xml_text(hdr))
      if (length(hdr_txt) >= 2 && any(grepl("^Region$", hdr_txt, ignore.case = TRUE)) &&
          any(grepl("^Hazard$", hdr_txt, ignore.case = TRUE))) { target <- tb; break }
    }
    if (is.null(target)) { message("WHO WER: Region/Hazard表が見つかりません"); return(NULL) }

    data_rows <- xml_find_all(target, ".//tr[position()>1]")
    out <- list()
    for (r in data_rows) {
      cells <- xml_find_all(r, "./*")
      if (length(cells) < 2) next
      region <- trimws(xml_text(cells[[1]]))
      # <br>区切りの「• 疾患名」箇条書きをxml_text()すると改行が失われ結合してしまうため、
      # 子ノード単位でテキストを集めて再分割する
      hazard_html <- as.character(cells[[2]])
      hazard_txt <- gsub("<br\\s*/?>", "\n", hazard_html, ignore.case = TRUE)
      hazard_txt <- gsub("<[^>]+>", "", hazard_txt)
      hazards <- trimws(gsub("^[••]\\s*", "", strsplit(hazard_txt, "\n")[[1]]))
      hazards <- hazards[nzchar(hazards)]
      if (length(hazards) == 0 || nchar(region) == 0) next
      for (hz in hazards) {
        out[[length(out) + 1]] <- tibble(
          source_id   = "who_wer",
          source_name = "WHO Weekly Epidemiological Record",
          category    = "国際",
          lang        = "en",
          title       = sprintf("WHO WER Highlighted Signal — %s: %s", region, hz),
          link        = issue_url,
          pub_date    = pub_date,
          summary     = sprintf("WHO Weekly Epidemiological Record「Highlighted signals and events」より。地域: %s／シグナル: %s", region, hz)
        )
      }
    }
    if (length(out) == 0) return(NULL)
    bind_rows(out)
  }, error = function(e) { message("WHO WER エラー: ", e$message); NULL })
}

# 今週のWER「Highlighted signals and events」でハイライトされた疾患の内部IDだけを
# 抽出する（他ソース記事の補強フラグ判定に使う。ユーザー指示 2026-08-18:
# WERそのものを記事一覧に加えるのではなく、他記事の参考・補強に使う）
get_who_wer_highlighted_diseases <- function() {
  wer <- fetch_who_wer_news()
  if (is.null(wer) || nrow(wer) == 0) return(character(0))
  hazards <- sub("^.*: ", "", wer$title)
  ids <- unique(unlist(lapply(hazards, function(hz) {
    tags <- tag_diseases(hz)
    strsplit(tags, ",")[[1]]
  })))
  ids[ids != "other"]
}

# ============================================================
# JIHS（国立健康危機管理研究機構）新着情報（HTMLスクレイピング。
# JIHS発足に伴いniid.jihs.go.jpの旧RSS(feed/)は廃止されたため、
# jihs.go.jpの新着情報一覧ページ（<dl><dt>日付</dt><dd><a>タイトル</a></dd>...）を直接解析する）
# ============================================================
JIHS_NEWS_URL <- "https://www.jihs.go.jp/content4/newinfo.html"

fetch_jihs_news <- function(timeout_sec = 15, n_results = 20) {
  message("JIHS 新着情報取得中...")
  tryCatch({
    resp <- GET(JIHS_NEWS_URL, timeout(timeout_sec),
                add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
    if (status_code(resp) != 200) {
      message("JIHS: HTTP ", status_code(resp)); return(NULL)
    }
    doc <- read_html(content(resp, "text", encoding = "UTF-8"))
    dts <- xml_find_all(doc, "//dl/dt")
    if (length(dts) == 0) return(NULL)

    rows <- lapply(dts, function(dt) {
      date_str <- trimws(xml_text(dt))
      dd <- xml_find_first(dt, "following-sibling::dd[1]")
      a  <- xml_find_first(dd, ".//a")
      title <- trimws(xml_text(a))
      href  <- xml_attr(a, "href")
      d <- suppressWarnings(as.Date(date_str, format = "%Y/%m/%d"))
      if (is.na(d) || is.na(title) || nchar(title) == 0) return(NULL)
      tibble(
        source_id   = "jihs",
        source_name = "JIHS (国立健康危機管理)",
        category    = "研究機関",
        lang        = "ja",
        title       = title,
        link        = if (grepl("^https?://", href)) href else paste0("https://www.jihs.go.jp/content4/", href),
        pub_date    = d,
        summary     = NA_character_
      )
    })
    bind_rows(Filter(Negate(is.null), rows)) %>% head(n_results)
  }, error = function(e) { message("JIHS エラー: ", e$message); NULL })
}

# ============================================================
# 中国CDC「全球伝染病事件リスク評価報告」（HTMLスクレイピング。RSS未提供のため
# 月次で発行される全球伝染病事件リスク評価報告(PDF)の一覧ページを直接解析する）
# ============================================================
CHINA_CDC_GLOBAL_URL <- "https://www.chinacdc.cn/jksj/jksj03/"

fetch_china_cdc_news <- function(timeout_sec = 15, n_results = 20) {
  message("中国CDC 全球伝染病事件リスク評価 取得中...")
  tryCatch({
    resp <- GET(CHINA_CDC_GLOBAL_URL, timeout(timeout_sec),
                add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
    if (status_code(resp) != 200) {
      message("中国CDC: HTTP ", status_code(resp)); return(NULL)
    }
    doc <- read_html(content(resp, "text", encoding = "UTF-8"))
    links <- xml_find_all(doc, "//a[contains(@href,'.pdf')]")
    if (length(links) == 0) return(NULL)

    rows <- lapply(links, function(a) {
      # <a>本文の直下テキストのみ抽出（末尾の<span>更新日付を除外するため、
      # 例: <a href="...">タイトル<span>2026-07-16</span></a>）
      title <- trimws(paste(xml_text(xml_find_all(a, "./text()")), collapse = ""))
      href  <- xml_attr(a, "href")
      # href例: "./202607/P020260716341152144978.pdf" → 年月"202607"を日付として使う
      ym <- regmatches(href, regexpr("(?<=/)\\d{6}(?=/)", href, perl = TRUE))
      if (length(ym) == 0 || nchar(title) == 0) return(NULL)
      d <- suppressWarnings(as.Date(paste0(ym, "01"), format = "%Y%m%d"))
      if (is.na(d)) return(NULL)
      full_url <- if (grepl("^https?://", href)) href
                  else paste0("https://www.chinacdc.cn/jksj/jksj03/", sub("^\\./", "", href))
      tibble(
        source_id   = "china_cdc",
        source_name = "中国CDC（中国疾病预防控制中心）",
        category    = "国際",
        lang        = "zh",
        title       = title,
        link        = full_url,
        pub_date    = d,
        summary     = NA_character_
      )
    })
    bind_rows(Filter(Negate(is.null), rows)) %>% distinct(title, .keep_all = TRUE) %>% head(n_results)
  }, error = function(e) { message("中国CDC エラー: ", e$message); NULL })
}

# ============================================================
# 香港CHP（衛生防護中心）プレスリリース（ヘッドレスブラウザ経由のHTMLスクレイピング。
# ページがJavaScriptで内容を描画するSPA形式のためRSS・静的HTML取得は不可。
# chromoteパッケージ（要Chrome/Chromiumインストール）でレンダリング後のHTMLを取得する。
# ローカル専用機能（tesseract/magickと同様、requireNamespaceで存在チェック）
# ============================================================
CHP_PRESS_URL <- "https://www.chp.gov.hk/en/media/116/index.html"

.chp_scrape_available <- function() requireNamespace("chromote", quietly = TRUE)

fetch_chp_news <- function(timeout_sec = 20, n_results = 20) {
  if (!.chp_scrape_available()) {
    message("香港CHP: chromoteパッケージ未インストールのためスキップ")
    return(NULL)
  }
  message("香港CHP プレスリリース取得中（ヘッドレスブラウザ）...")
  b <- NULL
  tryCatch({
    b <- chromote::ChromoteSession$new()
    b$Page$navigate(CHP_PRESS_URL, wait_ = TRUE)
    Sys.sleep(3)
    html <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
    b$close(); b <- NULL
    if (is.null(html) || nchar(html) == 0) return(NULL)

    doc <- read_html(html)
    # プレスリリース本体は info.gov.hk/gia/general/YYYYMM/DD/P... 形式のリンクとして
    # サーバー側で描画される（日付はURL自体に埋め込まれている）
    links <- xml_find_all(doc, "//a[contains(@href,'info.gov.hk/gia/general/')]")
    if (length(links) == 0) return(NULL)

    rows <- lapply(links, function(a) {
      title <- trimws(xml_text(a))
      href  <- xml_attr(a, "href")
      m <- regmatches(href, regexec("/general/(\\d{6})/(\\d{2})/", href))[[1]]
      if (length(m) < 3 || nchar(title) == 0) return(NULL)
      d <- suppressWarnings(as.Date(paste0(m[2], m[3]), format = "%Y%m%d"))
      if (is.na(d)) return(NULL)
      tibble(
        source_id   = "chp",
        source_name = "Hong Kong CHP",
        category    = "国際",
        lang        = "en",
        title       = title,
        link        = sub("\\?.*$", "", href),
        pub_date    = d,
        summary     = NA_character_
      )
    })
    bind_rows(Filter(Negate(is.null), rows)) %>% distinct(link, .keep_all = TRUE) %>% head(n_results)
  }, error = function(e) {
    message("香港CHP エラー: ", e$message)
    NULL
  }, finally = {
    if (!is.null(b)) tryCatch(b$close(), error = function(e) NULL)
  })
}

# ============================================================
# フランス Santé publique France「Les actualités」（HTMLスクレイピング。RSS(index.php/rss)
# はHTMLページへのリダイレクトになっており機能していないため、記事一覧ページを直接解析する）
# ============================================================
SPF_NEWS_URL <- "https://www.santepubliquefrance.fr/les-actualites"

fetch_france_spf_news <- function(timeout_sec = 15, n_results = 20) {
  message("Santé publique France 取得中...")
  tryCatch({
    resp <- GET(SPF_NEWS_URL, timeout(timeout_sec),
                add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
    if (status_code(resp) != 200) {
      message("SPF: HTTP ", status_code(resp)); return(NULL)
    }
    doc <- read_html(content(resp, "text", encoding = "UTF-8"))
    cards <- xml_find_all(doc,
      "//div[contains(concat(' ', normalize-space(@class), ' '), ' node--type-article ')]")
    if (length(cards) == 0) return(NULL)

    rows <- lapply(cards, function(card) {
      title <- trimws(xml_text(xml_find_first(card, ".//h3[contains(@class,'record__body__title')]")))
      date_str <- xml_attr(xml_find_first(card, ".//time"), "datetime")
      a <- xml_find_first(card, ".//a[contains(@class,'record__footer__link')]")
      href <- xml_attr(a, "href")
      if (nchar(title) == 0 || is.na(date_str) || is.na(href)) return(NULL)
      d <- suppressWarnings(as.Date(substr(date_str, 1, 10)))
      if (is.na(d)) return(NULL)
      tibble(
        source_id   = "spf",
        source_name = "Santé publique France",
        category    = "国際",
        lang        = "fr",
        title       = title,
        link        = if (grepl("^https?://", href)) href else paste0("https://www.santepubliquefrance.fr", href),
        pub_date    = d,
        summary     = NA_character_
      )
    })
    bind_rows(Filter(Negate(is.null), rows)) %>% distinct(link, .keep_all = TRUE) %>% head(n_results)
  }, error = function(e) { message("SPF エラー: ", e$message); NULL })
}

# ============================================================
# 日本感染症学会 新着情報（HTMLスクレイピング。RSS未提供のため、トップページの
# 「新着情報」欄（<dl class="pico_block_menu"> 内に <dt>日付</dt><dd><a>タイトル</a></dd>
# が交互に並ぶ）を直接解析する。日付は「2026年8月24日NEW 学会から」のように
# カテゴリ表記や「NEW」ラベルが続くことがあるため、先頭の年月日部分だけを抽出する
# （参照ログとの突合で未カバーと判明、2026-08-24）
# ============================================================
KANSENSHO_URL <- "https://www.kansensho.or.jp/"

fetch_kansensho_news <- function(timeout_sec = 15, n_results = 20) {
  message("日本感染症学会 新着情報 取得中...")
  tryCatch({
    resp <- GET(KANSENSHO_URL, timeout(timeout_sec),
                add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
    if (status_code(resp) != 200) return(NULL)
    doc <- read_html(content(resp, "text", encoding = "UTF-8"))
    dl <- xml_find_first(doc, "//dl[contains(@class,'pico_block_menu')]")
    if (inherits(dl, "xml_missing")) return(NULL)
    kids <- xml_children(dl)

    rows <- list()
    pending_date <- NA
    for (i in seq_along(kids)) {
      node <- kids[[i]]
      nm <- xml_name(node)
      txt <- trimws(gsub("\\s+", " ", xml_text(node)))
      if (nm == "dt") {
        m <- regmatches(txt, regexpr("[0-9]{4}年[0-9]{1,2}月[0-9]{1,2}日", txt))
        pending_date <- if (length(m) > 0 && nchar(m) > 0) {
          suppressWarnings(as.Date(gsub("年|月", "-", sub("日", "", m)), format = "%Y-%m-%d"))
        } else NA
      } else if (nm == "dd" && !is.na(pending_date)) {
        a <- xml_find_first(node, ".//a")
        if (!inherits(a, "xml_missing")) {
          title <- trimws(gsub("\\s+", " ", xml_text(a)))
          href  <- xml_attr(a, "href")
          if (nchar(title) > 0 && !is.na(href)) {
            rows[[length(rows) + 1]] <- tibble(
              source_id   = "kansensho",
              source_name = "日本感染症学会",
              category    = "国内学会",
              lang        = "ja",
              title       = title,
              link        = if (grepl("^https?://", href)) href else paste0("https://www.kansensho.or.jp", href),
              pub_date    = pending_date,
              summary     = NA_character_
            )
          }
        }
      }
    }
    bind_rows(rows) %>% distinct(link, .keep_all = TRUE) %>% arrange(desc(pub_date)) %>% head(n_results)
  }, error = function(e) { message("日本感染症学会 エラー: ", e$message); NULL })
}

# ============================================================
# 日本テレビ系列（NNN）ニュース「社会」カテゴリ一覧（HTMLスクレイピング。
# 全国のNNN系列局（日テレ本体・道内外の地方局）の速報記事が集約されるページで、
# RSS未提供のため直接解析する。一覧ページは「最新」のみを表示し個々の記事に
# 日付表記が無い（末尾の時刻のみ）ため、取得日をpub_dateとして扱う）
# ============================================================
NTV_SOCIETY_URL <- "https://news.ntv.co.jp/category/society"

fetch_ntv_news <- function(timeout_sec = 15, n_results = 30) {
  message("日テレNEWS（社会） 取得中...")
  tryCatch({
    resp <- GET(NTV_SOCIETY_URL, timeout(timeout_sec),
                add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
    if (status_code(resp) != 200) return(NULL)
    doc <- read_html(content(resp, "text", encoding = "UTF-8"))
    links <- xml_find_all(doc, "//a[contains(@href,'/category/society/')]")
    if (length(links) == 0) return(NULL)

    rows <- lapply(links, function(a) {
      title <- trimws(gsub("\\s+", " ", xml_text(a)))
      href  <- xml_attr(a, "href")
      if (nchar(title) == 0 || is.na(href)) return(NULL)
      # 末尾の「HH:MM」時刻表記はタイトルに含める必要が無いため取り除く
      title <- trimws(sub("[0-9]{1,2}:[0-9]{2}$", "", title))
      tibble(
        source_id   = "ntv_society",
        source_name = "日テレNEWS（社会）",
        category    = "メディア",
        lang        = "ja",
        title       = title,
        link        = if (grepl("^https?://", href)) href else paste0("https://news.ntv.co.jp", href),
        pub_date    = Sys.Date(),
        summary     = NA_character_
      )
    })
    bind_rows(Filter(Negate(is.null), rows)) %>% distinct(link, .keep_all = TRUE) %>% head(n_results)
  }, error = function(e) { message("日テレNEWS エラー: ", e$message); NULL })
}

# ============================================================
# 秋田県 報道発表資料（HTMLスクレイピング。RSS未提供のため年度別ページを直接解析する。
# 年度が変わると新しいgenre IDのページが作られるため、親ページ(genre/11699)から
# 「報道発表」を含むリンクのうち末尾の数字が最大のもの＝最新年度を動的に特定する）
# ============================================================
AKITA_PRESS_INDEX_URL <- "https://www.pref.akita.lg.jp/pages/genre/11699"

fetch_akita_press_news <- function(timeout_sec = 15, n_results = 20) {
  message("秋田県 報道発表資料 取得中...")
  tryCatch({
    idx_resp <- GET(AKITA_PRESS_INDEX_URL, timeout(timeout_sec),
                     add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
    if (status_code(idx_resp) != 200) return(NULL)
    idx_doc <- read_html(content(idx_resp, "text", encoding = "UTF-8"))
    links <- xml_find_all(idx_doc, "//a[contains(text(),'報道発表')]")
    if (length(links) == 0) return(NULL)
    hrefs <- xml_attr(links, "href")
    ids <- suppressWarnings(as.integer(regmatches(hrefs, regexpr("\\d+$", hrefs))))
    latest_url <- hrefs[which.max(ids)]
    if (is.na(latest_url)) return(NULL)

    resp <- GET(latest_url, timeout(timeout_sec),
                add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
    if (status_code(resp) != 200) return(NULL)
    doc <- read_html(content(resp, "text", encoding = "UTF-8"))
    items <- xml_find_all(doc, "//li[contains(@class,'c-list__item')]")
    if (length(items) == 0) return(NULL)

    rows <- lapply(items, function(li) {
      a <- xml_find_first(li, ".//a")
      title <- trimws(xml_text(a))
      href  <- xml_attr(a, "href")
      date_str <- xml_attr(xml_find_first(li, ".//time"), "datetime")
      if (nchar(title) == 0 || is.na(date_str)) return(NULL)
      d <- suppressWarnings(as.Date(date_str))
      if (is.na(d)) return(NULL)
      tibble(
        source_id   = "pref_akita",
        source_name = "秋田県（報道発表）",
        category    = "行政",
        lang        = "ja",
        title       = title,
        link        = if (grepl("^https?://", href)) href else paste0("https://www.pref.akita.lg.jp", href),
        pub_date    = d,
        summary     = NA_character_
      )
    })
    bind_rows(Filter(Negate(is.null), rows)) %>% distinct(link, .keep_all = TRUE) %>%
      arrange(desc(pub_date)) %>% head(n_results)
  }, error = function(e) { message("秋田県 エラー: ", e$message); NULL })
}

# ============================================================
# 奈良県 報道発表（HTMLスクレイピング。RSS未提供のため記事一覧ページを直接解析する）
# ============================================================
NARA_PRESS_URL <- "https://www.pref.nara.lg.jp/press/index.html"

fetch_nara_press_news <- function(timeout_sec = 15, n_results = 20) {
  message("奈良県 報道発表 取得中...")
  tryCatch({
    resp <- GET(NARA_PRESS_URL, timeout(timeout_sec),
                add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
    if (status_code(resp) != 200) return(NULL)
    doc <- read_html(content(resp, "text", encoding = "UTF-8"))
    items <- xml_find_all(doc, "//li[.//a and .//span]")
    if (length(items) == 0) return(NULL)

    rows <- lapply(items, function(li) {
      date_str <- trimws(xml_text(xml_find_first(li, ".//span")))
      a <- xml_find_first(li, ".//a")
      title <- trimws(xml_text(a))
      href  <- xml_attr(a, "href")
      if (nchar(title) == 0 || is.na(href)) return(NULL)
      d <- suppressWarnings(as.Date(gsub("年|月", "-", gsub("日", "", date_str)), format = "%Y-%m-%d"))
      if (is.na(d)) return(NULL)
      tibble(
        source_id   = "pref_nara",
        source_name = "奈良県（報道発表）",
        category    = "行政",
        lang        = "ja",
        title       = title,
        link        = if (grepl("^https?://", href)) href else paste0("https://www.pref.nara.lg.jp", href),
        pub_date    = d,
        summary     = NA_character_
      )
    })
    bind_rows(Filter(Negate(is.null), rows)) %>% distinct(link, .keep_all = TRUE) %>% head(n_results)
  }, error = function(e) { message("奈良県 エラー: ", e$message); NULL })
}

# ============================================================
# 佐賀県 報道発表・広報（HTMLスクレイピング。RSS未提供のため記事一覧ページを直接解析する）
# ============================================================
SAGA_PRESS_URL <- "https://www.pref.saga.lg.jp/list00693.html"

fetch_saga_press_news <- function(timeout_sec = 15, n_results = 20) {
  message("佐賀県 報道発表・広報 取得中...")
  tryCatch({
    resp <- GET(SAGA_PRESS_URL, timeout(timeout_sec),
                add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
    if (status_code(resp) != 200) return(NULL)
    doc <- read_html(content(resp, "text", encoding = "UTF-8"))
    blocks <- xml_find_all(doc, "//div[contains(@class,'mainblock')]")
    if (length(blocks) == 0) return(NULL)

    rows <- lapply(blocks, function(b) {
      date_str <- trimws(xml_text(xml_find_first(b, ".//span[contains(@class,'upddate')]")))
      a <- xml_find_first(b, ".//div[contains(@class,'title')]//a")
      title <- trimws(xml_text(a))
      href  <- xml_attr(a, "href")
      if (nchar(title) == 0 || is.na(href)) return(NULL)
      d <- suppressWarnings(as.Date(gsub("更新", "", date_str), format = "%Y年%m月%d日"))
      if (is.na(d)) return(NULL)
      tibble(
        source_id   = "pref_saga",
        source_name = "佐賀県（報道発表・広報）",
        category    = "行政",
        lang        = "ja",
        title       = title,
        link        = if (grepl("^https?://", href)) href else paste0("https://www.pref.saga.lg.jp/", href),
        pub_date    = d,
        summary     = NA_character_
      )
    })
    bind_rows(Filter(Negate(is.null), rows)) %>% distinct(link, .keep_all = TRUE) %>% head(n_results)
  }, error = function(e) { message("佐賀県 エラー: ", e$message); NULL })
}

# ============================================================
# 和歌山県 トップページ「新着情報」タブ（HTMLスクレイピング。RSS未提供のため
# トップページ内の新着情報タブ(c-list _date)を直接解析する）
# ============================================================
WAKAYAMA_TOP_URL <- "https://www.pref.wakayama.lg.jp/"

fetch_wakayama_news <- function(timeout_sec = 15, n_results = 20) {
  message("和歌山県 新着情報 取得中...")
  tryCatch({
    resp <- GET(WAKAYAMA_TOP_URL, timeout(timeout_sec),
                add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
    if (status_code(resp) != 200) return(NULL)
    doc <- read_html(content(resp, "text", encoding = "UTF-8"))
    items <- xml_find_all(doc, "//ul[contains(@class,'c-list') and contains(@class,'_date')]//li")
    if (length(items) == 0) return(NULL)

    rows <- lapply(items, function(li) {
      a <- xml_find_first(li, ".//a")
      href <- xml_attr(a, "href")
      date_str <- xml_attr(xml_find_first(li, ".//time"), "datetime")
      title <- trimws(xml_text(xml_find_first(li, ".//p")))
      if (nchar(title) == 0 || is.na(href) || is.na(date_str)) return(NULL)
      d <- suppressWarnings(as.Date(date_str, format = "%Y/%m/%d"))
      if (is.na(d)) return(NULL)
      tibble(
        source_id   = "pref_wakayama",
        source_name = "和歌山県（新着情報）",
        category    = "行政",
        lang        = "ja",
        title       = title,
        link        = if (grepl("^https?://", href)) href else paste0("https://www.pref.wakayama.lg.jp", href),
        pub_date    = d,
        summary     = NA_character_
      )
    })
    bind_rows(Filter(Negate(is.null), rows)) %>% distinct(link, .keep_all = TRUE) %>% head(n_results)
  }, error = function(e) { message("和歌山県 エラー: ", e$message); NULL })
}

# ============================================================
# 福岡県 新着情報一覧（HTMLスクレイピング。RSS未提供のため記事一覧ページを直接解析する）
# ============================================================
FUKUOKA_NEWS_URL <- "https://www.pref.fukuoka.lg.jp/soshiki/list1-1.html"

fetch_fukuoka_news <- function(timeout_sec = 15, n_results = 20) {
  message("福岡県 新着情報 取得中...")
  tryCatch({
    resp <- GET(FUKUOKA_NEWS_URL, timeout(timeout_sec),
                add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
    if (status_code(resp) != 200) return(NULL)
    doc <- read_html(content(resp, "text", encoding = "UTF-8"))
    items <- xml_find_all(doc, "//li[.//span[contains(@class,'article_date')]]")
    if (length(items) == 0) return(NULL)

    rows <- lapply(items, function(li) {
      date_str <- trimws(xml_text(xml_find_first(li, ".//span[contains(@class,'article_date')]")))
      a <- xml_find_first(li, ".//span[contains(@class,'article_title')]//a")
      title <- trimws(xml_text(a))
      href  <- xml_attr(a, "href")
      if (nchar(title) == 0 || is.na(href)) return(NULL)
      d <- suppressWarnings(as.Date(gsub("更新", "", date_str), format = "%Y年%m月%d日"))
      if (is.na(d)) return(NULL)
      tibble(
        source_id   = "pref_fukuoka",
        source_name = "福岡県（新着情報）",
        category    = "行政",
        lang        = "ja",
        title       = title,
        link        = if (grepl("^https?://", href)) href else paste0("https://www.pref.fukuoka.lg.jp", href),
        pub_date    = d,
        summary     = NA_character_
      )
    })
    bind_rows(Filter(Negate(is.null), rows)) %>% distinct(link, .keep_all = TRUE) %>% head(n_results)
  }, error = function(e) { message("福岡県 エラー: ", e$message); NULL })
}

# ============================================================
# 長崎県 コンテンツ情報一覧（HTMLスクレイピング。RSS未提供のため記事一覧ページを直接解析する）
# ============================================================
NAGASAKI_NEWS_URL <- "https://www.pref.nagasaki.jp/doc/"

fetch_nagasaki_news <- function(timeout_sec = 15, n_results = 20) {
  message("長崎県 新着情報 取得中...")
  tryCatch({
    resp <- GET(NAGASAKI_NEWS_URL, timeout(timeout_sec),
                add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
    if (status_code(resp) != 200) return(NULL)
    doc <- read_html(content(resp, "text", encoding = "UTF-8"))
    items <- xml_find_all(doc, "//ol[contains(@class,'news-list')]//li")
    if (length(items) == 0) return(NULL)

    rows <- lapply(items, function(li) {
      date_str <- xml_attr(xml_find_first(li, ".//time"), "datetime")
      a <- xml_find_first(li, ".//span//a")
      title <- trimws(xml_text(a))
      href  <- xml_attr(a, "href")
      if (nchar(title) == 0 || is.na(href) || is.na(date_str)) return(NULL)
      d <- suppressWarnings(as.Date(gsub("年|月", "-", gsub("日", "", date_str)), format = "%Y-%m-%d"))
      if (is.na(d)) return(NULL)
      tibble(
        source_id   = "pref_nagasaki",
        source_name = "長崎県",
        category    = "行政",
        lang        = "ja",
        title       = title,
        link        = if (grepl("^https?://", href)) href else paste0("https://www.pref.nagasaki.jp", href),
        pub_date    = d,
        summary     = NA_character_
      )
    })
    bind_rows(Filter(Negate(is.null), rows)) %>% distinct(link, .keep_all = TRUE) %>% head(n_results)
  }, error = function(e) { message("長崎県 エラー: ", e$message); NULL })
}

# ============================================================
# 福井県 新着情報一覧（HTMLスクレイピング。RSS未提供のため記事一覧ページを直接解析する。
# 日付は要素属性ではなく<li>内のテキスト末尾に"[YYYY年M月D日]"形式で埋め込まれている）
# ============================================================
FUKUI_NEWS_URL <- "https://www.pref.fukui.lg.jp/news.html"

fetch_fukui_news <- function(timeout_sec = 15, n_results = 20) {
  message("福井県 新着情報 取得中...")
  tryCatch({
    resp <- GET(FUKUI_NEWS_URL, timeout(timeout_sec),
                add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
    if (status_code(resp) != 200) return(NULL)
    doc <- read_html(content(resp, "text", encoding = "UTF-8"))
    items <- xml_find_all(doc, "//div[contains(@class,'block-list-article')]//li[a]")
    if (length(items) == 0) return(NULL)

    rows <- lapply(items, function(li) {
      a <- xml_find_first(li, ".//a")
      title <- trimws(xml_text(a))
      href  <- xml_attr(a, "href")
      li_text <- xml_text(li)
      m <- regmatches(li_text, regexpr("\\d{4}年\\d{1,2}月\\d{1,2}日", li_text))
      if (nchar(title) == 0 || is.na(href) || length(m) == 0) return(NULL)
      d <- suppressWarnings(as.Date(gsub("年|月", "-", gsub("日", "", m)), format = "%Y-%m-%d"))
      if (is.na(d)) return(NULL)
      tibble(
        source_id   = "pref_fukui",
        source_name = "福井県",
        category    = "行政",
        lang        = "ja",
        title       = title,
        link        = if (grepl("^https?://", href)) href else paste0("https://www.pref.fukui.lg.jp", href),
        pub_date    = d,
        summary     = NA_character_
      )
    })
    bind_rows(Filter(Negate(is.null), rows)) %>% distinct(link, .keep_all = TRUE) %>% head(n_results)
  }, error = function(e) { message("福井県 エラー: ", e$message); NULL })
}

# ============================================================
# 堺市 新着情報一覧（HTMLスクレイピング。RSS未提供のため記事一覧ページを直接解析する。
# 日付は要素属性ではなく<li>テキスト先頭に"YYYY年M月D日"形式で埋め込まれている）
# ============================================================
SAKAI_NEWS_URL <- "https://www.city.sakai.lg.jp/newlist/index.html"

fetch_sakai_news <- function(timeout_sec = 15, n_results = 20) {
  message("堺市 新着情報 取得中...")
  tryCatch({
    resp <- GET(SAKAI_NEWS_URL, timeout(timeout_sec),
                add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
    if (status_code(resp) != 200) return(NULL)
    doc <- read_html(content(resp, "text", encoding = "UTF-8"))
    items <- xml_find_all(doc, "//div[contains(@class,'wysiwyg_wp')]//li[a]")
    if (length(items) == 0) return(NULL)

    rows <- lapply(items, function(li) {
      a <- xml_find_first(li, ".//a")
      title <- trimws(xml_text(a))
      href  <- xml_attr(a, "href")
      li_text <- xml_text(li)
      m <- regmatches(li_text, regexpr("\\d{4}年\\d{1,2}月\\d{1,2}日", li_text))
      if (nchar(title) == 0 || is.na(href) || length(m) == 0) return(NULL)
      d <- suppressWarnings(as.Date(gsub("年|月", "-", gsub("日", "", m)), format = "%Y-%m-%d"))
      if (is.na(d)) return(NULL)
      tibble(
        source_id   = "city_sakai",
        source_name = "堺市",
        category    = "行政",
        lang        = "ja",
        title       = title,
        link        = if (grepl("^https?://", href)) href else paste0("https://www.city.sakai.lg.jp", href),
        pub_date    = d,
        summary     = NA_character_
      )
    })
    bind_rows(Filter(Negate(is.null), rows)) %>% distinct(link, .keep_all = TRUE) %>% head(n_results)
  }, error = function(e) { message("堺市 エラー: ", e$message); NULL })
}

# ============================================================
# 川崎市 報道発表資料（HTMLスクレイピング。RSS未提供のため月別ページを直接解析する。
# 日付ごとに<h3>M月D日の報道発表資料</h3>で見出しが分かれ、直後の<ul>に記事一覧が
# 続く構造のため、h3の日付をfollowing-sibling::ul[1]内の全記事に適用する）
# ============================================================
KAWASAKI_NEWS_URL <- "https://www.city.kawasaki.jp/templates/prs/0-Curr.html"

fetch_kawasaki_news <- function(timeout_sec = 15, n_results = 20) {
  message("川崎市 報道発表資料 取得中...")
  tryCatch({
    resp <- GET(KAWASAKI_NEWS_URL, timeout(timeout_sec),
                add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
    if (status_code(resp) != 200) return(NULL)
    doc <- read_html(content(resp, "text", encoding = "UTF-8"))
    headers <- xml_find_all(doc, "//h3[contains(text(),'の報道発表資料')]")
    if (length(headers) == 0) return(NULL)
    year <- format(Sys.Date(), "%Y")

    rows <- lapply(headers, function(h3) {
      htxt <- xml_text(h3)
      m <- regmatches(htxt, regexec("(\\d{1,2})月(\\d{1,2})日", htxt))[[1]]
      if (length(m) < 3) return(NULL)
      d <- suppressWarnings(as.Date(paste(year, m[2], m[3], sep = "-"), format = "%Y-%m-%d"))
      if (is.na(d)) return(NULL)
      # 年またぎ対応: 現在月より未来の月なら前年の記事とみなす
      if (as.integer(m[2]) > as.integer(format(Sys.Date(), "%m")) + 1)
        d <- d - 365
      ul <- xml_find_first(h3, "following-sibling::ul[1]")
      if (is.na(ul)) return(NULL)
      links <- xml_find_all(ul, ".//a")
      sub_rows <- lapply(links, function(a) {
        # <a>直下のテキストノードのみ抽出（末尾の<span class="ku/shi">部署名</span>を除外）
        title <- trimws(paste(xml_text(xml_find_all(a, "./text()")), collapse = ""))
        href  <- xml_attr(a, "href")
        if (nchar(title) == 0 || is.na(href)) return(NULL)
        tibble(
          source_id   = "city_kawasaki",
          source_name = "川崎市（報道発表資料）",
          category    = "行政",
          lang        = "ja",
          title       = title,
          link        = if (grepl("^https?://", href)) href else paste0("https://www.city.kawasaki.jp", href),
          pub_date    = d,
          summary     = NA_character_
        )
      })
      bind_rows(Filter(Negate(is.null), sub_rows))
    })
    bind_rows(Filter(Negate(is.null), rows)) %>% distinct(link, .keep_all = TRUE) %>% head(n_results)
  }, error = function(e) { message("川崎市 エラー: ", e$message); NULL })
}

# ============================================================
# 北九州市 新着情報一覧（HTMLスクレイピング。RSS未提供のため記事一覧ページを直接解析する）
# ============================================================
KITAKYUSHU_NEWS_URL <- "https://www.city.kitakyushu.lg.jp/newslist.html"

fetch_kitakyushu_news <- function(timeout_sec = 15, n_results = 20) {
  message("北九州市 新着情報 取得中...")
  tryCatch({
    resp <- GET(KITAKYUSHU_NEWS_URL, timeout(timeout_sec),
                add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
    if (status_code(resp) != 200) return(NULL)
    doc <- read_html(content(resp, "text", encoding = "UTF-8"))
    items <- xml_find_all(doc, "//li[contains(@class,'news__item')]")
    if (length(items) == 0) return(NULL)

    rows <- lapply(items, function(li) {
      date_str <- trimws(xml_text(xml_find_first(li, ".//p[contains(@class,'news__date')]")))
      a <- xml_find_first(li, ".//a")
      href  <- xml_attr(a, "href")
      title <- trimws(xml_text(xml_find_first(li, ".//p[contains(@class,'news__text')]")))
      if (nchar(title) == 0 || is.na(href) || nchar(date_str) == 0) return(NULL)
      d <- suppressWarnings(as.Date(gsub("年|月", "-", gsub("日", "", date_str)), format = "%Y-%m-%d"))
      if (is.na(d)) return(NULL)
      tibble(
        source_id   = "city_kitakyushu",
        source_name = "北九州市",
        category    = "行政",
        lang        = "ja",
        title       = title,
        link        = if (grepl("^https?://", href)) href else paste0("https://www.city.kitakyushu.lg.jp", href),
        pub_date    = d,
        summary     = NA_character_
      )
    })
    bind_rows(Filter(Negate(is.null), rows)) %>% distinct(link, .keep_all = TRUE) %>% head(n_results)
  }, error = function(e) { message("北九州市 エラー: ", e$message); NULL })
}

# ============================================================
# 横浜市 記者発表資料（HTMLスクレイピング。RSS未提供のため記事一覧ページを直接解析する）
# ============================================================
YOKOHAMA_NEWS_URL <- "https://www.city.yokohama.lg.jp/city-info/koho-kocho/press/"

fetch_yokohama_news <- function(timeout_sec = 15, n_results = 20) {
  message("横浜市 記者発表資料 取得中...")
  tryCatch({
    resp <- GET(YOKOHAMA_NEWS_URL, timeout(timeout_sec),
                add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
    if (status_code(resp) != 200) return(NULL)
    doc <- read_html(content(resp, "text", encoding = "UTF-8"))
    items <- xml_find_all(doc, "//ul[contains(@class,'info-list')]//li")
    if (length(items) == 0) return(NULL)

    rows <- lapply(items, function(li) {
      date_str <- trimws(xml_text(xml_find_first(li, ".//span[contains(@class,'date')]")))
      a <- xml_find_first(li, ".//span[contains(@class,'link')]//a")
      title <- trimws(xml_text(a))
      href  <- xml_attr(a, "href")
      if (nchar(title) == 0 || is.na(href) || nchar(date_str) == 0) return(NULL)
      d <- suppressWarnings(as.Date(gsub("年|月", "-", gsub("日", "", date_str)), format = "%Y-%m-%d"))
      if (is.na(d)) return(NULL)
      tibble(
        source_id   = "city_yokohama",
        source_name = "横浜市（記者発表資料）",
        category    = "行政",
        lang        = "ja",
        title       = title,
        link        = if (grepl("^https?://", href)) href else paste0("https://www.city.yokohama.lg.jp", href),
        pub_date    = d,
        summary     = NA_character_
      )
    })
    bind_rows(Filter(Negate(is.null), rows)) %>% distinct(link, .keep_all = TRUE) %>% head(n_results)
  }, error = function(e) { message("横浜市 エラー: ", e$message); NULL })
}

# ============================================================
# 神戸市 記者発表資料（HTMLスクレイピング。RSS未提供のため記事一覧ページを直接解析する。
# <h2>YYYY年M月</h2>の直後に<h3>M月D日（曜日）</h3>が複数続き、それぞれの直後の
# <ul class="information">に記事一覧が続く構造のため、直近のh2(年月)とh3(日)を
# 組み合わせて日付を復元する）
# ============================================================
KOBE_NEWS_URL <- "https://www.city.kobe.lg.jp/a57337/shise/press/index.html"

fetch_kobe_news <- function(timeout_sec = 15, n_results = 20) {
  message("神戸市 記者発表資料 取得中...")
  tryCatch({
    resp <- GET(KOBE_NEWS_URL, timeout(timeout_sec),
                add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
    if (status_code(resp) != 200) return(NULL)
    doc <- read_html(content(resp, "text", encoding = "UTF-8"))
    day_headers <- xml_find_all(doc, "//h3[contains(text(),'月') and contains(text(),'日')]")
    if (length(day_headers) == 0) return(NULL)

    rows <- lapply(day_headers, function(h3) {
      # 直近の年月見出し(h2, 例:"2026年7月")を遡って取得
      ym_node <- xml_find_first(h3, "preceding-sibling::h2[1]")
      ym_txt  <- if (!is.na(ym_node)) xml_text(ym_node) else format(Sys.Date(), "%Y年%m月")
      day_txt <- xml_text(h3)
      ym_m <- regmatches(ym_txt, regexec("(\\d{4})年(\\d{1,2})月", ym_txt))[[1]]
      d_m  <- regmatches(day_txt, regexec("(\\d{1,2})月(\\d{1,2})日", day_txt))[[1]]
      if (length(ym_m) < 3 || length(d_m) < 3) return(NULL)
      d <- suppressWarnings(as.Date(paste(ym_m[2], d_m[3], sep = "-"),
                                     format = paste0(ym_m[2], "-%d")))
      d <- suppressWarnings(as.Date(sprintf("%s-%02d-%02d", ym_m[2], as.integer(d_m[2]), as.integer(d_m[3]))))
      if (is.na(d)) return(NULL)
      ul <- xml_find_first(h3, "following-sibling::ul[1]")
      if (is.na(ul)) return(NULL)
      links <- xml_find_all(ul, ".//a")
      sub_rows <- lapply(links, function(a) {
        title <- trimws(xml_text(a))
        href  <- xml_attr(a, "href")
        if (nchar(title) == 0 || is.na(href)) return(NULL)
        tibble(
          source_id   = "city_kobe",
          source_name = "神戸市（記者発表資料）",
          category    = "行政",
          lang        = "ja",
          title       = title,
          link        = if (grepl("^https?://", href)) href else paste0("https://www.city.kobe.lg.jp", href),
          pub_date    = d,
          summary     = NA_character_
        )
      })
      bind_rows(Filter(Negate(is.null), sub_rows))
    })
    bind_rows(Filter(Negate(is.null), rows)) %>% distinct(link, .keep_all = TRUE) %>% head(n_results)
  }, error = function(e) { message("神戸市 エラー: ", e$message); NULL })
}

# ============================================================
# 福岡市 感染症関係報道発表資料（HTMLスクレイピング。感染性胃腸炎の集団発生・
# 腸管出血性大腸菌感染症の発生等、感染症カテゴリ専用の報道発表一覧ページを直接解析する。
# 各リンクがPDFで、リンクテキスト先頭に和暦"令和X年M月D日"が埋め込まれているため、
# 正規表現で抽出し西暦に変換する（令和1年=2019年））
# ============================================================
FUKUOKA_KANSEN_URL <- "https://www.city.fukuoka.lg.jp/hofuku/hokensho/kansensho/kansenshojoho/hodohappyou/kansenhoudou1.html"

fetch_fukuoka_kansen_news <- function(timeout_sec = 15, n_results = 30) {
  message("福岡市 感染症関係報道発表資料 取得中...")
  tryCatch({
    resp <- GET(FUKUOKA_KANSEN_URL, timeout(timeout_sec),
                add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
    if (status_code(resp) != 200) return(NULL)
    doc <- read_html(content(resp, "text", encoding = "UTF-8"))
    links <- xml_find_all(doc, "//a[contains(@class,'pdf')]")
    if (length(links) == 0) return(NULL)

    rows <- lapply(links, function(a) {
      title <- trimws(xml_text(a))
      href  <- xml_attr(a, "href")
      m <- regmatches(title, regexec("令和(\\d+)年(\\d{1,2})月(\\d{1,2})日", title))[[1]]
      if (length(m) < 4 || nchar(title) == 0 || is.na(href)) return(NULL)
      gyear <- as.integer(m[2]) + 2018
      d <- suppressWarnings(as.Date(sprintf("%d-%02d-%02d", gyear, as.integer(m[3]), as.integer(m[4]))))
      if (is.na(d)) return(NULL)
      tibble(
        source_id   = "city_fukuoka",
        source_name = "福岡市（感染症関係報道発表）",
        category    = "行政",
        lang        = "ja",
        title       = title,
        link        = if (grepl("^https?://", href)) href else paste0("https://www.city.fukuoka.lg.jp", href),
        pub_date    = d,
        summary     = NA_character_
      )
    })
    bind_rows(Filter(Negate(is.null), rows)) %>% distinct(link, .keep_all = TRUE) %>%
      arrange(desc(pub_date)) %>% head(n_results)
  }, error = function(e) { message("福岡市 エラー: ", e$message); NULL })
}

# ============================================================
# さいたま市 記者への提供資料（HTMLスクレイピング。RSS未提供のため、年度別ページから
# 最新の「令和X年度」を動的に特定し、さらにその中の最新の「令和X年M月」ページを
# 動的に特定してから記事一覧を解析する。各記事タイトル先頭に発表日
# "（令和X年M月D日発表）"が埋め込まれている）
# ============================================================
SAITAMA_PRESS_INDEX_URL <- "https://www.city.saitama.lg.jp/006/014/008/003/index.html"

fetch_saitama_news <- function(timeout_sec = 15, n_results = 20) {
  message("さいたま市 記者への提供資料 取得中...")
  tryCatch({
    idx_resp <- GET(SAITAMA_PRESS_INDEX_URL, timeout(timeout_sec),
                     add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
    if (status_code(idx_resp) != 200) return(NULL)
    idx_doc <- read_html(content(idx_resp, "text", encoding = "UTF-8"))
    fy_links <- xml_find_all(idx_doc, "//a[contains(text(),'年度')]")
    if (length(fy_links) == 0) return(NULL)
    fy_hrefs <- xml_attr(fy_links, "href")
    fy_ids <- suppressWarnings(as.integer(regmatches(fy_hrefs, regexpr("(?<=/)\\d+(?=/index\\.html$)", fy_hrefs, perl = TRUE))))
    fy_url <- fy_hrefs[which.max(fy_ids)]
    if (is.na(fy_url)) return(NULL)
    if (!grepl("^https?://", fy_url)) fy_url <- paste0("https://www.city.saitama.lg.jp", fy_url)

    fy_resp <- GET(fy_url, timeout(timeout_sec),
                    add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
    if (status_code(fy_resp) != 200) return(NULL)
    fy_doc <- read_html(content(fy_resp, "text", encoding = "UTF-8"))
    month_links <- xml_find_all(fy_doc, "//a[contains(text(),'年') and contains(text(),'月')]")
    if (length(month_links) == 0) return(NULL)
    month_hrefs <- xml_attr(month_links, "href")
    month_ids <- suppressWarnings(as.integer(regmatches(month_hrefs, regexpr("(?<=/)\\d+(?=/index\\.html$)", month_hrefs, perl = TRUE))))
    month_url <- month_hrefs[which.max(month_ids)]
    if (is.na(month_url)) return(NULL)
    if (!grepl("^https?://", month_url)) month_url <- paste0("https://www.city.saitama.lg.jp", month_url)

    resp <- GET(month_url, timeout(timeout_sec),
                add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
    if (status_code(resp) != 200) return(NULL)
    doc <- read_html(content(resp, "text", encoding = "UTF-8"))
    links <- xml_find_all(doc, "//a[contains(text(),'発表)') or contains(text(),'発表）')]")
    if (length(links) == 0) return(NULL)

    rows <- lapply(links, function(a) {
      title <- trimws(xml_text(a))
      href  <- xml_attr(a, "href")
      m <- regmatches(title, regexec("令和(\\d+)年(\\d{1,2})月(\\d{1,2})日発表", title))[[1]]
      if (length(m) < 4 || nchar(title) == 0 || is.na(href)) return(NULL)
      gyear <- as.integer(m[2]) + 2018
      d <- suppressWarnings(as.Date(sprintf("%d-%02d-%02d", gyear, as.integer(m[3]), as.integer(m[4]))))
      if (is.na(d)) return(NULL)
      tibble(
        source_id   = "city_saitama",
        source_name = "さいたま市（記者への提供資料）",
        category    = "行政",
        lang        = "ja",
        title       = title,
        link        = if (grepl("^https?://", href)) href else paste0("https://www.city.saitama.lg.jp", href),
        pub_date    = d,
        summary     = NA_character_
      )
    })
    bind_rows(Filter(Negate(is.null), rows)) %>% distinct(link, .keep_all = TRUE) %>% head(n_results)
  }, error = function(e) { message("さいたま市 エラー: ", e$message); NULL })
}

# ============================================================
# 大阪市 報道発表資料（HTMLスクレイピング。RSS未提供のため記事一覧ページを直接解析する。
# <h2 class="hdo_month_h2" id="hdo_YYYY_M">YYYY年M月</h2>で年月見出しが区切られ、
# その後に日付ごとの<h2>M月D日公開の報道発表資料</h2>と記事<ul>が複数続く構造）
# ============================================================
OSAKA_CITY_NEWS_URL <- "https://www.city.osaka.lg.jp/hodoshiryo/93-Curr.html"

fetch_osaka_city_news <- function(timeout_sec = 15, n_results = 20) {
  message("大阪市 報道発表資料 取得中...")
  tryCatch({
    resp <- GET(OSAKA_CITY_NEWS_URL, timeout(timeout_sec),
                add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
    if (status_code(resp) != 200) return(NULL)
    doc <- read_html(content(resp, "text", encoding = "UTF-8"))
    day_headers <- xml_find_all(doc, "//h2[contains(text(),'公開の報道発表資料')]")
    if (length(day_headers) == 0) return(NULL)

    rows <- lapply(day_headers, function(h2) {
      ym_node <- xml_find_first(h2, "preceding::h2[contains(@class,'hdo_month_h2')][1]")
      ym_txt  <- if (!is.na(ym_node)) xml_text(ym_node) else format(Sys.Date(), "%Y年%m月")
      ym_m <- regmatches(ym_txt, regexec("(\\d{4})年(\\d{1,2})月", ym_txt))[[1]]
      day_txt <- xml_text(h2)
      d_m <- regmatches(day_txt, regexec("(\\d{1,2})月(\\d{1,2})日", day_txt))[[1]]
      if (length(ym_m) < 3 || length(d_m) < 3) return(NULL)
      d <- suppressWarnings(as.Date(sprintf("%s-%02d-%02d", ym_m[2], as.integer(d_m[2]), as.integer(d_m[3]))))
      if (is.na(d)) return(NULL)
      # h2は<div class="h2_box">の子。記事一覧は親divの直後の<div class="hdo_sub_lower">内
      box <- xml_find_first(h2, "parent::div")
      sub <- if (!is.na(box)) xml_find_first(box, "following-sibling::div[contains(@class,'hdo_sub_lower')][1]") else NA
      if (is.na(sub)) return(NULL)
      links <- xml_find_all(sub, ".//a")
      sub_rows <- lapply(links, function(a) {
        title <- trimws(xml_text(a))
        href  <- xml_attr(a, "href")
        if (nchar(title) == 0 || is.na(href)) return(NULL)
        tibble(
          source_id   = "city_osaka",
          source_name = "大阪市（報道発表資料）",
          category    = "行政",
          lang        = "ja",
          title       = title,
          link        = if (grepl("^https?://", href)) href else paste0("https://www.city.osaka.lg.jp", href),
          pub_date    = d,
          summary     = NA_character_
        )
      })
      bind_rows(Filter(Negate(is.null), sub_rows))
    })
    bind_rows(Filter(Negate(is.null), rows)) %>% distinct(link, .keep_all = TRUE) %>% head(n_results)
  }, error = function(e) { message("大阪市 エラー: ", e$message); NULL })
}

# ============================================================
# 汎用: 「日付見出し + 直後に記事一覧が続く」構造のページを、日付見出しノードと
# 記事ノードの和集合をXPathで取得（文書順序を保持）し、順に走査して直近の見出しの
# 日付を各記事に割り当てる。新潟市・熊本市のようにul等の兄弟構造でグループ化されて
# いないページ（日付見出しと記事が単純に交互に並ぶだけ）に対応するための共通ロジック
# ============================================================
.scan_date_grouped_articles <- function(doc, header_xpath, item_xpath, parse_date) {
  combined_xpath <- paste0(header_xpath, " | ", item_xpath)
  nodes <- xml_find_all(doc, combined_xpath)
  if (length(nodes) == 0) return(list())
  header_nodes <- xml_find_all(doc, header_xpath)
  header_set <- as.character(header_nodes)

  current_date <- as.Date(NA)
  rows <- list()
  for (node in nodes) {
    if (as.character(node) %in% header_set) {
      current_date <- parse_date(xml_text(node))
    } else if (!is.na(current_date)) {
      rows[[length(rows) + 1]] <- list(node = node, date = current_date)
    }
  }
  rows
}

# ============================================================
# 新潟市 報道発表資料（HTMLスクレイピング。RSS未提供のため月別ページを直接解析する。
# <h2>令和X年M月D日（曜）</h2>の直後に複数の<p class="filelink"><a class="pdf">が
# ul等の入れ子なしに単純に続く構造のため、.scan_date_grouped_articles()で処理する）
# ============================================================
NIIGATA_NEWS_MONTH_URL <- "https://www.city.niigata.lg.jp/shisei/koho/houdou/%s.html"

fetch_niigata_news <- function(timeout_sec = 15, n_results = 20) {
  message("新潟市 報道発表資料 取得中...")
  tryCatch({
    ym <- format(Sys.Date(), "%Y%m")
    url <- sprintf(NIIGATA_NEWS_MONTH_URL, ym)
    resp <- GET(url, timeout(timeout_sec),
                add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
    if (status_code(resp) != 200) return(NULL)
    doc <- read_html(content(resp, "text", encoding = "UTF-8"))

    parse_date <- function(txt) {
      m <- regmatches(txt, regexec("令和(\\d+)年(\\d{1,2})月(\\d{1,2})日", txt))[[1]]
      if (length(m) < 4) return(as.Date(NA))
      gyear <- as.integer(m[2]) + 2018
      suppressWarnings(as.Date(sprintf("%d-%02d-%02d", gyear, as.integer(m[3]), as.integer(m[4]))))
    }
    grouped <- .scan_date_grouped_articles(
      doc,
      "//h2[contains(text(),'月') and contains(text(),'日') and contains(text(),'曜')]",
      "//p[contains(@class,'filelink')]/a[contains(@class,'pdf')]",
      parse_date
    )
    if (length(grouped) == 0) return(NULL)

    rows <- lapply(grouped, function(g) {
      title <- trimws(xml_text(g$node))
      href  <- xml_attr(g$node, "href")
      if (nchar(title) == 0 || is.na(href)) return(NULL)
      tibble(
        source_id   = "city_niigata",
        source_name = "新潟市（報道発表資料）",
        category    = "行政",
        lang        = "ja",
        title       = title,
        link        = if (grepl("^https?://", href)) href
                       else paste0("https://www.city.niigata.lg.jp/shisei/koho/houdou/", href),
        pub_date    = g$date,
        summary     = NA_character_
      )
    })
    bind_rows(Filter(Negate(is.null), rows)) %>% distinct(link, .keep_all = TRUE) %>% head(n_results)
  }, error = function(e) { message("新潟市 エラー: ", e$message); NULL })
}

# ============================================================
# 熊本市 報道発表（HTMLスクレイピング。RSS未提供のため月別ページを直接解析する。
# <h4>報道発表　令和X年M月</h4>で年月を示し、<h3>M月D日（曜日）発表</h3>の直後に
# 記事(PDFリンク)が単純に続く構造のため、.scan_date_grouped_articles()で処理する）
# ============================================================
KUMAMOTO_NEWS_URL <- "https://www.city.kumamoto.jp/kiji00371656/index.html"

fetch_kumamoto_news <- function(timeout_sec = 15, n_results = 20) {
  message("熊本市 報道発表 取得中...")
  tryCatch({
    resp <- GET(KUMAMOTO_NEWS_URL, timeout(timeout_sec),
                add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
    if (status_code(resp) != 200) return(NULL)
    doc <- read_html(content(resp, "text", encoding = "UTF-8"))

    ym_node <- xml_find_first(doc, "//h4[contains(text(),'報道発表')]")
    ym_txt  <- if (!is.na(ym_node)) xml_text(ym_node) else format(Sys.Date(), "令和8年%m月")
    ym_m <- regmatches(ym_txt, regexec("令和(\\d+)年(\\d{1,2})月", ym_txt))[[1]]
    gyear <- if (length(ym_m) >= 3) as.integer(ym_m[2]) + 2018 else as.integer(format(Sys.Date(), "%Y"))

    parse_date <- function(txt) {
      m <- regmatches(txt, regexec("(\\d{1,2})月(\\d{1,2})日", txt))[[1]]
      if (length(m) < 3) return(as.Date(NA))
      suppressWarnings(as.Date(sprintf("%d-%02d-%02d", gyear, as.integer(m[2]), as.integer(m[3]))))
    }
    grouped <- .scan_date_grouped_articles(
      doc,
      "//h3[contains(@class,'title') and contains(text(),'発表')]",
      "//div[contains(@class,'wys_template') and contains(@class,'wys_list')]//a",
      parse_date
    )
    if (length(grouped) == 0) return(NULL)

    rows <- lapply(grouped, function(g) {
      title <- trimws(xml_text(g$node))
      href  <- xml_attr(g$node, "href")
      if (nchar(title) == 0 || is.na(href)) return(NULL)
      tibble(
        source_id   = "city_kumamoto",
        source_name = "熊本市（報道発表）",
        category    = "行政",
        lang        = "ja",
        title       = title,
        link        = if (grepl("^https?://", href)) href else paste0("https://www.city.kumamoto.jp", href),
        pub_date    = g$date,
        summary     = NA_character_
      )
    })
    bind_rows(Filter(Negate(is.null), rows)) %>% distinct(link, .keep_all = TRUE) %>% head(n_results)
  }, error = function(e) { message("熊本市 エラー: ", e$message); NULL })
}

# ============================================================
# 郡山市 報道資料（HTMLスクレイピング。RSS未提供のため、月別インデックスページから
# 最新の「令和X年M月の報道資料」を動的に特定してから記事一覧を解析する。
# 年月は月別ページの<title>から取得し、各記事は<li><a>M月D日（曜）タイトル</a></li>
# 形式のため正規表現で日を抽出する）
# ============================================================
KORIYAMA_PRESS_INDEX_URL <- "https://www.city.koriyama.lg.jp/life/6/36/243/"

fetch_koriyama_news <- function(timeout_sec = 15, n_results = 20) {
  message("郡山市 報道資料 取得中...")
  tryCatch({
    idx_resp <- GET(KORIYAMA_PRESS_INDEX_URL, timeout(timeout_sec),
                     add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
    if (status_code(idx_resp) != 200) return(NULL)
    idx_doc <- read_html(content(idx_resp, "text", encoding = "UTF-8"))
    a <- xml_find_first(idx_doc, "//span[contains(@class,'article_title')]//a")
    if (is.na(a)) return(NULL)
    href <- xml_attr(a, "href")
    if (is.na(href)) return(NULL)
    month_url <- if (grepl("^https?://", href)) href else paste0("https://www.city.koriyama.lg.jp", href)

    resp <- GET(month_url, timeout(timeout_sec),
                add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
    if (status_code(resp) != 200) return(NULL)
    doc <- read_html(content(resp, "text", encoding = "UTF-8"))
    title_txt <- xml_text(xml_find_first(doc, "//title"))
    ym_m <- regmatches(title_txt, regexec("令和(\\d+)年(\\d{1,2})月", title_txt))[[1]]
    if (length(ym_m) < 3) return(NULL)
    gyear <- as.integer(ym_m[2]) + 2018
    gmonth <- as.integer(ym_m[3])

    items <- xml_find_all(doc, "//li[a[contains(@href,'.pdf')]]")
    if (length(items) == 0) return(NULL)

    rows <- lapply(items, function(li) {
      a2 <- xml_find_first(li, ".//a")
      title <- trimws(xml_text(a2))
      href2 <- xml_attr(a2, "href")
      dm <- regmatches(title, regexec("^(\\d{1,2})月(\\d{1,2})日", title))[[1]]
      if (length(dm) < 3 || nchar(title) == 0 || is.na(href2)) return(NULL)
      d <- suppressWarnings(as.Date(sprintf("%d-%02d-%02d", gyear, as.integer(dm[2]), as.integer(dm[3]))))
      if (is.na(d)) return(NULL)
      tibble(
        source_id   = "city_koriyama",
        source_name = "郡山市（報道資料）",
        category    = "行政",
        lang        = "ja",
        title       = title,
        link        = if (grepl("^https?://", href2)) href2 else paste0("https://www.city.koriyama.lg.jp", href2),
        pub_date    = d,
        summary     = NA_character_
      )
    })
    bind_rows(Filter(Negate(is.null), rows)) %>% distinct(link, .keep_all = TRUE) %>% head(n_results)
  }, error = function(e) { message("郡山市 エラー: ", e$message); NULL })
}

# ============================================================
# 宇都宮市 新着更新情報（HTMLスクレイピング。RSS未提供のため記事一覧ページを直接解析する。
# 日付が和暦"令和X年M月D日"形式のため西暦に変換する）
# ============================================================
UTSUNOMIYA_NEWS_URL <- "https://www.city.utsunomiya.lg.jp/arrival.html"

fetch_utsunomiya_news <- function(timeout_sec = 15, n_results = 20) {
  message("宇都宮市 新着更新情報 取得中...")
  tryCatch({
    resp <- GET(UTSUNOMIYA_NEWS_URL, timeout(timeout_sec),
                add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
    if (status_code(resp) != 200) return(NULL)
    doc <- read_html(content(resp, "text", encoding = "UTF-8"))
    items <- xml_find_all(doc, "//ul[contains(@class,'newslist')]//li")
    if (length(items) == 0) return(NULL)

    rows <- lapply(items, function(li) {
      date_str <- trimws(xml_text(xml_find_first(li, ".//span[contains(@class,'date')]")))
      a <- xml_find_first(li, ".//span[contains(@class,'newsli')]//a")
      title <- trimws(xml_text(a))
      href  <- xml_attr(a, "href")
      m <- regmatches(date_str, regexec("令和(\\d+)年(\\d{1,2})月(\\d{1,2})日", date_str))[[1]]
      if (length(m) < 4 || nchar(title) == 0 || is.na(href)) return(NULL)
      gyear <- as.integer(m[2]) + 2018
      d <- suppressWarnings(as.Date(sprintf("%d-%02d-%02d", gyear, as.integer(m[3]), as.integer(m[4]))))
      if (is.na(d)) return(NULL)
      tibble(
        source_id   = "city_utsunomiya",
        source_name = "宇都宮市",
        category    = "行政",
        lang        = "ja",
        title       = title,
        link        = if (grepl("^https?://", href)) href
                       else paste0("https://www.city.utsunomiya.lg.jp/", sub("^\\./", "", href)),
        pub_date    = d,
        summary     = NA_character_
      )
    })
    bind_rows(Filter(Negate(is.null), rows)) %>% distinct(link, .keep_all = TRUE) %>% head(n_results)
  }, error = function(e) { message("宇都宮市 エラー: ", e$message); NULL })
}

# ============================================================
# 旭川市 記者発表（HTMLスクレイピング。RSS未提供のため、年度別インデックスページから
# 最新の「令和X年度の記者発表一覧」を動的に特定してから記事一覧を解析する。
# 各記事タイトル末尾に"（令和X年M月D日）"が埋め込まれている）
# ============================================================
ASAHIKAWA_PRESS_INDEX_URL <- "https://www.city.asahikawa.hokkaido.jp/700/723/735/index.html"

fetch_asahikawa_news <- function(timeout_sec = 15, n_results = 20) {
  message("旭川市 記者発表 取得中...")
  tryCatch({
    idx_resp <- GET(ASAHIKAWA_PRESS_INDEX_URL, timeout(timeout_sec),
                     add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
    if (status_code(idx_resp) != 200) return(NULL)
    idx_doc <- read_html(content(idx_resp, "text", encoding = "UTF-8"))
    links <- xml_find_all(idx_doc, "//a[contains(text(),'年度の記者発表一覧')]")
    if (length(links) == 0) return(NULL)
    hrefs <- xml_attr(links, "href")
    ids <- suppressWarnings(as.integer(regmatches(hrefs, regexpr("(?<=/d)\\d+(?=\\.html$)", hrefs, perl = TRUE))))
    latest_url <- hrefs[which.max(ids)]
    if (is.na(latest_url)) return(NULL)
    if (!grepl("^https?://", latest_url)) latest_url <- paste0("https://www.city.asahikawa.hokkaido.jp", latest_url)

    resp <- GET(latest_url, timeout(timeout_sec),
                add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
    if (status_code(resp) != 200) return(NULL)
    doc <- read_html(content(resp, "text", encoding = "UTF-8"))
    items <- xml_find_all(doc, "//div[contains(@class,'article')]//li[a]")
    if (length(items) == 0) return(NULL)

    rows <- lapply(items, function(li) {
      a <- xml_find_first(li, ".//a")
      title <- trimws(xml_text(a))
      href  <- xml_attr(a, "href")
      m <- regmatches(title, regexec("令和(\\d+)年(\\d{1,2})月(\\d{1,2})日", title))[[1]]
      if (length(m) < 4 || nchar(title) == 0 || is.na(href)) return(NULL)
      gyear <- as.integer(m[2]) + 2018
      d <- suppressWarnings(as.Date(sprintf("%d-%02d-%02d", gyear, as.integer(m[3]), as.integer(m[4]))))
      if (is.na(d)) return(NULL)
      tibble(
        source_id   = "city_asahikawa",
        source_name = "旭川市（記者発表）",
        category    = "行政",
        lang        = "ja",
        title       = title,
        link        = if (grepl("^https?://", href)) href else paste0("https://www.city.asahikawa.hokkaido.jp", href),
        pub_date    = d,
        summary     = NA_character_
      )
    })
    bind_rows(Filter(Negate(is.null), rows)) %>% distinct(link, .keep_all = TRUE) %>% head(n_results)
  }, error = function(e) { message("旭川市 エラー: ", e$message); NULL })
}

# ============================================================
# 函館市 新着情報（HTMLスクレイピング。RSS未提供のため記事一覧ページを直接解析する。
# <h2 class="date">YYYY年M月D日</h2>の直後の<ul>に記事一覧が続く構造のため、
# .scan_date_grouped_articles()で処理する）
# ============================================================
HAKODATE_NEWS_URL <- "https://www.city.hakodate.hokkaido.jp/docs/"

fetch_hakodate_news <- function(timeout_sec = 15, n_results = 20) {
  message("函館市 新着情報 取得中...")
  tryCatch({
    resp <- GET(HAKODATE_NEWS_URL, timeout(timeout_sec),
                add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
    if (status_code(resp) != 200) return(NULL)
    doc <- read_html(content(resp, "text", encoding = "UTF-8"))

    parse_date <- function(txt) {
      m <- regmatches(txt, regexec("(\\d{4})年(\\d{1,2})月(\\d{1,2})日", txt))[[1]]
      if (length(m) < 4) return(as.Date(NA))
      suppressWarnings(as.Date(sprintf("%s-%02d-%02d", m[2], as.integer(m[3]), as.integer(m[4]))))
    }
    grouped <- .scan_date_grouped_articles(
      doc,
      "//div[contains(@class,'docs')]/h2[contains(@class,'date')]",
      "//div[contains(@class,'docs')]/ul/li//a",
      parse_date
    )
    if (length(grouped) == 0) return(NULL)

    rows <- lapply(grouped, function(g) {
      title <- trimws(xml_text(g$node))
      href  <- xml_attr(g$node, "href")
      if (nchar(title) == 0 || is.na(href)) return(NULL)
      tibble(
        source_id   = "city_hakodate",
        source_name = "函館市",
        category    = "行政",
        lang        = "ja",
        title       = title,
        link        = if (grepl("^https?://", href)) href else paste0("https://www.city.hakodate.hokkaido.jp", href),
        pub_date    = g$date,
        summary     = NA_character_
      )
    })
    bind_rows(Filter(Negate(is.null), rows)) %>% distinct(link, .keep_all = TRUE) %>% head(n_results)
  }, error = function(e) { message("函館市 エラー: ", e$message); NULL })
}

# ============================================================
# 前橋市 報道資料（HTMLスクレイピング。RSS未提供のため記事一覧ページを直接解析する。
# <h2>令和X年M月D日</h2>の直後に<p class="file-link-item">記事(PDF)が複数続く構造のため
# .scan_date_grouped_articles()で処理する）
# ============================================================
MAEBASHI_NEWS_URL <- "https://www.city.maebashi.gunma.jp/soshiki/seisaku/kohobrand/gyomu/17/50207.html"

fetch_maebashi_news <- function(timeout_sec = 15, n_results = 20) {
  message("前橋市 報道資料 取得中...")
  tryCatch({
    resp <- GET(MAEBASHI_NEWS_URL, timeout(timeout_sec),
                add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
    if (status_code(resp) != 200) return(NULL)
    doc <- read_html(content(resp, "text", encoding = "UTF-8"))

    parse_date <- function(txt) {
      m <- regmatches(txt, regexec("令和(\\d+)年(\\d{1,2})月(\\d{1,2})日", txt))[[1]]
      if (length(m) < 4) return(as.Date(NA))
      gyear <- as.integer(m[2]) + 2018
      suppressWarnings(as.Date(sprintf("%d-%02d-%02d", gyear, as.integer(m[3]), as.integer(m[4]))))
    }
    grouped <- .scan_date_grouped_articles(
      doc,
      "//h2[contains(.,'令和') and contains(.,'月') and contains(.,'日')]",
      "//p[contains(@class,'file-link-item')]/a",
      parse_date
    )
    if (length(grouped) == 0) return(NULL)

    rows <- lapply(grouped, function(g) {
      title <- trimws(xml_text(g$node))
      href  <- xml_attr(g$node, "href")
      if (nchar(title) == 0 || is.na(href)) return(NULL)
      tibble(
        source_id   = "city_maebashi",
        source_name = "前橋市（報道資料）",
        category    = "行政",
        lang        = "ja",
        title       = title,
        link        = if (grepl("^https?://", href)) href
                       else paste0("https:", href),
        pub_date    = g$date,
        summary     = NA_character_
      )
    })
    bind_rows(Filter(Negate(is.null), rows)) %>% distinct(link, .keep_all = TRUE) %>% head(n_results)
  }, error = function(e) { message("前橋市 エラー: ", e$message); NULL })
}

# ============================================================
# 越谷市 新着情報（HTMLスクレイピング。RSS未提供のため記事一覧ページを直接解析する）
# ============================================================
KOSHIGAYA_NEWS_URL <- "https://www.city.koshigaya.saitama.jp/allNewsList.html"

fetch_koshigaya_news <- function(timeout_sec = 15, n_results = 20) {
  message("越谷市 新着情報 取得中...")
  tryCatch({
    resp <- GET(KOSHIGAYA_NEWS_URL, timeout(timeout_sec),
                add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
    if (status_code(resp) != 200) return(NULL)
    doc <- read_html(content(resp, "text", encoding = "UTF-8"))
    items <- xml_find_all(doc, "//li[.//span[contains(@class,'date')]]")
    if (length(items) == 0) return(NULL)

    rows <- lapply(items, function(li) {
      date_str <- trimws(xml_text(xml_find_first(li, ".//span[contains(@class,'date')]")))
      a <- xml_find_first(li, ".//span[contains(@class,'link')]//a")
      title <- trimws(xml_text(a))
      href  <- xml_attr(a, "href")
      if (nchar(title) == 0 || is.na(href) || nchar(date_str) == 0) return(NULL)
      d <- suppressWarnings(as.Date(gsub("年|月", "-", gsub("日", "", date_str)), format = "%Y-%m-%d"))
      if (is.na(d)) return(NULL)
      tibble(
        source_id   = "city_koshigaya",
        source_name = "越谷市",
        category    = "行政",
        lang        = "ja",
        title       = title,
        link        = if (grepl("^https?://", href)) href else paste0("https://www.city.koshigaya.saitama.jp", href),
        pub_date    = d,
        summary     = NA_character_
      )
    })
    bind_rows(Filter(Negate(is.null), rows)) %>% distinct(link, .keep_all = TRUE) %>% head(n_results)
  }, error = function(e) { message("越谷市 エラー: ", e$message); NULL })
}

# ============================================================
# 柏市 新着情報一覧（HTMLスクレイピング。RSS未提供のため記事一覧ページを直接解析する。
# 日付が要素属性ではなく<li>テキスト末尾に"（M月D日）"形式で埋め込まれており、
# 年の記載が無いため現在の年を基準に、月が現在月より大幅に先の場合は前年とみなす）
# ============================================================
KASHIWA_NEWS_URL <- "https://www.city.kashiwa.lg.jp/shinchaku/index.html"

fetch_kashiwa_news <- function(timeout_sec = 15, n_results = 20) {
  message("柏市 新着情報 取得中...")
  tryCatch({
    resp <- GET(KASHIWA_NEWS_URL, timeout(timeout_sec),
                add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
    if (status_code(resp) != 200) return(NULL)
    doc <- read_html(content(resp, "text", encoding = "UTF-8"))
    items <- xml_find_all(doc, "//li[a]")
    if (length(items) == 0) return(NULL)
    this_year <- as.integer(format(Sys.Date(), "%Y"))
    this_month <- as.integer(format(Sys.Date(), "%m"))

    rows <- lapply(items, function(li) {
      a <- xml_find_first(li, ".//a")
      title <- trimws(xml_text(a))
      href  <- xml_attr(a, "href")
      m <- regmatches(title, regexpr("\\d{1,2}月\\d{1,2}日", title))
      if (length(m) == 0 || nchar(m) == 0 || nchar(title) == 0 || is.na(href)) return(NULL)
      dm <- regmatches(m, regexec("(\\d{1,2})月(\\d{1,2})日", m))[[1]]
      if (length(dm) < 3) return(NULL)
      mon <- as.integer(dm[2]); day <- as.integer(dm[3])
      yr <- if (mon > this_month + 1) this_year - 1 else this_year
      d <- suppressWarnings(as.Date(sprintf("%d-%02d-%02d", yr, mon, day)))
      if (is.na(d)) return(NULL)
      tibble(
        source_id   = "city_kashiwa",
        source_name = "柏市",
        category    = "行政",
        lang        = "ja",
        title       = title,
        link        = if (grepl("^https?://", href)) href else paste0("https://www.city.kashiwa.lg.jp", href),
        pub_date    = d,
        summary     = NA_character_
      )
    })
    bind_rows(Filter(Negate(is.null), rows)) %>% distinct(link, .keep_all = TRUE) %>% head(n_results)
  }, error = function(e) { message("柏市 エラー: ", e$message); NULL })
}

# ============================================================
# 福井市 新着一覧（HTMLスクレイピング。RSS未提供のため記事一覧ページを直接解析する）
# ============================================================
FUKUI_CITY_NEWS_URL <- "https://www.city.fukui.lg.jp/news.html"

fetch_fukui_city_news <- function(timeout_sec = 15, n_results = 20) {
  message("福井市 新着一覧 取得中...")
  tryCatch({
    resp <- GET(FUKUI_CITY_NEWS_URL, timeout(timeout_sec),
                add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
    if (status_code(resp) != 200) return(NULL)
    doc <- read_html(content(resp, "text", encoding = "UTF-8"))
    items <- xml_find_all(doc, "//li[.//span[contains(@class,'date')]]")
    if (length(items) == 0) return(NULL)

    rows <- lapply(items, function(li) {
      date_str <- trimws(xml_text(xml_find_first(li, ".//span[contains(@class,'date')]")))
      a <- xml_find_first(li, ".//span[contains(@class,'title')]//a")
      title <- trimws(xml_text(a))
      href  <- xml_attr(a, "href")
      if (nchar(title) == 0 || is.na(href) || nchar(date_str) == 0) return(NULL)
      d <- suppressWarnings(as.Date(gsub("年|月", "-", gsub("日", "", date_str)), format = "%Y-%m-%d"))
      if (is.na(d)) return(NULL)
      tibble(
        source_id   = "city_fukui2",
        source_name = "福井市",
        category    = "行政",
        lang        = "ja",
        title       = title,
        link        = if (grepl("^https?://", href)) href else paste0("https://www.city.fukui.lg.jp", href),
        pub_date    = d,
        summary     = NA_character_
      )
    })
    bind_rows(Filter(Negate(is.null), rows)) %>% distinct(link, .keep_all = TRUE) %>% head(n_results)
  }, error = function(e) { message("福井市 エラー: ", e$message); NULL })
}

# ============================================================
# 長野市 新着情報一覧（HTMLスクレイピング。RSS未提供のため記事一覧ページを直接解析する。
# テーブル形式(<tr><td class="date">日付</td><td><a>タイトル</a></td></tr>)）
# ============================================================
NAGANO_CITY_NEWS_URL <- "https://www.city.nagano.nagano.jp/menu/2/shinchaku/index.html"

fetch_nagano_city_news <- function(timeout_sec = 15, n_results = 20) {
  message("長野市 新着情報 取得中...")
  tryCatch({
    resp <- GET(NAGANO_CITY_NEWS_URL, timeout(timeout_sec),
                add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
    if (status_code(resp) != 200) return(NULL)
    doc <- read_html(content(resp, "text", encoding = "UTF-8"))
    rows_tr <- xml_find_all(doc, "//table[contains(@class,'list_table')]//tr[td[contains(@class,'date')]]")
    if (length(rows_tr) == 0) return(NULL)

    rows <- lapply(rows_tr, function(tr) {
      date_str <- trimws(xml_text(xml_find_first(tr, ".//td[contains(@class,'date')]")))
      a <- xml_find_first(tr, ".//td[not(contains(@class,'date'))]//a")
      title <- trimws(xml_text(a))
      href  <- xml_attr(a, "href")
      if (nchar(title) == 0 || is.na(href) || nchar(date_str) == 0) return(NULL)
      d <- suppressWarnings(as.Date(gsub("年|月", "-", gsub("日", "", date_str)), format = "%Y-%m-%d"))
      if (is.na(d)) return(NULL)
      tibble(
        source_id   = "city_nagano2",
        source_name = "長野市",
        category    = "行政",
        lang        = "ja",
        title       = title,
        link        = if (grepl("^https?://", href)) href else paste0("https://www.city.nagano.nagano.jp", href),
        pub_date    = d,
        summary     = NA_character_
      )
    })
    bind_rows(Filter(Negate(is.null), rows)) %>% distinct(link, .keep_all = TRUE) %>% head(n_results)
  }, error = function(e) { message("長野市 エラー: ", e$message); NULL })
}

# ============================================================
# 高松市 新着情報一覧（HTMLスクレイピング。RSS未提供のため記事一覧ページを直接解析する）
# ============================================================
TAKAMATSU_NEWS_URL <- "https://www.city.takamatsu.kagawa.jp/kurashi/allNewsList.html"

fetch_takamatsu_news <- function(timeout_sec = 15, n_results = 20) {
  message("高松市 新着情報 取得中...")
  tryCatch({
    resp <- GET(TAKAMATSU_NEWS_URL, timeout(timeout_sec),
                add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
    if (status_code(resp) != 200) return(NULL)
    doc <- read_html(content(resp, "text", encoding = "UTF-8"))
    items <- xml_find_all(doc, "//li[.//span[contains(@class,'date')]]")
    if (length(items) == 0) return(NULL)

    rows <- lapply(items, function(li) {
      date_str <- trimws(xml_text(xml_find_first(li, ".//span[contains(@class,'date')]")))
      a <- xml_find_first(li, ".//span[contains(@class,'infotxt')]//a")
      title <- trimws(xml_text(a))
      href  <- xml_attr(a, "href")
      if (nchar(title) == 0 || is.na(href) || nchar(date_str) == 0) return(NULL)
      d <- suppressWarnings(as.Date(gsub("年|月", "-", gsub("日", "", date_str)), format = "%Y-%m-%d"))
      if (is.na(d)) return(NULL)
      tibble(
        source_id   = "city_takamatsu",
        source_name = "高松市",
        category    = "行政",
        lang        = "ja",
        title       = title,
        link        = if (grepl("^https?://", href)) href else paste0("https://www.city.takamatsu.kagawa.jp", href),
        pub_date    = d,
        summary     = NA_character_
      )
    })
    bind_rows(Filter(Negate(is.null), rows)) %>% distinct(link, .keep_all = TRUE) %>% head(n_results)
  }, error = function(e) { message("高松市 エラー: ", e$message); NULL })
}

# ============================================================
# つくば市 新着情報（JSON API。新着情報一覧ページ(news_list.html)はJavaScriptで
# index.update.jsonを取得して動的に描画する構造のため、静的HTML/RSSでは記事一覧を
# 取得できない。ブラウザの通信を確認しこのJSON APIを発見。同一のCMSベンダーを利用する
# 複数の自治体（金沢市・大津市・松江市・寝屋川市・八戸市等）で共通のため汎用関数化する）
# ============================================================
.fetch_index_update_json_news <- function(base_url, source_id, source_name,
                                           timeout_sec = 15, n_results = 20) {
  tryCatch({
    url <- paste0(sub("/$", "", base_url), "/index.update.json")
    resp <- GET(url, timeout(timeout_sec),
                add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
    if (status_code(resp) != 200) return(NULL)
    items <- jsonlite::fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyDataFrame = TRUE)
    if (is.null(items) || nrow(items) == 0) return(NULL)

    tibble(
      source_id   = source_id,
      source_name = source_name,
      category    = "行政",
      lang        = "ja",
      title       = items$page_name,
      link        = items$url,
      pub_date    = as.Date(substr(items$publish_datetime, 1, 10)),
      summary     = NA_character_
    ) %>% filter(!is.na(title), nchar(title) > 0, !is.na(link)) %>%
      distinct(link, .keep_all = TRUE) %>% head(n_results)
  }, error = function(e) { message(source_name, " エラー: ", e$message); NULL })
}

fetch_tsukuba_news <- function(timeout_sec = 15, n_results = 20) {
  message("つくば市 新着情報 取得中...")
  .fetch_index_update_json_news("https://www.city.tsukuba.lg.jp", "city_tsukuba", "つくば市",
                                 timeout_sec, n_results)
}

fetch_kanazawa_news <- function(timeout_sec = 15, n_results = 20) {
  message("金沢市 新着情報 取得中...")
  .fetch_index_update_json_news("https://www4.city.kanazawa.lg.jp", "city_kanazawa", "金沢市",
                                 timeout_sec, n_results)
}

fetch_otsu_news <- function(timeout_sec = 15, n_results = 20) {
  message("大津市 新着情報 取得中...")
  .fetch_index_update_json_news("https://www.city.otsu.lg.jp", "city_otsu", "大津市",
                                 timeout_sec, n_results)
}

fetch_matsue_news <- function(timeout_sec = 15, n_results = 20) {
  message("松江市 新着情報 取得中...")
  .fetch_index_update_json_news("https://www.city.matsue.lg.jp", "city_matsue", "松江市",
                                 timeout_sec, n_results)
}

fetch_neyagawa_news <- function(timeout_sec = 15, n_results = 20) {
  message("寝屋川市 新着情報 取得中...")
  .fetch_index_update_json_news("https://www.city.neyagawa.osaka.jp", "city_neyagawa", "寝屋川市",
                                 timeout_sec, n_results)
}

fetch_hachinohe_news <- function(timeout_sec = 15, n_results = 20) {
  message("八戸市 新着情報 取得中...")
  .fetch_index_update_json_news("https://www.city.hachinohe.aomori.jp", "city_hachinohe", "八戸市",
                                 timeout_sec, n_results)
}

fetch_saga_news <- function(timeout_sec = 15, n_results = 20) {
  message("佐賀市 新着情報 取得中...")
  .fetch_index_update_json_news("https://www.city.saga.lg.jp", "city_saga", "佐賀市",
                                 timeout_sec, n_results)
}

fetch_kawaguchi_news <- function(timeout_sec = 15, n_results = 20) {
  message("川口市 新着情報 取得中...")
  .fetch_index_update_json_news("https://www.city.kawaguchi.lg.jp", "city_kawaguchi", "川口市",
                                 timeout_sec, n_results)
}

# ============================================================
# 船橋市 新着情報一覧（HTMLスクレイピング。RSS未提供のため記事一覧ページを直接解析する。
# 日付がタイトル末尾に"(令和X(YYYY)年M月D日)"形式で埋め込まれている）
# ============================================================
FUNABASHI_NEWS_URL <- "https://www.city.funabashi.lg.jp/information/news.html"

fetch_funabashi_news <- function(timeout_sec = 15, n_results = 20) {
  message("船橋市 新着情報 取得中...")
  tryCatch({
    resp <- GET(FUNABASHI_NEWS_URL, timeout(timeout_sec),
                add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
    if (status_code(resp) != 200) return(NULL)
    doc <- read_html(content(resp, "text", encoding = "UTF-8"))
    items <- xml_find_all(doc, "//li[a]")
    if (length(items) == 0) return(NULL)

    rows <- lapply(items, function(li) {
      a <- xml_find_first(li, ".//a")
      title <- trimws(xml_text(a))
      href  <- xml_attr(a, "href")
      m <- regmatches(title, regexec("令和\\d+\\((\\d{4})\\)年(\\d{1,2})月(\\d{1,2})日\\)$", title, perl = TRUE))[[1]]
      if (length(m) < 4 || nchar(title) == 0 || is.na(href)) return(NULL)
      d <- suppressWarnings(as.Date(sprintf("%s-%02d-%02d", m[2], as.integer(m[3]), as.integer(m[4]))))
      if (is.na(d)) return(NULL)
      tibble(
        source_id   = "city_funabashi",
        source_name = "船橋市",
        category    = "行政",
        lang        = "ja",
        title       = title,
        link        = if (grepl("^https?://", href)) href else paste0("https://www.city.funabashi.lg.jp", href),
        pub_date    = d,
        summary     = NA_character_
      )
    })
    bind_rows(Filter(Negate(is.null), rows)) %>% distinct(link, .keep_all = TRUE) %>% head(n_results)
  }, error = function(e) { message("船橋市 エラー: ", e$message); NULL })
}

# ============================================================
# 荒川区・渋谷区・品川区・藤沢市 新着情報一覧（HTMLスクレイピング）
# RSS未提供のため、ブラウザのネットワーク検証で確認したテーブル/リスト構造を
# XPathで直接解析する（他の保健所設置自治体と同様のパターン）
# ============================================================
fetch_arakawa_news <- function(timeout_sec = 15, n_results = 20) {
  message("荒川区 新着情報 取得中...")
  tryCatch({
    resp <- GET("https://www.city.arakawa.tokyo.jp/kouhou/news/index.html", timeout(timeout_sec),
                add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
    if (status_code(resp) != 200) return(NULL)
    doc <- read_html(content(resp, "text", encoding = "UTF-8"))
    rows_tr <- xml_find_all(doc, "//table[contains(@class,'list_table')]//tr")
    if (length(rows_tr) == 0) return(NULL)

    rows <- lapply(rows_tr, function(tr) {
      date_txt <- trimws(xml_text(xml_find_first(tr, ".//td[contains(@class,'date')]")))
      a <- xml_find_first(tr, ".//td[not(contains(@class,'date'))]//a")
      if (is.na(a)) return(NULL)
      title <- trimws(xml_text(a))
      href  <- xml_attr(a, "href")
      dm <- regmatches(date_txt, regexec("(\\d{4})年(\\d{1,2})月(\\d{1,2})日", date_txt))[[1]]
      if (length(dm) < 4 || nchar(title) == 0 || is.na(href)) return(NULL)
      d <- suppressWarnings(as.Date(sprintf("%s-%02d-%02d", dm[2], as.integer(dm[3]), as.integer(dm[4]))))
      if (is.na(d)) return(NULL)
      tibble(
        source_id   = "city_arakawa",
        source_name = "荒川区",
        category    = "行政",
        lang        = "ja",
        title       = title,
        link        = if (grepl("^https?://", href)) href else paste0("https://www.city.arakawa.tokyo.jp", href),
        pub_date    = d,
        summary     = NA_character_
      )
    })
    bind_rows(Filter(Negate(is.null), rows)) %>% distinct(link, .keep_all = TRUE) %>% head(n_results)
  }, error = function(e) { message("荒川区 エラー: ", e$message); NULL })
}

fetch_shibuya_news <- function(timeout_sec = 15, n_results = 20) {
  message("渋谷区 新着情報 取得中...")
  tryCatch({
    resp <- GET("https://www.city.shibuya.tokyo.jp/contents/news/", timeout(timeout_sec),
                add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
    if (status_code(resp) != 200) return(NULL)
    doc <- read_html(content(resp, "text", encoding = "UTF-8"))
    items <- xml_find_all(doc, "//ul[contains(@class,'m-list-links')]//li/a")
    if (length(items) == 0) return(NULL)

    rows <- lapply(items, function(a) {
      title <- trimws(xml_text(xml_find_first(a, ".//p[contains(@class,'title')]")))
      date_txt <- trimws(xml_text(xml_find_first(a, ".//p[contains(@class,'date')]")))
      href <- xml_attr(a, "href")
      dm <- regmatches(date_txt, regexec("(\\d{4})年(\\d{1,2})月(\\d{1,2})日", date_txt))[[1]]
      if (length(dm) < 4 || nchar(title) == 0 || is.na(href)) return(NULL)
      d <- suppressWarnings(as.Date(sprintf("%s-%02d-%02d", dm[2], as.integer(dm[3]), as.integer(dm[4]))))
      if (is.na(d)) return(NULL)
      tibble(
        source_id   = "city_shibuya",
        source_name = "渋谷区",
        category    = "行政",
        lang        = "ja",
        title       = title,
        link        = if (grepl("^https?://", href)) href else paste0("https://www.city.shibuya.tokyo.jp", href),
        pub_date    = d,
        summary     = NA_character_
      )
    })
    bind_rows(Filter(Negate(is.null), rows)) %>% distinct(link, .keep_all = TRUE) %>% head(n_results)
  }, error = function(e) { message("渋谷区 エラー: ", e$message); NULL })
}

fetch_shinagawa_news <- function(timeout_sec = 15, n_results = 20) {
  message("品川区 新着情報 取得中...")
  tryCatch({
    resp <- GET("https://www.city.shinagawa.tokyo.jp/PC/re_direct/hpg000016838.html", timeout(timeout_sec),
                add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
    if (status_code(resp) != 200) return(NULL)
    doc <- read_html(content(resp, "text", encoding = "UTF-8"))
    items <- xml_find_all(doc, "//ul[contains(@class,'link')]/li")
    if (length(items) == 0) return(NULL)

    rows <- lapply(items, function(li) {
      date_txt <- trimws(xml_text(xml_find_first(li, ".//p[contains(@class,'news-list-date')]")))
      a <- xml_find_first(li, ".//p[contains(@class,'news-list-txt')]//a")
      if (is.na(a)) return(NULL)
      title <- trimws(xml_text(a))
      href  <- xml_attr(a, "href")
      dm <- regmatches(date_txt, regexec("(\\d{4})年(\\d{1,2})月(\\d{1,2})日", date_txt))[[1]]
      if (length(dm) < 4 || nchar(title) == 0 || is.na(href)) return(NULL)
      d <- suppressWarnings(as.Date(sprintf("%s-%02d-%02d", dm[2], as.integer(dm[3]), as.integer(dm[4]))))
      if (is.na(d)) return(NULL)
      tibble(
        source_id   = "city_shinagawa",
        source_name = "品川区",
        category    = "行政",
        lang        = "ja",
        title       = title,
        link        = if (grepl("^https?://", href)) href else paste0("https://www.city.shinagawa.tokyo.jp", href),
        pub_date    = d,
        summary     = NA_character_
      )
    })
    bind_rows(Filter(Negate(is.null), rows)) %>% distinct(link, .keep_all = TRUE) %>% head(n_results)
  }, error = function(e) { message("品川区 エラー: ", e$message); NULL })
}

fetch_fujisawa_news <- function(timeout_sec = 15, n_results = 20) {
  message("藤沢市 新着情報 取得中...")
  tryCatch({
    resp <- GET("https://www.city.fujisawa.kanagawa.jp/shinchaku/index.html", timeout(timeout_sec),
                add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
    if (status_code(resp) != 200) return(NULL)
    doc <- read_html(content(resp, "text", encoding = "UTF-8"))
    rows_tr <- xml_find_all(doc, "//table[contains(@class,'news_tbl')]//tr")
    if (length(rows_tr) == 0) return(NULL)

    today <- Sys.Date()
    cur_year <- as.integer(format(today, "%Y"))
    rows <- lapply(rows_tr, function(tr) {
      date_txt <- trimws(xml_text(xml_find_first(tr, ".//td[contains(@class,'news_date')]")))
      a <- xml_find_first(tr, ".//td[contains(@class,'news_link')]//a")
      if (is.na(a)) return(NULL)
      title <- trimws(xml_text(a))
      href  <- xml_attr(a, "href")
      # 年の記載がない「M月D日」形式のため、当日を基準に年を推定する
      # （未来3日以上先の日付になる場合は前年の記事とみなす）
      dm <- regmatches(date_txt, regexec("^(\\d{1,2})月(\\d{1,2})日", date_txt))[[1]]
      if (length(dm) < 3 || nchar(title) == 0 || is.na(href)) return(NULL)
      mo <- as.integer(dm[2]); da <- as.integer(dm[3])
      d <- suppressWarnings(as.Date(sprintf("%d-%02d-%02d", cur_year, mo, da)))
      if (is.na(d)) return(NULL)
      if (d > today + 3) d <- suppressWarnings(as.Date(sprintf("%d-%02d-%02d", cur_year - 1, mo, da)))
      if (is.na(d)) return(NULL)
      tibble(
        source_id   = "city_fujisawa",
        source_name = "藤沢市",
        category    = "行政",
        lang        = "ja",
        title       = title,
        link        = if (grepl("^https?://", href)) href else paste0("https://www.city.fujisawa.kanagawa.jp", href),
        pub_date    = d,
        summary     = NA_character_
      )
    })
    bind_rows(Filter(Negate(is.null), rows)) %>% distinct(link, .keep_all = TRUE) %>% head(n_results)
  }, error = function(e) { message("藤沢市 エラー: ", e$message); NULL })
}

# ============================================================
# PubMed E-utilities — アウトブレイク関連論文取得（APIキー不要）
# ============================================================
PUBMED_QUERIES <- c(
  "outbreak[Title] Japan",
  "epidemic[Title] Japan",
  "infectious disease surveillance Japan",
  "influenza epidemic Japan",
  "emerging infectious disease Japan"
)

fetch_pubmed <- function(queries = PUBMED_QUERIES, max_results = 5, days_back = 365) {
  message("PubMed 取得中...")
  results <- lapply(queries, function(q) {
    tryCatch({
      # 検索
      search_url <- paste0(
        "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi",
        "?db=pubmed&term=", URLencode(q, reserved = TRUE),
        "&retmax=", max_results,
        "&sort=pub+date&format=json",
        "&datetype=pdat&reldate=", days_back
      )
      r1 <- GET(search_url, timeout(8))
      if (status_code(r1) != 200) return(NULL)
      ids <- fromJSON(content(r1, "text", encoding = "UTF-8"))$esearchresult$idlist
      if (length(ids) == 0) return(NULL)

      # サマリー取得
      sum_url <- paste0(
        "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi",
        "?db=pubmed&id=", paste(ids, collapse = ","),
        "&format=json"
      )
      r2 <- GET(sum_url, timeout(8))
      if (status_code(r2) != 200) return(NULL)
      sums <- fromJSON(content(r2, "text", encoding = "UTF-8"))$result
      uid  <- sums$uids
      if (is.null(uid) || length(uid) == 0) return(NULL)

      rows <- lapply(uid, function(id) {
        art <- sums[[id]]
        pd_str <- tryCatch(as.character(art$pubdate), error = function(e) "")
        pub_date <- tryCatch({
          d <- as.Date(NA)
          for (fmt in c("%Y %b %d", "%Y %b", "%Y")) {
            ds <- trimws(pd_str)
            # "%Y %b" 形式は日なしなので補完
            if (fmt == "%Y %b") ds <- paste0(ds, " 01")
            d <- suppressWarnings(as.Date(ds, format = fmt))
            if (!is.na(d)) break
          }
          d
        }, error = function(e) as.Date(NA))

        authors <- tryCatch(art$authors$name[1], error = function(e) "")
        journal <- tryCatch(as.character(art$source), error = function(e) "")

        tibble(
          source_id   = "pubmed",
          source_name = "PubMed",
          category    = "研究機関",
          lang        = "en",
          title       = as.character(art$title),
          link        = paste0("https://pubmed.ncbi.nlm.nih.gov/", id, "/"),
          pub_date    = pub_date,
          summary     = paste0("[", journal, "] ", coalesce(authors, ""))
        )
      })
      bind_rows(rows)
    }, error = function(e) { message("PubMed エラー(", q, "): ", e$message); NULL })
  })
  bind_rows(Filter(Negate(is.null), results))
}

# ============================================================
# X (Twitter) API v2 — 最新ツイート検索
# ============================================================
TWITTER_DISEASE_QUERIES <- list(
  flu     = "(インフルエンザ OR 流行) 感染 -is:retweet lang:ja",
  covid   = "(新型コロナ OR COVID-19) 感染 -is:retweet lang:ja",
  rsv     = "RSウイルス 感染 -is:retweet lang:ja",
  ari     = "(急性呼吸器 OR ARI) 感染 -is:retweet lang:ja",
  general = "(感染症 OR アウトブレイク OR 流行) -is:retweet lang:ja"
)

fetch_twitter <- function(bearer_token, disease_ids = names(TWITTER_DISEASE_QUERIES),
                          max_results = 20) {
  if (is.null(bearer_token) || nchar(trimws(bearer_token)) < 10) return(NULL)

  all_tweets <- lapply(disease_ids, function(disease) {
    query <- TWITTER_DISEASE_QUERIES[[disease]]
    tryCatch({
      resp <- GET(
        "https://api.twitter.com/2/tweets/search/recent",
        add_headers(Authorization = paste("Bearer", trimws(bearer_token))),
        query = list(
          query       = query,
          max_results = max_results,
          "tweet.fields" = "created_at,public_metrics,author_id",
          expansions  = "author_id",
          "user.fields" = "name,username"
        ),
        timeout(15)
      )
      if (status_code(resp) == 429) {
        message("X API rate limit exceeded"); return(NULL)
      }
      if (status_code(resp) != 200) {
        message("X API error: ", status_code(resp)); return(NULL)
      }
      body <- fromJSON(content(resp, "text", encoding="UTF-8"), flatten=TRUE)
      if (is.null(body$data) || nrow(body$data) == 0) return(NULL)

      # ユーザー名マップ
      user_map <- setNames(
        body$includes$users$name,
        body$includes$users$id
      )

      body$data %>%
        transmute(
          source_id   = "twitter",
          source_name = "X (Twitter)",
          category    = "SNS",
          lang        = "ja",
          title       = paste0(
            "@", coalesce(user_map[author_id], "unknown"), ": ",
            substr(text, 1, 100)
          ),
          link = paste0("https://twitter.com/i/web/status/", id),
          pub_date    = as.Date(created_at),
          summary     = text,
          disease_hint = disease,
          retweet_count = coalesce(public_metrics.retweet_count, 0L),
          like_count    = coalesce(public_metrics.like_count, 0L)
        )
    }, error = function(e) { message("X API エラー: ", e$message); NULL })
  })

  df <- bind_rows(Filter(Negate(is.null), all_tweets))
  if (nrow(df) == 0) NULL else df
}

# ============================================================
# シグナル分類・疾患タグ
# ============================================================
# 消化器系疾患タグ（食中毒・感染性胃腸炎系）
GI_DISEASE_TAGS <- c("gi", "gi_rota", "ehec", "general")

# テキストから症例数・患者数を抽出する
#
# マッチ位置の直前に「全国」「昨年」等の比較対象を示す語がある数値は除外する。
# 記事は「全国では1000人の患者が報告されていますが、県内では3人です」のように
# 全国合計・前年実績等の比較数値を併記することが多く、単純にmax()を取ると
# 今回・当地の実際の症例数（3人）ではなく無関係な比較数値（1000人）を
# 拾ってしまい、is_gi判定（case_n>=100→「高」）を大きく誤らせる
# （2026-08-18 ユーザー指摘の精査で発覚。実例で「全国では1000人...県内では3人」
# から1000が抽出されることを確認）。
EXTRACT_CASE_COUNT_EXCLUDE_WORDS <- c("全国では", "全国で", "全国の", "全国合計",
                                       "昨年は", "昨年", "前年", "去年", "前回", "前週")
EXTRACT_CASE_COUNT_EXCLUDE_WINDOW <- 10L

extract_case_count <- function(text) {
  # 日本語: 「XX人」「XX名」「XX例」「XX件」「XX患者」
  # 英語: "XX cases" "XX patients" "XX people"
  # 数値部分は「\d[\d,]*」（1桁も許容）。従来「\d[\d,]+」（2文字目以降を必須）
  # だったため、「3人の患者」のような1桁の症例数がまったく抽出できていなかった
  # （2026-08-18 ユーザー指摘の精査で発覚）。
  patterns <- c(
    "(\\d[\\d,]*)\\s*人(?:が|の|以上|超)",
    "(\\d[\\d,]*)\\s*名(?:が|の|以上|超)",
    "(\\d[\\d,]*)\\s*例(?:が|の|以上|超|報告)",
    "(\\d[\\d,]*)\\s*件(?:が|の|以上|超|報告)",
    "(\\d[\\d,]*)\\s*人?の患者",
    "(\\d[\\d,]*)\\s*cases?",
    "(\\d[\\d,]*)\\s*patients?",
    "(\\d[\\d,]*)\\s*people\\s*(infected|affected|sick)"
  )
  nums <- c()
  for (pat in patterns) {
    loc <- gregexpr(pat, text, perl=TRUE, ignore.case=TRUE)[[1]]
    if (loc[1] == -1L) next
    lens <- attr(loc, "match.length")
    for (i in seq_along(loc)) {
      matched_str <- substr(text, loc[i], loc[i] + lens[i] - 1L)
      num_str <- regmatches(matched_str, regexpr("\\d[\\d,]*", matched_str, perl=TRUE))
      num <- suppressWarnings(as.numeric(gsub(",", "", num_str)))
      if (is.na(num)) next
      ctx <- substr(text, max(1L, loc[i] - EXTRACT_CASE_COUNT_EXCLUDE_WINDOW), loc[i] - 1L)
      if (any(vapply(EXTRACT_CASE_COUNT_EXCLUDE_WORDS, function(w) grepl(w, ctx, fixed=TRUE), logical(1)))) next
      nums <- c(nums, num)
    }
  }
  if (length(nums) == 0) return(NA_real_)
  max(nums)  # 除外後の記事中の最大値を採用
}

# ============================================================
# 都道府県判定
# ============================================================

# 地方紙・地方メディア名 → 都道府県マッピング
MEDIA_PREF_MAP <- c(
  # 北海道
  "北海道新聞"="北海道", "道新"="北海道", "hokkaido"="北海道",
  # 東北
  "河北新報"="宮城県", "岩手日報"="岩手県", "デーリー東北"="青森県",
  "東奥日報"="青森県", "秋田魁新報"="秋田県", "山形新聞"="山形県",
  "福島民報"="福島県", "福島民友"="福島県",
  # 関東
  "茨城新聞"="茨城県", "下野新聞"="栃木県", "上毛新聞"="群馬県",
  "埼玉新聞"="埼玉県", "千葉日報"="千葉県",
  "神奈川新聞"="神奈川県", "カナロコ"="神奈川県",
  # 甲信越・北陸
  "山梨日日新聞"="山梨県", "信濃毎日新聞"="長野県", "信毎"="長野県",
  "新潟日報"="新潟県", "北日本新聞"="富山県", "北陸中日新聞"="石川県",
  "北国新聞"="石川県", "福井新聞"="福井県",
  # 東海
  "静岡新聞"="静岡県", "中日新聞"="愛知県", "岐阜新聞"="岐阜県",
  "伊勢新聞"="三重県",
  # 近畿
  "京都新聞"="京都府", "神戸新聞"="兵庫県", "奈良新聞"="奈良県",
  "紀伊民報"="和歌山県", "滋賀報知新聞"="滋賀県",
  # 中国・四国
  "中国新聞"="広島県", "山陰中央新報"="島根県", "日本海新聞"="鳥取県",
  "山陽新聞"="岡山県", "宇部日報"="山口県", "防長新聞"="山口県",
  "徳島新聞"="徳島県", "四国新聞"="香川県", "愛媛新聞"="愛媛県",
  "高知新聞"="高知県",
  # 九州・沖縄
  "西日本新聞"="福岡県", "佐賀新聞"="佐賀県", "長崎新聞"="長崎県",
  "熊本日日新聞"="熊本県", "熊日"="熊本県", "大分合同新聞"="大分県",
  "宮崎日日新聞"="宮崎県", "南日本新聞"="鹿児島県", "琉球新報"="沖縄県",
  "沖縄タイムス"="沖縄県",
  # 行政・医療機関
  "pref\\.hokkaido"="北海道", "pref\\.miyagi"="宮城県",
  "pref\\.tokyo"="東京都", "metro\\.tokyo"="東京都",
  "pref\\.osaka"="大阪府", "pref\\.aichi"="愛知県",
  "pref\\.fukuoka"="福岡県", "pref\\.kanagawa"="神奈川県",
  "pref\\.saitama"="埼玉県", "pref\\.chiba"="千葉県",
  "pref\\.hyogo"="兵庫県", "pref\\.hiroshima"="広島県",
  "pref\\.kyoto"="京都府", "pref\\.shizuoka"="静岡県",
  "pref\\.niigata"="新潟県", "pref\\.ibaraki"="茨城県",
  "pref\\.nagano"="長野県", "pref\\.gifu"="岐阜県",
  "pref\\.tochigi"="栃木県", "pref\\.gunma"="群馬県",
  "pref\\.okayama"="岡山県", "pref\\.fukushima"="福島県",
  "pref\\.mie"="三重県", "pref\\.kumamoto"="熊本県",
  "pref\\.kagoshima"="鹿児島県", "pref\\.okinawa"="沖縄県",
  "pref\\.nara"="奈良県", "pref\\.nagasaki"="長崎県",
  "pref\\.oita"="大分県", "pref\\.yamaguchi"="山口県",
  "pref\\.ehime"="愛媛県", "pref\\.kochi"="高知県",
  "pref\\.kagawa"="香川県", "pref\\.tokushima"="徳島県",
  "pref\\.saga"="佐賀県", "pref\\.miyazaki"="宮崎県",
  "pref\\.wakayama"="和歌山県", "pref\\.yamanashi"="山梨県",
  "pref\\.shiga"="滋賀県", "pref\\.fukui"="福井県",
  "pref\\.ishikawa"="石川県", "pref\\.toyama"="富山県",
  "pref\\.akita"="秋田県", "pref\\.yamagata"="山形県",
  "pref\\.iwate"="岩手県", "pref\\.aomori"="青森県",
  "pref\\.tottori"="鳥取県", "pref\\.shimane"="島根県",
  "pref\\.iwate"="岩手県", "pref\\.iwate"="岩手県"
)

# All municipalities -> prefecture mapping (from Ministry of Internal Affairs)
# Sorted by name length descending (longer names matched first to reduce false positives)
CITY_PREF_MAP <- c(
  "四日市市"="三重県",
  "いなべ市"="三重県",
  "木曽岬町"="三重県",
  "南伊勢町"="三重県",
  "伊勢市"="三重県",
  "松阪市"="三重県",
  "桑名市"="三重県",
  "鈴鹿市"="三重県",
  "名張市"="三重県",
  "尾鷲市"="三重県",
  "亀山市"="三重県",
  "鳥羽市"="三重県",
  "熊野市"="三重県",
  "志摩市"="三重県",
  "伊賀市"="三重県",
  "東員町"="三重県",
  "菰野町"="三重県",
  "朝日町"="三重県",
  "川越町"="三重県",
  "多気町"="三重県",
  "明和町"="三重県",
  "大台町"="三重県",
  "玉城町"="三重県",
  "度会町"="三重県",
  "大紀町"="三重県",
  "紀北町"="三重県",
  "御浜町"="三重県",
  "紀宝町"="三重県",
  "津市"="三重県",
  "宇治田原町"="京都府",
  "福知山市"="京都府",
  "長岡京市"="京都府",
  "京田辺市"="京都府",
  "京丹後市"="京都府",
  "木津川市"="京都府",
  "大山崎町"="京都府",
  "久御山町"="京都府",
  "南山城村"="京都府",
  "京丹波町"="京都府",
  "与謝野町"="京都府",
  "京都市"="京都府",
  "舞鶴市"="京都府",
  "綾部市"="京都府",
  "宇治市"="京都府",
  "宮津市"="京都府",
  "亀岡市"="京都府",
  "城陽市"="京都府",
  "向日市"="京都府",
  "八幡市"="京都府",
  "南丹市"="京都府",
  "井手町"="京都府",
  "笠置町"="京都府",
  "和束町"="京都府",
  "精華町"="京都府",
  "伊根町"="京都府",
  "吉野ヶ里町"="佐賀県",
  "伊万里市"="佐賀県",
  "みやき町"="佐賀県",
  "佐賀市"="佐賀県",
  "唐津市"="佐賀県",
  "鳥栖市"="佐賀県",
  "多久市"="佐賀県",
  "武雄市"="佐賀県",
  "鹿島市"="佐賀県",
  "小城市"="佐賀県",
  "嬉野市"="佐賀県",
  "神埼市"="佐賀県",
  "基山町"="佐賀県",
  "上峰町"="佐賀県",
  "玄海町"="佐賀県",
  "有田町"="佐賀県",
  "大町町"="佐賀県",
  "江北町"="佐賀県",
  "白石町"="佐賀県",
  "太良町"="佐賀県",
  "丹波篠山市"="兵庫県",
  "南あわじ市"="兵庫県",
  "加古川市"="兵庫県",
  "たつの市"="兵庫県",
  "猪名川町"="兵庫県",
  "新温泉町"="兵庫県",
  "神戸市"="兵庫県",
  "姫路市"="兵庫県",
  "尼崎市"="兵庫県",
  "明石市"="兵庫県",
  "西宮市"="兵庫県",
  "洲本市"="兵庫県",
  "芦屋市"="兵庫県",
  "伊丹市"="兵庫県",
  "相生市"="兵庫県",
  "豊岡市"="兵庫県",
  "赤穂市"="兵庫県",
  "西脇市"="兵庫県",
  "宝塚市"="兵庫県",
  "三木市"="兵庫県",
  "高砂市"="兵庫県",
  "川西市"="兵庫県",
  "小野市"="兵庫県",
  "三田市"="兵庫県",
  "加西市"="兵庫県",
  "養父市"="兵庫県",
  "丹波市"="兵庫県",
  "朝来市"="兵庫県",
  "淡路市"="兵庫県",
  "宍粟市"="兵庫県",
  "加東市"="兵庫県",
  "多可町"="兵庫県",
  "稲美町"="兵庫県",
  "播磨町"="兵庫県",
  "市川町"="兵庫県",
  "福崎町"="兵庫県",
  "神河町"="兵庫県",
  "太子町"="兵庫県",
  "上郡町"="兵庫県",
  "佐用町"="兵庫県",
  "香美町"="兵庫県",
  "新十津川町"="北海道",
  "上富良野町"="北海道",
  "中富良野町"="北海道",
  "南富良野町"="北海道",
  "音威子府村"="北海道",
  "利尻富士町"="北海道",
  "新ひだか町"="北海道",
  "岩見沢市"="北海道",
  "苫小牧市"="北海道",
  "歌志内市"="北海道",
  "富良野市"="北海道",
  "北広島市"="北海道",
  "新篠津村"="北海道",
  "木古内町"="北海道",
  "長万部町"="北海道",
  "上ノ国町"="北海道",
  "厚沢部町"="北海道",
  "せたな町"="北海道",
  "黒松内町"="北海道",
  "ニセコ町"="北海道",
  "留寿都村"="北海道",
  "喜茂別町"="北海道",
  "倶知安町"="北海道",
  "神恵内村"="北海道",
  "赤井川村"="北海道",
  "奈井江町"="北海道",
  "上砂川町"="北海道",
  "妹背牛町"="北海道",
  "秩父別町"="北海道",
  "東神楽町"="北海道",
  "幌加内町"="北海道",
  "初山別村"="北海道",
  "浜頓別町"="北海道",
  "中頓別町"="北海道",
  "小清水町"="北海道",
  "訓子府町"="北海道",
  "佐呂間町"="北海道",
  "西興部村"="北海道",
  "洞爺湖町"="北海道",
  "むかわ町"="北海道",
  "えりも町"="北海道",
  "上士幌町"="北海道",
  "中札内村"="北海道",
  "弟子屈町"="北海道",
  "中標津町"="北海道",
  "留夜別村"="北海道",
  "札幌市"="北海道",
  "函館市"="北海道",
  "小樽市"="北海道",
  "旭川市"="北海道",
  "室蘭市"="北海道",
  "釧路市"="北海道",
  "帯広市"="北海道",
  "北見市"="北海道",
  "夕張市"="北海道",
  "網走市"="北海道",
  "留萌市"="北海道",
  "稚内市"="北海道",
  "美唄市"="北海道",
  "芦別市"="北海道",
  "江別市"="北海道",
  "赤平市"="北海道",
  "紋別市"="北海道",
  "士別市"="北海道",
  "名寄市"="北海道",
  "三笠市"="北海道",
  "根室市"="北海道",
  "千歳市"="北海道",
  "滝川市"="北海道",
  "砂川市"="北海道",
  "深川市"="北海道",
  "登別市"="北海道",
  "恵庭市"="北海道",
  "伊達市"="北海道",
  "石狩市"="北海道",
  "北斗市"="北海道",
  "当別町"="北海道",
  "松前町"="北海道",
  "福島町"="北海道",
  "知内町"="北海道",
  "七飯町"="北海道",
  "鹿部町"="北海道",
  "八雲町"="北海道",
  "江差町"="北海道",
  "乙部町"="北海道",
  "奥尻町"="北海道",
  "今金町"="北海道",
  "島牧村"="北海道",
  "寿都町"="北海道",
  "蘭越町"="北海道",
  "真狩村"="北海道",
  "京極町"="北海道",
  "共和町"="北海道",
  "岩内町"="北海道",
  "積丹町"="北海道",
  "古平町"="北海道",
  "仁木町"="北海道",
  "余市町"="北海道",
  "南幌町"="北海道",
  "由仁町"="北海道",
  "長沼町"="北海道",
  "栗山町"="北海道",
  "月形町"="北海道",
  "浦臼町"="北海道",
  "雨竜町"="北海道",
  "北竜町"="北海道",
  "沼田町"="北海道",
  "鷹栖町"="北海道",
  "当麻町"="北海道",
  "比布町"="北海道",
  "愛別町"="北海道",
  "上川町"="北海道",
  "東川町"="北海道",
  "美瑛町"="北海道",
  "占冠村"="北海道",
  "和寒町"="北海道",
  "剣淵町"="北海道",
  "下川町"="北海道",
  "美深町"="北海道",
  "中川町"="北海道",
  "増毛町"="北海道",
  "小平町"="北海道",
  "苫前町"="北海道",
  "羽幌町"="北海道",
  "遠別町"="北海道",
  "天塩町"="北海道",
  "猿払村"="北海道",
  "枝幸町"="北海道",
  "豊富町"="北海道",
  "礼文町"="北海道",
  "利尻町"="北海道",
  "幌延町"="北海道",
  "美幌町"="北海道",
  "津別町"="北海道",
  "斜里町"="北海道",
  "清里町"="北海道",
  "置戸町"="北海道",
  "遠軽町"="北海道",
  "湧別町"="北海道",
  "滝上町"="北海道",
  "興部町"="北海道",
  "雄武町"="北海道",
  "大空町"="北海道",
  "豊浦町"="北海道",
  "壮瞥町"="北海道",
  "白老町"="北海道",
  "厚真町"="北海道",
  "安平町"="北海道",
  "日高町"="北海道",
  "平取町"="北海道",
  "新冠町"="北海道",
  "浦河町"="北海道",
  "様似町"="北海道",
  "音更町"="北海道",
  "士幌町"="北海道",
  "鹿追町"="北海道",
  "新得町"="北海道",
  "清水町"="北海道",
  "芽室町"="北海道",
  "更別村"="北海道",
  "大樹町"="北海道",
  "広尾町"="北海道",
  "幕別町"="北海道",
  "池田町"="北海道",
  "豊頃町"="北海道",
  "本別町"="北海道",
  "足寄町"="北海道",
  "陸別町"="北海道",
  "浦幌町"="北海道",
  "釧路町"="北海道",
  "厚岸町"="北海道",
  "浜中町"="北海道",
  "標茶町"="北海道",
  "鶴居村"="北海道",
  "白糠町"="北海道",
  "別海町"="北海道",
  "標津町"="北海道",
  "羅臼町"="北海道",
  "色丹村"="北海道",
  "留別村"="北海道",
  "紗那村"="北海道",
  "蘂取村"="北海道",
  "森町"="北海道",
  "泊村"="北海道",
  "大網白里市"="千葉県",
  "九十九里町"="千葉県",
  "木更津市"="千葉県",
  "習志野市"="千葉県",
  "八千代市"="千葉県",
  "我孫子市"="千葉県",
  "鎌ケ谷市"="千葉県",
  "四街道市"="千葉県",
  "袖ケ浦市"="千葉県",
  "南房総市"="千葉県",
  "いすみ市"="千葉県",
  "酒々井町"="千葉県",
  "横芝光町"="千葉県",
  "大多喜町"="千葉県",
  "千葉市"="千葉県",
  "銚子市"="千葉県",
  "市川市"="千葉県",
  "船橋市"="千葉県",
  "館山市"="千葉県",
  "松戸市"="千葉県",
  "野田市"="千葉県",
  "茂原市"="千葉県",
  "成田市"="千葉県",
  "佐倉市"="千葉県",
  "東金市"="千葉県",
  "勝浦市"="千葉県",
  "市原市"="千葉県",
  "流山市"="千葉県",
  "鴨川市"="千葉県",
  "君津市"="千葉県",
  "富津市"="千葉県",
  "浦安市"="千葉県",
  "八街市"="千葉県",
  "印西市"="千葉県",
  "白井市"="千葉県",
  "富里市"="千葉県",
  "匝瑳市"="千葉県",
  "香取市"="千葉県",
  "山武市"="千葉県",
  "神崎町"="千葉県",
  "多古町"="千葉県",
  "東庄町"="千葉県",
  "芝山町"="千葉県",
  "一宮町"="千葉県",
  "睦沢町"="千葉県",
  "長生村"="千葉県",
  "白子町"="千葉県",
  "長柄町"="千葉県",
  "長南町"="千葉県",
  "御宿町"="千葉県",
  "鋸南町"="千葉県",
  "旭市"="千葉県",
  "柏市"="千葉県",
  "栄町"="千葉県",
  "かつらぎ町"="和歌山県",
  "那智勝浦町"="和歌山県",
  "和歌山市"="和歌山県",
  "紀の川市"="和歌山県",
  "紀美野町"="和歌山県",
  "九度山町"="和歌山県",
  "有田川町"="和歌山県",
  "みなべ町"="和歌山県",
  "日高川町"="和歌山県",
  "上富田町"="和歌山県",
  "すさみ町"="和歌山県",
  "古座川町"="和歌山県",
  "海南市"="和歌山県",
  "橋本市"="和歌山県",
  "有田市"="和歌山県",
  "御坊市"="和歌山県",
  "田辺市"="和歌山県",
  "新宮市"="和歌山県",
  "岩出市"="和歌山県",
  "高野町"="和歌山県",
  "湯浅町"="和歌山県",
  "広川町"="和歌山県",
  "美浜町"="和歌山県",
  "日高町"="和歌山県",
  "由良町"="和歌山県",
  "印南町"="和歌山県",
  "白浜町"="和歌山県",
  "太地町"="和歌山県",
  "北山村"="和歌山県",
  "串本町"="和歌山県",
  "さいたま市"="埼玉県",
  "ふじみ野市"="埼玉県",
  "ときがわ町"="埼玉県",
  "東松山市"="埼玉県",
  "春日部市"="埼玉県",
  "富士見市"="埼玉県",
  "鶴ヶ島市"="埼玉県",
  "毛呂山町"="埼玉県",
  "小鹿野町"="埼玉県",
  "東秩父村"="埼玉県",
  "川越市"="埼玉県",
  "熊谷市"="埼玉県",
  "川口市"="埼玉県",
  "行田市"="埼玉県",
  "秩父市"="埼玉県",
  "所沢市"="埼玉県",
  "飯能市"="埼玉県",
  "加須市"="埼玉県",
  "本庄市"="埼玉県",
  "狭山市"="埼玉県",
  "羽生市"="埼玉県",
  "鴻巣市"="埼玉県",
  "深谷市"="埼玉県",
  "上尾市"="埼玉県",
  "草加市"="埼玉県",
  "越谷市"="埼玉県",
  "戸田市"="埼玉県",
  "入間市"="埼玉県",
  "朝霞市"="埼玉県",
  "志木市"="埼玉県",
  "和光市"="埼玉県",
  "新座市"="埼玉県",
  "桶川市"="埼玉県",
  "久喜市"="埼玉県",
  "北本市"="埼玉県",
  "八潮市"="埼玉県",
  "三郷市"="埼玉県",
  "蓮田市"="埼玉県",
  "坂戸市"="埼玉県",
  "幸手市"="埼玉県",
  "日高市"="埼玉県",
  "吉川市"="埼玉県",
  "白岡市"="埼玉県",
  "伊奈町"="埼玉県",
  "三芳町"="埼玉県",
  "越生町"="埼玉県",
  "滑川町"="埼玉県",
  "嵐山町"="埼玉県",
  "小川町"="埼玉県",
  "川島町"="埼玉県",
  "吉見町"="埼玉県",
  "鳩山町"="埼玉県",
  "横瀬町"="埼玉県",
  "皆野町"="埼玉県",
  "長瀞町"="埼玉県",
  "美里町"="埼玉県",
  "神川町"="埼玉県",
  "上里町"="埼玉県",
  "寄居町"="埼玉県",
  "宮代町"="埼玉県",
  "杉戸町"="埼玉県",
  "松伏町"="埼玉県",
  "蕨市"="埼玉県",
  "豊後高田市"="大分県",
  "豊後大野市"="大分県",
  "津久見市"="大分県",
  "大分市"="大分県",
  "別府市"="大分県",
  "中津市"="大分県",
  "日田市"="大分県",
  "佐伯市"="大分県",
  "臼杵市"="大分県",
  "竹田市"="大分県",
  "杵築市"="大分県",
  "宇佐市"="大分県",
  "由布市"="大分県",
  "国東市"="大分県",
  "姫島村"="大分県",
  "日出町"="大分県",
  "九重町"="大分県",
  "玖珠町"="大分県",
  "河内長野市"="大阪府",
  "大阪狭山市"="大阪府",
  "千早赤阪村"="大阪府",
  "岸和田市"="大阪府",
  "泉大津市"="大阪府",
  "泉佐野市"="大阪府",
  "富田林市"="大阪府",
  "寝屋川市"="大阪府",
  "羽曳野市"="大阪府",
  "藤井寺市"="大阪府",
  "東大阪市"="大阪府",
  "四條畷市"="大阪府",
  "大阪市"="大阪府",
  "豊中市"="大阪府",
  "池田市"="大阪府",
  "吹田市"="大阪府",
  "高槻市"="大阪府",
  "貝塚市"="大阪府",
  "守口市"="大阪府",
  "枚方市"="大阪府",
  "茨木市"="大阪府",
  "八尾市"="大阪府",
  "松原市"="大阪府",
  "大東市"="大阪府",
  "和泉市"="大阪府",
  "箕面市"="大阪府",
  "柏原市"="大阪府",
  "門真市"="大阪府",
  "摂津市"="大阪府",
  "高石市"="大阪府",
  "泉南市"="大阪府",
  "交野市"="大阪府",
  "阪南市"="大阪府",
  "島本町"="大阪府",
  "豊能町"="大阪府",
  "能勢町"="大阪府",
  "忠岡町"="大阪府",
  "熊取町"="大阪府",
  "田尻町"="大阪府",
  "太子町"="大阪府",
  "河南町"="大阪府",
  "堺市"="大阪府",
  "岬町"="大阪府",
  "大和高田市"="奈良県",
  "大和郡山市"="奈良県",
  "田原本町"="奈良県",
  "明日香村"="奈良県",
  "野迫川村"="奈良県",
  "十津川村"="奈良県",
  "下北山村"="奈良県",
  "上北山村"="奈良県",
  "東吉野村"="奈良県",
  "奈良市"="奈良県",
  "天理市"="奈良県",
  "橿原市"="奈良県",
  "桜井市"="奈良県",
  "五條市"="奈良県",
  "御所市"="奈良県",
  "生駒市"="奈良県",
  "香芝市"="奈良県",
  "葛城市"="奈良県",
  "宇陀市"="奈良県",
  "山添村"="奈良県",
  "平群町"="奈良県",
  "三郷町"="奈良県",
  "斑鳩町"="奈良県",
  "安堵町"="奈良県",
  "川西町"="奈良県",
  "三宅町"="奈良県",
  "曽爾村"="奈良県",
  "御杖村"="奈良県",
  "高取町"="奈良県",
  "上牧町"="奈良県",
  "王寺町"="奈良県",
  "広陵町"="奈良県",
  "河合町"="奈良県",
  "吉野町"="奈良県",
  "大淀町"="奈良県",
  "下市町"="奈良県",
  "黒滝村"="奈良県",
  "天川村"="奈良県",
  "川上村"="奈良県",
  "気仙沼市"="宮城県",
  "多賀城市"="宮城県",
  "東松島市"="宮城県",
  "七ヶ宿町"="宮城県",
  "大河原町"="宮城県",
  "七ヶ浜町"="宮城県",
  "南三陸町"="宮城県",
  "仙台市"="宮城県",
  "石巻市"="宮城県",
  "塩竈市"="宮城県",
  "白石市"="宮城県",
  "名取市"="宮城県",
  "角田市"="宮城県",
  "岩沼市"="宮城県",
  "登米市"="宮城県",
  "栗原市"="宮城県",
  "大崎市"="宮城県",
  "富谷市"="宮城県",
  "蔵王町"="宮城県",
  "村田町"="宮城県",
  "柴田町"="宮城県",
  "川崎町"="宮城県",
  "丸森町"="宮城県",
  "亘理町"="宮城県",
  "山元町"="宮城県",
  "松島町"="宮城県",
  "利府町"="宮城県",
  "大和町"="宮城県",
  "大郷町"="宮城県",
  "大衡村"="宮城県",
  "色麻町"="宮城県",
  "加美町"="宮城県",
  "涌谷町"="宮城県",
  "美里町"="宮城県",
  "女川町"="宮城県",
  "えびの市"="宮崎県",
  "西米良村"="宮崎県",
  "高千穂町"="宮崎県",
  "日之影町"="宮崎県",
  "五ヶ瀬町"="宮崎県",
  "宮崎市"="宮崎県",
  "都城市"="宮崎県",
  "延岡市"="宮崎県",
  "日南市"="宮崎県",
  "小林市"="宮崎県",
  "日向市"="宮崎県",
  "串間市"="宮崎県",
  "西都市"="宮崎県",
  "三股町"="宮崎県",
  "高原町"="宮崎県",
  "国富町"="宮崎県",
  "高鍋町"="宮崎県",
  "新富町"="宮崎県",
  "木城町"="宮崎県",
  "川南町"="宮崎県",
  "都農町"="宮崎県",
  "門川町"="宮崎県",
  "諸塚村"="宮崎県",
  "椎葉村"="宮崎県",
  "美郷町"="宮崎県",
  "綾町"="宮崎県",
  "小矢部市"="富山県",
  "富山市"="富山県",
  "高岡市"="富山県",
  "魚津市"="富山県",
  "氷見市"="富山県",
  "滑川市"="富山県",
  "黒部市"="富山県",
  "砺波市"="富山県",
  "南砺市"="富山県",
  "射水市"="富山県",
  "舟橋村"="富山県",
  "上市町"="富山県",
  "立山町"="富山県",
  "入善町"="富山県",
  "朝日町"="富山県",
  "山陽小野田市"="山口県",
  "周防大島町"="山口県",
  "田布施町"="山口県",
  "下関市"="山口県",
  "宇部市"="山口県",
  "山口市"="山口県",
  "防府市"="山口県",
  "下松市"="山口県",
  "岩国市"="山口県",
  "長門市"="山口県",
  "柳井市"="山口県",
  "美祢市"="山口県",
  "周南市"="山口県",
  "和木町"="山口県",
  "上関町"="山口県",
  "平生町"="山口県",
  "阿武町"="山口県",
  "萩市"="山口県",
  "光市"="山口県",
  "寒河江市"="山形県",
  "尾花沢市"="山形県",
  "大石田町"="山形県",
  "真室川町"="山形県",
  "山形市"="山形県",
  "米沢市"="山形県",
  "鶴岡市"="山形県",
  "酒田市"="山形県",
  "新庄市"="山形県",
  "上山市"="山形県",
  "村山市"="山形県",
  "長井市"="山形県",
  "天童市"="山形県",
  "東根市"="山形県",
  "南陽市"="山形県",
  "山辺町"="山形県",
  "中山町"="山形県",
  "河北町"="山形県",
  "西川町"="山形県",
  "朝日町"="山形県",
  "大江町"="山形県",
  "金山町"="山形県",
  "最上町"="山形県",
  "舟形町"="山形県",
  "大蔵村"="山形県",
  "鮭川村"="山形県",
  "戸沢村"="山形県",
  "高畠町"="山形県",
  "川西町"="山形県",
  "小国町"="山形県",
  "白鷹町"="山形県",
  "飯豊町"="山形県",
  "三川町"="山形県",
  "庄内町"="山形県",
  "遊佐町"="山形県",
  "南アルプス市"="山梨県",
  "富士河口湖町"="山梨県",
  "富士吉田市"="山梨県",
  "市川三郷町"="山梨県",
  "上野原市"="山梨県",
  "富士川町"="山梨県",
  "山中湖村"="山梨県",
  "丹波山村"="山梨県",
  "甲府市"="山梨県",
  "都留市"="山梨県",
  "山梨市"="山梨県",
  "大月市"="山梨県",
  "韮崎市"="山梨県",
  "北杜市"="山梨県",
  "甲斐市"="山梨県",
  "笛吹市"="山梨県",
  "甲州市"="山梨県",
  "中央市"="山梨県",
  "早川町"="山梨県",
  "身延町"="山梨県",
  "南部町"="山梨県",
  "昭和町"="山梨県",
  "道志村"="山梨県",
  "西桂町"="山梨県",
  "忍野村"="山梨県",
  "鳴沢村"="山梨県",
  "小菅村"="山梨県",
  "美濃加茂市"="岐阜県",
  "多治見市"="岐阜県",
  "中津川市"="岐阜県",
  "各務原市"="岐阜県",
  "関ケ原町"="岐阜県",
  "輪之内町"="岐阜県",
  "揖斐川町"="岐阜県",
  "八百津町"="岐阜県",
  "東白川村"="岐阜県",
  "岐阜市"="岐阜県",
  "大垣市"="岐阜県",
  "高山市"="岐阜県",
  "美濃市"="岐阜県",
  "瑞浪市"="岐阜県",
  "羽島市"="岐阜県",
  "恵那市"="岐阜県",
  "土岐市"="岐阜県",
  "可児市"="岐阜県",
  "山県市"="岐阜県",
  "瑞穂市"="岐阜県",
  "飛騨市"="岐阜県",
  "本巣市"="岐阜県",
  "郡上市"="岐阜県",
  "下呂市"="岐阜県",
  "海津市"="岐阜県",
  "岐南町"="岐阜県",
  "笠松町"="岐阜県",
  "養老町"="岐阜県",
  "垂井町"="岐阜県",
  "神戸町"="岐阜県",
  "安八町"="岐阜県",
  "大野町"="岐阜県",
  "池田町"="岐阜県",
  "北方町"="岐阜県",
  "坂祝町"="岐阜県",
  "富加町"="岐阜県",
  "川辺町"="岐阜県",
  "七宗町"="岐阜県",
  "白川町"="岐阜県",
  "御嵩町"="岐阜県",
  "白川村"="岐阜県",
  "関市"="岐阜県",
  "吉備中央町"="岡山県",
  "瀬戸内市"="岡山県",
  "西粟倉村"="岡山県",
  "久米南町"="岡山県",
  "岡山市"="岡山県",
  "倉敷市"="岡山県",
  "津山市"="岡山県",
  "玉野市"="岡山県",
  "笠岡市"="岡山県",
  "井原市"="岡山県",
  "総社市"="岡山県",
  "高梁市"="岡山県",
  "新見市"="岡山県",
  "備前市"="岡山県",
  "赤磐市"="岡山県",
  "真庭市"="岡山県",
  "美作市"="岡山県",
  "浅口市"="岡山県",
  "和気町"="岡山県",
  "早島町"="岡山県",
  "里庄町"="岡山県",
  "矢掛町"="岡山県",
  "新庄村"="岡山県",
  "鏡野町"="岡山県",
  "勝央町"="岡山県",
  "奈義町"="岡山県",
  "美咲町"="岡山県",
  "陸前高田市"="岩手県",
  "大船渡市"="岩手県",
  "八幡平市"="岩手県",
  "西和賀町"="岩手県",
  "金ケ崎町"="岩手県",
  "田野畑村"="岩手県",
  "盛岡市"="岩手県",
  "宮古市"="岩手県",
  "花巻市"="岩手県",
  "北上市"="岩手県",
  "久慈市"="岩手県",
  "遠野市"="岩手県",
  "一関市"="岩手県",
  "釜石市"="岩手県",
  "二戸市"="岩手県",
  "奥州市"="岩手県",
  "滝沢市"="岩手県",
  "雫石町"="岩手県",
  "葛巻町"="岩手県",
  "岩手町"="岩手県",
  "紫波町"="岩手県",
  "矢巾町"="岩手県",
  "平泉町"="岩手県",
  "住田町"="岩手県",
  "大槌町"="岩手県",
  "山田町"="岩手県",
  "岩泉町"="岩手県",
  "普代村"="岩手県",
  "軽米町"="岩手県",
  "野田村"="岩手県",
  "九戸村"="岩手県",
  "洋野町"="岩手県",
  "一戸町"="岩手県",
  "隠岐の島町"="島根県",
  "奥出雲町"="島根県",
  "津和野町"="島根県",
  "西ノ島町"="島根県",
  "松江市"="島根県",
  "浜田市"="島根県",
  "出雲市"="島根県",
  "益田市"="島根県",
  "大田市"="島根県",
  "安来市"="島根県",
  "江津市"="島根県",
  "雲南市"="島根県",
  "飯南町"="島根県",
  "川本町"="島根県",
  "美郷町"="島根県",
  "邑南町"="島根県",
  "吉賀町"="島根県",
  "海士町"="島根県",
  "知夫村"="島根県",
  "安芸高田市"="広島県",
  "安芸太田町"="広島県",
  "大崎上島町"="広島県",
  "神石高原町"="広島県",
  "東広島市"="広島県",
  "廿日市市"="広島県",
  "江田島市"="広島県",
  "北広島町"="広島県",
  "広島市"="広島県",
  "竹原市"="広島県",
  "三原市"="広島県",
  "尾道市"="広島県",
  "福山市"="広島県",
  "府中市"="広島県",
  "三次市"="広島県",
  "庄原市"="広島県",
  "大竹市"="広島県",
  "府中町"="広島県",
  "海田町"="広島県",
  "熊野町"="広島県",
  "世羅町"="広島県",
  "呉市"="広島県",
  "坂町"="広島県",
  "佐那河内村"="徳島県",
  "東みよし町"="徳島県",
  "小松島市"="徳島県",
  "吉野川市"="徳島県",
  "つるぎ町"="徳島県",
  "徳島市"="徳島県",
  "鳴門市"="徳島県",
  "阿南市"="徳島県",
  "阿波市"="徳島県",
  "美馬市"="徳島県",
  "三好市"="徳島県",
  "勝浦町"="徳島県",
  "上勝町"="徳島県",
  "石井町"="徳島県",
  "神山町"="徳島県",
  "那賀町"="徳島県",
  "牟岐町"="徳島県",
  "美波町"="徳島県",
  "海陽町"="徳島県",
  "松茂町"="徳島県",
  "北島町"="徳島県",
  "藍住町"="徳島県",
  "板野町"="徳島県",
  "上板町"="徳島県",
  "四国中央市"="愛媛県",
  "久万高原町"="愛媛県",
  "宇和島市"="愛媛県",
  "八幡浜市"="愛媛県",
  "新居浜市"="愛媛県",
  "松山市"="愛媛県",
  "今治市"="愛媛県",
  "西条市"="愛媛県",
  "大洲市"="愛媛県",
  "伊予市"="愛媛県",
  "西予市"="愛媛県",
  "東温市"="愛媛県",
  "上島町"="愛媛県",
  "松前町"="愛媛県",
  "砥部町"="愛媛県",
  "内子町"="愛媛県",
  "伊方町"="愛媛県",
  "松野町"="愛媛県",
  "鬼北町"="愛媛県",
  "愛南町"="愛媛県",
  "北名古屋市"="愛知県",
  "名古屋市"="愛知県",
  "春日井市"="愛知県",
  "尾張旭市"="愛知県",
  "みよし市"="愛知県",
  "長久手市"="愛知県",
  "阿久比町"="愛知県",
  "南知多町"="愛知県",
  "豊橋市"="愛知県",
  "岡崎市"="愛知県",
  "一宮市"="愛知県",
  "瀬戸市"="愛知県",
  "半田市"="愛知県",
  "豊川市"="愛知県",
  "津島市"="愛知県",
  "碧南市"="愛知県",
  "刈谷市"="愛知県",
  "豊田市"="愛知県",
  "安城市"="愛知県",
  "西尾市"="愛知県",
  "蒲郡市"="愛知県",
  "犬山市"="愛知県",
  "常滑市"="愛知県",
  "江南市"="愛知県",
  "小牧市"="愛知県",
  "稲沢市"="愛知県",
  "新城市"="愛知県",
  "東海市"="愛知県",
  "大府市"="愛知県",
  "知多市"="愛知県",
  "知立市"="愛知県",
  "高浜市"="愛知県",
  "岩倉市"="愛知県",
  "豊明市"="愛知県",
  "日進市"="愛知県",
  "田原市"="愛知県",
  "愛西市"="愛知県",
  "清須市"="愛知県",
  "弥富市"="愛知県",
  "あま市"="愛知県",
  "東郷町"="愛知県",
  "豊山町"="愛知県",
  "大口町"="愛知県",
  "扶桑町"="愛知県",
  "大治町"="愛知県",
  "蟹江町"="愛知県",
  "飛島村"="愛知県",
  "東浦町"="愛知県",
  "美浜町"="愛知県",
  "武豊町"="愛知県",
  "幸田町"="愛知県",
  "設楽町"="愛知県",
  "東栄町"="愛知県",
  "豊根村"="愛知県",
  "新発田市"="新潟県",
  "小千谷市"="新潟県",
  "十日町市"="新潟県",
  "糸魚川市"="新潟県",
  "阿賀野市"="新潟県",
  "南魚沼市"="新潟県",
  "出雲崎町"="新潟県",
  "粟島浦村"="新潟県",
  "新潟市"="新潟県",
  "長岡市"="新潟県",
  "三条市"="新潟県",
  "柏崎市"="新潟県",
  "加茂市"="新潟県",
  "見附市"="新潟県",
  "村上市"="新潟県",
  "妙高市"="新潟県",
  "五泉市"="新潟県",
  "上越市"="新潟県",
  "佐渡市"="新潟県",
  "魚沼市"="新潟県",
  "胎内市"="新潟県",
  "聖籠町"="新潟県",
  "弥彦村"="新潟県",
  "田上町"="新潟県",
  "阿賀町"="新潟県",
  "湯沢町"="新潟県",
  "津南町"="新潟県",
  "刈羽村"="新潟県",
  "関川村"="新潟県",
  "燕市"="新潟県",
  "東久留米市"="東京都",
  "武蔵村山市"="東京都",
  "あきる野市"="東京都",
  "千代田区"="東京都",
  "世田谷区"="東京都",
  "江戸川区"="東京都",
  "八王子市"="東京都",
  "武蔵野市"="東京都",
  "小金井市"="東京都",
  "東村山市"="東京都",
  "国分寺市"="東京都",
  "東大和市"="東京都",
  "西東京市"="東京都",
  "日の出町"="東京都",
  "奥多摩町"="東京都",
  "神津島村"="東京都",
  "御蔵島村"="東京都",
  "青ヶ島村"="東京都",
  "小笠原村"="東京都",
  "中央区"="東京都",
  "新宿区"="東京都",
  "文京区"="東京都",
  "台東区"="東京都",
  "墨田区"="東京都",
  "江東区"="東京都",
  "品川区"="東京都",
  "目黒区"="東京都",
  "大田区"="東京都",
  "渋谷区"="東京都",
  "中野区"="東京都",
  "杉並区"="東京都",
  "豊島区"="東京都",
  "荒川区"="東京都",
  "板橋区"="東京都",
  "練馬区"="東京都",
  "足立区"="東京都",
  "葛飾区"="東京都",
  "立川市"="東京都",
  "三鷹市"="東京都",
  "青梅市"="東京都",
  "府中市"="東京都",
  "昭島市"="東京都",
  "調布市"="東京都",
  "町田市"="東京都",
  "小平市"="東京都",
  "日野市"="東京都",
  "国立市"="東京都",
  "福生市"="東京都",
  "狛江市"="東京都",
  "清瀬市"="東京都",
  "多摩市"="東京都",
  "稲城市"="東京都",
  "羽村市"="東京都",
  "瑞穂町"="東京都",
  "檜原村"="東京都",
  "大島町"="東京都",
  "利島村"="東京都",
  "新島村"="東京都",
  "三宅村"="東京都",
  "八丈町"="東京都",
  "港区"="東京都",
  "北区"="東京都",
  "那須塩原市"="栃木県",
  "那須烏山市"="栃木県",
  "宇都宮市"="栃木県",
  "大田原市"="栃木県",
  "さくら市"="栃木県",
  "上三川町"="栃木県",
  "高根沢町"="栃木県",
  "那珂川町"="栃木県",
  "足利市"="栃木県",
  "栃木市"="栃木県",
  "佐野市"="栃木県",
  "鹿沼市"="栃木県",
  "日光市"="栃木県",
  "小山市"="栃木県",
  "真岡市"="栃木県",
  "矢板市"="栃木県",
  "下野市"="栃木県",
  "益子町"="栃木県",
  "茂木町"="栃木県",
  "市貝町"="栃木県",
  "芳賀町"="栃木県",
  "壬生町"="栃木県",
  "野木町"="栃木県",
  "塩谷町"="栃木県",
  "那須町"="栃木県",
  "宜野湾市"="沖縄県",
  "豊見城市"="沖縄県",
  "うるま市"="沖縄県",
  "宮古島市"="沖縄県",
  "大宜味村"="沖縄県",
  "今帰仁村"="沖縄県",
  "宜野座村"="沖縄県",
  "嘉手納町"="沖縄県",
  "北中城村"="沖縄県",
  "与那原町"="沖縄県",
  "南風原町"="沖縄県",
  "渡嘉敷村"="沖縄県",
  "座間味村"="沖縄県",
  "渡名喜村"="沖縄県",
  "南大東村"="沖縄県",
  "北大東村"="沖縄県",
  "伊平屋村"="沖縄県",
  "伊是名村"="沖縄県",
  "久米島町"="沖縄県",
  "八重瀬町"="沖縄県",
  "多良間村"="沖縄県",
  "与那国町"="沖縄県",
  "那覇市"="沖縄県",
  "石垣市"="沖縄県",
  "浦添市"="沖縄県",
  "名護市"="沖縄県",
  "糸満市"="沖縄県",
  "沖縄市"="沖縄県",
  "南城市"="沖縄県",
  "国頭村"="沖縄県",
  "本部町"="沖縄県",
  "恩納村"="沖縄県",
  "金武町"="沖縄県",
  "伊江村"="沖縄県",
  "読谷村"="沖縄県",
  "北谷町"="沖縄県",
  "中城村"="沖縄県",
  "西原町"="沖縄県",
  "粟国村"="沖縄県",
  "竹富町"="沖縄県",
  "東村"="沖縄県",
  "近江八幡市"="滋賀県",
  "東近江市"="滋賀県",
  "大津市"="滋賀県",
  "彦根市"="滋賀県",
  "長浜市"="滋賀県",
  "草津市"="滋賀県",
  "守山市"="滋賀県",
  "栗東市"="滋賀県",
  "甲賀市"="滋賀県",
  "野洲市"="滋賀県",
  "湖南市"="滋賀県",
  "高島市"="滋賀県",
  "米原市"="滋賀県",
  "日野町"="滋賀県",
  "竜王町"="滋賀県",
  "愛荘町"="滋賀県",
  "豊郷町"="滋賀県",
  "甲良町"="滋賀県",
  "多賀町"="滋賀県",
  "あさぎり町"="熊本県",
  "上天草市"="熊本県",
  "南小国町"="熊本県",
  "南阿蘇村"="熊本県",
  "津奈木町"="熊本県",
  "多良木町"="熊本県",
  "熊本市"="熊本県",
  "八代市"="熊本県",
  "人吉市"="熊本県",
  "荒尾市"="熊本県",
  "水俣市"="熊本県",
  "玉名市"="熊本県",
  "山鹿市"="熊本県",
  "菊池市"="熊本県",
  "宇土市"="熊本県",
  "宇城市"="熊本県",
  "阿蘇市"="熊本県",
  "天草市"="熊本県",
  "合志市"="熊本県",
  "美里町"="熊本県",
  "玉東町"="熊本県",
  "南関町"="熊本県",
  "長洲町"="熊本県",
  "和水町"="熊本県",
  "大津町"="熊本県",
  "菊陽町"="熊本県",
  "小国町"="熊本県",
  "産山村"="熊本県",
  "高森町"="熊本県",
  "西原村"="熊本県",
  "御船町"="熊本県",
  "嘉島町"="熊本県",
  "益城町"="熊本県",
  "甲佐町"="熊本県",
  "山都町"="熊本県",
  "氷川町"="熊本県",
  "芦北町"="熊本県",
  "湯前町"="熊本県",
  "水上村"="熊本県",
  "相良村"="熊本県",
  "五木村"="熊本県",
  "山江村"="熊本県",
  "球磨村"="熊本県",
  "苓北町"="熊本県",
  "錦町"="熊本県",
  "宝達志水町"="石川県",
  "かほく市"="石川県",
  "野々市市"="石川県",
  "中能登町"="石川県",
  "金沢市"="石川県",
  "七尾市"="石川県",
  "小松市"="石川県",
  "輪島市"="石川県",
  "珠洲市"="石川県",
  "加賀市"="石川県",
  "羽咋市"="石川県",
  "白山市"="石川県",
  "能美市"="石川県",
  "川北町"="石川県",
  "津幡町"="石川県",
  "内灘町"="石川県",
  "志賀町"="石川県",
  "穴水町"="石川県",
  "能登町"="石川県",
  "相模原市"="神奈川県",
  "横須賀市"="神奈川県",
  "小田原市"="神奈川県",
  "茅ヶ崎市"="神奈川県",
  "伊勢原市"="神奈川県",
  "海老名市"="神奈川県",
  "南足柄市"="神奈川県",
  "湯河原町"="神奈川県",
  "横浜市"="神奈川県",
  "川崎市"="神奈川県",
  "平塚市"="神奈川県",
  "鎌倉市"="神奈川県",
  "藤沢市"="神奈川県",
  "逗子市"="神奈川県",
  "三浦市"="神奈川県",
  "秦野市"="神奈川県",
  "厚木市"="神奈川県",
  "大和市"="神奈川県",
  "座間市"="神奈川県",
  "綾瀬市"="神奈川県",
  "葉山町"="神奈川県",
  "寒川町"="神奈川県",
  "大磯町"="神奈川県",
  "二宮町"="神奈川県",
  "中井町"="神奈川県",
  "大井町"="神奈川県",
  "松田町"="神奈川県",
  "山北町"="神奈川県",
  "開成町"="神奈川県",
  "箱根町"="神奈川県",
  "真鶴町"="神奈川県",
  "愛川町"="神奈川県",
  "清川村"="神奈川県",
  "あわら市"="福井県",
  "永平寺町"="福井県",
  "南越前町"="福井県",
  "おおい町"="福井県",
  "福井市"="福井県",
  "敦賀市"="福井県",
  "小浜市"="福井県",
  "大野市"="福井県",
  "勝山市"="福井県",
  "鯖江市"="福井県",
  "越前市"="福井県",
  "坂井市"="福井県",
  "池田町"="福井県",
  "越前町"="福井県",
  "美浜町"="福井県",
  "高浜町"="福井県",
  "若狭町"="福井県",
  "北九州市"="福岡県",
  "大牟田市"="福岡県",
  "久留米市"="福岡県",
  "筑紫野市"="福岡県",
  "大野城市"="福岡県",
  "太宰府市"="福岡県",
  "うきは市"="福岡県",
  "みやま市"="福岡県",
  "那珂川市"="福岡県",
  "大刀洗町"="福岡県",
  "みやこ町"="福岡県",
  "福岡市"="福岡県",
  "直方市"="福岡県",
  "飯塚市"="福岡県",
  "田川市"="福岡県",
  "柳川市"="福岡県",
  "八女市"="福岡県",
  "筑後市"="福岡県",
  "大川市"="福岡県",
  "行橋市"="福岡県",
  "豊前市"="福岡県",
  "中間市"="福岡県",
  "小郡市"="福岡県",
  "春日市"="福岡県",
  "宗像市"="福岡県",
  "古賀市"="福岡県",
  "福津市"="福岡県",
  "宮若市"="福岡県",
  "嘉麻市"="福岡県",
  "朝倉市"="福岡県",
  "糸島市"="福岡県",
  "宇美町"="福岡県",
  "篠栗町"="福岡県",
  "志免町"="福岡県",
  "須恵町"="福岡県",
  "新宮町"="福岡県",
  "久山町"="福岡県",
  "粕屋町"="福岡県",
  "芦屋町"="福岡県",
  "水巻町"="福岡県",
  "岡垣町"="福岡県",
  "遠賀町"="福岡県",
  "小竹町"="福岡県",
  "鞍手町"="福岡県",
  "桂川町"="福岡県",
  "筑前町"="福岡県",
  "東峰村"="福岡県",
  "大木町"="福岡県",
  "広川町"="福岡県",
  "香春町"="福岡県",
  "添田町"="福岡県",
  "糸田町"="福岡県",
  "川崎町"="福岡県",
  "大任町"="福岡県",
  "福智町"="福岡県",
  "苅田町"="福岡県",
  "吉富町"="福岡県",
  "上毛町"="福岡県",
  "築上町"="福岡県",
  "赤村"="福岡県",
  "会津若松市"="福島県",
  "会津坂下町"="福島県",
  "会津美里町"="福島県",
  "いわき市"="福島県",
  "須賀川市"="福島県",
  "喜多方市"="福島県",
  "二本松市"="福島県",
  "南相馬市"="福島県",
  "檜枝岐村"="福島県",
  "南会津町"="福島県",
  "北塩原村"="福島県",
  "西会津町"="福島県",
  "猪苗代町"="福島県",
  "福島市"="福島県",
  "郡山市"="福島県",
  "白河市"="福島県",
  "相馬市"="福島県",
  "田村市"="福島県",
  "伊達市"="福島県",
  "本宮市"="福島県",
  "桑折町"="福島県",
  "国見町"="福島県",
  "川俣町"="福島県",
  "大玉村"="福島県",
  "鏡石町"="福島県",
  "天栄村"="福島県",
  "下郷町"="福島県",
  "只見町"="福島県",
  "磐梯町"="福島県",
  "湯川村"="福島県",
  "柳津町"="福島県",
  "三島町"="福島県",
  "金山町"="福島県",
  "昭和村"="福島県",
  "西郷村"="福島県",
  "泉崎村"="福島県",
  "中島村"="福島県",
  "矢吹町"="福島県",
  "棚倉町"="福島県",
  "矢祭町"="福島県",
  "鮫川村"="福島県",
  "石川町"="福島県",
  "玉川村"="福島県",
  "平田村"="福島県",
  "浅川町"="福島県",
  "古殿町"="福島県",
  "三春町"="福島県",
  "小野町"="福島県",
  "広野町"="福島県",
  "楢葉町"="福島県",
  "富岡町"="福島県",
  "川内村"="福島県",
  "大熊町"="福島県",
  "双葉町"="福島県",
  "浪江町"="福島県",
  "葛尾村"="福島県",
  "新地町"="福島県",
  "飯舘村"="福島県",
  "塙町"="福島県",
  "由利本荘市"="秋田県",
  "上小阿仁村"="秋田県",
  "北秋田市"="秋田県",
  "にかほ市"="秋田県",
  "五城目町"="秋田県",
  "八郎潟町"="秋田県",
  "東成瀬村"="秋田県",
  "秋田市"="秋田県",
  "能代市"="秋田県",
  "横手市"="秋田県",
  "大館市"="秋田県",
  "男鹿市"="秋田県",
  "湯沢市"="秋田県",
  "鹿角市"="秋田県",
  "潟上市"="秋田県",
  "大仙市"="秋田県",
  "仙北市"="秋田県",
  "小坂町"="秋田県",
  "藤里町"="秋田県",
  "三種町"="秋田県",
  "八峰町"="秋田県",
  "井川町"="秋田県",
  "大潟村"="秋田県",
  "美郷町"="秋田県",
  "羽後町"="秋田県",
  "みなかみ町"="群馬県",
  "伊勢崎市"="群馬県",
  "みどり市"="群馬県",
  "下仁田町"="群馬県",
  "中之条町"="群馬県",
  "長野原町"="群馬県",
  "東吾妻町"="群馬県",
  "千代田町"="群馬県",
  "前橋市"="群馬県",
  "高崎市"="群馬県",
  "桐生市"="群馬県",
  "太田市"="群馬県",
  "沼田市"="群馬県",
  "館林市"="群馬県",
  "渋川市"="群馬県",
  "藤岡市"="群馬県",
  "富岡市"="群馬県",
  "安中市"="群馬県",
  "榛東村"="群馬県",
  "吉岡町"="群馬県",
  "上野村"="群馬県",
  "神流町"="群馬県",
  "南牧村"="群馬県",
  "甘楽町"="群馬県",
  "嬬恋村"="群馬県",
  "草津町"="群馬県",
  "高山村"="群馬県",
  "片品村"="群馬県",
  "川場村"="群馬県",
  "昭和村"="群馬県",
  "玉村町"="群馬県",
  "板倉町"="群馬県",
  "明和町"="群馬県",
  "大泉町"="群馬県",
  "邑楽町"="群馬県",
  "かすみがうら市"="茨城県",
  "つくばみらい市"="茨城県",
  "ひたちなか市"="茨城県",
  "常陸太田市"="茨城県",
  "常陸大宮市"="茨城県",
  "龍ケ崎市"="茨城県",
  "北茨城市"="茨城県",
  "つくば市"="茨城県",
  "小美玉市"="茨城県",
  "八千代町"="茨城県",
  "水戸市"="茨城県",
  "日立市"="茨城県",
  "土浦市"="茨城県",
  "古河市"="茨城県",
  "石岡市"="茨城県",
  "結城市"="茨城県",
  "下妻市"="茨城県",
  "常総市"="茨城県",
  "高萩市"="茨城県",
  "笠間市"="茨城県",
  "取手市"="茨城県",
  "牛久市"="茨城県",
  "鹿嶋市"="茨城県",
  "潮来市"="茨城県",
  "守谷市"="茨城県",
  "那珂市"="茨城県",
  "筑西市"="茨城県",
  "坂東市"="茨城県",
  "稲敷市"="茨城県",
  "桜川市"="茨城県",
  "神栖市"="茨城県",
  "行方市"="茨城県",
  "鉾田市"="茨城県",
  "茨城町"="茨城県",
  "大洗町"="茨城県",
  "城里町"="茨城県",
  "東海村"="茨城県",
  "大子町"="茨城県",
  "美浦村"="茨城県",
  "阿見町"="茨城県",
  "河内町"="茨城県",
  "五霞町"="茨城県",
  "利根町"="茨城県",
  "境町"="茨城県",
  "新上五島町"="長崎県",
  "佐世保市"="長崎県",
  "南島原市"="長崎県",
  "東彼杵町"="長崎県",
  "波佐見町"="長崎県",
  "小値賀町"="長崎県",
  "長崎市"="長崎県",
  "島原市"="長崎県",
  "諫早市"="長崎県",
  "大村市"="長崎県",
  "平戸市"="長崎県",
  "松浦市"="長崎県",
  "対馬市"="長崎県",
  "壱岐市"="長崎県",
  "五島市"="長崎県",
  "西海市"="長崎県",
  "雲仙市"="長崎県",
  "長与町"="長崎県",
  "時津町"="長崎県",
  "川棚町"="長崎県",
  "佐々町"="長崎県",
  "野沢温泉村"="長野県",
  "駒ヶ根市"="長野県",
  "安曇野市"="長野県",
  "南相木村"="長野県",
  "北相木村"="長野県",
  "佐久穂町"="長野県",
  "軽井沢町"="長野県",
  "御代田町"="長野県",
  "下諏訪町"="長野県",
  "富士見町"="長野県",
  "南箕輪村"="長野県",
  "南木曽町"="長野県",
  "小布施町"="長野県",
  "山ノ内町"="長野県",
  "木島平村"="長野県",
  "長野市"="長野県",
  "松本市"="長野県",
  "上田市"="長野県",
  "岡谷市"="長野県",
  "飯田市"="長野県",
  "諏訪市"="長野県",
  "須坂市"="長野県",
  "小諸市"="長野県",
  "伊那市"="長野県",
  "中野市"="長野県",
  "大町市"="長野県",
  "飯山市"="長野県",
  "茅野市"="長野県",
  "塩尻市"="長野県",
  "佐久市"="長野県",
  "千曲市"="長野県",
  "東御市"="長野県",
  "小海町"="長野県",
  "川上村"="長野県",
  "南牧村"="長野県",
  "立科町"="長野県",
  "青木村"="長野県",
  "長和町"="長野県",
  "辰野町"="長野県",
  "箕輪町"="長野県",
  "飯島町"="長野県",
  "中川村"="長野県",
  "宮田村"="長野県",
  "松川町"="長野県",
  "高森町"="長野県",
  "阿南町"="長野県",
  "阿智村"="長野県",
  "平谷村"="長野県",
  "根羽村"="長野県",
  "下條村"="長野県",
  "売木村"="長野県",
  "天龍村"="長野県",
  "泰阜村"="長野県",
  "喬木村"="長野県",
  "豊丘村"="長野県",
  "大鹿村"="長野県",
  "上松町"="長野県",
  "木祖村"="長野県",
  "王滝村"="長野県",
  "大桑村"="長野県",
  "木曽町"="長野県",
  "麻績村"="長野県",
  "生坂村"="長野県",
  "山形村"="長野県",
  "朝日村"="長野県",
  "筑北村"="長野県",
  "池田町"="長野県",
  "松川村"="長野県",
  "白馬村"="長野県",
  "小谷村"="長野県",
  "坂城町"="長野県",
  "高山村"="長野県",
  "信濃町"="長野県",
  "小川村"="長野県",
  "飯綱町"="長野県",
  "原村"="長野県",
  "栄村"="長野県",
  "五所川原市"="青森県",
  "おいらせ町"="青森県",
  "十和田市"="青森県",
  "つがる市"="青森県",
  "外ヶ浜町"="青森県",
  "鰺ヶ沢町"="青森県",
  "西目屋村"="青森県",
  "田舎館村"="青森県",
  "野辺地町"="青森県",
  "六ヶ所村"="青森県",
  "風間浦村"="青森県",
  "青森市"="青森県",
  "弘前市"="青森県",
  "八戸市"="青森県",
  "黒石市"="青森県",
  "三沢市"="青森県",
  "むつ市"="青森県",
  "平川市"="青森県",
  "平内町"="青森県",
  "今別町"="青森県",
  "蓬田村"="青森県",
  "深浦町"="青森県",
  "藤崎町"="青森県",
  "大鰐町"="青森県",
  "板柳町"="青森県",
  "鶴田町"="青森県",
  "中泊町"="青森県",
  "七戸町"="青森県",
  "六戸町"="青森県",
  "横浜町"="青森県",
  "東北町"="青森県",
  "大間町"="青森県",
  "東通村"="青森県",
  "佐井村"="青森県",
  "三戸町"="青森県",
  "五戸町"="青森県",
  "田子町"="青森県",
  "南部町"="青森県",
  "階上町"="青森県",
  "新郷村"="青森県",
  "伊豆の国市"="静岡県",
  "富士宮市"="静岡県",
  "御殿場市"="静岡県",
  "御前崎市"="静岡県",
  "牧之原市"="静岡県",
  "東伊豆町"="静岡県",
  "南伊豆町"="静岡県",
  "西伊豆町"="静岡県",
  "川根本町"="静岡県",
  "静岡市"="静岡県",
  "浜松市"="静岡県",
  "沼津市"="静岡県",
  "熱海市"="静岡県",
  "三島市"="静岡県",
  "伊東市"="静岡県",
  "島田市"="静岡県",
  "富士市"="静岡県",
  "磐田市"="静岡県",
  "焼津市"="静岡県",
  "掛川市"="静岡県",
  "藤枝市"="静岡県",
  "袋井市"="静岡県",
  "下田市"="静岡県",
  "裾野市"="静岡県",
  "湖西市"="静岡県",
  "伊豆市"="静岡県",
  "菊川市"="静岡県",
  "河津町"="静岡県",
  "松崎町"="静岡県",
  "函南町"="静岡県",
  "清水町"="静岡県",
  "長泉町"="静岡県",
  "小山町"="静岡県",
  "吉田町"="静岡県",
  "森町"="静岡県",
  "東かがわ市"="香川県",
  "まんのう町"="香川県",
  "善通寺市"="香川県",
  "観音寺市"="香川県",
  "さぬき市"="香川県",
  "小豆島町"="香川県",
  "宇多津町"="香川県",
  "多度津町"="香川県",
  "高松市"="香川県",
  "丸亀市"="香川県",
  "坂出市"="香川県",
  "三豊市"="香川県",
  "土庄町"="香川県",
  "三木町"="香川県",
  "直島町"="香川県",
  "綾川町"="香川県",
  "琴平町"="香川県",
  "土佐清水市"="高知県",
  "四万十市"="高知県",
  "奈半利町"="高知県",
  "仁淀川町"="高知県",
  "中土佐町"="高知県",
  "四万十町"="高知県",
  "高知市"="高知県",
  "室戸市"="高知県",
  "安芸市"="高知県",
  "南国市"="高知県",
  "土佐市"="高知県",
  "須崎市"="高知県",
  "宿毛市"="高知県",
  "香南市"="高知県",
  "香美市"="高知県",
  "東洋町"="高知県",
  "田野町"="高知県",
  "安田町"="高知県",
  "北川村"="高知県",
  "馬路村"="高知県",
  "芸西村"="高知県",
  "本山町"="高知県",
  "大豊町"="高知県",
  "土佐町"="高知県",
  "大川村"="高知県",
  "いの町"="高知県",
  "佐川町"="高知県",
  "越知町"="高知県",
  "梼原町"="高知県",
  "日高村"="高知県",
  "津野町"="高知県",
  "大月町"="高知県",
  "三原村"="高知県",
  "黒潮町"="高知県",
  "湯梨浜町"="鳥取県",
  "日吉津村"="鳥取県",
  "鳥取市"="鳥取県",
  "米子市"="鳥取県",
  "倉吉市"="鳥取県",
  "境港市"="鳥取県",
  "岩美町"="鳥取県",
  "若桜町"="鳥取県",
  "智頭町"="鳥取県",
  "八頭町"="鳥取県",
  "三朝町"="鳥取県",
  "琴浦町"="鳥取県",
  "北栄町"="鳥取県",
  "大山町"="鳥取県",
  "南部町"="鳥取県",
  "伯耆町"="鳥取県",
  "日南町"="鳥取県",
  "日野町"="鳥取県",
  "江府町"="鳥取県",
  "いちき串木野市"="鹿児島県",
  "薩摩川内市"="鹿児島県",
  "南さつま市"="鹿児島県",
  "鹿児島市"="鹿児島県",
  "阿久根市"="鹿児島県",
  "西之表市"="鹿児島県",
  "志布志市"="鹿児島県",
  "南九州市"="鹿児島県",
  "さつま町"="鹿児島県",
  "東串良町"="鹿児島県",
  "南大隅町"="鹿児島県",
  "中種子町"="鹿児島県",
  "南種子町"="鹿児島県",
  "屋久島町"="鹿児島県",
  "瀬戸内町"="鹿児島県",
  "徳之島町"="鹿児島県",
  "鹿屋市"="鹿児島県",
  "枕崎市"="鹿児島県",
  "出水市"="鹿児島県",
  "指宿市"="鹿児島県",
  "垂水市"="鹿児島県",
  "日置市"="鹿児島県",
  "曽於市"="鹿児島県",
  "霧島市"="鹿児島県",
  "奄美市"="鹿児島県",
  "伊佐市"="鹿児島県",
  "姶良市"="鹿児島県",
  "三島村"="鹿児島県",
  "十島村"="鹿児島県",
  "長島町"="鹿児島県",
  "湧水町"="鹿児島県",
  "大崎町"="鹿児島県",
  "錦江町"="鹿児島県",
  "肝付町"="鹿児島県",
  "大和村"="鹿児島県",
  "宇検村"="鹿児島県",
  "龍郷町"="鹿児島県",
  "喜界町"="鹿児島県",
  "天城町"="鹿児島県",
  "伊仙町"="鹿児島県",
  "和泊町"="鹿児島県",
  "知名町"="鹿児島県",
  "与論町"="鹿児島県"
)
HOKENJO_PREF_MAP <- c(
  "札幌"="北海道","函館"="北海道","旭川"="北海道","釧路"="北海道","帯広"="北海道",
  "青森"="青森県","八戸"="青森県","弘前"="青森県",
  "盛岡"="岩手県","一関"="岩手県",
  "仙台"="宮城県","大崎"="宮城県","石巻"="宮城県",
  "秋田"="秋田県","大館"="秋田県",
  "山形"="山形県","鶴岡"="山形県",
  "福島"="福島県","郡山"="福島県","いわき"="福島県",
  "水戸"="茨城県","つくば"="茨城県","土浦"="茨城県",
  "宇都宮"="栃木県","小山"="栃木県",
  "前橋"="群馬県","高崎"="群馬県",
  "さいたま"="埼玉県","川越"="埼玉県","熊谷"="埼玉県",
  "千葉"="千葉県","船橋"="千葉県","松戸"="千葉県","柏"="千葉県",
  "新宿"="東京都","渋谷"="東京都","足立"="東京都","江戸川"="東京都",
  "世田谷"="東京都","板橋"="東京都","練馬"="東京都","大田"="東京都",
  "多摩"="東京都","八王子"="東京都","立川"="東京都",
  "横浜"="神奈川県","川崎"="神奈川県","相模原"="神奈川県","横須賀"="神奈川県",
  "藤沢"="神奈川県","小田原"="神奈川県",
  "新潟"="新潟県","長岡"="新潟県","上越"="新潟県",
  "富山"="富山県","高岡"="富山県",
  "金沢"="石川県","小松"="石川県",
  "福井"="福井県","敦賀"="福井県",
  "甲府"="山梨県",
  "長野"="長野県","松本"="長野県","上田"="長野県","飯田"="長野県",
  "岐阜"="岐阜県","大垣"="岐阜県",
  "静岡"="静岡県","浜松"="静岡県","沼津"="静岡県",
  "名古屋"="愛知県","豊橋"="愛知県","岡崎"="愛知県","一宮"="愛知県",
  "津"="三重県","四日市"="三重県","伊賀"="三重県",
  "大津"="滋賀県","草津"="滋賀県",
  "京都"="京都府","宇治"="京都府",
  "大阪"="大阪府","堺"="大阪府","豊中"="大阪府","吹田"="大阪府",
  "神戸"="兵庫県","姫路"="兵庫県","尼崎"="兵庫県","西宮"="兵庫県",
  "奈良"="奈良県","橿原"="奈良県",
  "和歌山"="和歌山県","田辺"="和歌山県",
  "鳥取"="鳥取県","米子"="鳥取県",
  "松江"="島根県","出雲"="島根県",
  "岡山"="岡山県","倉敷"="岡山県",
  "広島"="広島県","福山"="広島県","呉"="広島県",
  "山口"="山口県","下関"="山口県","宇部"="山口県",
  "徳島"="徳島県",
  "高松"="香川県",
  "松山"="愛媛県","今治"="愛媛県",
  "高知"="高知県",
  "福岡"="福岡県","北九州"="福岡県","久留米"="福岡県",
  "佐賀"="佐賀県","唐津"="佐賀県",
  "長崎"="長崎県","佐世保"="長崎県",
  "熊本"="熊本県","八代"="熊本県",
  "大分"="大分県","別府"="大分県",
  "宮崎"="宮崎県","都城"="宮崎県",
  "鹿児島"="鹿児島県","霧島"="鹿児島県",
  "那覇"="沖縄県","沖縄"="沖縄県","コザ"="沖縄県","名護"="沖縄県"
)

# 47都道府県名リスト（「都道府県」サフィックスあり・なし両方）
PREF_NAMES_JA <- c(
  "北海道",
  "青森県","岩手県","宮城県","秋田県","山形県","福島県",
  "茨城県","栃木県","群馬県","埼玉県","千葉県","東京都","神奈川県",
  "新潟県","富山県","石川県","福井県","山梨県","長野県","岐阜県",
  "静岡県","愛知県","三重県",
  "滋賀県","京都府","大阪府","兵庫県","奈良県","和歌山県",
  "鳥取県","島根県","岡山県","広島県","山口県",
  "徳島県","香川県","愛媛県","高知県",
  "福岡県","佐賀県","長崎県","熊本県","大分県","宮崎県","鹿児島県","沖縄県"
)
# 短縮形（「県」「都」「府」「道」なし）
PREF_SHORT_JA <- sub("(都|道|府|県)$", "", PREF_NAMES_JA)

# 都道府県判定: title, summary, source_name, link から都道府県を推定
# 戻り値: 都道府県名（"北海道"等）または NA
detect_pref <- function(title = "", summary = "", source_name = "", link = "") {
  title       <- if (is.na(title))       "" else title
  summary     <- if (is.na(summary))     "" else summary
  source_name <- if (is.na(source_name)) "" else source_name
  link        <- if (is.na(link))        "" else link

  # 1) リンクURL・ソース名からメディアマッピング
  combined_meta <- paste(tolower(source_name), tolower(link))
  for (pat in names(MEDIA_PREF_MAP)) {
    if (grepl(pat, combined_meta, perl = TRUE, ignore.case = TRUE))
      return(MEDIA_PREF_MAP[[pat]])
  }

  # タイトルに「コンゴ」等の海外キーワードが含まれる場合、本文（要約）は
  # NPO本部所在地・記者発信地（「東京発」等）・広告文（求人情報等）といった
  # 記事本題と無関係な地名を含みうるため、地名検索の対象をタイトルのみに限定する
  # （例: 「コンゴでエボラ流行」という記事の要約に「京都府京都市」のNPO本部住所や
  # 「東京都」の求人広告が含まれていても、国内記事として誤判定しない）。
  title_has_overseas <- grepl(.OVERSEAS_KW_PATTERN, tolower(title), perl = TRUE)

  # 2) ソース名・タイトル・本文から都道府県名（完全形）を検索
  full_text <- if (title_has_overseas) paste(source_name, title) else paste(source_name, title, summary)
  for (i in seq_along(PREF_NAMES_JA)) {
    if (grepl(PREF_NAMES_JA[i], full_text, fixed = TRUE))
      return(PREF_NAMES_JA[i])
  }

  # 3) 主要市町村名（○○市・○○区・○○町・○○村）を検索
  for (city in names(CITY_PREF_MAP)) {
    if (grepl(city, full_text, fixed = TRUE))
      return(CITY_PREF_MAP[[city]])
  }

  # 4) 保健所名パターン（「○○保健所」「○○保健センター」「○○市保健」）
  for (area in names(HOKENJO_PREF_MAP)) {
    pat <- paste0(area, "(保健所|保健センター|市保健|区保健|町保健|圏域)")
    if (grepl(pat, full_text, perl = TRUE))
      return(HOKENJO_PREF_MAP[[area]])
  }

  # 5) 短縮形（「東京」「大阪」等）で検索（誤検知を抑えるため2文字以上）
  for (i in seq_along(PREF_SHORT_JA)) {
    nm <- PREF_SHORT_JA[i]
    if (nchar(nm) >= 2 && grepl(nm, full_text, fixed = TRUE))
      return(PREF_NAMES_JA[i])
  }

  NA_character_
}

# ベクトル版 detect_pref()。CITY_PREF_MAP（約1,700件）等を記事1件ごとに
# 線形走査すると件数の多いEBSキャッシュでは非常に遅くなる（実測: 2859件で
# detect_pref単体が約13秒）ため、「候補（都道府県名・市区町村名等）ごとに、
# まだ判定が確定していない行だけをベクトルgreplでまとめて判定する」方式に
# 変更したもの。判定順序・各tierでの短絡（先に確定した行はそれ以降を評価
# しない）はdetect_pref()と完全に同じになるよう実装している。
# 呼び出し側はmapply(detect_pref, ...)の代わりにこちらを使うことで、
# 「記事数 × 候補数」だったR関数呼び出しを「候補数」回のベクトル演算に
# 削減できる。
detect_pref_vec <- function(titles, summaries, source_names = "", links = "") {
  n <- length(titles)
  titles    <- ifelse(is.na(titles), "", titles)
  summaries <- ifelse(is.na(summaries), "", summaries)
  if (length(source_names) == 1) source_names <- rep(source_names, n)
  if (length(links) == 1) links <- rep(links, n)
  source_names <- ifelse(is.na(source_names), "", source_names)
  links        <- ifelse(is.na(links), "", links)

  result    <- rep(NA_character_, n)
  remaining <- rep(TRUE, n)

  # 1) リンクURL・ソース名からメディアマッピング
  combined_meta <- paste(tolower(source_names), tolower(links))
  for (pat in names(MEDIA_PREF_MAP)) {
    if (!any(remaining)) break
    hit <- remaining & grepl(pat, combined_meta, perl = TRUE, ignore.case = TRUE)
    if (any(hit)) { result[hit] <- MEDIA_PREF_MAP[[pat]]; remaining[hit] <- FALSE }
  }
  if (!any(remaining)) return(result)

  title_has_overseas <- grepl(.OVERSEAS_KW_PATTERN, tolower(titles), perl = TRUE)
  full_text <- ifelse(title_has_overseas,
                       paste(source_names, titles),
                       paste(source_names, titles, summaries))

  # 2) 都道府県名（完全形）
  for (i in seq_along(PREF_NAMES_JA)) {
    if (!any(remaining)) break
    hit <- remaining & grepl(PREF_NAMES_JA[i], full_text, fixed = TRUE)
    if (any(hit)) { result[hit] <- PREF_NAMES_JA[i]; remaining[hit] <- FALSE }
  }
  if (!any(remaining)) return(result)

  # 3) 主要市町村名
  for (city in names(CITY_PREF_MAP)) {
    if (!any(remaining)) break
    hit <- remaining & grepl(city, full_text, fixed = TRUE)
    if (any(hit)) { result[hit] <- CITY_PREF_MAP[[city]]; remaining[hit] <- FALSE }
  }
  if (!any(remaining)) return(result)

  # 4) 保健所名パターン
  for (area in names(HOKENJO_PREF_MAP)) {
    if (!any(remaining)) break
    pat <- paste0(area, "(保健所|保健センター|市保健|区保健|町保健|圏域)")
    hit <- remaining & grepl(pat, full_text, perl = TRUE)
    if (any(hit)) { result[hit] <- HOKENJO_PREF_MAP[[area]]; remaining[hit] <- FALSE }
  }
  if (!any(remaining)) return(result)

  # 5) 短縮形
  for (i in seq_along(PREF_SHORT_JA)) {
    if (!any(remaining)) break
    nm <- PREF_SHORT_JA[i]
    if (nchar(nm) < 2) next
    hit <- remaining & grepl(nm, full_text, fixed = TRUE)
    if (any(hit)) { result[hit] <- PREF_NAMES_JA[i]; remaining[hit] <- FALSE }
  }
  result
}

# ============================================================
# 文字種比率による言語判定（gsub方式・長文でも安定）
detect_lang <- function(text) {
  if (is.na(text) || nchar(trimws(text)) == 0) return("unknown")
  count_chars <- function(pattern) nchar(gsub(pattern, "", text, perl = TRUE))
  total  <- nchar(text)
  n_hira  <- total - count_chars("[^぀-ゟ]")  # ひらがな
  n_kana  <- total - count_chars("[^゠-ヿ]")  # カタカナ
  n_hangul<- total - count_chars("[^가-힣]")  # ハングル
  n_arabic<- total - count_chars("[^؀-ۿ]")  # アラビア文字
  n_cjk   <- total - count_chars("[^一-鿿]")  # CJK漢字
  n_latin <- total - count_chars("[^A-Za-z]")          # ラテン文字

  if ((n_hira + n_kana) > 0)  return("ja")  # ひらがな/カタカナ1文字でも→日本語確定
  if (n_hangul > 2)            return("ko")
  if (n_arabic > 2)            return("ar")
  if (n_cjk > n_latin * 0.3)  return("zh")  # 漢字がラテン文字の30%超→中国語
  if (n_latin > 3)             return("en")
  return("unknown")
}

classify_signal <- function(text, disease_tags = NA) {
  tl <- tolower(text)

  # ① 症例数による判定（消化器系疾患）
  case_n <- extract_case_count(text)
  is_gi <- !is.na(disease_tags) &&
    any(sapply(strsplit(as.character(disease_tags), ",")[[1]], function(t) trimws(t) %in% GI_DISEASE_TAGS))
  if (is_gi) {
    if (!is.na(case_n)) {
      if (case_n >= 100) return("高")
      if (case_n >= 10)  return("低")
    }
  }

  # ② 時制キーワードで「直近の流行状況」かどうか判定
  has_temporal <- any(sapply(TEMPORAL_KEYWORDS, function(k) grepl(tolower(k), tl, perl=FALSE)))

  # ③ シグナルキーワードでレベル判定
  is_high   <- any(sapply(SIGNAL_KEYWORDS$high,   function(k) grepl(tolower(k), tl, fixed=TRUE)))
  is_medium <- any(sapply(SIGNAL_KEYWORDS$medium, function(k) grepl(tolower(k), tl, fixed=TRUE)))
  is_low    <- any(sapply(SIGNAL_KEYWORDS$low,    function(k) grepl(tolower(k), tl, fixed=TRUE)))

  if (is_high)                    return("高")
  if (is_medium && has_temporal)  return("中")
  if (is_medium)                  return("低")
  if (is_low)                     return("低")

  # ④ 一般的な健康情報・予防啓発（③までで何のシグナルにも一致しなかった場合のみ
  #    「参考」とする）。実際の警報記事の多くは末尾に「手洗い・うがいの徹底を」
  #    等の定型的な予防啓発文言を含むため、この判定を③より先に行うと、
  #    「急増」「警報」「死者」等の明確な警戒シグナルを含む記事まで一律
  #    「参考」に格下げしてしまうバグがあった（2026-08-18 ユーザー指摘）。
  is_generic <- any(sapply(GENERIC_HEALTH_KEYWORDS, function(k) grepl(tolower(k), tl, fixed=TRUE)))
  if (is_generic) return("参考")

  # ⑤ 自治体等のプレスリリースに基づく症例発生報告（具体的な症例数の記載がある）は、
  #    シグナルキーワードに一致しなくても症例発生の一次情報である可能性が高いため、
  #    「参考（FYI）」ではなく最低でも「低（Signal Low）」として扱う
  if (!is.na(case_n) && case_n > 0) return("低")
  "参考"
}

# キーワード一致判定: "CRE"「HeV」「Hib」のような短い（2〜5文字）英字の略語は、
# fixed=TRUE の単純部分一致だと "increase"「screening」「two weeks」等ありふれた
# 英単語・英語表現の内部にたまたま出現して誤マッチしやすい
# （例: CRE ⊂ increase、WEE ⊂ two weeks、Hib ⊂ exhibit/inhibit）。
# そのため元表記が英数字のみで構成される短い略語（大文字小文字問わず）は
# 単語境界(\b)付き正規表現でマッチさせ、それ以外（日本語や3文字超の英語フレーズ等）は
# 従来どおりfixed文字列一致とする。
keyword_matches <- function(keyword, text_lower) {
  kw_lower <- tolower(keyword)
  is_short_token <- grepl("^[A-Za-z0-9-]{2,5}$", keyword)
  if (is_short_token) {
    grepl(paste0("\\b", kw_lower, "\\b"), text_lower, perl = TRUE)
  } else {
    grepl(kw_lower, text_lower, fixed = TRUE)
  }
}

tag_diseases <- function(text, hint = NA) {
  tl <- tolower(text)
  matched <- c()
  for (disease in names(DISEASE_KEYWORDS)) {
    if (any(sapply(DISEASE_KEYWORDS[[disease]], function(k) keyword_matches(k, tl)))) {
      matched <- c(matched, disease)
    }
  }
  if (!is.na(hint) && !(hint %in% matched)) matched <- c(matched, hint)
  if (length(matched) == 0) matched <- "other"
  paste(unique(matched), collapse=",")
}

# ── アラート文言と無関係な疾患タグの誤結び付き対策 ──────────────────
# 地方感染症情報センターの週報等は、多疾患の定点報告データを1記事にまとめて
# 掲載することが多い。tag_diseases()は記事全文からキーワード一致した疾患を
# 全部タグ付けするため、「○○病について注意報を発令」のように特定の1疾患への
# 警戒文言が、同じ記事内で定期的に数値が併記されているだけの他疾患（インフル
# エンザ等）にまで誤って結び付き、それらの疾患で絞り込んだ際にも警戒シグナル
# として表示されてしまう問題があった（2026-07-24 ユーザー指摘）。
# 対策として、シグナルがFYIでない かつ 3疾患以上がタグ付けされた記事（＝週報等の
# 多疾患まとめ記事の可能性が高い）に限り、警戒を示す語（下記）の近傍
# （前後ALERT_PROXIMITY_WINDOW文字）に疾患名キーワードが実際に出現する疾患だけを
# 残す。近傍に該当する疾患が1つも無い場合は判定不能とみなし元のタグ一覧を保持する
# （フェイルセーフ。誤って全疾患のタグを消してしまうより、従来通りの挙動に留める）。
ALERT_PROXIMITY_KEYWORDS <- c(
  "警報","注意報","急増","急拡大","流行","アウトブレイク","クラスター",
  "集団感染","集団発生","拡大","異常","重症化","死亡","警戒",
  "emergency","alert","warning","outbreak","surge","cluster"
)
ALERT_PROXIMITY_WINDOW <- 80L

# 感染症法の一類感染症等、1例の発生自体が重大事象となる希少疾患は、記事中に
# 「警報」「急増」等の定型的な警戒語が付随しないことが多い（1例の報告自体が
# ニュース価値を持つため）。これらは近傍判定の対象から除外し、タグを必ず保持する
# （2026-07-24 ユーザー指摘。エボラ関連記事でこの問題が実際に発生していた）
RARE_SINGLE_CASE_DISEASE_IDS <- c(
  "ebola", "crimean_congo", "smallpox", "south_am_hem", "plague",
  "marburg", "lassa", "polio", "diphtheria", "anthrax", "botulism",
  "tularemia", "glanders", "melioidosis"
)

restrict_disease_tags_to_alert_context <- function(text, disease_tags, signal_level) {
  tags <- trimws(strsplit(as.character(disease_tags), ",", fixed = TRUE)[[1]])
  real_tags <- tags[tags %in% names(DISEASE_KEYWORDS)]
  # 実疾患タグが1つ以下なら取り違えようがないため対象外
  if (length(real_tags) <= 1 || is.na(signal_level) ||
      as.character(signal_level) == "FYI" || is.na(text)) {
    return(disease_tags)
  }
  # 希少疾患タグが含まれる場合は、そのタグだけ無条件に保持対象へ先に確保しておく
  rare_present <- tags[tags %in% RARE_SINGLE_CASE_DISEASE_IDS]

  # tag_diseases()と同じくtolower()した上でマッチさせる（大文字小文字の違いで
  # 英語の疾患名キーワードが一致しなくなるのを防ぐ。例:"Ebola"は小文字キーワード
  # "ebola"と本来一致すべきだが、大文字小文字を区別すると一致しなくなる）
  tl <- tolower(text)
  find_positions <- function(keywords) {
    unlist(lapply(keywords, function(k) {
      m <- gregexpr(tolower(k), tl, fixed = TRUE)[[1]]
      if (m[1] == -1L) integer(0) else as.integer(m)
    }))
  }

  alert_pos <- find_positions(ALERT_PROXIMITY_KEYWORDS)
  if (length(alert_pos) == 0) return(disease_tags)  # 警戒語が無ければ判定不能→元のまま

  keep <- rare_present  # 希少疾患タグは近傍判定なしで無条件保持
  for (d in tags) {
    if (d %in% rare_present) next  # 既に保持済み
    if (!(d %in% names(DISEASE_KEYWORDS))) { keep <- c(keep, d); next }  # other/general等はそのまま保持
    disease_pos <- find_positions(DISEASE_KEYWORDS[[d]])
    if (length(disease_pos) == 0) next
    if (any(sapply(disease_pos, function(dp) any(abs(dp - alert_pos) <= ALERT_PROXIMITY_WINDOW)))) {
      keep <- c(keep, d)
    }
  }
  # フェイルセーフ: 近傍判定で実疾患タグが1つも残らなかった場合（テキストが長く
  # 警戒語と疾患名が離れている等、判定が信頼できないケース）は、疾患タグを
  # 誤って全滅させるより元の全タグを保持する方が安全なため、元のまま返す
  kept_real <- keep[keep %in% names(DISEASE_KEYWORDS)]
  if (length(kept_real) == 0) return(disease_tags)
  paste(unique(keep), collapse = ",")
}

# disease_tags（カンマ区切りの疾患IDリスト、例:"gi,other"）に特定の疾患IDが
# 含まれるかを判定する。grepl(did, disease_tags, fixed=TRUE)による単純な部分一致では
# 例えば did="gi" が "legionella"（"le-gi-onella"の中に"gi"を含む）に誤ってマッチして
# しまうため、カンマで分割した完全一致で判定する。
has_disease_tag <- function(disease_tags, did) {
  # strsplit()+vapply()で1行ずつ判定していたのをやめ、カンマ区切りタグを
  # 前後カンマ付きの正規化文字列にしてgrepl(fixed=TRUE)で一括判定する
  # （grepl自体がベクトル化されているため、行数が多いほど効果が大きい）。
  x <- ifelse(is.na(disease_tags), "", as.character(disease_tags))
  x <- paste0(",", gsub("\\s*,\\s*", ",", trimws(x)), ",")
  grepl(paste0(",", did, ","), x, fixed = TRUE)
}

signal_weight <- function(level) {
  c("Signal High"=3, "Signal Low"=2, "FYI"=0.5)[as.character(level)]
}

signal_color <- function(level) {
  c("Signal High"="#e74c3c","Signal Low"="#e67e22","FYI"="#95a5a6")[as.character(level)]
}

# ============================================================
# WHO EBS 7基準スクリーニング（再利用可能版）
# 新規取得データだけでなく、キャッシュに残る既存記事も含めた
# データフレーム全体に対して呼び出せる。screen_entry() のロジック
# 修正が、フィードの表示範囲外に出て再取得されなくなった過去記事にも
# 反映されるようにするため、マージ後の全件に対して再実行する用途を想定。
# ============================================================
rescreen_ebs_data <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(df)
  if (!exists("screen_entry", mode = "function")) return(df)

  # is_noise_article()のルール追加・修正が、フィード表示範囲外に出て再取得されなく
  # なった過去記事（既にキャッシュに入っている記事）にも遡及的に反映されるよう、
  # signal_levelの再判定と同様にここでもノイズ記事を除外する。
  # 従来この関数はscreen_entry()（signal_levelの再判定）のみを行い、ノイズ除外は
  # 新規取得時のfetch_all_ebs()側でしか適用されなかったため、is_noise_article()に
  # ルールを追加してもキャッシュ済みの既存記事は除外されないままだった
  # （実例: 2026-07-25 ユーザー指摘。研究開発資金・行政計画・追悼記事等が
  # Signal High/Lowのまま残ってしまっていた）。PubMedは対象外（学術論文の
  # タイトルにこれらのパターンが誤爆するリスクを避けるため従来通りスキップ）。
  if (exists("is_noise_article", mode = "function")) {
    is_noise <- vapply(seq_len(nrow(df)), function(i) {
      if (coalesce(df$source_id[i], "") == "pubmed") return(FALSE)
      tryCatch(isTRUE(is_noise_article(coalesce(df$title[i], ""), coalesce(df$summary[i], ""))),
               error = function(e) FALSE)
    }, logical(1))
    if (any(is_noise)) df <- df[!is_noise, , drop = FALSE]
    if (nrow(df) == 0) return(df)
  }

  fyi_result <- list(
    unusual_unexpected="", serious_ph_country="", serious_ph_japan="",
    epidemic_prone="", mass_exposure="", high_profile="", special_pathogen="",
    signal_weight="FYI", disease_category=NA_character_,
    disease_name_en=NA_character_, disease_name_ja=NA_character_,
    location=NA_character_, region=NA_character_
  )

  screen_fn <- function(i) {
    # PubMedは常にFYI（スクリーニング不要）
    if (coalesce(df$source_id[i], "") == "pubmed") return(fyi_result)
    ep <- if ("ebs_pref" %in% names(df)) coalesce(df$ebs_pref[i], NA_character_) else NA_character_
    is_ov <- is_overseas_article(
      title       = coalesce(df$title[i], ""),
      summary     = coalesce(df$summary[i], ""),
      ebs_pref    = ep,
      source_id   = coalesce(df$source_id[i], ""),
      source_name = coalesce(df$source_name[i], "")
    )
    tryCatch(
      screen_entry(
        title       = coalesce(df$title[i], ""),
        summary     = coalesce(df$summary[i], ""),
        source_id   = coalesce(df$source_id[i], ""),
        source_name = coalesce(df$source_name[i], ""),
        lang        = coalesce(df$lang[i], ""),
        is_overseas = is_ov
      ),
      error = function(e) fyi_result
    )
  }

  screen_results <- lapply(seq_len(nrow(df)), screen_fn)

  df$signal_level  <- factor(sapply(screen_results, `[[`, "signal_weight"),
                              levels = c("Signal High","Signal Low","FYI"))
  df$ebs_unusual   <- sapply(screen_results, `[[`, "unusual_unexpected")
  df$ebs_serious_c <- sapply(screen_results, `[[`, "serious_ph_country")
  df$ebs_serious_j <- sapply(screen_results, `[[`, "serious_ph_japan")
  df$ebs_epidemic  <- sapply(screen_results, `[[`, "epidemic_prone")
  df$ebs_mass      <- sapply(screen_results, `[[`, "mass_exposure")
  df$ebs_high      <- sapply(screen_results, `[[`, "high_profile")
  df$ebs_special   <- sapply(screen_results, `[[`, "special_pathogen")
  df$ebs_disease_en <- sapply(screen_results, `[[`, "disease_name_en")
  df$ebs_disease_ja <- sapply(screen_results, `[[`, "disease_name_ja")
  df$ebs_location  <- sapply(screen_results, `[[`, "location")
  df$ebs_region    <- sapply(screen_results, `[[`, "region")
  df$signal_weight <- sapply(df$signal_level, signal_weight)

  # 多疾患まとめ記事（地方感染症情報センターの週報等）で、特定1疾患への警戒文言が
  # 他の定期報告疾患にまで誤ってタグ付けされるのを防ぐ（disease_tags列がある場合のみ）
  if ("disease_tags" %in% names(df)) {
    full_text <- paste(coalesce(df$title, ""), coalesce(df$summary, ""))
    df$disease_tags <- mapply(function(txt, tags, sig) {
      tryCatch(restrict_disease_tags_to_alert_context(txt, tags, sig),
               error = function(e) tags)
    }, full_text, df$disease_tags, df$signal_level)
  }

  df
}

# ============================================================
# 全ソース統合取得
# ============================================================
fetch_all_ebs <- function(sources      = EBS_SOURCES,
                          use_gnews    = TRUE,
                          bearer_token = NULL,
                          max_age_days = 365) {
  # 固定RSS
  rss_results <- lapply(sources, fetch_rss_feed)
  all_df <- bind_rows(Filter(Negate(is.null), rss_results))

  # WHO EIOS
  eios <- tryCatch(fetch_who_eios(), error = function(e) NULL)
  if (!is.null(eios) && nrow(eios) > 0) {
    if (!"retweet_count" %in% names(eios)) eios$retweet_count <- NA_integer_
    if (!"like_count"    %in% names(eios)) eios$like_count    <- NA_integer_
    all_df <- bind_rows(all_df, eios)
  }

  # WHO Disease Outbreak News（JSON API）
  don <- tryCatch(fetch_who_don(), error = function(e) NULL)
  if (!is.null(don) && nrow(don) > 0) {
    if (!"retweet_count" %in% names(don)) don$retweet_count <- NA_integer_
    if (!"like_count"    %in% names(don)) don$like_count    <- NA_integer_
    all_df <- bind_rows(all_df, don)
  }

  # WHO Weekly Epidemiological Record（Highlighted signals and events）は、
  # それ自体を1件のEBS記事として一覧に混ぜるのではなく、他ソース由来の記事の
  # 「補強フラグ」として使う（ユーザー指示 2026-08-18）。ここでは今週WHOが
  # ハイライトした疾患IDの集合だけを取得しておき、後段のシグナル判定で
  # disease_tagsと突き合わせる
  who_wer_disease_ids <- tryCatch(get_who_wer_highlighted_diseases(), error = function(e) character(0))

  # ECDC ニュース（HTMLスクレイピング）
  ecdc <- tryCatch(fetch_ecdc_news(), error = function(e) NULL)
  if (!is.null(ecdc) && nrow(ecdc) > 0) {
    if (!"retweet_count" %in% names(ecdc)) ecdc$retweet_count <- NA_integer_
    if (!"like_count"    %in% names(ecdc)) ecdc$like_count    <- NA_integer_
    all_df <- bind_rows(all_df, ecdc)
  }

  # JIHS 新着情報（HTMLスクレイピング）
  jihs <- tryCatch(fetch_jihs_news(), error = function(e) NULL)
  if (!is.null(jihs) && nrow(jihs) > 0) {
    if (!"retweet_count" %in% names(jihs)) jihs$retweet_count <- NA_integer_
    if (!"like_count"    %in% names(jihs)) jihs$like_count    <- NA_integer_
    all_df <- bind_rows(all_df, jihs)
  }

  # 中国CDC 全球伝染病事件リスク評価（HTMLスクレイピング）
  china_cdc <- tryCatch(fetch_china_cdc_news(), error = function(e) NULL)
  if (!is.null(china_cdc) && nrow(china_cdc) > 0) {
    if (!"retweet_count" %in% names(china_cdc)) china_cdc$retweet_count <- NA_integer_
    if (!"like_count"    %in% names(china_cdc)) china_cdc$like_count    <- NA_integer_
    all_df <- bind_rows(all_df, china_cdc)
  }

  # 香港CHP プレスリリース（ヘッドレスブラウザ、ローカル専用）
  chp <- tryCatch(fetch_chp_news(), error = function(e) NULL)
  if (!is.null(chp) && nrow(chp) > 0) {
    if (!"retweet_count" %in% names(chp)) chp$retweet_count <- NA_integer_
    if (!"like_count"    %in% names(chp)) chp$like_count    <- NA_integer_
    all_df <- bind_rows(all_df, chp)
  }

  # Santé publique France（HTMLスクレイピング）
  spf <- tryCatch(fetch_france_spf_news(), error = function(e) NULL)
  if (!is.null(spf) && nrow(spf) > 0) {
    if (!"retweet_count" %in% names(spf)) spf$retweet_count <- NA_integer_
    if (!"like_count"    %in% names(spf)) spf$like_count    <- NA_integer_
    all_df <- bind_rows(all_df, spf)
  }

  # 秋田県・奈良県・佐賀県・和歌山県・福岡県・長崎県・福井県 報道発表（HTMLスクレイピング）
  for (fn in list(fetch_akita_press_news, fetch_nara_press_news, fetch_saga_press_news,
                   fetch_wakayama_news, fetch_fukuoka_news, fetch_nagasaki_news, fetch_fukui_news,
                   fetch_sakai_news, fetch_kawasaki_news, fetch_kitakyushu_news,
                   fetch_yokohama_news, fetch_kobe_news, fetch_fukuoka_kansen_news,
                   fetch_saitama_news, fetch_osaka_city_news, fetch_niigata_news, fetch_kumamoto_news,
                   fetch_koriyama_news, fetch_utsunomiya_news, fetch_asahikawa_news, fetch_hakodate_news,
                   fetch_maebashi_news, fetch_koshigaya_news, fetch_kashiwa_news,
                   fetch_fukui_city_news, fetch_nagano_city_news, fetch_takamatsu_news,
                   fetch_tsukuba_news, fetch_kanazawa_news, fetch_otsu_news, fetch_matsue_news,
                   fetch_neyagawa_news, fetch_hachinohe_news, fetch_saga_news, fetch_kawaguchi_news,
                   fetch_funabashi_news, fetch_arakawa_news, fetch_shibuya_news, fetch_shinagawa_news,
                   fetch_fujisawa_news, fetch_kansensho_news, fetch_ntv_news)) {
    d <- tryCatch(fn(), error = function(e) NULL)
    if (!is.null(d) && nrow(d) > 0) {
      if (!"retweet_count" %in% names(d)) d$retweet_count <- NA_integer_
      if (!"like_count"    %in% names(d)) d$like_count    <- NA_integer_
      all_df <- bind_rows(all_df, d)
    }
  }

  # Google News
  if (use_gnews) {
    message("Google News 取得中...")
    gn <- tryCatch(fetch_google_news_all(), error = function(e) NULL)
    if (!is.null(gn) && nrow(gn) > 0) {
      if (!"retweet_count" %in% names(gn)) gn$retweet_count <- NA_integer_
      if (!"like_count"    %in% names(gn)) gn$like_count    <- NA_integer_
      all_df <- bind_rows(all_df, gn)
    }
  }

  # PubMed
  pm <- tryCatch(fetch_pubmed(), error = function(e) { message("PubMed失敗: ", e$message); NULL })
  if (!is.null(pm) && nrow(pm) > 0) {
    if (!"retweet_count" %in% names(pm)) pm$retweet_count <- NA_integer_
    if (!"like_count"    %in% names(pm)) pm$like_count    <- NA_integer_
    all_df <- bind_rows(all_df, pm)
  }

  # X (Twitter) API
  if (!is.null(bearer_token) && nchar(trimws(bearer_token)) > 10) {
    message("X API 取得中...")
    tw <- tryCatch(
      fetch_twitter(bearer_token),
      error = function(e) { message("X API失敗: ", e$message); NULL }
    )
    if (!is.null(tw)) all_df <- bind_rows(all_df, tw)
  }

  if (nrow(all_df) == 0) return(make_demo_ebs())

  # 後処理
  hint_col <- if ("disease_hint" %in% names(all_df)) all_df$disease_hint else NA_character_

  # 感染症関連記事のみ残す（タイトル+本文に感染症キーワードが含まれるもの）
  # 「予防」「厚生労働省」等の一般語単独では、熱中症対策・認知症予防カフェ・喫煙所政策等の
  # 感染症と無関係な健康・行政記事まで拾ってしまうため、admission用キーワードからは除外する
  # （感染症名や流行・アウトブレイク等、より具体的な語のみで採否を判定する）。
  INFECT_FILTER_KEYWORDS <- c(
    "感染","infectious","infection","disease","疾患","epidemic","pandemic",
    "outbreak","アウトブレイク","流行","サーベイランス","surveillance",
    "ウイルス","virus","bacteria","細菌","病原","pathogen",
    "ワクチン","vaccine","vaccination",
    "quarantine","隔離","検疫",
    "インフルエンザ","コロナ","COVID","RSウイルス","麻疹","はしか","デング",
    "エボラ","マラリア","結核","梅毒","百日咳","エムポックス","mpox",
    # JIHSは意図的に含めない: fetch_jihs_news()で取得する記事はJIHS自身の発表であるため
    # タイトルに機関名「JIHS」がほぼ必ず含まれてしまい、受賞・調印・講座案内等の
    # 感染症と無関係な記事までフィルタを素通りしてしまう原因になっていた
    "WHO","CDC","ECDC","ProMED","公衆衛生",
    # Google Newsの検索クエリ経由で拾われやすいが、上記の一般語に含まれていなかった
    # 疾患名（誤って無関係記事が紛れ込んでいた事例を踏まえ追加）
    "カルバペネム","CRE","A型肝炎","Ａ型肝炎","hepatitis a",
    "レジオネラ","legionella","集団発生","患者が発生","感染者が発生","感染が確認"
  )

  message("後処理開始: 取得件数 ", nrow(all_df))

  all_df <- all_df %>%
    filter(!is.na(title), nchar(title) > 3,
           is.na(pub_date) | pub_date >= Sys.Date() - max_age_days) %>%
    distinct(title, pub_date, source_id, .keep_all = TRUE)

  message("重複除去後: ", nrow(all_df), " 件")

  all_df <- all_df %>%
    mutate(
      full_text    = paste(coalesce(title,""), coalesce(summary,"")),
      # Google Newsはtitle/summary双方の末尾にメディア名サフィックス（「日本経済新聞」
      # 「東京新聞」等）を含むため、地名・言語判定の前に除去する
      title_body   = mapply(strip_gnews_suffix, coalesce(title,""), coalesce(source_id,"")),
      summary_body = mapply(strip_gnews_suffix, coalesce(summary,""), coalesce(source_id,"")),
      lang         = mapply(detect_lang, title_body),
      disease_tags = mapply(tag_diseases, full_text,
                            if ("disease_hint" %in% names(.)) disease_hint else NA),
      ebs_pref     = mapply(detect_pref,
                            title_body, summary_body,
                            coalesce(source_name,""), coalesce(link,"")),
      retweet_count = if ("retweet_count" %in% names(.)) retweet_count else NA_integer_,
      like_count    = if ("like_count"    %in% names(.)) like_count    else NA_integer_
    ) %>%
    filter(
      # disease_tagsがGoogle Newsの検索クエリ（disease_hint）由来のタグのみで、
      # 記事本文に実際の感染症関連語が一切含まれない場合（検索結果に紛れ込んだ無関係記事）
      # を除外するため、disease_tagsの有無に関わらず必ず感染症関連語の有無を確認する
      # （以前はdisease_tagsが付与されていればこのチェックを素通りしてしまい、
      # 　クレジットカード会社の破産・都政コラム等の無関係記事が誤って
      # 　CRE・A型肝炎・結核等のタグ付きで紛れ込む原因になっていた）
      vapply(full_text, function(txt) {
        tryCatch({
          tl <- tolower(txt)
          isTRUE(any(sapply(INFECT_FILTER_KEYWORDS, function(kw) keyword_matches(kw, tl))))
        }, error = function(e) FALSE)
      }, FUN.VALUE = logical(1), USE.NAMES = FALSE)
    ) %>%
    filter(
      # 個々の記事本文の文字化け・異常な内容等で is_noise_article が予期せぬエラーを
      # 起こしても、1件のせいでEBS取得処理全体がクラッシュしないよう行単位で保護する
      !vapply(seq_len(nrow(.)), function(i)
        tryCatch(isTRUE(is_noise_article(coalesce(title[i], ""), coalesce(summary[i], ""))),
                 error = function(e) FALSE),
        FUN.VALUE = logical(1))
    )

  message("感染症フィルタ後: ", nrow(all_df), " 件")

  # WHO EBS 7基準ルールベーススクリーニング → signal_level を決定
  if (exists("screen_entry", mode = "function") && nrow(all_df) > 0) {
    message("EBSスクリーニング開始: ", nrow(all_df), " 件")
    all_df <- rescreen_ebs_data(all_df)
    message("EBSスクリーニング完了")
  } else {
    all_df$signal_level  <- factor("FYI", levels = c("Signal High","Signal Low","FYI"))
    all_df$signal_weight <- sapply(all_df$signal_level, signal_weight)
  }

  # WHO WER補強フラグ: 今週WHOがHighlighted signalsとして取り上げた疾患を
  # disease_tagsに含む記事は、シグナルレベルを1段階引き上げる
  # （FYI→Signal Low→Signal High）。WER自体は記事として一覧に加えない
  # （ユーザー指示 2026-08-18）。
  if (length(who_wer_disease_ids) > 0 && nrow(all_df) > 0) {
    who_wer_flag <- vapply(all_df$disease_tags, function(tags) {
      any(strsplit(as.character(tags), ",")[[1]] %in% who_wer_disease_ids)
    }, logical(1))
    if (any(who_wer_flag)) {
      lv <- as.character(all_df$signal_level)
      bump <- c("FYI" = "Signal Low", "Signal Low" = "Signal High", "Signal High" = "Signal High")
      lv[who_wer_flag] <- unname(bump[lv[who_wer_flag]])
      all_df$signal_level <- factor(lv, levels = c("Signal High","Signal Low","FYI"))
      all_df$signal_weight <- sapply(all_df$signal_level, signal_weight)
      all_df$summary[who_wer_flag] <- paste0(
        "【WHO WERで今週ハイライトされたシグナルと一致】", all_df$summary[who_wer_flag])
    }
  }

  all_df <- all_df %>%
    arrange(signal_level, desc(pub_date)) %>%
    select(source_id, source_name, category, lang, title, link,
           pub_date, summary, signal_level, signal_weight, disease_tags,
           retweet_count, like_count,
           starts_with("ebs_"))

  all_df
}

# ============================================================
# Google Trends 取得
# ============================================================

# 疾患別検索キーワード（日本語）
GTRENDS_KEYWORDS <- list(
  # 定点把握
  flu       = "インフルエンザ",
  rsv       = "RSウイルス",
  covid     = "新型コロナウイルス",
  hfmd      = "手足口病",
  mycop     = "マイコプラズマ肺炎",
  varicella = "水痘 水ぼうそう",
  mumps     = "おたふくかぜ",
  gi        = "感染性胃腸炎",
  # 全数把握
  measles   = "麻疹 はしか",
  rubella   = "風疹",
  pertussis = "百日咳",
  syphilis  = "梅毒",
  ehec      = "O157 腸管出血性大腸菌",
  dengue    = "デング熱",
  mpox      = "エムポックス"
)

# 都道府県 → gtrendsR geo コード (JP-XX)
PREF_GEO_MAP <- c(
  "全国"     = "JP",
  "北海道"   = "JP-01", "青森県" = "JP-02", "岩手県" = "JP-03",
  "宮城県"   = "JP-04", "秋田県" = "JP-05", "山形県" = "JP-06",
  "福島県"   = "JP-07", "茨城県" = "JP-08", "栃木県" = "JP-09",
  "群馬県"   = "JP-10", "埼玉県" = "JP-11", "千葉県" = "JP-12",
  "東京都"   = "JP-13", "神奈川県"="JP-14", "新潟県" = "JP-15",
  "富山県"   = "JP-16", "石川県" = "JP-17", "福井県" = "JP-18",
  "山梨県"   = "JP-19", "長野県" = "JP-20", "岐阜県" = "JP-21",
  "静岡県"   = "JP-22", "愛知県" = "JP-23", "三重県" = "JP-24",
  "滋賀県"   = "JP-25", "京都府" = "JP-26", "大阪府" = "JP-27",
  "兵庫県"   = "JP-28", "奈良県" = "JP-29", "和歌山県"="JP-30",
  "鳥取県"   = "JP-31", "島根県" = "JP-32", "岡山県" = "JP-33",
  "広島県"   = "JP-34", "山口県" = "JP-35", "徳島県" = "JP-36",
  "香川県"   = "JP-37", "愛媛県" = "JP-38", "高知県" = "JP-39",
  "福岡県"   = "JP-40", "佐賀県" = "JP-41", "長崎県" = "JP-42",
  "熊本県"   = "JP-43", "大分県" = "JP-44", "宮崎県" = "JP-45",
  "鹿児島県" = "JP-46", "沖縄県" = "JP-47"
)

gtrends_cache_path <- function(geo = "JP") {
  file.path("data", paste0("gtrends_cache_", gsub("-", "_", geo), ".rds"))
}

fetch_google_trends <- function(keywords = GTRENDS_KEYWORDS,
                                geo = "JP",
                                time = "today 12-m",
                                force = FALSE) {
  cache_file <- gtrends_cache_path(geo)
  # キャッシュ確認（6時間以内なら再利用）
  if (!force && file.exists(cache_file)) {
    cache_age <- as.numeric(difftime(Sys.time(),
      file.info(cache_file)$mtime, units = "hours"))
    if (cache_age < 6) {
      tryCatch(return(readRDS(cache_file)), error = function(e) NULL)
    }
  }

  if (!requireNamespace("gtrendsR", quietly = TRUE)) {
    message("gtrendsR パッケージが必要です")
    return(NULL)
  }

  # 5キーワードずつ分割して取得（GTrendsの上限対策）
  kw_vec   <- unlist(keywords)
  kw_names <- names(kw_vec)
  chunks   <- split(seq_along(kw_vec), ceiling(seq_along(kw_vec) / 5))

  rate_limited <- FALSE
  all_iot <- lapply(seq_along(chunks), function(ci) {
    if (rate_limited) return(NULL)
    idx <- chunks[[ci]]
    if (ci > 1) Sys.sleep(runif(1, 3, 6))  # チャンク間に3〜6秒待機
    tryCatch({
      res <- gtrendsR::gtrends(
        keyword = kw_vec[idx],
        geo     = geo,
        time    = time,
        onlyInterest = TRUE
      )
      iot <- res$interest_over_time
      if (is.null(iot) || nrow(iot) == 0) return(NULL)
      kw_to_id <- setNames(kw_names[idx], kw_vec[idx])
      iot %>%
        dplyr::mutate(
          date       = as.Date(date),
          hits       = suppressWarnings(as.integer(hits)),
          disease_id = kw_to_id[keyword]
        ) %>%
        dplyr::select(date, keyword, disease_id, hits) %>%
        dplyr::filter(!is.na(hits))
    }, error = function(e) {
      msg <- e$message
      if (grepl("429", msg)) {
        message("Google Trends レート制限（429）: キャッシュを使用します")
        rate_limited <<- TRUE
      } else {
        message("Google Trends エラー: ", msg)
      }
      NULL
    })
  })

  result <- dplyr::bind_rows(Filter(Negate(is.null), all_iot))

  # 429でキャッシュが存在すればそちらを返す
  if (rate_limited && file.exists(cache_file)) {
    message("レート制限のため既存キャッシュを返します")
    return(tryCatch(readRDS(cache_file), error = function(e) NULL))
  }

  if (nrow(result) > 0) {
    dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
    saveRDS(result, cache_file)
  }
  if (nrow(result) == 0) NULL else result
}

# ============================================================
# EBSシグナル週次集計（IBS相関分析用）
# ============================================================
aggregate_ebs_weekly <- function(ebs_data, disease_filter = NULL) {
  d <- ebs_data
  if (!is.null(disease_filter) && disease_filter != "all") {
    d <- d %>% filter(grepl(disease_filter, disease_tags, fixed=TRUE))
  }
  d %>%
    mutate(
      year = year(pub_date),
      week = isoweek(pub_date)
    ) %>%
    group_by(year, week) %>%
    summarise(
      ebs_count         = n(),
      ebs_signal_index  = sum(signal_weight, na.rm=TRUE),
      ebs_high_count    = sum(signal_level == "Signal High"),
      ebs_medium_count  = sum(signal_level == "Signal Low"),
      .groups = "drop"
    ) %>%
    mutate(date = as.Date(paste0(year, "-W", sprintf("%02d", week), "-1"),
                          format = "%Y-W%W-%u"))
}

# ============================================================
# デモデータ（RSS取得失敗時）
# ============================================================
make_demo_ebs <- function() {
  items <- list(
    list(src="厚生労働省",cat="行政",lang="ja",sig="Signal Low",dis="flu,general",
         title="インフルエンザの流行状況について（第25週）",
         link="https://www.mhlw.go.jp/stf/seisakunitsuite/bunya/kenkou_iryou/kenkou/kekkaku-kansenshou/index.html",
         summ="全国の定点医療機関からの報告によると、インフルエンザの定点あたり報告数は前週比1.3倍となり、警戒基準に近づいています。特に北海道・東北地方で高い報告数が見られます。",
         days=0),
    list(src="WHO DON",cat="国際",lang="en",sig="FYI",dis="flu",
         title="Influenza update — 2025, week 25",
         link="https://www.who.int/teams/global-influenza-programme",
         summ="Global influenza activity remains elevated. H3N2 is predominant in the Northern Hemisphere. Southern Hemisphere countries are entering their influenza season.",
         days=1),
    list(src="ProMED Mail",cat="国際",lang="en",sig="Signal Low",dis="covid",
         title="COVID-19 Update: New Subvariant XEC Detected in Multiple Countries",
         link="https://promedmail.org/",
         summ="A new Omicron subvariant XEC has been detected with potential immune escape characteristics. WHO is monitoring the situation. Cases reported in 12 countries.",
         days=2),
    list(src="NHK 健康・医療",cat="メディア",lang="ja",sig="Signal Low",dis="rsv",
         title="RSウイルス感染症が増加傾向　乳幼児に注意",
         link="https://www3.nhk.or.jp/news/",
         summ="RSウイルス感染症の報告数が増加しており、特に0〜2歳の乳幼児で重症化リスクに注意が必要です。複数の小児科医が入院患者の増加を報告しています。",
         days=3),
    list(src="ECDC",cat="国際",lang="en",sig="FYI",dis="measles,flu",
         title="Communicable disease threats report, week 26, 2025",
         link="https://www.ecdc.europa.eu/en/threats-and-outbreaks",
         summ="Measles outbreaks reported in 3 EU/EEA countries. Influenza B Victoria lineage co-circulating with A/H3N2. Dengue cases increasing in southern Europe.",
         days=5),
    list(src="JIHS",cat="研究機関",lang="ja",sig="FYI",dis="flu,covid,rsv,ari",
         title="感染症発生動向調査週報（IDWR）第25週速報",
         link="https://www.niid.jihs.go.jp/",
         summ="第25週（6月16日〜6月22日）のIDWR速報データが公開されました。インフルエンザ・COVID-19・RSウイルスの状況を報告します。",
         days=7),
    list(src="Google News",cat="ニュース",lang="ja",sig="Signal High",dis="flu,general",
         title="インフルエンザ感染者急増、都内の病院でベッド不足の懸念",
         link="https://news.google.com/",
         summ="東京都内の複数の病院でインフルエンザによる入院患者が急増しており、一部の医療機関でベッドの逼迫が報告されています。",
         days=0),
    list(src="Google News",cat="ニュース",lang="ja",sig="Signal Low",dis="covid",
         title="新型コロナ感染者数が4週連続増加　夏の第12波か",
         link="https://news.google.com/",
         summ="厚生労働省の発表によると、新型コロナウイルスの定点把握報告数が4週連続で増加しており、専門家は夏の感染拡大波の可能性を指摘しています。",
         days=1),
    list(src="X (Twitter)",cat="SNS",lang="ja",sig="Signal High",dis="flu",
         title="@NHK_Health: 【速報】今週のインフルエンザ報告数が警戒水準を超えました",
         link="https://twitter.com/",
         summ="今週のインフルエンザ定点あたり報告数が警戒水準（10.0）を超えたことが確認されました #感染症 #インフルエンザ",
         days=0),
    list(src="X (Twitter)",cat="SNS",lang="ja",sig="FYI",dis="rsv,ari",
         title="@kenkoujoho: RSウイルス感染症に関する注意喚起",
         link="https://twitter.com/",
         summ="秋から冬にかけてRSウイルス感染症が増加する時期です。乳幼児のいるご家庭は手洗い・換気を徹底してください #RSウイルス #感染症予防",
         days=2)
  )

  bind_rows(lapply(items, function(x) {
    tibble(
      source_id=x$src, source_name=x$src, category=x$cat, lang=x$lang,
      title=x$title, link=x$link,
      pub_date=Sys.Date()-x$days, summary=x$summ,
      signal_level=factor(x$sig, levels=c("Signal High","Signal Low","FYI")),
      signal_weight=signal_weight(x$sig),
      disease_tags=x$dis,
      retweet_count=NA_integer_, like_count=NA_integer_
    )
  }))
}