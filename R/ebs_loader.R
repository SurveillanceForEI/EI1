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

# 国際ソース：日本キーワードがなければ原則海外
.OVERSEAS_SOURCE_IDS_LOADER <- c("reliefweb")

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
    for (w in .COUNTRY_MATCH_FALSE_POSITIVE_WORDS) s <- gsub(w, "", s, fixed = TRUE)
    s
  }
  title_low <- strip_fp(tolower(title_body))
  title_has_overseas <- any(sapply(.OVERSEAS_KW_LOADER,
    function(k) grepl(k, title_low, fixed = TRUE)))
  txt <- if (title_has_overseas) title_low else strip_fp(tolower(paste(title_body, summary_body)))

  has_japan    <- any(sapply(.JAPAN_KW_LOADER,    function(k) grepl(k, txt, fixed = TRUE)))
  has_overseas <- any(sapply(.OVERSEAS_KW_LOADER, function(k) grepl(k, txt, fixed = TRUE)))

  # メディア名自体が海外を示す場合（例:「Vietnam.vn」「CGTN」等）も海外シグナルとして扱う。
  # 記事本文が現地語の翻訳等で国名に言及しないケース（Vietnam.vn等の現地メディアがベトナム
  # 国内向け記事をそのまま配信している場合）を拾うため。Google News経由はsource_name列が
  # 常に"Google News"固定なので、titleサフィックスから元メディア名を抽出して判定する。
  # ただしメディア名の「日本」等は（日本経済新聞・ニューズウィーク日本版等と同様）
  # 国内シグナルとしては使わない。
  gnews_media   <- extract_gnews_media_name(title, source_id)
  media_low     <- tolower(paste(coalesce(source_name, ""), gnews_media))
  media_has_overseas <- any(sapply(.OVERSEAS_KW_LOADER,
    function(k) grepl(k, media_low, fixed = TRUE)))

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
  # 政令指定都市・中核市（保健所設置自治体）は継続調査中
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
  list(id="cidrap",  name="CIDRAP",                  lang="en", category="研究機関",
       url="https://www.cidrap.umn.edu/rss.xml"),
  list(id="ont",     name="Outbreak News Today",     lang="en", category="研究機関",
       url="https://outbreaknewstoday.com/feed/"),
  # ── 日本メディア ────────────────────────────────────────────
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
extract_case_count <- function(text) {
  # 日本語: 「XX人」「XX名」「XX例」「XX件」「XX患者」
  # 英語: "XX cases" "XX patients" "XX people"
  patterns <- c(
    "(\\d[\\d,]+)\\s*人(?:が|の|以上|超)",
    "(\\d[\\d,]+)\\s*名(?:が|の|以上|超)",
    "(\\d[\\d,]+)\\s*例(?:が|の|以上|超|報告)",
    "(\\d[\\d,]+)\\s*件(?:が|の|以上|超|報告)",
    "(\\d[\\d,]+)\\s*人?の患者",
    "(\\d[\\d,]+)\\s*cases?",
    "(\\d[\\d,]+)\\s*patients?",
    "(\\d[\\d,]+)\\s*people\\s*(infected|affected|sick)"
  )
  nums <- c()
  for (pat in patterns) {
    m <- regmatches(text, gregexpr(pat, text, perl=TRUE, ignore.case=TRUE))[[1]]
    if (length(m) > 0) {
      extracted <- gsub("[^0-9]", "", regmatches(m, regexpr("\\d[\\d,]*", m, perl=TRUE)))
      extracted <- as.numeric(gsub(",", "", extracted))
      nums <- c(nums, extracted[!is.na(extracted)])
    }
  }
  if (length(nums) == 0) return(NA_real_)
  max(nums)  # 記事中の最大値を採用
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
  title_has_overseas <- any(sapply(.OVERSEAS_KW_LOADER,
    function(k) grepl(k, tolower(title), fixed = TRUE)))

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

  # ① 一般的な健康情報・予防啓発 → 問答無用で「参考」
  is_generic <- any(sapply(GENERIC_HEALTH_KEYWORDS, function(k) grepl(tolower(k), tl, fixed=TRUE)))
  if (is_generic) return("参考")

  # ② 症例数による判定（消化器系疾患）
  case_n <- extract_case_count(text)
  is_gi <- !is.na(disease_tags) &&
    any(sapply(strsplit(as.character(disease_tags), ",")[[1]], function(t) trimws(t) %in% GI_DISEASE_TAGS))
  if (is_gi) {
    if (!is.na(case_n)) {
      if (case_n >= 100) return("高")
      if (case_n >= 10)  return("低")
    }
  }

  # ③ 時制キーワードで「直近の流行状況」かどうか判定
  has_temporal <- any(sapply(TEMPORAL_KEYWORDS, function(k) grepl(tolower(k), tl, perl=FALSE)))

  # ④ シグナルキーワードでレベル判定
  is_high   <- any(sapply(SIGNAL_KEYWORDS$high,   function(k) grepl(tolower(k), tl, fixed=TRUE)))
  is_medium <- any(sapply(SIGNAL_KEYWORDS$medium, function(k) grepl(tolower(k), tl, fixed=TRUE)))
  is_low    <- any(sapply(SIGNAL_KEYWORDS$low,    function(k) grepl(tolower(k), tl, fixed=TRUE)))

  if (is_high)                    return("高")
  if (is_medium && has_temporal)  return("中")
  if (is_medium)                  return("低")
  if (is_low)                     return("低")

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

# disease_tags（カンマ区切りの疾患IDリスト、例:"gi,other"）に特定の疾患IDが
# 含まれるかを判定する。grepl(did, disease_tags, fixed=TRUE)による単純な部分一致では
# 例えば did="gi" が "legionella"（"le-gi-onella"の中に"gi"を含む）に誤ってマッチして
# しまうため、カンマで分割した完全一致で判定する。
has_disease_tag <- function(disease_tags, did) {
  vapply(strsplit(as.character(disease_tags), ",", fixed = TRUE), function(tags) {
    did %in% trimws(tags)
  }, logical(1))
}

signal_weight <- function(level) {
  c("Signal High"=3, "Signal Low"=2, "FYI"=0.5)[as.character(level)]
}

signal_color <- function(level) {
  c("Signal High"="#e74c3c","Signal Low"="#e67e22","FYI"="#95a5a6")[as.character(level)]
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
                   fetch_wakayama_news, fetch_fukuoka_news, fetch_nagasaki_news, fetch_fukui_news)) {
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
    "WHO","CDC","ECDC","JIHS","ProMED","公衆衛生",
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

    fyi_result <- list(
      unusual_unexpected="", serious_ph_country="", serious_ph_japan="",
      epidemic_prone="", mass_exposure="", high_profile="", special_pathogen="",
      signal_weight="FYI", disease_category=NA_character_,
      disease_name_en=NA_character_, disease_name_ja=NA_character_,
      location=NA_character_, region=NA_character_
    )

    screen_fn <- function(i) {
      # PubMedは常にFYI（スクリーニング不要）
      if (coalesce(all_df$source_id[i], "") == "pubmed") return(fyi_result)
      # 海外記事かどうかを判定（ebs_pref + テキスト地名）
      ep <- if ("ebs_pref" %in% names(all_df)) coalesce(all_df$ebs_pref[i], NA_character_) else NA_character_
      is_ov <- is_overseas_article(
        title       = coalesce(all_df$title[i], ""),
        summary     = coalesce(all_df$summary[i], ""),
        ebs_pref    = ep,
        source_id   = coalesce(all_df$source_id[i], ""),
        source_name = coalesce(all_df$source_name[i], "")
      )
      tryCatch(
        screen_entry(
          title       = coalesce(all_df$title[i], ""),
          summary     = coalesce(all_df$summary[i], ""),
          source_id   = coalesce(all_df$source_id[i], ""),
          source_name = coalesce(all_df$source_name[i], ""),
          lang        = coalesce(all_df$lang[i], ""),
          is_overseas = is_ov
        ),
        error = function(e) list(
          unusual_unexpected="", serious_ph_country="", serious_ph_japan="",
          epidemic_prone="", mass_exposure="", high_profile="", special_pathogen="",
          signal_weight="FYI", disease_category=NA_character_,
          disease_name_en=NA_character_, disease_name_ja=NA_character_,
          location=NA_character_, region=NA_character_
        )
      )
    }

    screen_results <- lapply(seq_len(nrow(all_df)), screen_fn)

    message("EBSスクリーニング完了")
    all_df$signal_level  <- factor(sapply(screen_results, `[[`, "signal_weight"),
                                   levels = c("Signal High","Signal Low","FYI"))
    all_df$ebs_unusual   <- sapply(screen_results, `[[`, "unusual_unexpected")
    all_df$ebs_serious_c <- sapply(screen_results, `[[`, "serious_ph_country")
    all_df$ebs_serious_j <- sapply(screen_results, `[[`, "serious_ph_japan")
    all_df$ebs_epidemic  <- sapply(screen_results, `[[`, "epidemic_prone")
    all_df$ebs_mass      <- sapply(screen_results, `[[`, "mass_exposure")
    all_df$ebs_high      <- sapply(screen_results, `[[`, "high_profile")
    all_df$ebs_special   <- sapply(screen_results, `[[`, "special_pathogen")
    all_df$ebs_disease_en <- sapply(screen_results, `[[`, "disease_name_en")
    all_df$ebs_disease_ja <- sapply(screen_results, `[[`, "disease_name_ja")
    all_df$ebs_location  <- sapply(screen_results, `[[`, "location")
    all_df$ebs_region    <- sapply(screen_results, `[[`, "region")
  } else {
    all_df$signal_level <- factor("FYI", levels = c("Signal High","Signal Low","FYI"))
  }

  all_df$signal_weight <- sapply(all_df$signal_level, signal_weight)

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