# NAS上のClaude収集EBS CSV（国内: DomesticEBS_YYYYMMDD.csv / 国外: OverseasEBS_YYYYMMDD.csv）を
# EBSニュースのカード用データ（ebs_startup_cache.rds と同じ列構成）に変換して取り込む。
#
# - シグナル（Signal分類）・地域・都道府県・7基準（Unusual等）はCSVの値をそのまま使う
#   （Rスクレイピング側の再スクリーニングで上書きされないよう、source_idを claude_* にして
#     rescreen_ebs_data()/ノイズ除去の対象外にする）
# - 国内/国外の振り分けはCSVの「国内か国外か」を csv_scope 列に保持し、アプリ側で優先使用する
# - CSVのファイル名の日付は掲載日より1日ずれるため、日付は必ず「情報掲載日」を使う
# - 重複は、同一URL・正規化タイトル一致・タイトル類似（文字2-gramのJaccard）で除く。
#   Rスクレイピング由来の既存行と重複する場合は、シグナル・地域が付いているCSV側を残す
#
# 前提: R/ebs_rule_screening.R, R/ebs_loader.R（tag_diseases, signal_weight, classify_location）を
#       source済みであること

CLAUDE_CSV_DIR_CANDIDATES <- c(
  "//192.168.132.2/public1/episynq/japan_surveillance/data/EBSchatGPT",
  "//episynq-NAS1/public1/episynq/japan_surveillance/data/EBSchatGPT",
  "data/EBSchatGPT"
)

.cc_pick_dir <- function(dirs = CLAUDE_CSV_DIR_CANDIDATES) {
  for (d in dirs) if (dir.exists(d)) return(d)
  NULL
}

.cc_norm_url <- function(u) {
  u <- tolower(trimws(u))
  u <- sub("^https?://(www\\.)?", "", u)
  u <- sub("[?#].*$", "", u)
  sub("/+$", "", u)
}

.cc_urls <- function(x) {
  if (is.na(x)) return(character(0))
  u <- regmatches(x, gregexpr("https?://[^[:space:]]+", x))[[1]]
  unique(.cc_norm_url(u))
}

.cc_norm_title <- function(t) gsub("[[:space:][:punct:]　、。・：；「」『』（）【】［］〈〉《》―－ー…！？]", "", tolower(t))

.cc_bigrams <- function(s) {
  n <- nchar(s)
  if (n < 2) return(s)
  unique(substring(s, 1:(n - 1), 2:n))
}

.cc_jaccard <- function(a, b) {
  if (length(a) == 0 || length(b) == 0) return(0)
  length(intersect(a, b)) / length(union(a, b))
}

.cc_flag <- function(x) ifelse(!is.na(x) & grepl("^該当$", trimws(x)), "✓", "")

# ── 情報源サイト名 ─────────────────────────────────────────────
# カードの出典欄には「Official」「Media (Official sourceなし)」のような分類ではなく、
# 情報源のサイト名を出す。URLのドメインから、既知のサイトは名称に、未知のサイトはドメイン名にする。
# Yahoo!ニュース・livedoor等の配信サイトは、見出し末尾の「（媒体名）」を優先して使う。
.CC_SITE_NAMES <- c(
  "who.int" = "WHO", "afro.who.int" = "WHOアフリカ地域事務局", "emro.who.int" = "WHO東地中海地域事務局",
  "cdc.gov" = "米国CDC", "ecdc.europa.eu" = "ECDC", "europa.eu" = "欧州連合", "cidrap.umn.edu" = "CIDRAP",
  "afludiary.blogspot.com" = "Avian Flu Diary", "gov.uk" = "英国政府（UKHSA等）", "reliefweb.int" = "ReliefWeb",
  "polioeradication.org" = "GPEI", "msf.org" = "国境なき医師団", "kdca.go.kr" = "韓国疾病管理庁",
  "cdc.gov.tw" = "台湾CDC", "chp.gov.hk" = "香港CHP", "chinacdc.cn" = "中国CDC", "nicd.ac.za" = "南アフリカNICD",
  "rki.de" = "ドイツRKI", "santepubliquefrance.fr" = "フランス公衆衛生局", "promedmail.org" = "ProMED",
  "reuters.com" = "Reuters", "apnews.com" = "AP通信", "bbc.com" = "BBC", "bbc.co.uk" = "BBC", "cnn.com" = "CNN",
  "aljazeera.com" = "Al Jazeera", "politico.eu" = "POLITICO", "politico.com" = "POLITICO", "statnews.com" = "STAT",
  "healthpolicy-watch.news" = "Health Policy Watch", "thelancet.com" = "The Lancet", "bmj.com" = "BMJ",
  "mhlw.go.jp" = "厚生労働省", "jihs.go.jp" = "JIHS（国立健康危機管理研究機構）", "niid.go.jp" = "国立感染症研究所",
  "nhk.or.jp" = "NHK", "kyodonews.net" = "共同通信", "nikkei.com" = "日本経済新聞", "asahi.com" = "朝日新聞",
  "mainichi.jp" = "毎日新聞", "yomiuri.co.jp" = "読売新聞", "sankei.com" = "産経新聞", "jiji.com" = "時事通信",
  "japantimes.co.jp" = "The Japan Times", "fnn.jp" = "FNN", "tbs.co.jp" = "TBS", "ntv.co.jp" = "日本テレビ",
  "minyu-net.com" = "福島民友新聞", "fukushima-minpo.co.jp" = "福島民報", "nnn.co.jp" = "日本海新聞",
  "hokkaido-np.co.jp" = "北海道新聞", "kahoku.news" = "河北新報", "hokkoku.co.jp" = "北國新聞",
  "niigata-nippo.co.jp" = "新潟日報", "chunichi.co.jp" = "中日新聞", "kobe-np.co.jp" = "神戸新聞",
  "kyoto-np.co.jp" = "京都新聞", "sanyonews.jp" = "山陽新聞", "chugoku-np.co.jp" = "中国新聞",
  "nishinippon.co.jp" = "西日本新聞", "ryukyushimpo.jp" = "琉球新報", "okinawatimes.co.jp" = "沖縄タイムス",
  "yahoo.co.jp" = "Yahoo!ニュース", "news.livedoor.com" = "livedoorニュース", "msn.com" = "MSN",
  "news.google.com" = "Google ニュース", "topics.smt.docomo.ne.jp" = "dメニューニュース"
)

.CC_PREF_ROMAJI <- c(
  hokkaido = "北海道", aomori = "青森県", iwate = "岩手県", miyagi = "宮城県", akita = "秋田県", yamagata = "山形県",
  fukushima = "福島県", ibaraki = "茨城県", tochigi = "栃木県", gunma = "群馬県", saitama = "埼玉県", chiba = "千葉県",
  tokyo = "東京都", kanagawa = "神奈川県", niigata = "新潟県", toyama = "富山県", ishikawa = "石川県", fukui = "福井県",
  yamanashi = "山梨県", nagano = "長野県", gifu = "岐阜県", shizuoka = "静岡県", aichi = "愛知県", mie = "三重県",
  shiga = "滋賀県", kyoto = "京都府", osaka = "大阪府", hyogo = "兵庫県", nara = "奈良県", wakayama = "和歌山県",
  tottori = "鳥取県", shimane = "島根県", okayama = "岡山県", hiroshima = "広島県", yamaguchi = "山口県",
  tokushima = "徳島県", kagawa = "香川県", ehime = "愛媛県", kochi = "高知県", fukuoka = "福岡県", saga = "佐賀県",
  nagasaki = "長崎県", kumamoto = "熊本県", oita = "大分県", miyazaki = "宮崎県", kagoshima = "鹿児島県", okinawa = "沖縄県"
)

# 市のドメイン名（city.<slug>.…）→ 市名。R取得側のソース定義（EBS_SOURCES等）の名称から作る
.cc_city_names <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    out <- character(0)
    add <- function(url, name) {
      slug <- regmatches(url, regexec("city\\.([a-z0-9-]+)\\.", url))[[1]]
      nm <- sub("[（( 　].*$", "", name)
      if (length(slug) == 2 && nzchar(nm)) out[slug[2]] <<- nm
    }
    out[c("hakodate", "kuki", "kumamoto", "yokohama", "chiba", "nagasaki", "kagoshima", "himeji", "nagoya", "tottori",
          "sapporo", "kitakyushu", "kurume", "sagamihara", "fujisawa", "chigasaki", "hirakata", "fukuyama", "toshima")] <-
      c("函館市", "久喜市", "熊本市", "横浜市", "千葉市", "長崎市", "鹿児島市", "姫路市", "名古屋市", "鳥取市",
        "札幌市", "北九州市", "久留米市", "相模原市", "藤沢市", "茅ヶ崎市", "枚方市", "福山市", "豊島区")
    if (exists("EBS_SOURCES")) for (s in EBS_SOURCES) if (!is.null(s$url) && !is.null(s$name)) add(s$url, s$name)
    f <- "R/ebs_loader.R"   # 各スクリプトは作業フォルダをプロジェクトルートにしてから呼ぶ
    f <- if (file.exists(f)) f else NA_character_
    if (!is.na(f)) {
      txt <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
      m <- regmatches(txt, gregexpr("\"https?://[^\"]*city\\.[^\"]*\"\\s*,\\s*\"city_[a-z0-9_]+\"\\s*,\\s*\"[^\"]+\"", txt))[[1]]
      for (z in m) { p <- strsplit(z, "\"")[[1]]; add(p[2], p[6]) }
    }
    cache <<- out
    out
  }
})

# ホスト名 -> サイト名（長いキーを優先して末尾一致）。都道府県・市の公式サイトは自治体名に、
# それ以外の未知のサイトはwww.を除いたホスト名にする
.cc_host_name <- function(url) {
  host <- tolower(sub("^https?://([^/:?#]+).*$", "\\1", url))
  host <- sub("^www[0-9]?\\.", "", host)
  keys <- names(.CC_SITE_NAMES)[order(-nchar(names(.CC_SITE_NAMES)))]
  hit <- keys[vapply(keys, function(k) host == k || endsWith(host, paste0(".", k)), logical(1))]
  if (length(hit)) return(unname(.CC_SITE_NAMES[hit[1]]))
  pm <- regmatches(host, regexec("(^|\\.)pref\\.([a-z]+)\\.", host))[[1]]
  if (length(pm) == 3 && pm[3] %in% names(.CC_PREF_ROMAJI)) return(unname(.CC_PREF_ROMAJI[pm[3]]))
  if (grepl("(^|\\.)metro\\.tokyo\\.", host)) return("東京都")
  cm <- regmatches(host, regexec("(^|\\.)city\\.([a-z0-9-]+)\\.", host))[[1]]
  if (length(cm) == 3) { nm <- .cc_city_names()[cm[3]]; if (!is.na(nm)) return(unname(nm)) }
  if (nzchar(host)) host else "情報源不明"
}

# 1記事分: 情報源URL群と情報源タイトル群から、出典欄に出すサイト名（最大2件＋ほかN件）を作る
.cc_source_label <- function(urls_txt, titles_txt) {
  if (is.na(urls_txt)) return("情報源不明")
  urls <- regmatches(urls_txt, gregexpr("https?://[^[:space:]]+", urls_txt))[[1]]
  if (!length(urls)) return("情報源不明")
  tt <- if (is.na(titles_txt)) character(0) else strsplit(titles_txt, "\n")[[1]]
  names_i <- vapply(seq_along(urls), function(i) {
    nm <- .cc_host_name(urls[i])
    if (grepl("yahoo\\.co\\.jp|livedoor|msn\\.com|news\\.google", urls[i]) && i <= length(tt)) {
      m <- regmatches(tt[i], regexec("[（(]([^（）()]{2,25})[）)]\\s*$", tt[i]))[[1]]
      if (length(m) == 2) return(m[2])
    }
    nm
  }, character(1))
  u <- unique(names_i)
  if (length(u) <= 2) paste(u, collapse = "、") else paste0(paste(u[1:2], collapse = "、"), " ほか", length(u) - 2, "件")
}

# 「情報源サイト名」列（改行区切り）→ 出典欄ラベル（最大2件＋ほかN件）。空ならNULL
.cc_site_label <- function(sites_txt) {
  if (is.na(sites_txt) || !nzchar(trimws(sites_txt))) return(NULL)
  u <- unique(trimws(strsplit(sites_txt, "\n")[[1]])); u <- u[nzchar(u)]
  if (!length(u)) return(NULL)
  if (length(u) <= 2) paste(u, collapse = "、") else paste0(paste(u[1:2], collapse = "、"), " ほか", length(u) - 2, "件")
}

# WHO地域コード→アプリの地域ラベル（国名で判定できなかった場合のフォールバック）
.cc_region_from_who <- function(who) {
  m <- c(AFRO = "アフリカ (Africa)", AMRO = "中南米・カリブ (Central & South America/Caribbean)",
         EMRO = "中東(Middle East)", EURO = "ヨーロッパ (Europe)",
         SEARO = "アジア (Asia)", WPRO = "アジア (Asia)")
  unname(ifelse(is.na(who), "不明", m[toupper(trimws(who))]))
}

# CSV1ファイル -> キャッシュ形式
.cc_convert_file <- function(path, scope_default) {
  x <- tryCatch(read.csv(path, fileEncoding = "UTF-8", stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("NA", "")),
                error = function(e) tryCatch(read.csv(path, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("NA", "")),
                                             error = function(e2) NULL))
  if (is.null(x) || nrow(x) == 0 || !"タイトル" %in% names(x)) return(NULL)
  col <- function(n) if (n %in% names(x)) as.character(x[[n]]) else rep(NA_character_, nrow(x))
  title <- trimws(col("タイトル"))
  keep <- !is.na(title) & nzchar(title)
  x <- x[keep, , drop = FALSE]; if (nrow(x) == 0) return(NULL)
  title <- title[keep]
  col <- function(n) if (n %in% names(x)) as.character(x[[n]]) else rep(NA_character_, nrow(x))

  pub <- as.Date(col("情報掲載日"), format = "%Y/%m/%d")
  det <- as.Date(col("探知日"), format = "%Y/%m/%d")
  pub <- ifelse(is.na(pub), det, pub); pub <- as.Date(pub, origin = "1970-01-01")

  # カードに出す本文は「AI要約」。国外は英語のAI要約に対するAI翻訳（日本語）を表示する。
  # 国内はAI要約とAI翻訳が同一なのでAI要約。
  # 本文が取得できず要約が無い記事は、「概要」列に書かれた事情説明（取得失敗・権利・URL未確認など）は
  # 出さず、「本文未取得」とだけ表示する
  ai_sum <- col("AI要約"); ai_tr <- col("AI翻訳")
  use_tr <- !is.na(ai_tr) & (is.na(ai_sum) | ai_tr != ai_sum)
  summary <- ifelse(use_tr, ai_tr, ifelse(!is.na(ai_sum), ai_sum, NA_character_))
  no_body <- is.na(summary)
  summary[no_body] <- "本文未取得"
  ai_label <- ifelse(no_body, NA_character_, ifelse(use_tr, "AI要約（AI翻訳）", "AI要約"))
  scope <- ifelse(is.na(col("国内か国外か")), scope_default, col("国内か国外か"))
  sig_raw <- col("Signal分類")
  sig <- ifelse(grepl("^Signal High", sig_raw), "Signal High", ifelse(grepl("^Signal Low", sig_raw), "Signal Low", "FYI"))
  src_cls <- col("情報源の分類")
  official <- !is.na(src_cls) & grepl("^Official$", trimws(src_cls))
  first_url <- vapply(col("情報源"), function(u) { m <- regmatches(u, regexpr("https?://[^[:space:]]+", u)); if (length(m)) m else NA_character_ }, character(1), USE.NAMES = FALSE)
  src_title <- vapply(strsplit(ifelse(is.na(col("情報源タイトル")), "", col("情報源タイトル")), "\n"), function(z) if (length(z)) z[1] else NA_character_, character(1))

  country <- col("国名")
  loc <- lapply(seq_along(title), function(i) {
    r <- if (!is.na(country[i])) tryCatch(classify_location(country[i], ""), error = function(e) NULL) else NULL
    if (is.null(r) || identical(r$location, "Unknown") || identical(r$region, "不明")) list(location = ifelse(is.na(country[i]), "Unknown", country[i]), region = .cc_region_from_who(col("地域")[i])) else r
  })
  full_text <- paste(title, ifelse(is.na(summary), "", summary), ifelse(is.na(col("疾患名")), "", col("疾患名")))

  df <- data.frame(
    source_id = ifelse(official, "claude_official", "claude_media"),
    # 2026/10/09以降のCSVは27列目「情報源サイト名」（媒体名、URLがない行も必須）を持つので、あればそれを優先する。
    # Google News RSS等の見出し掲載元リンクの行（2026/10/10〜）も「Google News（NHKニュース）」のように媒体名が出る
    source_name = vapply(seq_along(title), function(i) { s <- .cc_site_label(col("情報源サイト名")[i]); if (!is.null(s)) s else .cc_source_label(col("情報源")[i], col("情報源タイトル")[i]) }, character(1)),
    category = "収集",
    lang = "ja",
    title = title,
    link = first_url,
    pub_date = pub,
    summary = summary,
    signal_level = factor(sig, levels = c("Signal High", "Signal Low", "FYI")),
    stringsAsFactors = FALSE
  )
  df$signal_weight <- unname(signal_weight(df$signal_level))
  df$disease_tags <- vapply(full_text, function(t) tryCatch(tag_diseases(t, NA), error = function(e) "other"), character(1), USE.NAMES = FALSE)
  df$retweet_count <- NA_integer_; df$like_count <- NA_integer_
  df$ebs_pref <- ifelse(is.na(col("都道府県")), NA_character_, col("都道府県"))
  df$ebs_unusual <- .cc_flag(col("Unusual/unexpected"))
  df$ebs_serious_c <- .cc_flag(col("Serious PH impact in the country"))
  df$ebs_serious_j <- .cc_flag(col("Serious PH impact to Japan"))
  df$ebs_epidemic <- .cc_flag(col("Epidemic-prone"))
  df$ebs_mass <- .cc_flag(col("Mass exposure"))
  df$ebs_high <- .cc_flag(col("High profile"))
  df$ebs_special <- .cc_flag(col("Special pathogen/bioterrorism agents"))
  df$ebs_disease_en <- NA_character_
  df$ebs_disease_ja <- col("疾患名")
  df$ebs_location <- vapply(loc, function(z) z$location, character(1))
  df$ebs_region <- vapply(loc, function(z) z$region, character(1))
  df$ai_label <- ai_label
  # 国外: 元の言語（英語）のAI要約。「元の言語で読む」表示で使う
  df$summary_orig <- ifelse(use_tr, ai_sum, NA_character_)
  df$csv_scope <- scope
  df$csv_urls <- vapply(col("情報源"), function(u) paste(.cc_urls(u), collapse = " "), character(1), USE.NAMES = FALSE)
  df$csv_file <- basename(path)
  df
}

# 全CSVを読み込み、CSV内の重複を除いて返す
load_claude_csv_ebs <- function(dir = .cc_pick_dir()) {
  if (is.null(dir)) { message("Claude CSV: NASフォルダにアクセスできないためスキップ"); return(NULL) }
  files <- list.files(dir, pattern = "^(Domestic|Overseas)EBS_[0-9]{8}\\.csv$", full.names = TRUE)
  files <- files[suppressWarnings(file.info(files)$size) > 0]
  if (length(files) == 0) return(NULL)
  parts <- lapply(files, function(f) .cc_convert_file(f, if (grepl("Overseas", basename(f))) "国外" else "国内"))
  parts <- Filter(Negate(is.null), parts)
  if (length(parts) == 0) return(NULL)
  out <- do.call(rbind, parts)
  # ファイル名の日付が新しい方を優先して重複を除く
  fdate <- as.integer(sub("^.*_([0-9]{8})\\.csv$", "\\1", out$csv_file))
  out <- out[order(-fdate, out$signal_level), ]
  out <- .cc_dedupe_self(out)
  rownames(out) <- NULL
  out
}

.cc_dedupe_self <- function(d, jac = 0.85) {
  # 同一URL・同一タイトル・類似タイトルを、同じ国内/国外かつ掲載日の差が2日以内のものに限って
  # 重複とみなす（福井県「インフルエンザ関連情報」のように毎日更新される固定ページは別ニュース）
  nt <- .cc_norm_title(d$title); keep <- rep(TRUE, nrow(d))
  big <- lapply(nt, .cc_bigrams)
  urls <- lapply(strsplit(d$csv_urls, " ", fixed = TRUE), function(z) z[nzchar(z)])
  kept_idx <- integer(0)
  for (i in seq_len(nrow(d))) {
    cand <- kept_idx[d$csv_scope[kept_idx] == d$csv_scope[i] & !is.na(d$pub_date[kept_idx]) & !is.na(d$pub_date[i]) &
                       abs(as.numeric(d$pub_date[kept_idx] - d$pub_date[i])) <= 2]
    dup <- FALSE
    for (j in cand) {
      if (length(urls[[i]]) && any(urls[[i]] %in% urls[[j]])) { dup <- TRUE; break }
      if (nt[i] == nt[j] || .cc_jaccard(big[[i]], big[[j]]) >= jac) { dup <- TRUE; break }
    }
    if (dup) keep[i] <- FALSE else kept_idx <- c(kept_idx, i)
  }
  d[keep, , drop = FALSE]
}

# キャッシュにCSV由来の行を反映する（冪等）。
# 1) 既存のclaude_*行を取り除く  2) Rスクレイピング由来の行のうちCSVと明らかに重複するものを除く
# 3) CSV由来の行を追加する
apply_claude_csv_to_cache <- function(cache, csv = load_claude_csv_ebs(), jac = 0.8, verbose = TRUE) {
  if (is.null(csv) || nrow(csv) == 0) return(cache)
  base <- cache[is.na(cache$source_id) | !grepl("^claude_", cache$source_id), , drop = FALSE]
  n0 <- nrow(base)
  lo <- min(csv$pub_date, na.rm = TRUE) - 3
  cand_i <- which(!is.na(base$pub_date) & base$pub_date >= lo)
  # URLが同じでも、福井県「インフルエンザ関連情報」のような日々更新される固定ページは
  # 別日の別ニュースなので、CSV行の掲載日から±3日以内に限って重複とみなす
  url_rows <- strsplit(csv$csv_urls, " ", fixed = TRUE)
  url_df <- data.frame(url = unlist(url_rows), date = rep(csv$pub_date, lengths(url_rows)), stringsAsFactors = FALSE)
  url_df <- url_df[nzchar(url_df$url), , drop = FALSE]
  nt_csv <- .cc_norm_title(csv$title); big_csv <- lapply(nt_csv, .cc_bigrams)
  drop <- logical(nrow(base))
  for (i in cand_i) {
    if (!is.na(base$link[i])) {
      same <- which(url_df$url == .cc_norm_url(base$link[i]))
      if (length(same) && any(abs(as.numeric(url_df$date[same] - base$pub_date[i])) <= 3, na.rm = TRUE)) { drop[i] <- TRUE; next }
    }
    nt <- .cc_norm_title(base$title[i]); if (!nzchar(nt)) next
    near <- which(!is.na(csv$pub_date) & abs(as.numeric(csv$pub_date - base$pub_date[i])) <= 3)
    if (!length(near)) next
    if (any(nt_csv[near] == nt)) { drop[i] <- TRUE; next }
    b <- .cc_bigrams(nt)
    for (j in near) if (.cc_jaccard(b, big_csv[[j]]) >= jac) { drop[i] <- TRUE; break }
  }
  base <- base[!drop, , drop = FALSE]
  csv_add <- csv; csv_add$csv_urls <- NULL
  out <- dplyr::bind_rows(base, csv_add)
  if (verbose) message(sprintf("Claude CSV反映: CSV %d件（国内%d/国外%d）、既存の重複%d件を除外 → キャッシュ %d件",
                               nrow(csv), sum(csv$csv_scope == "国内"), sum(csv$csv_scope == "国外"), sum(drop), nrow(out)))
  out
}
