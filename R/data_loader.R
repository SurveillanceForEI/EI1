# ============================================================
# data_loader.R — JIHS 感染症発生動向調査 実データ取得
# 出典: 国立健康危機管理研究機構 (JIHS)
# URL: https://id-info.jihs.go.jp/surveillance/idwr/provisional/
# 注意: 速報値（暫定値）のため、確定値とは異なる場合があります
# ============================================================

library(dplyr)
library(tidyr)
library(lubridate)
library(httr)

JIHS_BASE      <- "https://id-info.jihs.go.jp/surveillance/idwr/provisional"
NIID_BASE      <- "https://id-info.jihs.go.jp/niid/images/idwr/sokuho"
NIID_MAX_YEAR  <- 2022  # 2022年以前は旧NIIDパス、2023年以降は新JIHSパス

# ── 都道府県マスタ ──────────────────────────────────────────
PREF_MASTER <- tibble(
  pref_code = 1:47,
  pref_name = c(
    "北海道","青森県","岩手県","宮城県","秋田県",
    "山形県","福島県","茨城県","栃木県","群馬県",
    "埼玉県","千葉県","東京都","神奈川県","新潟県",
    "富山県","石川県","福井県","山梨県","長野県",
    "岐阜県","静岡県","愛知県","三重県","滋賀県",
    "京都府","大阪府","兵庫県","奈良県","和歌山県",
    "鳥取県","島根県","岡山県","広島県","山口県",
    "徳島県","香川県","愛媛県","高知県","福岡県",
    "佐賀県","長崎県","熊本県","大分県","宮崎県",
    "鹿児島県","沖縄県"
  ),
  region = c(
    "北海道・東北","北海道・東北","北海道・東北","北海道・東北","北海道・東北",
    "北海道・東北","北海道・東北","関東","関東","関東",
    "関東","関東","関東","関東","中部",
    "中部","中部","中部","中部","中部",
    "中部","中部","中部","中部","近畿",
    "近畿","近畿","近畿","近畿","近畿",
    "中国・四国","中国・四国","中国・四国","中国・四国","中国・四国",
    "中国・四国","中国・四国","中国・四国","中国・四国","九州・沖縄",
    "九州・沖縄","九州・沖縄","九州・沖縄","九州・沖縄","九州・沖縄",
    "九州・沖縄","九州・沖縄"
  ),
  sentinel_sites = c(
    150,45,42,75,38,38,60,90,65,65,
    190,175,370,210,65,32,38,22,30,60,
    65,120,185,55,42,100,220,160,48,38,
    22,22,70,105,52,27,35,52,28,175,
    32,52,72,45,42,72,50
  ),
  # ── デフォルメ日本地図グリッド上の位置（活動レベル一覧・都道府県別タブ用）──
  # row: 北を1、南に向かうほど大きい値 / col: 西を1、東に向かうほど大きい値
  # 実際の緯度経度ではなく、視覚的におおよその配置になるよう手動で割り当てたもの
  grid_row = c(
    1,3,4,5,4,5,6,6,6,7,
    7,7,8,9,6,7,7,8,8,7,
    8,9,9,10,9,9,10,9,10,12,
    9,9,10,10,10,11,11,12,12,11,
    12,13,12,11,13,14,16
  ),
  grid_col = c(
    7,7,8,8,6,6,7,9,8,6,
    8,9,8,8,5,4,3,4,7,7,
    6,7,6,6,5,4,4,3,5,5,
    2,1,3,2,1,5,4,3,4,1,
    1,1,2,2,2,1,1
  )
)

# ── 疾患設定 ────────────────────────────────────────────────
# CSV疾患名（全角含む）→ disease_id へのマッピング
# 年によって列数が変わるため、列インデックスは動的解析で取得する
TEITEN_LABEL_MAP <- c(
  "インフルエンザ"             = "flu",
  "ＲＳウイルス感染症"         = "rsv",
  "咽頭結膜熱"                 = "phar_conj",
  "Ａ群溶血性レンサ球菌咽頭炎" = "strep",
  "感染性胃腸炎"               = "gi",
  "水痘"                       = "varicella",
  "手足口病"                   = "hfmd",
  "伝染性紅斑"                 = "erythema",
  "突発性発しん"               = "roseola",
  "ヘルパンギーナ"             = "herp_ang",
  "流行性耳下腺炎"             = "mumps",
  "急性出血性結膜炎"           = "hem_conj",
  "流行性角結膜炎"             = "epid_conj",
  "細菌性髄膜炎"               = "bact_mening",
  "無菌性髄膜炎"               = "asep_mening",
  "マイコプラズマ肺炎"         = "mycop",
  "クラミジア肺炎"             = "chlamydia",
  "感染性胃腸炎（ロタウイルス）" = "gi_rota",
  "COVID-19"                   = "covid"
)

# ARIは別CSV（col_teiten固定）
TEITEN_DISEASE_COLS <- list(
  ari = list(label="急性呼吸器感染症（ARI）", col_teiten=3, csv="ari")
)

DISEASE_CONFIG <- list(
  # alert_threshold: 研究班報告ベースの注意報・警報基準値がある疾患は
  # list(keiho_start=警報開始, keiho_end=警報終息, chuiho_start=注意報開始[任意])。
  # 出典: 厚生労働科学研究「効果的な感染症サーベイランスの評価ならびに改良に関する研究」研究班報告書
  # （定点あたり報告数）。国が定める公式の注意報・警報基準ではなく、あくまで研究班報告ベースの値。
  # この基準がない疾患は文献等の単一参考値、または設定なし(NULL)。
  flu        = list(label="インフルエンザ",               color="#3498db", alert_threshold=list(keiho_start=30, keiho_end=10, chuiho_start=10), unit="定点あたり報告数"),
  rsv        = list(label="RSウイルス感染症",             color="#f39c12", alert_threshold=NULL, unit="定点あたり報告数"),
  phar_conj  = list(label="咽頭結膜熱",                   color="#16a085", alert_threshold=list(keiho_start=3,  keiho_end=1,   chuiho_start=NULL), unit="定点あたり報告数"),
  strep      = list(label="A群溶血性レンサ球菌咽頭炎",   color="#27ae60", alert_threshold=list(keiho_start=8,  keiho_end=4,   chuiho_start=NULL), unit="定点あたり報告数"),
  gi         = list(label="感染性胃腸炎",                 color="#e67e22", alert_threshold=list(keiho_start=20, keiho_end=12, chuiho_start=NULL), unit="定点あたり報告数"),
  varicella  = list(label="水痘",                         color="#8e44ad", alert_threshold=list(keiho_start=7,  keiho_end=4,   chuiho_start=4),    unit="定点あたり報告数"),
  hfmd       = list(label="手足口病",                     color="#e91e63", alert_threshold=list(keiho_start=5,  keiho_end=2,   chuiho_start=NULL), unit="定点あたり報告数"),
  erythema   = list(label="伝染性紅斑",                   color="#c0392b", alert_threshold=list(keiho_start=2,  keiho_end=1,   chuiho_start=NULL), unit="定点あたり報告数"),
  roseola    = list(label="突発性発しん",                 color="#d35400", alert_threshold=NULL, unit="定点あたり報告数"),
  herp_ang   = list(label="ヘルパンギーナ",               color="#e74c3c", alert_threshold=list(keiho_start=6,  keiho_end=2,   chuiho_start=NULL), unit="定点あたり報告数"),
  mumps      = list(label="流行性耳下腺炎",               color="#2980b9", alert_threshold=list(keiho_start=6,  keiho_end=2,   chuiho_start=3),    unit="定点あたり報告数"),
  hem_conj   = list(label="急性出血性結膜炎",             color="#1abc9c", alert_threshold=list(keiho_start=6,  keiho_end=0.1, chuiho_start=NULL), unit="定点あたり報告数"),
  epid_conj  = list(label="流行性角結膜炎",               color="#3498db", alert_threshold=list(keiho_start=8,  keiho_end=4,   chuiho_start=NULL), unit="定点あたり報告数"),
  bact_mening= list(label="細菌性髄膜炎",                 color="#7f8c8d", alert_threshold=NULL, unit="定点あたり報告数"),
  asep_mening= list(label="無菌性髄膜炎",                 color="#95a5a6", alert_threshold=NULL, unit="定点あたり報告数"),
  mycop      = list(label="マイコプラズマ肺炎",           color="#00bcd4", alert_threshold=NULL, unit="定点あたり報告数"),
  chlamydia  = list(label="クラミジア肺炎",               color="#009688", alert_threshold=NULL, unit="定点あたり報告数"),
  gi_rota    = list(label="感染性胃腸炎（ロタウイルス）", color="#ff9800", alert_threshold=NULL, unit="定点あたり報告数"),
  covid      = list(label="COVID-19",                     color="#e74c3c", alert_threshold=NULL, unit="定点あたり報告数"),
  ari        = list(label="急性呼吸器感染症（ARI）",      color="#9b59b6", alert_threshold=NULL, unit="定点あたり報告数")
)

# ── シリアルインターバル（疾患別・出典付き） ─────────────────
# source: 文献引用。"推定値" は直接のSI研究なく潜伏期間から推定
# ── シリアルインターバル（疾患別・出典付き） ─────────────────
# published=TRUE: 査読済み文献によるSI推定値（Rt計算に使用）
# published=FALSE: 潜伏期間等からの推定値（Rt計算対象外）
SERIAL_INTERVALS <- list(
  flu        = list(mean=2.6,  sd=1.5,  published=TRUE,
    source="Vink et al. (2014) Am J Epidemiol 180:865-875 [系統的レビュー]"),
  rsv        = list(mean=7.5,  sd=3.0,  published=TRUE,
    source="Vink et al. (2014) Am J Epidemiol 180:865-875 [系統的レビュー]"),
  phar_conj  = list(mean=8.0,  sd=3.0,  published=FALSE, derived_from="incubation_period",
    source="推定値（アデノウイルス潜伏期間5–12日, 平均~8日; CDC MMWR 2018 [LA County EKC outbreak]; PMID:26732024）"),
  strep      = list(mean=2.5,  sd=1.5,  published=FALSE, derived_from="incubation_period",
    source="推定値（A群溶連菌潜伏期間2–5日; CDC GAS factsheet; IDSA 2012 guideline PMC7108032）"),
  gi         = list(mean=3.6,  sd=2.0,  published=TRUE,
    source="Sukhrie et al. (2012) PLoS ONE 7:e34321 [ノロウイルス集団発生]"),
  varicella  = list(mean=14.0, sd=3.0,  published=TRUE,
    source="Vink et al. (2014) Am J Epidemiol 180:865-875 [系統的レビュー]"),
  hfmd       = list(mean=5.5,  sd=2.5,  published=TRUE,
    source="Liu et al. (2015) PLoS ONE 10:e0127005 [手足口病EV71/CA16]"),
  erythema   = list(mean=17.0, sd=5.0,  published=FALSE, derived_from="incubation_period",
    source="推定値（Parvovirus B19発疹まで潜伏期間17–18日; CDC Parvovirus B19 Infection Control page; 伝染性は発疹前のウイルス血症期のみ）"),
  roseola    = list(mean=10.0, sd=3.0,  published=FALSE, derived_from="incubation_period",
    source="推定値（HHV-6潜伏期間5–15日, 平均~9–10日; StatPearls NBK448190; MSD Manual）"),
  herp_ang   = list(mean=5.0,  sd=2.0,  published=FALSE, derived_from="incubation_period",
    source="推定値（エンテロウイルス潜伏期間3–5日; StatPearls NBK507792; HFMDと同系統）"),
  mumps      = list(mean=18.0, sd=5.0,  published=TRUE,
    source="Vink et al. (2014) Am J Epidemiol 180:865-875 [系統的レビュー]"),
  hem_conj   = list(mean=1.5,  sd=0.5,  published=FALSE, derived_from="incubation_period",
    source="推定値（EV70/CA24潜伏期間12–72時間; CDC/EID; Okinawa 1994 EV70 outbreak PMID:10424307）"),
  epid_conj  = list(mean=9.0,  sd=3.0,  published=FALSE, derived_from="incubation_period",
    source="推定値（アデノウイルス潜伏期間5–19日, 平均~9日; CDC MMWR 2018 LA County outbreak mean 9d PMID:30521501）"),
  bact_mening= list(mean=3.0,  sd=2.0,  published=FALSE, derived_from="incubation_period",
    source="推定値（髄膜炎菌潜伏期間1–10日, 典型的3–4日; CDC Pink Book Ch.14; 散発性のためRt解釈は限定的）"),
  asep_mening= list(mean=5.0,  sd=2.0,  published=FALSE, derived_from="incubation_period",
    source="推定値（エンテロウイルス潜伏期間3–7日の中央値; WorkSafeBC; StatPearls Aseptic Meningitis）"),
  mycop      = list(mean=23.0, sd=7.0,  published=TRUE,
    source="Chalker & Morrow (2013) Pediatr Pulmonol 48:S5-S13; 潜伏期間1–4週"),
  chlamydia  = list(mean=28.0, sd=7.0,  published=FALSE, derived_from="incubation_period",
    source="推定値（Chlamydophila pneumoniae症例間隔~30日, 潜伏期間21–28日; PMID:11869264 CMI review; PMC4451914 AF Academy outbreak）"),
  gi_rota    = list(mean=4.9,  sd=2.0,  published=TRUE,
    source="Grimwood et al. (1983) BMJ 287:575-577 [家庭内伝播研究, 小児SI=4.9日, 成人=6.4日]; 潜伏期間: Teunis et al. (2013) BMC Infect Dis PMID:24050580"),
  covid      = list(mean=3.3,  sd=2.0,  published=TRUE,
    source="Nishiura et al. (2020) Int J Infect Dis 93:284-286; Omicron期は短縮傾向"),
  ari        = list(mean=3.1,  sd=1.5,  published=TRUE,
    source="Levy et al. (2013) Am J Epidemiol 177:1443-1451 [Bangkok家庭内研究, n=414接触者; ARI定義でSI mean=3.1d, SD=1.5d] PMID:23629874"),
  # ── 全数把握疾患（文献SI値あり） ─────────────────────────
  measles    = list(mean=12.0, sd=2.5,  published=TRUE,
    source="Vink et al. (2014) Am J Epidemiol 180:865-875 [系統的レビュー]"),
  rubella    = list(mean=18.0, sd=3.0,  published=TRUE,
    source="Vink et al. (2014) Am J Epidemiol 180:865-875 [系統的レビュー]"),
  pertussis  = list(mean=9.0,  sd=2.0,  published=TRUE,
    source="Vink et al. (2014) Am J Epidemiol 180:865-875 [系統的レビュー]"),
  mpox       = list(mean=9.6,  sd=3.5,  published=TRUE,
    source="Miura et al. (2022) Euro Surveill 27:2200682 [2022年流行データ]"),
  igas       = list(mean=2.0,  sd=4.0,  published=TRUE,
    source="Mearkle et al. (2017) Euro Surveill 22(19):30532 [英国家庭内発症間隔 中央値2日, 範囲0–28日, n=24クラスター] PMC5476984; 二次攻撃率<0.22%のためRt解釈は限定的"),
  ehec       = list(mean=4.0,  sd=2.0,  published=FALSE, derived_from="incubation_period",
    source="推定値（EHEC O157潜伏期間1–10日, 典型的3–4日; WHO/CDC; 症例の約80%は食品媒介のためRtは過大推定の可能性; PMID:19715594）"),
  # ── 追加：A型肝炎・デング熱 ──────────────────────────────
  hep_a      = list(mean=23.9, sd=20.9, published=TRUE,
    source="Zhang & Iacono (2018) PLoS ONE [中国・小学校集団発生, n=32]; R0=2.1-2.8"),
  dengue     = list(mean=16.0, sd=5.0,  published=FALSE, derived_from="incubation_period",
    source="推定値（ヒト潜伏期間5.9日[Chan & Johansson 2012 PLoS ONE] + Aedes蚊外因性潜伏期8-12日; ベクター媒介のため直接SI推定困難）")
)

# ── CP932 テキスト読み込みユーティリティ ────────────────────
read_jihs_csv <- function(url, timeout_sec = 20) {
  tryCatch({
    resp <- GET(url, timeout(timeout_sec),
                add_headers("User-Agent"="JapanSurveillanceDashboard/1.0"))
    if (status_code(resp) != 200) return(NULL)
    raw_bytes <- content(resp, "raw")
    text <- iconv(rawToChar(raw_bytes), from="CP932", to="UTF-8", sub="")
    text
  }, error=function(e) { message("取得失敗 ", url, ": ", e$message); NULL })
}

# ── 週別インデックス取得 ──────────────────────────────────
fetch_available_weeks <- function(year) {
  if (year <= NIID_MAX_YEAR) {
    # 旧NIIDパスはインデックスページがないため全週を返す（404は取得時にスキップ）
    max_wk <- if (year %% 400 == 0 || (year %% 4 == 0 && year %% 100 != 0)) 53L else 52L
    return(1:max_wk)
  }

  # 新JIHSパス
  url <- paste0(JIHS_BASE, "/", year, "/index.html")
  text <- tryCatch({
    resp <- GET(url, timeout(15),
                add_headers("User-Agent"="JapanSurveillanceDashboard/1.0"))
    if (status_code(resp) != 200) return(NULL)
    rawToChar(content(resp, "raw"))
  }, error=function(e) NULL)

  if (is.null(text)) return(NULL)

  m <- regmatches(text, gregexpr("\\./([0-9]{2})/index\\.html", text))[[1]]
  weeks <- as.integer(sub("./(\\d{2})/index\\.html", "\\1", m))
  sort(unique(weeks))
}

# ── CSVフィールド分割ユーティリティ ─────────────────────────
parse_csv_fields <- function(line) {
  trimws(strsplit(gsub('^"|"$', "", line), '","')[[1]])
}

# ── 1週分の定点把握CSVをパース（動的列検出） ─────────────────
parse_teiten_csv <- function(text, year, week) {
  if (is.null(text)) return(NULL)
  lines <- strsplit(text, "\n")[[1]]
  if (length(lines) < 6) return(NULL)

  # 週の開始日計算
  week_start <- as.Date(paste0(year, "-01-01")) + (week - 1) * 7

  # 行3: 疾患名, 行4: 報告/定当 → 動的に「疾患ID→定当列インデックス」を構築
  disease_row <- parse_csv_fields(lines[3])  # 例: c("", "インフルエンザ", "", "ＲＳウイルス", ...)
  type_row    <- parse_csv_fields(lines[4])  # 例: c("", "報告", "定当", "報告", "定当", ...)

  # disease_id → 定当列インデックス（1始まり）のマップ
  col_map <- list()
  for (i in seq_along(disease_row)) {
    dname <- disease_row[i]
    if (dname == "" || is.na(dname)) next
    did <- TEITEN_LABEL_MAP[dname]
    if (is.na(did) || !(did %in% names(DISEASE_CONFIG))) next
    # この疾患ペアの「定当」は i+1 列目（報告=i, 定当=i+1）
    teiten_col <- i + 1
    if (teiten_col <= length(type_row) && grepl("定当|定点", type_row[teiten_col])) {
      col_map[[did]] <- teiten_col
    } else if (i + 1 <= length(type_row)) {
      col_map[[did]] <- teiten_col  # 万が一ラベルがなくても位置で取得
    }
  }
  if (length(col_map) == 0) return(NULL)

  # データ行（行5以降）をパース
  data_rows <- lines[5:length(lines)]
  data_rows <- data_rows[nchar(trimws(data_rows)) > 0]

  results <- lapply(data_rows, function(row) {
    fields <- parse_csv_fields(row)
    if (length(fields) < 3) return(NULL)

    pref <- fields[1]
    if (pref == "" || pref == "総数" || grepl("^合計|^全国", pref)) return(NULL)

    pref_row <- PREF_MASTER %>% filter(pref_name == pref)
    if (nrow(pref_row) == 0) return(NULL)

    rows_list <- lapply(names(col_map), function(did) {
      ci  <- col_map[[did]]
      val <- suppressWarnings(as.numeric(if (ci <= length(fields)) fields[ci] else NA_character_))
      tibble(
        year             = as.integer(year),
        week             = as.integer(week),
        date             = week_start,
        pref_code        = pref_row$pref_code,
        pref_name        = pref,
        region           = pref_row$region,
        disease          = did,
        disease_label    = DISEASE_CONFIG[[did]]$label,
        reports_per_site = val,
        is_provisional   = TRUE,
        data_source      = "JIHS IDWR速報（暫定値）"
      )
    })
    bind_rows(rows_list)
  })
  bind_rows(Filter(Negate(is.null), results))
}

# ── 1週分のARIデータをパース ─────────────────────────────
parse_ari_csv <- function(text, year, week) {
  if (is.null(text)) return(NULL)
  lines <- strsplit(text, "\n")[[1]]
  if (length(lines) < 5) return(NULL)

  week_start <- as.Date(paste0(year, "-01-01")) + (week - 1) * 7
  data_rows <- lines[5:length(lines)]
  data_rows <- data_rows[nchar(trimws(data_rows)) > 0]

  results <- lapply(data_rows, function(row) {
    fields <- strsplit(trimws(row), ",")[[1]]
    if (length(fields) < 3) return(NULL)
    pref <- trimws(gsub('"', "", fields[1]))
    if (pref == "" || pref == "総数") return(NULL)

    pref_row <- PREF_MASTER %>% filter(pref_name == pref)
    if (nrow(pref_row) == 0) return(NULL)

    val <- suppressWarnings(as.numeric(gsub('"', "", fields[3])))
    tibble(
      year=as.integer(year), week=as.integer(week),
      date=week_start,
      pref_code=pref_row$pref_code, pref_name=pref, region=pref_row$region,
      disease="ari", disease_label="ARI（急性呼吸器感染症）",
      reports_per_site=val,
      is_provisional=TRUE, data_source="JIHS IDWR速報（暫定値）"
    )
  })
  bind_rows(Filter(Negate(is.null), results))
}

# ── 週ごとの全データ取得 ─────────────────────────────────
fetch_week_data <- function(year, week) {
  wk_str <- sprintf("%02d", week)

  if (year <= NIID_MAX_YEAR) {
    # 旧NIIDパス（2022年以前）
    teiten_url <- sprintf("%s/idwr-%d/%d%s/%d-%s-teiten.csv", NIID_BASE, year, year, wk_str, year, wk_str)
    teiten_text <- read_jihs_csv(teiten_url)
    # 旧NIIDにはARICSVなし
    bind_rows(parse_teiten_csv(teiten_text, year, week))
  } else {
    # 新JIHSパス（2023年以降）
    base <- paste0(JIHS_BASE, "/", year, "/", wk_str, "/")
    teiten_url <- paste0(base, year, "-", wk_str, "-teiten.csv")
    ari_url    <- paste0(base, year, "-", wk_str, "-ari.csv")
    teiten_text <- read_jihs_csv(teiten_url)
    ari_text    <- read_jihs_csv(ari_url)
    bind_rows(
      parse_teiten_csv(teiten_text, year, week),
      parse_ari_csv(ari_text, year, week)
    )
  }
}

# ── 複数週のデータを並列取得（キャッシュ付き） ────────────
CACHE_DIR <- "data/cache"

get_cache_path <- function(year, week) {
  dir.create(CACHE_DIR, recursive=TRUE, showWarnings=FALSE)
  file.path(CACHE_DIR, sprintf("%d-%02d.rds", year, week))
}

fetch_week_cached <- function(year, week, force=FALSE) {
  cache_path <- get_cache_path(year, week)
  if (!force && file.exists(cache_path)) {
    return(readRDS(cache_path))
  }
  message(sprintf("取得中: %d年第%d週...", year, week))
  d <- fetch_week_data(year, week)
  if (!is.null(d) && nrow(d) > 0) saveRDS(d, cache_path)
  d
}

# ── キャッシュからまとめて読み込み ───────────────────────
load_all_cached <- function() {
  files <- list.files(CACHE_DIR, pattern="\\.rds$", full.names=TRUE)
  if (length(files) == 0) return(NULL)
  message("キャッシュ読み込み中 (", length(files), "週)...")
  df <- bind_rows(lapply(files, readRDS))
  # 旧キャッシュで hits 列に格納されていた値を reports_per_site に統合
  if ("hits" %in% names(df) && "reports_per_site" %in% names(df)) {
    df <- df %>%
      mutate(reports_per_site = dplyr::coalesce(reports_per_site, as.numeric(hits))) %>%
      select(-hits)
  }
  if ("keyword"    %in% names(df)) df <- df %>% select(-keyword)
  if ("disease_id" %in% names(df)) df <- df %>% select(-disease_id)
  if ("date" %in% names(df) && !inherits(df$date, "Date"))
    df$date <- as.Date(df$date)
  if ("year" %in% names(df))  df$year  <- as.integer(df$year)
  if ("week" %in% names(df))  df$week  <- as.integer(df$week)
  df
}

# ── メイン: キャッシュ優先・差分のみ取得 ─────────────────
get_surveillance_data <- function(years=2024:2026,
                                  disease_ids=names(DISEASE_CONFIG),
                                  force_refresh=FALSE) {
  # ① キャッシュから読み込み（高速）
  cached_data <- if (!force_refresh) load_all_cached() else NULL

  # ② 各年の最新週を確認して未取得分を差分取得
  new_data <- list()
  cur_year <- as.integer(format(Sys.Date(), "%Y"))
  cur_week <- as.integer(format(Sys.Date(), "%V"))

  for (yr in years) {
    # 過去年かつキャッシュが存在する場合はインデックス取得をスキップ
    if (!force_refresh && yr < cur_year) {
      max_wk <- if (yr %% 400 == 0 || (yr %% 4 == 0 && yr %% 100 != 0)) 53L else 52L
      cached_wks <- which(file.exists(sapply(1:max_wk, function(w) get_cache_path(yr, w))))
      if (length(cached_wks) > 40) {
        next  # 十分キャッシュ済み → スキップ
      }
    }

    avail_weeks <- tryCatch(
      fetch_available_weeks(yr),
      error=function(e) { message(yr, "年: インデックス取得失敗"); NULL }
    )
    if (is.null(avail_weeks) || length(avail_weeks) == 0) next

    # キャッシュにない週だけ取得
    missing_weeks <- avail_weeks[!file.exists(
      sapply(avail_weeks, function(w) get_cache_path(yr, w))
    )]
    if (force_refresh) missing_weeks <- avail_weeks

    if (length(missing_weeks) > 0) {
      message(yr, "年: ", length(missing_weeks), "週を新規取得中...")
      week_data <- lapply(missing_weeks, function(wk) {
        tryCatch(
          fetch_week_cached(yr, wk, force=force_refresh),
          error=function(e) { message("エラー ", yr, "W", wk, ": ", e$message); NULL }
        )
      })
      new_data[[as.character(yr)]] <- bind_rows(Filter(Negate(is.null), week_data))
    }
  }

  result <- bind_rows(cached_data, bind_rows(new_data))

  # 旧形式列の統合・除去
  if (!is.null(result) && nrow(result) > 0) {
    if ("hits" %in% names(result) && "reports_per_site" %in% names(result)) {
      result <- result %>%
        mutate(reports_per_site = dplyr::coalesce(reports_per_site, as.numeric(hits))) %>%
        select(-hits)
    }
    if ("keyword"    %in% names(result)) result <- result %>% select(-keyword)
    if ("disease_id" %in% names(result)) result <- result %>% select(-disease_id)
  }

  if (is.null(result) || nrow(result) == 0) {
    message("データなし。デモデータを使用します。")
    return(generate_demo_fallback(disease_ids))
  }

  # 対象年度・疾患フィルタ
  result <- result %>%
    filter(year %in% years, disease %in% disease_ids) %>%
    select(-any_of("region")) %>%
    left_join(PREF_MASTER %>% select(pref_code, region), by="pref_code") %>%
    arrange(year, week, pref_code, disease)

  message("データ準備完了: ", nrow(result), "行 | ",
          min(result$year), "年第", min(result$week[result$year==min(result$year)]), "週〜",
          max(result$year), "年第", max(result$week[result$year==max(result$year)]), "週")
  result
}

# ── 警戒レベル ───────────────────────────────────────────
# IBS方式の週次逸脱スコア(0-3)算出ヘルパー
# 過去5年・同一週±2週の移動窓データと比較し mu, s, ibs_score を付与する。
# main_df: 対象系列 (date, year, week, reports_per_site) 1行以上
# hist_df: 比較対象の全期間データ（date_rangeに依存しない同一系列の履歴）
compute_ibs_band <- function(main_df, hist_df) {
  n <- nrow(main_df)
  if (n == 0) {
    return(dplyr::mutate(main_df, mu = numeric(0), s = numeric(0),
                          has_hist = logical(0), ibs_score = numeric(0)))
  }
  rows <- lapply(seq_len(n), function(i) {
    w <- main_df$week[i]; y <- main_df$year[i]; v <- main_df$reports_per_site[i]
    ws <- unique(pmax(1L, pmin(53L, (w - 2L):(w + 2L))))
    h  <- hist_df[hist_df$week %in% ws & hist_df$year >= y - 5 & hist_df$year < y, ]
    cnt <- sum(!is.na(h$reports_per_site))
    mu  <- mean(h$reports_per_site, na.rm = TRUE)
    s   <- if (cnt >= 3) sd(h$reports_per_site, na.rm = TRUE) else NA_real_
    has <- cnt >= 3 && !is.nan(mu) && !is.na(s)
    score <- if (!has || is.na(v)) NA_real_ else
      if (v >= mu + 2 * s) 3 else if (v >= mu + s) 2 else if (v >= mu) 1 else 0
    data.frame(mu = ifelse(has, mu, NA_real_), s = ifelse(has, s, NA_real_),
               has_hist = has, ibs_score = score)
  })
  cbind(main_df, do.call(rbind, rows))
}

# 流行レベル判定の中核ロジック: 参考基準値（注意報/警報相当）・Rt値・IBS方式の
# 過去5年比較（±SD）の3指標をそれぞれ0-3点でスコア化し、重み付き平均（傾斜配分）で
# 統合スコア（0-3、四捨五入済み）を返す。classify_alert() のラベル変換前の数値版であり、
# kpi_integrated（統合活動レベルカード）など他の画面でも同じロジックを再利用するために
# thresh を直接受け取る形（disease_id に依存しない）にしてある。
#
# 共線性への対処:
#   ① 参考基準値（固定閾値との比較）と ③ IBS方式・過去5年比較（季節調整済み平均との比較）は
#   どちらも「現在の報告数の水準」を異なる基準で評価したものであり、相関が強い（同じ情報の
#   二重カウントになりやすい）。一方 ② Rt は感染動態（増加率）に基づく独立性の高い指標。
#   そこで①③をまず1つの「水準スコア」に統合し（level_weightsで内部配分）、
#   独立性の高い②Rt（動態スコア）と group_weights（既定1:1）で組み合わせる2段階方式とする。
#
# 利用可能な指標が一部の場合は、その指標だけで重みを正規化して評価する。
# value / rt_value / ibs_score は単一値（現在値）でもベクトル（同じ長さの時系列）でも可。
# thresh は以下のいずれか:
#  - list(keiho_start=, keiho_end=, chuiho_start=NULL可) : 研究班報告ベースの注意報・警報基準値
#    （厚生労働科学研究「効果的な感染症サーベイランスの評価ならびに改良に関する研究」研究班報告書。
#     国の公式基準ではない）
#  - 単一の数値: この基準がない疾患向けの簡易参考値（従来方式、×2/×0.3の目安で代用）
compute_alert_score <- function(value, thresh = NULL, rt_value = NULL, ibs_score = NULL,
                                 group_weights = c(level = 0.5, trend = 0.5),
                                 level_weights = c(thresh = 0.3, ibs = 0.4)) {
  n <- length(value)

  thresh_score <- rep(NA_real_, n)
  if (is.list(thresh)) {
    # 研究班報告ベースの注意報開始／警報開始／警報終息基準値による3段階判定
    keiho_start  <- thresh$keiho_start
    keiho_end    <- if (!is.null(thresh$keiho_end)) thresh$keiho_end else 0
    # chuiho_start が定義されていない疾患は、値がchuiho_start以上でも該当しないよう
    # -Inf（決して満たされない条件）を代用する（&&のベクトル化不可を避けるため）
    chuiho_start_v <- if (is.null(thresh$chuiho_start)) -Inf else thresh$chuiho_start
    has_chuiho <- !is.null(thresh$chuiho_start)
    thresh_score <- ifelse(value >= keiho_start, 3,
                    ifelse(has_chuiho & value >= chuiho_start_v, 2,
                    ifelse(value >= keiho_end, 1, 0)))
  } else if (!is.null(thresh)) {
    thresh_score <- ifelse(value >= thresh * 2,   3,
                    ifelse(value >= thresh,       2,
                    ifelse(value >= thresh * 0.3, 1, 0)))
  }

  rt_score <- rep(NA_real_, n)
  if (!is.null(rt_value) && !all(is.na(rt_value))) {
    rt_score_raw <- ifelse(rt_value >= 2.0, 3,
                     ifelse(rt_value >= 1.5, 2,
                     ifelse(rt_value >= 1.0, 1, 0)))
    rt_score <- rep(rt_score_raw, length.out = n)
  }

  ibs_score_vec <- rep(NA_real_, n)
  if (!is.null(ibs_score) && !all(is.na(ibs_score))) {
    ibs_score_vec <- rep(ibs_score, length.out = n)
  }

  # ── ステップ1: 共線性のある①参考基準値と③IBS過去5年比較を「水準スコア」に統合 ──
  level_mat <- cbind(thresh = thresh_score, ibs = ibs_score_vec)
  lw <- level_weights[colnames(level_mat)]
  level_score <- sapply(seq_len(n), function(i) {
    v <- level_mat[i, ]
    avail <- !is.na(v)
    if (!any(avail)) return(NA_real_)
    ww <- lw[avail] / sum(lw[avail])
    sum(v[avail] * ww)
  })

  # ── ステップ2: 水準スコアと独立指標②Rt（動態スコア）をgroup_weightsで統合 ──
  mat <- cbind(level = level_score, trend = rt_score)
  gw  <- group_weights[colnames(mat)]
  sapply(seq_len(n), function(i) {
    v <- mat[i, ]
    avail <- !is.na(v)
    if (!any(avail)) return(NA_real_)
    ww <- gw[avail] / sum(gw[avail])
    round(sum(v[avail] * ww))
  })
}

# classify_alert(): compute_alert_score() のラベル変換版（DISEASE_CONFIGから閾値を自動解決）
classify_alert <- function(value, disease_id, rt_value = NULL, ibs_score = NULL,
                            group_weights = c(level = 0.5, trend = 0.5),
                            level_weights = c(thresh = 0.3, ibs = 0.4)) {
  n <- length(value)
  if (!(disease_id %in% names(DISEASE_CONFIG))) return(rep("―", n))
  thresh <- DISEASE_CONFIG[[disease_id]]$alert_threshold

  score <- compute_alert_score(value, thresh, rt_value, ibs_score, group_weights, level_weights)
  labels <- c("基準以下", "流行期（レベル1）", "注意（レベル2）", "警戒（レベル3）")
  ifelse(is.na(score), "―", labels[score + 1])
}

alert_color <- function(level) {
  col <- c("警戒（レベル3）"="#c0392b","注意（レベル2）"="#e67e22",
           "流行期（レベル1）"="#f1c40f","基準以下"="#27ae60","―"="#95a5a6")[as.character(level)]
  ifelse(is.na(col), "#95a5a6", col)
}

# ── 全数把握疾患のIBS評価方式の自動判定・スコア計算 ─────────────────
# 全数把握疾患は「季節性のある反復流行疾患」（例: RSウイルス感染症など）と
# 「排除対象・散発疾患」（例: 麻しん、風しん等、報告数がほぼ0〜数件で
# 年単位の周期性が成立しない疾患）が混在するため、一律の「同時期×過去5年比較」
# 判定では散発疾患を正しく評価できない。既存データから季節性の強さを自動判定し、
# 疾患ごとに評価方式を切り替える。
#
# 季節性判定: 週別（week of year）の平均報告数を算出し、そのばらつき（変動係数=CV）
# が一定以上であれば「特定の時期に集中する=季節性あり」とみなす。
# データが少ない（3年未満）／報告がほぼ皆無の疾患は季節性なし扱いとする。
detect_seasonality <- function(hist_d, value_col = "reports_per_site") {
  if (is.null(hist_d) || nrow(hist_d) == 0) return(FALSE)
  v <- hist_d[[value_col]]
  if (is.null(v) || all(is.na(v)) || sum(v, na.rm = TRUE) <= 0) return(FALSE)
  n_years <- length(unique(hist_d$year[!is.na(v) & v > 0]))
  if (n_years < 3) return(FALSE)
  wk_mean <- tapply(v, hist_d$week, mean, na.rm = TRUE)
  wk_mean <- wk_mean[!is.na(wk_mean)]
  if (length(wk_mean) < 10) return(FALSE)
  m <- mean(wk_mean)
  if (is.na(m) || m <= 0) return(FALSE)
  cv <- sd(wk_mean) / m
  !is.na(cv) && cv > 0.6
}

# 全数把握疾患のIBSバンド判定（季節性の有無で自動的に評価方式を切替）
#  季節性あり: 従来方式（同時期±2週×過去5年平均±SDとの比較、2週連続+2SDでscore=3）
#  季節性なし: 直近の推移との比較（EARS C2ライク。直近baseline平均＋ポアソン近似の
#              分散を用いた閾値判定。年単位の周期比較が成立しない散発疾患向け）
zensu_ibs_band <- function(cur_val, cur_date, cur_week, cur_year,
                            prev_val, prev_date, prev_week, prev_year,
                            hist_d, value_col = "reports_per_site") {
  seasonal <- detect_seasonality(hist_d, value_col)

  if (seasonal) {
    calc <- function(val, w, y) {
      ws <- unique(pmax(1L, pmin(53L, (w - 2L):(w + 2L))))
      h  <- hist_d[hist_d$week %in% ws & hist_d$year >= y - 5 & hist_d$year < y, , drop = FALSE]
      v  <- h[[value_col]]
      n  <- sum(!is.na(v))
      mu <- mean(v, na.rm = TRUE)
      s  <- if (n >= 3) sd(v, na.rm = TRUE) else NA
      has <- n >= 3 && !is.nan(mu) && !is.na(s)
      list(mu = mu, s = s, has_hist = has,
           exceeds2sd = has && !is.na(val) && val > 0 && val >= mu + 2 * s,
           exceeds1sd = has && !is.na(val) && val > 0 && val >= mu + s,
           abovemu    = has && !is.na(val) && val > 0 && val >= mu)
    }
    cb <- calc(cur_val, cur_week, cur_year)
    pb <- calc(prev_val, prev_week, prev_year)
    score <- if (!cb$has_hist) 0L
             else if (cb$exceeds2sd && pb$exceeds2sd) 3L
             else if (cb$exceeds2sd || cb$exceeds1sd) 2L
             else if (cb$abovemu) 1L
             else 0L
    label <- if (!cb$has_hist) "基準値なし" else
      c("0"="平均以下","1"="平均〜+1SD","2"="+1〜+2SD","3"="+2SD超過（2週連続）")[as.character(score)]
    detail <- if (!cb$has_hist) "過去データ不足"
              else sprintf("%d件（基準 %.1f±%.1f）", as.integer(cur_val), cb$mu, cb$s)
    list(score = score, label = unname(label), detail = detail,
         method = "seasonal", has_hist = cb$has_hist)
  } else {
    ts_ordered <- hist_d[!is.na(hist_d$date) & hist_d$date <= cur_date, , drop = FALSE]
    ts_ordered <- ts_ordered[order(ts_ordered$date), ]
    v <- ts_ordered[[value_col]]
    n <- length(v)
    base_end   <- n - 2L                     # 直近2週はガードバンドとして除外（EARS C2方式）
    base_start <- base_end - 6L
    if (n < 9 || base_start < 1) {
      return(list(score = 0L, label = "基準値なし", detail = "過去データ不足",
                   method = "sporadic", has_hist = FALSE))
    }
    baseline <- v[base_start:base_end]
    mu    <- mean(baseline, na.rm = TRUE)
    sigma <- sqrt(max(mu, 1))                # ポアソン近似（分散≈平均、下限1）
    val   <- cur_val
    score <- if (is.na(val)) 0L
             else if (val >= mu + 3 * sigma) 3L
             else if (val >= mu + 2 * sigma) 2L
             else if (val > mu) 1L
             else 0L
    label <- c("0"="平常（散発なし）","1"="やや増加","2"="増加","3"="急増")[as.character(score)]
    detail <- sprintf("%d件（直近%d週平均 %.1f）", as.integer(val), length(baseline), mu)
    list(score = score, label = unname(label), detail = detail,
         method = "sporadic", has_hist = TRUE)
  }
}

# alert_threshold（list形式の研究班報告ベース基準値、または単一数値の簡易参考値）を表示用文字列に変換する
format_alert_threshold <- function(thresh) {
  if (is.null(thresh)) return(NULL)
  if (is.list(thresh)) {
    parts <- c(
      if (!is.null(thresh$chuiho_start)) paste0("注意報開始", thresh$chuiho_start) else NULL,
      paste0("警報開始", thresh$keiho_start),
      if (!is.null(thresh$keiho_end)) paste0("警報終息", thresh$keiho_end) else NULL
    )
    paste(parts, collapse="／")
  } else {
    paste0("参考基準値", thresh)
  }
}

# alert_threshold から警報開始基準値（グラフの参考線用）を取り出す
alert_threshold_keiho_start <- function(thresh) {
  if (is.null(thresh)) return(NULL)
  if (is.list(thresh)) thresh$keiho_start else thresh
}

# 現在値が基準値に対してどの水準にあるかを平易な言葉で表す（KPIカード表示用）
thresh_level_label <- function(value, thresh) {
  if (is.null(thresh) || is.na(value)) return(NULL)
  if (is.list(thresh)) {
    ks <- thresh$keiho_start; ke <- if (!is.null(thresh$keiho_end)) thresh$keiho_end else 0
    cs <- thresh$chuiho_start
    if (value >= ks) "警報"
    else if (!is.null(cs) && value >= cs) "注意報"
    else if (value >= ke) "流行期並み"
    else "平常"
  } else {
    if (value >= thresh * 2) "警戒"
    else if (value >= thresh) "注意"
    else if (value >= thresh * 0.3) "流行期並み"
    else "平常"
  }
}

# ── デモデータフォールバック ─────────────────────────────
generate_demo_fallback <- function(disease_ids=names(DISEASE_CONFIG), year_range=2024:2026) {
  set.seed(42)
  DEMO_PATTERNS <- list(
    flu  =function(w){pmax(exp(-0.5*((w-4)/3)^2)+exp(-0.5*((w-52)/3)^2),0.01)},
    rsv  =function(w){pmax(exp(-0.5*((w-44)/5)^2)+0.6*exp(-0.5*((w-26)/6)^2),0.01)},
    covid=function(w){pmax(0.7*exp(-0.5*((w-32)/5)^2)+exp(-0.5*((w-6)/6)^2),0.05)},
    ari  =function(w){pmax(exp(-0.5*((w-5)/6)^2)+0.3*exp(-0.5*((w-50)/5)^2),0.02)},
    strep=function(w){pmax(exp(-0.5*((w-10)/6)^2),0.01)},
    gi   =function(w){pmax(exp(-0.5*((w-48)/8)^2),0.01)},
    hfmd =function(w){pmax(exp(-0.5*((w-28)/6)^2),0.01)},
    mycop=function(w){pmax(exp(-0.5*((w-38)/8)^2)+0.3*exp(-0.5*((w-10)/6)^2),0.01)}
  )
  DEMO_BASE <- list(flu=8,rsv=2,covid=5,ari=15,strep=6,gi=8,hfmd=3,mycop=1.5)

  rows <- list()
  for (yr in year_range) {
    for (wk in 1:52) {
      d <- as.Date(paste0(yr,"-01-01")) + (wk-1)*7
      for (did in disease_ids) {
        if (!did %in% names(DEMO_PATTERNS)) next
        base <- DEMO_BASE[[did]] * DEMO_PATTERNS[[did]](wk)
        for (i in seq_len(nrow(PREF_MASTER))) {
          pref <- PREF_MASTER[i,]
          val <- max(0, rnbinom(1, mu=base*runif(1,0.5,1.8), size=3))
          rows[[length(rows)+1]] <- list(
            year=yr,week=wk,date=d,
            pref_code=pref$pref_code, pref_name=pref$pref_name, region=pref$region,
            disease=did, disease_label=DISEASE_CONFIG[[did]]$label,
            reports_per_site=round(val/ifelse(did=="ari",1,1),2),
            is_provisional=FALSE, data_source="デモデータ（参考値）"
          )
        }
      }
    }
  }
  message("デモデータ生成完了: ", length(rows), "行")
  bind_rows(rows)
}
