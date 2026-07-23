# ============================================================
# zensu_loader.R  — 全数把握疾患データ取得・解析
# ============================================================

library(httr)
library(dplyr)
library(lubridate)

ZENSU_CACHE_DIR <- "data/cache_zensu"

# ── 全数把握疾患設定 ─────────────────────────────────────────
ZENSU_DISEASE_CONFIG <- list(
  # 1類
  ebola         = list(label="エボラ出血熱",               class="1類", color="#8e0000"),
  crimean_congo = list(label="クリミア・コンゴ出血熱",     class="1類", color="#8e0000"),
  smallpox      = list(label="痘そう",                     class="1類", color="#8e0000"),
  south_am_hem  = list(label="南米出血熱",                 class="1類", color="#8e0000"),
  plague        = list(label="ペスト",                     class="1類", color="#8e0000"),
  marburg       = list(label="マールブルグ病",             class="1類", color="#8e0000"),
  lassa         = list(label="ラッサ熱",                   class="1類", color="#8e0000"),
  # 2類
  polio         = list(label="急性灰白髄炎",               class="2類", color="#c0392b"),
  tb            = list(label="結核",                       class="2類", color="#c0392b"),
  diphtheria    = list(label="ジフテリア",                 class="2類", color="#c0392b"),
  sars          = list(label="重症急性呼吸器症候群",       class="2類", color="#c0392b"),
  mers          = list(label="中東呼吸器症候群",           class="2類", color="#c0392b"),
  avian_h5n1    = list(label="鳥インフルエンザ（Ｈ５Ｎ１）", class="2類", color="#c0392b"),
  avian_h7n9    = list(label="鳥インフルエンザ（Ｈ７Ｎ９）", class="2類", color="#c0392b"),
  # 3類
  cholera       = list(label="コレラ",                     class="3類", color="#e67e22"),
  dysentery     = list(label="細菌性赤痢",                 class="3類", color="#e67e22"),
  ehec          = list(label="腸管出血性大腸菌感染症",     class="3類", color="#e67e22"),
  typhoid       = list(label="腸チフス",                   class="3類", color="#e67e22"),
  paratyphoid   = list(label="パラチフス",                 class="3類", color="#e67e22"),
  # 4類
  hep_e         = list(label="Ｅ型肝炎",                   class="4類", color="#f39c12"),
  west_nile     = list(label="ウエストナイル熱",           class="4類", color="#f39c12"),
  hep_a         = list(label="Ａ型肝炎",                   class="4類", color="#f39c12"),
  echinococcus  = list(label="エキノコックス症",           class="4類", color="#f39c12"),
  mpox          = list(label="エムポックス",               class="4類", color="#f39c12"),
  yellow_fever  = list(label="黄熱",                       class="4類", color="#f39c12"),
  psittacosis   = list(label="オウム病",                   class="4類", color="#f39c12"),
  omsk_hem      = list(label="オムスク出血熱",             class="4類", color="#f39c12"),
  relapsing_f   = list(label="回帰熱",                     class="4類", color="#f39c12"),
  kyasanur      = list(label="キャサヌル森林病",           class="4類", color="#f39c12"),
  q_fever       = list(label="Ｑ熱",                       class="4類", color="#f39c12"),
  rabies        = list(label="狂犬病",                     class="4類", color="#f39c12"),
  coccidioides  = list(label="コクシジオイデス症",         class="4類", color="#f39c12"),
  zika          = list(label="ジカウイルス感染症",         class="4類", color="#f39c12"),
  sfts          = list(label="重症熱性血小板減少症候群",   class="4類", color="#f39c12"),
  hfrs          = list(label="腎症候性出血熱",             class="4類", color="#f39c12"),
  wee           = list(label="西部ウマ脳炎",               class="4類", color="#f39c12"),
  tick_enceph   = list(label="ダニ媒介脳炎",               class="4類", color="#f39c12"),
  anthrax       = list(label="炭疽",                       class="4類", color="#f39c12"),
  chikungunya   = list(label="チクングニア熱",             class="4類", color="#f39c12"),
  scrub         = list(label="つつが虫病",                 class="4類", color="#f39c12"),
  dengue        = list(label="デング熱",                   class="4類", color="#f39c12"),
  eee           = list(label="東部ウマ脳炎",               class="4類", color="#f39c12"),
  avian_other   = list(label="鳥インフルエンザ(Ｈ５Ｎ１を除く）", class="4類", color="#f39c12"),
  nipah         = list(label="ニパウイルス感染症",         class="4類", color="#f39c12"),
  spotted_f     = list(label="日本紅斑熱",                 class="4類", color="#f39c12"),
  japanese_enc  = list(label="日本脳炎",                   class="4類", color="#f39c12"),
  hps           = list(label="ハンタウイルス肺症候群",     class="4類", color="#f39c12"),
  b_virus       = list(label="Ｂウイルス病",               class="4類", color="#f39c12"),
  glanders      = list(label="鼻疽",                       class="4類", color="#f39c12"),
  brucella      = list(label="ブルセラ症",                 class="4類", color="#f39c12"),
  vee           = list(label="ベネズエラウマ脳炎",         class="4類", color="#f39c12"),
  hendra        = list(label="ヘンドラウイルス感染症",     class="4類", color="#f39c12"),
  typhus        = list(label="発しんチフス",               class="4類", color="#f39c12"),
  botulism      = list(label="ボツリヌス症",               class="4類", color="#f39c12"),
  malaria       = list(label="マラリア",                   class="4類", color="#f39c12"),
  tularemia     = list(label="野兎病",                     class="4類", color="#f39c12"),
  lyme          = list(label="ライム病",                   class="4類", color="#f39c12"),
  lyssavirus    = list(label="リッサウイルス感染症",       class="4類", color="#f39c12"),
  rift_valley   = list(label="リフトバレー熱",             class="4類", color="#f39c12"),
  melioidosis   = list(label="類鼻疽",                     class="4類", color="#f39c12"),
  legionella    = list(label="レジオネラ症",               class="4類", color="#f39c12"),
  leptospira    = list(label="レプトスピラ症",             class="4類", color="#f39c12"),
  rocky_mtn     = list(label="ロッキー山紅斑熱",           class="4類", color="#f39c12"),
  # 5類全数
  ameba         = list(label="アメーバ赤痢",               class="5類全数", color="#2980b9"),
  hep_viral     = list(label="ウイルス性肝炎",             class="5類全数", color="#2980b9"),
  cre           = list(label="カルバペネム耐性腸内細菌目細菌感染症", class="5類全数", color="#2980b9"),
  acute_flaccid = list(label="急性弛緩性麻痺",             class="5類全数", color="#2980b9"),
  encephalitis  = list(label="急性脳炎",                   class="5類全数", color="#2980b9"),
  cryptospor    = list(label="クリプトスポリジウム症",     class="5類全数", color="#2980b9"),
  cjd           = list(label="クロイツフェルト・ヤコブ病", class="5類全数", color="#2980b9"),
  igas          = list(label="劇症型溶血性レンサ球菌感染症", class="5類全数", color="#2980b9"),
  aids          = list(label="後天性免疫不全症候群",       class="5類全数", color="#2980b9"),
  giardia       = list(label="ジアルジア症",               class="5類全数", color="#2980b9"),
  invasive_hib  = list(label="侵襲性インフルエンザ菌感染症", class="5類全数", color="#2980b9"),
  invasive_mening = list(label="侵襲性髄膜炎菌感染症",    class="5類全数", color="#2980b9"),
  invasive_pneu = list(label="侵襲性肺炎球菌感染症",      class="5類全数", color="#2980b9"),
  varicella_hosp = list(label="水痘（入院例）",            class="5類全数", color="#2980b9"),
  crs           = list(label="先天性風しん症候群",         class="5類全数", color="#2980b9"),
  mdra          = list(label="多剤耐性緑膿菌感染症",       class="5類全数", color="#2980b9"),
  syphilis      = list(label="梅毒",                       class="5類全数", color="#2980b9"),
  crypto_dissem = list(label="播種性クリプトコックス症",   class="5類全数", color="#2980b9"),
  tetanus       = list(label="破傷風",                     class="5類全数", color="#2980b9"),
  vrsa          = list(label="バンコマイシン耐性黄色ブドウ球菌感染症", class="5類全数", color="#2980b9"),
  vre           = list(label="バンコマイシン耐性腸球菌感染症", class="5類全数", color="#2980b9"),
  pertussis     = list(label="百日咳",                     class="5類全数", color="#2980b9"),
  rubella       = list(label="風しん",                     class="5類全数", color="#2980b9"),
  measles       = list(label="麻しん",                     class="5類全数", color="#2980b9"),
  dra           = list(label="薬剤耐性アシネトバクター感染症", class="5類全数", color="#2980b9")
)

# 疾患ラベル → ID マッピング
ZENSU_LABEL_MAP <- c(
  # 1類
  "エボラ出血熱"                               = "ebola",
  "クリミア・コンゴ出血熱"                     = "crimean_congo",
  "痘そう"                                     = "smallpox",
  "南米出血熱"                                 = "south_am_hem",
  "ペスト"                                     = "plague",
  "マールブルグ病"                             = "marburg",
  "ラッサ熱"                                   = "lassa",
  # 2類
  "急性灰白髄炎"                               = "polio",
  "結核"                                       = "tb",
  "ジフテリア"                                 = "diphtheria",
  "重症急性呼吸器症候群"                       = "sars",
  "中東呼吸器症候群"                           = "mers",
  "鳥インフルエンザ（Ｈ５Ｎ１）"             = "avian_h5n1",
  "鳥インフルエンザ（Ｈ７Ｎ９）"             = "avian_h7n9",
  # 3類
  "コレラ"                                     = "cholera",
  "細菌性赤痢"                                 = "dysentery",
  "腸管出血性大腸菌感染症"                     = "ehec",
  "腸チフス"                                   = "typhoid",
  "パラチフス"                                 = "paratyphoid",
  # 4類
  "Ｅ型肝炎"                                   = "hep_e",
  "ウエストナイル熱"                           = "west_nile",
  "Ａ型肝炎"                                   = "hep_a",
  "エキノコックス症"                           = "echinococcus",
  "エムポックス"                               = "mpox",
  "黄熱"                                       = "yellow_fever",
  "オウム病"                                   = "psittacosis",
  "オムスク出血熱"                             = "omsk_hem",
  "回帰熱"                                     = "relapsing_f",
  "キャサヌル森林病"                           = "kyasanur",
  "Ｑ熱"                                       = "q_fever",
  "狂犬病"                                     = "rabies",
  "コクシジオイデス症"                         = "coccidioides",
  "ジカウイルス感染症"                         = "zika",
  "重症熱性血小板減少症候群"                   = "sfts",
  "腎症候性出血熱"                             = "hfrs",
  "西部ウマ脳炎"                               = "wee",
  "ダニ媒介脳炎"                               = "tick_enceph",
  "炭疽"                                       = "anthrax",
  "チクングニア熱"                             = "chikungunya",
  "つつが虫病"                                 = "scrub",
  "デング熱"                                   = "dengue",
  "東部ウマ脳炎"                               = "eee",
  "鳥インフルエンザ(Ｈ５Ｎ１を除く）"        = "avian_other",
  "ニパウイルス感染症"                         = "nipah",
  "日本紅斑熱"                                 = "spotted_f",
  "日本脳炎"                                   = "japanese_enc",
  "ハンタウイルス肺症候群"                     = "hps",
  "Ｂウイルス病"                               = "b_virus",
  "鼻疽"                                       = "glanders",
  "ブルセラ症"                                 = "brucella",
  "ベネズエラウマ脳炎"                         = "vee",
  "ヘンドラウイルス感染症"                     = "hendra",
  "発しんチフス"                               = "typhus",
  "ボツリヌス症"                               = "botulism",
  "マラリア"                                   = "malaria",
  "野兎病"                                     = "tularemia",
  "ライム病"                                   = "lyme",
  "リッサウイルス感染症"                       = "lyssavirus",
  "リフトバレー熱"                             = "rift_valley",
  "類鼻疽"                                     = "melioidosis",
  "レジオネラ症"                               = "legionella",
  "レプトスピラ症"                             = "leptospira",
  "ロッキー山紅斑熱"                           = "rocky_mtn",
  # 5類全数
  "アメーバ赤痢"                               = "ameba",
  "ウイルス性肝炎"                             = "hep_viral",
  "カルバペネム耐性腸内細菌目細菌感染症"       = "cre",
  "急性弛緩性麻痺"                             = "acute_flaccid",
  "急性脳炎"                                   = "encephalitis",
  "クリプトスポリジウム症"                     = "cryptospor",
  "クロイツフェルト・ヤコブ病"                 = "cjd",
  "劇症型溶血性レンサ球菌感染症"               = "igas",
  "後天性免疫不全症候群"                       = "aids",
  "ジアルジア症"                               = "giardia",
  "侵襲性インフルエンザ菌感染症"               = "invasive_hib",
  "侵襲性髄膜炎菌感染症"                       = "invasive_mening",
  "侵襲性肺炎球菌感染症"                       = "invasive_pneu",
  "水痘（入院例）"                             = "varicella_hosp",
  "先天性風しん症候群"                         = "crs",
  "多剤耐性緑膿菌感染症"                       = "mdra",
  "梅毒"                                       = "syphilis",
  "播種性クリプトコックス症"                   = "crypto_dissem",
  "破傷風"                                     = "tetanus",
  "バンコマイシン耐性黄色ブドウ球菌感染症"     = "vrsa",
  "バンコマイシン耐性腸球菌感染症"             = "vre",
  "百日咳"                                     = "pertussis",
  "風しん"                                     = "rubella",
  "麻しん"                                     = "measles",
  "薬剤耐性アシネトバクター感染症"             = "dra"
)

# ── CSV パーサー ─────────────────────────────────────────────
parse_zensu_csv <- function(text, year, week) {
  if (is.null(text) || nchar(text) < 10) return(NULL)
  lines <- strsplit(text, "\r\n|\n|\r")[[1]]
  if (length(lines) < 5) return(NULL)

  week_start <- as.Date(paste0(year, "-01-01")) + (week - 1) * 7

  # 行3: 疾患名ヘッダー
  disease_fields <- strsplit(lines[3], ",")[[1]]

  # 疾患名が入っている列インデックスを収集（2列ごとに報告・累積）
  col_map <- list()
  col_idx <- 1
  for (i in seq_along(disease_fields)) {
    dname <- trimws(disease_fields[i])
    if (dname == "" || is.na(dname)) next
    # 全角英字正規化して照合
    dname_norm <- iconv(dname, "UTF-8", "UTF-8")
    did <- ZENSU_LABEL_MAP[dname_norm]
    if (!is.na(did)) {
      col_map[[did]] <- i  # 報告列（次列が累積）
    }
  }
  if (length(col_map) == 0) return(NULL)

  # 行5以降: 都道府県データ（行5=総数、行6=北海道…）
  data_rows <- lines[6:length(lines)]
  data_rows <- data_rows[nchar(trimws(data_rows)) > 0]

  results <- lapply(data_rows, function(row) {
    fields <- strsplit(row, ",")[[1]]
    if (length(fields) < 2) return(NULL)
    pref <- trimws(fields[1])
    if (pref == "" || pref == "総数") return(NULL)
    pref_row <- PREF_MASTER %>% filter(pref_name == pref)
    if (nrow(pref_row) == 0) return(NULL)

    rows_list <- lapply(names(col_map), function(did) {
      ci  <- col_map[[did]]
      val <- suppressWarnings(as.integer(if (ci <= length(fields)) fields[ci] else NA_character_))
      cum <- suppressWarnings(as.integer(if ((ci+1) <= length(fields)) fields[ci+1] else NA_character_))
      tibble(
        year        = as.integer(year),
        week        = as.integer(week),
        date        = week_start,
        pref_code   = pref_row$pref_code,
        pref_name   = pref,
        disease     = did,
        disease_label = ZENSU_DISEASE_CONFIG[[did]]$label,
        disease_class = ZENSU_DISEASE_CONFIG[[did]]$class,
        cases       = val,
        cumulative  = cum
      )
    })
    bind_rows(rows_list)
  })
  bind_rows(Filter(Negate(is.null), results))
}

# ── URL 構築 ──────────────────────────────────────────────────
get_zensu_url <- function(year, week) {
  wk_str <- sprintf("%02d", week)
  if (year <= NIID_MAX_YEAR) {
    sprintf("%s/idwr-%d/%d%s/%d-%s-zensu.csv", NIID_BASE, year, year, wk_str, year, wk_str)
  } else {
    sprintf("%s/%d/%s/%d-%s-zensu.csv", JIHS_BASE, year, wk_str, year, wk_str)
  }
}

# ── 週別取得・キャッシュ ──────────────────────────────────────
get_zensu_cache_path <- function(year, week) {
  dir.create(ZENSU_CACHE_DIR, recursive = TRUE, showWarnings = FALSE)
  file.path(ZENSU_CACHE_DIR, sprintf("%d-%02d.rds", year, week))
}

fetch_zensu_week <- function(year, week, force = FALSE) {
  cache_path <- get_zensu_cache_path(year, week)
  if (!force && file.exists(cache_path)) return(readRDS(cache_path))

  url  <- get_zensu_url(year, week)
  resp <- tryCatch(
    GET(url, timeout(20), add_headers("User-Agent" = "JapanSurveillanceDashboard/1.0")),
    error = function(e) NULL
  )
  if (is.null(resp) || status_code(resp) != 200) return(NULL)
  raw  <- content(resp, "raw")
  text <- iconv(rawToChar(raw), from = "CP932", to = "UTF-8", sub = "")
  d    <- parse_zensu_csv(text, year, week)
  if (!is.null(d) && nrow(d) > 0) saveRDS(d, cache_path)
  d
}

# ── キャッシュ全読み込み ─────────────────────────────────────
load_all_zensu_cached <- function() {
  dir.create(ZENSU_CACHE_DIR, recursive = TRUE, showWarnings = FALSE)
  files <- list.files(ZENSU_CACHE_DIR, pattern = "\\.rds$", full.names = TRUE)
  if (length(files) == 0) return(NULL)
  message("全数キャッシュ読み込み中 (", length(files), "週)...")
  raw_list <- lapply(files, readRDS)
  df <- bind_rows(raw_list)
  rm(raw_list); gc(FALSE)
  # メモリ削減: 低カーディナリティの文字列列をfactor化。
  # disease列はZENSU_DISEASE_CONFIG[[disease]]のリストキーとして使われるため
  # factor化しない（[[はfactorを整数コードで引いてしまうため）
  # factor()は変換中に旧文字列列と新factor列が一時的に両方メモリ上に存在するため、
  # 列ごとにgc()を挟んでピークメモリを抑える（メモリ逼迫環境でのOOM対策）
  for (col in c("pref_name", "region", "disease_label", "disease_class", "data_source")) {
    if (col %in% names(df)) {
      df[[col]] <- factor(df[[col]])
      gc(FALSE)
    }
  }
  df
}

# ── メイン: 定点データと同じパターン（キャッシュ優先・差分のみ取得）──
get_zensu_data <- function(years = 2020:2026, force = FALSE) {
  # ① 全キャッシュを一括読み込み
  cached_data <- if (!force) load_all_zensu_cached() else NULL

  cur_year <- as.integer(format(Sys.Date(), "%Y"))
  new_data  <- list()

  for (yr in years) {
    # 過去年でキャッシュが40週以上あればスキップ（定点と同じ閾値）
    if (!force && yr < cur_year) {
      max_wk <- if (yr %% 400 == 0 || (yr %% 4 == 0 && yr %% 100 != 0)) 53L else 52L
      cached_wks <- which(file.exists(
        sapply(1:max_wk, function(w) get_zensu_cache_path(yr, w))
      ))
      if (length(cached_wks) > 40) next
    }

    # 利用可能週一覧を取得（定点と共用の fetch_available_weeks を使う）
    avail_weeks <- tryCatch(
      fetch_available_weeks(yr),
      error = function(e) { message("全数 ", yr, "年: インデックス取得失敗"); NULL }
    )
    if (is.null(avail_weeks) || length(avail_weeks) == 0) next

    # キャッシュにない週だけ取得
    missing <- avail_weeks[!file.exists(
      sapply(avail_weeks, function(w) get_zensu_cache_path(yr, w))
    )]
    if (force) missing <- avail_weeks

    if (length(missing) > 0) {
      message("全数 ", yr, "年: ", length(missing), "週を取得中...")
      week_data <- lapply(missing, function(w) {
        tryCatch(fetch_zensu_week(yr, w, force), error = function(e) NULL)
      })
      new_data[[as.character(yr)]] <- bind_rows(Filter(Negate(is.null), week_data))
    }
  }

  result <- bind_rows(cached_data, bind_rows(new_data))
  if (is.null(result) || nrow(result) == 0) return(NULL)

  result %>%
    filter(year %in% years) %>%
    left_join(PREF_MASTER %>% select(pref_code, region), by = "pref_code") %>%
    arrange(year, week, pref_code, disease)
}
