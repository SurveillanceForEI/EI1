# ============================================================
# 保健所別マップ タブ用ヘルパー
# ------------------------------------------------------------
# data/hokenjo_current.rds （scripts/refresh_hokenjo_data.R で生成）
# と data/geo/hokenjo_boundaries/<slug>.geojson、
# data/geo/hokenjo_name_map.csv （週報側と境界側の保健所名 表記ゆれ
# 正規化テーブル）を組み合わせて、都道府県+疾患を選んだときの
# 保健所単位の地図・比較グラフ用データを作る。
# ============================================================

source("R/hokenjo_boundary_pipeline.R")   # .PREF_SLUG を使う

HOKENJO_CURRENT_PATH <- "data/hokenjo_current.rds"
HOKENJO_HISTORY_PATH <- "data/hokenjo_history.rds"
HOKENJO_NAME_MAP_PATH <- "data/geo/hokenjo_name_map.csv"

# ISO週定義（月曜始まり）で、年+週番号から「YYYY年第N週（M/D〜M/D）」形式の
# 表示ラベルを作る。週報側のweek_labelは自治体ごとに書式がバラバラなため、
# スライダー表示用には暦から一律に計算する
hokenjo_week_period_label <- function(week_num, year = 2026) {
  base <- as.Date(sprintf("%d-01-04", year))  # ISO週1は必ず1/4を含む
  approx_date <- base + (week_num - 1) * 7
  monday <- approx_date - (as.integer(format(approx_date, "%u")) - 1)
  sunday <- monday + 6
  sprintf("%d年第%d週（%s〜%s）", year, week_num,
          format(monday, "%m/%d"), format(sunday, "%m/%d"))
}

load_hokenjo_current <- function() {
  if (!file.exists(HOKENJO_CURRENT_PATH)) return(NULL)
  d <- tryCatch(readRDS(HOKENJO_CURRENT_PATH), error = function(e) NULL)
  if (is.null(d)) return(NULL)
  # data/hokenjo_current.rds は複数のR実行環境（異なるロケール/R
  # バージョン）で生成されうるため、文字列のエンコーディング指定が
  # "unknown"のまま保存され、別環境で読み込むと文字化けすることがある。
  # 明示的にUTF-8として読み直すことで、実行環境に依存せず正しく
  # 表示されるようにする。
  # 生成元のR実行環境ではバイト列自体はUTF-8で書き出されているが、
  # 別環境（別ロケール）で読み込むとエンコーディングの"マーキング"が
  # native/unknown表示になる。enc2utf8()（バイト変換）ではなく
  # Encoding<-での明示的な再マーキングのみを行う（バイトは既に正しい
  # UTF-8のため、変換をかけると逆に文字化けする）
  chr_cols <- vapply(d, is.character, logical(1))
  for (col in names(d)[chr_cols]) {
    Encoding(d[[col]]) <- "UTF-8"
  }
  d
}

load_hokenjo_history <- function() {
  if (!file.exists(HOKENJO_HISTORY_PATH)) return(NULL)
  d <- tryCatch(readRDS(HOKENJO_HISTORY_PATH), error = function(e) NULL)
  if (is.null(d)) return(NULL)
  chr_cols <- vapply(d, is.character, logical(1))
  for (col in names(d)[chr_cols]) {
    Encoding(d[[col]]) <- "UTF-8"
  }
  # week_num（1〜52）は年をまたいで再利用されるため、週報スライダーで
  # 「2025年〜最新」のような年をまたぐ連続範囲を作れるよう、week_labelから
  # 実際の西暦年を抜き出しておく（年+週番号の複合キーの元になる）
  d$hokenjo_year <- vapply(d$week_label, .hokenjo_extract_year, integer(1))
  d
}

# week_labelは自治体ごとに書式がバラバラ（西暦/令和、全角/半角数字、
# 日付埋め込みなど）なため、複数パターンを順に試して西暦年を取り出す
.hokenjo_extract_year <- function(label) {
  if (is.na(label)) return(NA_integer_)
  s <- chartr("０１２３４５６７８９", "0123456789", label)
  m <- regmatches(s, regexpr("(20[0-9]{2})年", s))
  if (length(m) > 0 && nzchar(m)) return(as.integer(sub("年", "", m)))
  m <- regmatches(s, regexec("令和\\s*([0-9]+)\\s*年", s))[[1]]
  if (length(m) == 2) return(as.integer(m[2]) + 2018L)
  # 日付埋め込み形式は「2024.12.30〜2025.1.5」のように年またぎ週で
  # 開始日・終了日の年が異なることがあるため、最初の一致ではなく
  # 区間の終わり側=より新しい年を採用する（ISO週の所属年の慣例に合わせる）
  matched <- regmatches(s, gregexpr("(20[0-9]{2})[.\\/-]", s))[[1]]
  if (length(matched) > 0) return(max(as.integer(substr(matched, 1, 4))))
  # 福井県の週報ラベル "1 (R. 6.12.30 ～ R. 7. 1. 5)" のような、
  # 令和の略記「R.」＋週末（終了日側）の年から西暦を推定する
  # (開始日と終了日で年またぎがあり得るため、区間の終わり側=より新しい方を採る)
  rm <- gregexpr("R\\.\\s*([0-9]+)\\s*\\.", s)[[1]]
  if (rm[1] != -1) {
    matched <- regmatches(s, gregexpr("R\\.\\s*([0-9]+)\\s*\\.", s))[[1]]
    nums <- as.integer(sub("R\\.\\s*([0-9]+)\\s*\\.", "\\1", matched))
    if (length(nums) > 0) return(max(nums, na.rm = TRUE) + 2018L)
  }
  NA_integer_
}

# 年+週番号を「年*100+週番号」の単一整数キーに変換する（スライダーの値として使う）
hokenjo_year_week_key <- function(year, week_num) year * 100L + week_num

# 複合キーから「YYYY年第N週（M/D〜M/D）」ラベルを作る
hokenjo_week_period_label_key <- function(key) {
  year <- key %/% 100L
  week_num <- key %% 100L
  if (is.na(year) || is.na(week_num) || week_num < 1 || week_num > 53) return("")
  hokenjo_week_period_label(week_num, year)
}

load_hokenjo_name_map <- function() {
  if (!file.exists(HOKENJO_NAME_MAP_PATH)) return(NULL)
  tryCatch(utils::read.csv(HOKENJO_NAME_MAP_PATH, stringsAsFactors = FALSE, encoding = "UTF-8"),
            error = function(e) NULL)
}

# データが存在する都道府県一覧（保健所別マップタブの選択肢用）
hokenjo_available_prefs <- function(current_data) {
  if (is.null(current_data) || nrow(current_data) == 0) return(character(0))
  present <- unique(as.character(current_data$pref))
  # 都道府県コード順（.PREF_SLUGの定義順=JIS X 0401順）に並べる
  ordered <- names(.PREF_SLUG)[names(.PREF_SLUG) %in% present]
  # 万一.PREF_SLUGに無い県名が含まれていた場合も取りこぼさないよう末尾に追加
  c(ordered, setdiff(present, ordered))
}

# 指定県で選択可能な疾患一覧
hokenjo_available_diseases <- function(current_data, pref) {
  if (is.null(current_data)) return(character(0))
  sort(unique(current_data$disease[current_data$pref == pref]))
}

# 都道府県の境界GeoJSONを読み込む（st_read）。ファイルが無ければNULL
load_hokenjo_boundary <- function(pref) {
  slug <- .PREF_SLUG[[pref]]
  if (is.null(slug)) return(NULL)
  path <- file.path("data/geo/hokenjo_boundaries", paste0(slug, ".geojson"))
  if (!file.exists(path)) return(NULL)
  tryCatch(sf::st_read(path, quiet = TRUE), error = function(e) NULL)
}

# 週報側の保健所名を境界側の名称に正規化する（hokenjo_name_map.csvの
# report_name -> cw_name 対応を使う。マップが無い/一致しない場合は
# そのままの名前を返す＝境界側の名称と週報側の名称が元々一致している
# 大多数の県はこれで問題ない）
normalize_hokenjo_names <- function(df, pref, name_map) {
  # マッピング対象が無い（＝境界側と週報側の名称がそのまま一致する）県でも
  # 後続のjoinで使うhokenjo_boundary_name列は常に用意する
  if (is.null(name_map) || nrow(name_map) == 0) {
    df$hokenjo_boundary_name <- df$hokenjo
    return(df)
  }
  sub <- name_map[name_map$pref == pref & !is.na(name_map$report_name), ]
  if (nrow(sub) == 0) {
    df$hokenjo_boundary_name <- df$hokenjo
    return(df)
  }
  # report_name（週報側）→ cw_name（境界側）の対応表
  lut <- setNames(sub$cw_name, sub$report_name)
  matched <- lut[df$hokenjo]
  df$hokenjo_boundary_name <- ifelse(!is.na(matched), matched, df$hokenjo)
  df
}

# メイン画面の疾患ラベル（DISEASE_CONFIG由来）と、県ごとに表記ゆれのある
# 週報側疾患名（全角/半角、半角カタカナ、括弧の有無など）を突き合わせる
# ための正規化
.normalize_disease_label <- function(x) {
  # 半角カタカナ（ﾚﾝｻ・ｸﾗﾐｼﾞｱ・ｵｳﾑ等）を全角に統一してから、
  # 続く全角→半角変換（英数字・括弧・ハイフン）に流す
  if (requireNamespace("stringi", quietly = TRUE)) {
    x <- stringi::stri_trans_general(x, "Halfwidth-Fullwidth")
  }
  x <- chartr(
    "ＡＢＣＤＥＦＧＨＩＪＫＬＭＮＯＰＱＲＳＴＵＶＷＸＹＺ０１２３４５６７８９（）－",
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789()-", x)
  x <- gsub("[ 　]", "", x)
  # 「発疹」「発しん」のように漢字/ひらがな表記が県によって異なる疾患名を統一
  x <- gsub("発疹", "発しん", x, fixed = TRUE)
  # 「COVID-19」と「新型コロナウイルス感染症」は同一疾患の別表記のため統一する
  x <- gsub("新型コロナウイルス感染症", "COVID-19", x, fixed = TRUE)
  trimws(x)
}

# メイン画面の疾患ラベル(target_label)に対応する、指定県のHOKENJO_CURRENT側
# 疾患名を1つ返す（一致しなければNULL）。ロタ/オウム病除く等、括弧内の
# 限定条件で意味が変わる疾患があるため、括弧を除いた完全一致を優先し、
# 見つからない場合のみ「主要キーワードを含む」ゆるい一致にフォールバックする。
resolve_hokenjo_disease <- function(current_data, pref, target_label) {
  if (is.null(current_data) || is.null(target_label)) return(NULL)
  avail <- unique(current_data$disease[current_data$pref == pref])
  if (length(avail) == 0) return(NULL)

  target_norm <- .normalize_disease_label(target_label)
  avail_norm <- .normalize_disease_label(avail)

  # 1) 正規化後の完全一致
  hit <- avail[avail_norm == target_norm]
  if (length(hit) > 0) return(hit[1])

  # 2) 括弧内を除いた本体名での一致＋曖昧disambiguationキーワード確認
  strip_paren <- function(x) trimws(gsub("\\(.*?\\)", "", x))
  target_core <- strip_paren(target_norm)
  avail_core <- strip_paren(avail_norm)
  guard_words <- c("ロタ", "オウム", "ARI", "入院")
  target_guards <- guard_words[sapply(guard_words, function(g) grepl(g, target_norm, fixed = TRUE))]

  cand <- avail[avail_core == target_core]
  if (length(cand) == 0) return(NULL)
  if (length(cand) == 1) return(cand[1])

  cand_norm <- .normalize_disease_label(cand)
  for (c in cand) {
    c_norm <- .normalize_disease_label(c)
    c_guards <- guard_words[sapply(guard_words, function(g) grepl(g, c_norm, fixed = TRUE))]
    if (identical(sort(c_guards), sort(target_guards))) return(c)
  }
  NULL
}

# resolve_hokenjo_disease()は「標準ラベル→県の生の疾患名」の一方向だが、
# CSVエクスポート等で「県の生の疾患名→標準ラベル」の逆引きが必要なため、
# 候補ラベル一覧を総当たりして対応表を作る（都道府県ごとに一度だけ計算）
hokenjo_disease_label_map <- function(history_data, pref, candidate_labels) {
  if (is.null(history_data)) return(character(0))
  pref_data <- history_data[history_data$pref == pref, ]
  avail <- unique(pref_data$disease)
  if (length(avail) == 0) return(character(0))
  map <- setNames(rep(NA_character_, length(avail)), avail)
  for (lab in candidate_labels) {
    hit <- resolve_hokenjo_disease(pref_data, pref, lab)
    if (!is.null(hit) && hit %in% names(map) && is.na(map[hit])) map[hit] <- lab
  }
  map
}

# 都道府県+疾患を指定して、境界sfオブジェクトにcount/rateを結合したものを返す
build_hokenjo_map_data <- function(current_data, pref, disease, name_map = NULL) {
  boundary <- load_hokenjo_boundary(pref)
  if (is.null(boundary)) return(NULL)

  d <- current_data[current_data$pref == pref & current_data$disease == disease, ]
  if (nrow(d) == 0) return(NULL)
  d <- normalize_hokenjo_names(d, pref, name_map)
  names(d)[names(d) == "hokenjo"] <- "hokenjo_report"  # 境界側の"hokenjo"列と名前が衝突しないように退避
  # 同じ保健所名が複数行あると1対多結合になり得るため、代表1行に絞る
  d <- d[!duplicated(d$hokenjo_boundary_name), ]

  merged <- dplyr::left_join(boundary, d, by = c("hokenjo" = "hokenjo_boundary_name"))
  sf::st_as_sf(merged)
}
