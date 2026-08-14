# ============================================================
# 保健所管轄区域 境界データ 生成パイプライン
# ------------------------------------------------------------
# 市区町村境界（data/geo/municipality/*.json、国土数値情報N03を
# smartnews-smri/japan-topography が1%簡略化してGeoJSON化したもの）
# を、保健所ごとに指定した市区町村コード群でdissolve（結合）し、
# 保健所管轄区域のポリゴンを生成する。
#
# 出典・ライセンス：
#   市区町村境界データは国土数値情報（国土交通省）N03行政区域データを
#   smartnews-smri/japan-topography が変換・簡略化したもの。
#   https://github.com/smartnews-smri/japan-topography
#   国土数値情報利用約款（出典明記の上、商用・改変可）に基づく。
#   このアプリで使用する際は「国土数値情報（国土交通省）」の出典表記が必要。
#
# 使い方：
#   source("R/hokenjo_boundary_pipeline.R")
#   source("R/hokenjo_municipality_crosswalk.R")   # 都道府県ごとの保健所⇔市区町村コード対応表
#   gunma_hokenjo_sf <- dissolve_to_hokenjo("gunma", HOKENJO_MUNI_CROSSWALK[["群馬県"]])
# ============================================================

library(sf)

GEO_MUNI_DIR <- "data/geo/municipality"

# 都道府県名（漢字）→ ファイル名のローマ字スラッグ
.PREF_SLUG <- c(
  "北海道" = "hokkaido", "青森県" = "aomori", "岩手県" = "iwate", "宮城県" = "miyagi",
  "秋田県" = "akita", "山形県" = "yamagata", "福島県" = "fukushima", "茨城県" = "ibaraki",
  "栃木県" = "tochigi", "群馬県" = "gunma", "埼玉県" = "saitama", "千葉県" = "chiba",
  "東京都" = "tokyo", "神奈川県" = "kanagawa", "新潟県" = "niigata", "富山県" = "toyama",
  "石川県" = "ishikawa", "福井県" = "fukui", "山梨県" = "yamanashi", "長野県" = "nagano",
  "岐阜県" = "gifu", "静岡県" = "shizuoka", "愛知県" = "aichi", "三重県" = "mie",
  "滋賀県" = "shiga", "京都府" = "kyoto", "大阪府" = "osaka", "兵庫県" = "hyogo",
  "奈良県" = "nara", "和歌山県" = "wakayama", "鳥取県" = "tottori", "島根県" = "shimane",
  "岡山県" = "okayama", "広島県" = "hiroshima", "山口県" = "yamaguchi", "徳島県" = "tokushima",
  "香川県" = "kagawa", "愛媛県" = "ehime", "高知県" = "kochi", "福岡県" = "fukuoka",
  "佐賀県" = "saga", "長崎県" = "nagasaki", "熊本県" = "kumamoto", "大分県" = "oita",
  "宮崎県" = "miyazaki", "鹿児島県" = "kagoshima", "沖縄県" = "okinawa"
)

.pref_file <- function(pref_kanji) {
  slug <- .PREF_SLUG[[pref_kanji]]
  if (is.null(slug)) stop("未知の都道府県名: ", pref_kanji)
  code <- sprintf("%02d", which(names(.PREF_SLUG) == pref_kanji))
  path <- file.path(GEO_MUNI_DIR, paste0(code, "_", slug, ".json"))
  if (!file.exists(path)) stop("境界データが見つかりません: ", path)
  path
}

# 都道府県の市区町村ポリゴンを読み込む（N03_007 = 市区町村コード[JIS5桁]）
load_pref_municipalities <- function(pref_kanji) {
  d <- st_read(.pref_file(pref_kanji), quiet = TRUE)
  d$muni_code <- as.character(d$N03_007)
  d$muni_name <- d$N03_004
  d$gun_name  <- d$N03_003
  d
}

# 市区町村コード（またはmuni_name）→保健所名 の対応表を使って dissolve する
# crosswalk: data.frame(muni_code=character, hokenjo=character) または
#            data.frame(muni_name=character, hokenjo=character)
dissolve_to_hokenjo <- function(pref_kanji, crosswalk) {
  muni <- load_pref_municipalities(pref_kanji)

  if ("muni_code" %in% names(crosswalk)) {
    muni2 <- merge(muni, crosswalk[, c("muni_code", "hokenjo")], by = "muni_code", all.x = TRUE)
  } else if ("muni_name" %in% names(crosswalk)) {
    muni2 <- merge(muni, crosswalk[, c("muni_name", "hokenjo")], by = "muni_name", all.x = TRUE)
  } else {
    stop("crosswalk には muni_code または muni_name 列が必要です")
  }

  unmatched <- muni2[is.na(muni2$hokenjo), ]
  if (nrow(unmatched) > 0) {
    warning(nrow(unmatched), "件の市区町村が保健所に割り当てられていません: ",
            paste(unmatched$muni_name, collapse = "、"))
  }

  muni2 <- muni2[!is.na(muni2$hokenjo), ]
  agg <- aggregate(muni2["hokenjo"], by = list(hokenjo = muni2$hokenjo), FUN = function(x) x[1])
  agg <- agg[, "hokenjo", drop = FALSE]
  st_geometry(agg) <- st_make_valid(st_geometry(agg))
  agg
}

# 保健所ポリゴンをGeoJSONとして保存（Shinyのleaflet等で読み込む用）
save_hokenjo_geojson <- function(hokenjo_sf, path) {
  if (file.exists(path)) unlink(path)
  st_write(hokenjo_sf, path, driver = "GeoJSON", quiet = TRUE)
  invisible(path)
}
