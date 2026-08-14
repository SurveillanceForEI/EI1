# ============================================================
# 保健所別 定点把握感染症データ 取得パイプライン 共通スキーマ
# ------------------------------------------------------------
# 47都道府県それぞれの週報（PDF/CSV/Excel/HTML/専用システム）を
# パースする関数群を R/hokenjo_fetch/<slug>.R に1県1ファイルで実装する。
# 各パーサーは必ず以下の標準スキーマの data.frame を返すこと：
#
#   pref      : 都道府県名（例: "群馬県"）
#   week_label: 週表示（例: "2026年第31週"）。取得元にない場合はNA可
#   hokenjo   : 保健所・地域区分名（hokenjo_boundaries/*.geojson の
#               hokenjo プロパティ、または hokenjo_name_map.csv の
#               report_name と一致させる）
#   disease   : 疾患名（原文のまま。正規化は別途 disease_name_map で行う）
#   count     : 報告数（人）。数値。欠損/非公表は NA
#   rate      : 定点当たり報告数。数値。欠損/非公表は NA
#
# 各パーサーファイルは fetch_<slug>(week_url = NULL) という関数を
# エクスポートすること。week_url を省略した場合は
# hokenjo_data_sources.R の sample_url（最新週）を使う。
#
# 実行方法（例）:
#   source("R/hokenjo_fetch/hokenjo_fetch_schema.R")
#   source("R/hokenjo_fetch/gunma.R")
#   df <- fetch_gunma()
#   validate_hokenjo_fetch(df)
# ============================================================

HOKENJO_FETCH_SCHEMA_COLS <- c("pref", "week_label", "hokenjo", "disease", "count", "rate")

# 取得結果の形式チェック（パーサー実装時のセルフテスト用）
validate_hokenjo_fetch <- function(df, pref_expected = NULL) {
  problems <- character(0)

  missing_cols <- setdiff(HOKENJO_FETCH_SCHEMA_COLS, names(df))
  if (length(missing_cols) > 0) {
    problems <- c(problems, paste("列が不足:", paste(missing_cols, collapse = "、")))
  }
  if (length(problems) > 0) {
    cat("NG:\n"); for (p in problems) cat(" -", p, "\n")
    return(invisible(FALSE))
  }

  if (nrow(df) == 0) problems <- c(problems, "0行（データが取得できていない）")
  if (!is.null(pref_expected) && !all(df$pref == pref_expected)) {
    problems <- c(problems, "pref列に想定外の値がある")
  }
  if (!is.numeric(df$count) && !all(is.na(df$count))) problems <- c(problems, "count列が数値型でない")
  if (!is.numeric(df$rate) && !all(is.na(df$rate))) problems <- c(problems, "rate列が数値型でない")
  if (any(is.na(df$hokenjo) | df$hokenjo == "")) problems <- c(problems, "hokenjo列に空値がある")
  if (any(is.na(df$disease) | df$disease == "")) problems <- c(problems, "disease列に空値がある")

  n_hokenjo <- length(unique(df$hokenjo))
  n_disease <- length(unique(df$disease))
  cat(sprintf("行数=%d  保健所数=%d  疾患数=%d\n", nrow(df), n_hokenjo, n_disease))

  if (length(problems) > 0) {
    cat("NG:\n"); for (p in problems) cat(" -", p, "\n")
    return(invisible(FALSE))
  }
  cat("OK\n")
  invisible(TRUE)
}

# 全角数字・カンマ・ダッシュ等を含む文字列を数値に変換する共通ヘルパー
# 「－」「-」「―」「なし」等は NA（報告なし/非公表）として扱う
parse_hokenjo_number <- function(x) {
  x <- as.character(x)
  x <- chartr("０１２３４５６７８９．", "0123456789.", x)
  x <- gsub(",", "", x)
  x <- trimws(x)
  x[x %in% c("-", "―", "－", "…", "*", "", "なし", "NA")] <- NA
  suppressWarnings(as.numeric(x))
}
