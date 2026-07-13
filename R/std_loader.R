# ============================================================
# std_loader.R — 月報疾患（性感染症・薬剤耐性菌）データ取得・解析
#   性器クラミジア感染症・性器ヘルペスウイルス感染症・尖圭コンジローマ・
#   淋菌感染症・メチシリン耐性黄色ブドウ球菌感染症・ペニシリン耐性肺炎球菌感染症
#   （・薬剤耐性緑膿菌感染症：2026年4月6日より全数把握対象に変更）
#
# データソース: 感染症発生動向調査週報（IDWR）PDF内の月報セクション
#   「報告数・定点当り報告数，疾病・都道府県・性別」（総数）表。
# 週報1号につき月1回程度しか掲載されないため、対象年の全号を走査して
# 該当月が見つかるまで探索する（入院サーベイランスと同じPDFキャッシュを
# 再利用するため追加のダウンロードは不要）。
# ============================================================

library(dplyr)

STD_DATA_CACHE <- "data/cache_hosp/std_data.rds"

# 表に掲載される疾患の並び順（固定）。薬剤耐性緑膿菌感染症は2026年4月6日
# より全数把握に変更されたため、それ以降の号では表から外れ列数が6列に減る
STD_DISEASE_CONFIG <- list(
  chlamydia_genital = list(label = "性器クラミジア感染症",         color = "#e91e63"),
  herpes_genital     = list(label = "性器ヘルペスウイルス感染症",   color = "#9c27b0"),
  condyloma          = list(label = "尖圭コンジローマ",             color = "#673ab7"),
  gonorrhea          = list(label = "淋菌感染症",                   color = "#3f51b5"),
  mrsa                = list(label = "メチシリン耐性黄色ブドウ球菌感染症", color = "#009688"),
  prsp                = list(label = "ペニシリン耐性肺炎球菌感染症", color = "#795548"),
  mdrp_old            = list(label = "薬剤耐性緑膿菌感染症",         color = "#607d8b")
)
STD_DISEASE_ORDER <- names(STD_DISEASE_CONFIG)

# 都道府県行かどうかを判定し、数値・小数（"-"は0件）のトークン列を取り出す
# （入院サーベイランス表の.parse_pref_rowと同様のロジックだが、
#  「報告数」「定点当り」がペアになった小数値を含むため専用に実装）
.parse_std_pref_row <- function(line_trim, pref_names) {
  for (p in pref_names) {
    positions <- gregexpr(p, line_trim, fixed = TRUE)[[1]]
    if (positions[1] == -1) next
    if (positions[1] != 1) next
    last_pos <- positions[length(positions)]
    rest <- trimws(substring(line_trim, last_pos + nchar(p)))
    if (!grepl("^[0-9.[:space:]-]+$", rest)) next
    toks <- regmatches(rest, gregexpr("-|[0-9]+\\.[0-9]+|[0-9]+", rest))[[1]]
    if (length(toks) == 0 || length(toks) %% 2 != 0) next
    return(list(pref = p, toks = toks))
  }
  NULL
}

# ── PDF1件から、月報（総数）表を抽出する ─────────────────────
# 戻り値: data.frame(year, month, date, pref_name, disease, reports, per_site)
#         見つからない場合はNULL
parse_idwr_std_pdf <- function(pdf_path) {
  pages <- tryCatch(pdftools::pdf_text(pdf_path), error = function(e) NULL)
  if (is.null(pages) || length(pages) == 0) return(NULL)

  pref_names <- PREF_MASTER$pref_name
  target_page <- NULL
  for (i in seq_along(pages)) {
    pg <- pages[[i]]
    if (grepl("報告数・定点当り報告数", pg, fixed = TRUE) &&
        grepl("（総数）", pg, fixed = TRUE) &&
        grepl("性器クラミジア", pg, fixed = TRUE)) {
      target_page <- i
      break
    }
  }
  if (is.null(target_page)) return(NULL)

  pg_text <- pages[[target_page]]
  heading_pos <- regexpr("報告数・定点当り報告数", pg_text)
  search_block <- substring(pg_text, heading_pos)
  ym_m <- regmatches(search_block, regexpr("[0-9]{4}年[0-9]{1,2}月", search_block))
  if (length(ym_m) == 0) return(NULL)
  ym <- regmatches(ym_m, regexec("([0-9]{4})年([0-9]{1,2})月", ym_m))[[1]]
  yr <- as.integer(ym[2]); mo <- as.integer(ym[3])

  lines <- strsplit(pg_text, "\n")[[1]]
  collected <- list()
  n_disease <- NA_integer_
  for (ln in lines) {
    lt <- trimws(ln)
    if (lt == "") next
    row <- .parse_std_pref_row(lt, pref_names)
    if (!is.null(row) && is.null(collected[[row$pref]])) {
      collected[[row$pref]] <- row$toks
      if (is.na(n_disease)) n_disease <- length(row$toks) / 2
    }
  }
  if (length(collected) == 0 || is.na(n_disease)) return(NULL)

  ids <- STD_DISEASE_ORDER[seq_len(min(n_disease, length(STD_DISEASE_ORDER)))]
  rows <- lapply(names(collected), function(p) {
    toks <- collected[[p]]
    if (length(toks) < n_disease * 2) return(NULL)
    reports  <- toks[seq(1, n_disease * 2, by = 2)]
    per_site <- toks[seq(2, n_disease * 2, by = 2)]
    reports  <- ifelse(reports == "-", 0, suppressWarnings(as.numeric(reports)))
    per_site <- ifelse(per_site == "-", 0, suppressWarnings(as.numeric(per_site)))
    data.frame(pref_name = p, disease = ids[seq_len(n_disease)],
               reports = reports[seq_len(n_disease)], per_site = per_site[seq_len(n_disease)],
               stringsAsFactors = FALSE)
  })
  out <- bind_rows(rows)
  if (nrow(out) == 0) return(NULL)
  out$year  <- yr
  out$month <- mo
  out$date  <- as.Date(sprintf("%d-%02d-01", yr, mo))
  out %>% select(year, month, date, pref_name, disease, reports, per_site) %>%
    distinct(year, month, pref_name, disease, .keep_all = TRUE)
}

# ── 対象年の全号（キャッシュ済みPDF）を走査し、月報データを収集する ──
# 入院サーベイランスと同じPDFキャッシュ(data/cache_hosp_pdf)を再利用するため、
# 未取得の号のみダウンロードする
update_std_data <- function(years, force_current_month = TRUE) {
  old <- if (file.exists(STD_DATA_CACHE)) {
    tryCatch(readRDS(STD_DATA_CACHE), error = function(e) NULL)
  } else NULL

  .sort_issues <- function(x) x[order(as.integer(sub("-.*", "", x)))]
  this_ym <- format(Sys.Date(), "%Y-%m")

  all_new <- list()
  for (yr in years) {
    issues <- .sort_issues(fetch_idwr_pdf_index(yr))
    if (length(issues) == 0) next
    already_months <- if (!is.null(old)) unique(old$month[old$year == yr]) else integer(0)

    for (iss in issues) {
      path <- download_idwr_pdf(yr, iss, force = FALSE)
      if (is.null(path)) next
      parsed <- tryCatch(parse_idwr_std_pdf(path), error = function(e) NULL)
      if (is.null(parsed) || nrow(parsed) == 0) next
      this_month_ym <- sprintf("%d-%02d", parsed$year[1], parsed$month[1])
      # 既に取得済みの月は、今月分（速報の訂正に備え再取得）以外はスキップ
      if (parsed$month[1] %in% already_months && !(force_current_month && this_month_ym == this_ym)) next
      all_new[[length(all_new) + 1]] <- parsed
    }
  }

  new_df <- if (length(all_new) > 0) bind_rows(all_new) else NULL
  merged <- if (!is.null(new_df) && !is.null(old)) {
    bind_rows(new_df, old) %>% distinct(year, month, pref_name, disease, .keep_all = TRUE)
  } else if (!is.null(new_df)) new_df else old

  if (!is.null(merged) && nrow(merged) > 0) {
    merged <- merged %>% arrange(date, pref_name, disease)
    dir.create(dirname(STD_DATA_CACHE), showWarnings = FALSE, recursive = TRUE)
    saveRDS(merged, STD_DATA_CACHE)
  }
  merged
}

load_std_cached <- function() {
  if (file.exists(STD_DATA_CACHE)) {
    tryCatch(readRDS(STD_DATA_CACHE), error = function(e) NULL)
  } else NULL
}

# ── 過去5年・同月±1か月平均±2SD帯を計算する（月次版） ────────
compute_std_band <- function(main_df, hist_df, value_col) {
  if (is.null(main_df) || nrow(main_df) == 0) return(main_df)
  rows <- lapply(seq_len(nrow(main_df)), function(i) {
    mo <- main_df$month[i]; y <- main_df$year[i]
    ms <- unique(pmax(1L, pmin(12L, (mo - 1):(mo + 1))))  # 前後1か月
    h <- hist_df[hist_df$month %in% ms & hist_df$year >= y - 5 & hist_df$year < y, , drop = FALSE]
    v <- h[[value_col]]
    n <- sum(!is.na(v))
    mu <- mean(v, na.rm = TRUE)
    s  <- if (n >= 3) sd(v, na.rm = TRUE) else NA_real_
    has <- n >= 3 && !is.nan(mu) && !is.na(s)
    data.frame(
      ymin = if (has) max(0, mu - 2 * s) else NA_real_,
      ymax = if (has) mu + 2 * s else NA_real_,
      has_hist = has
    )
  })
  cbind(main_df, do.call(rbind, rows))
}

# ── 月報疾患のIBS方式評価（直近2か月×過去5年・同月±1か月比較） ──
# 定点把握疾患のzensu_ibs_band（季節性ありの分岐）と同一ロジックの月次版。
# +2SD超過は直近2か月連続で該当した場合のみscore=3。
std_ibs_score <- function(cur_val, cur_year, cur_month, prev_val, prev_year, prev_month,
                           hist_d, value_col = "reports") {
  calc <- function(val, y, mo) {
    ms <- unique(pmax(1L, pmin(12L, (mo - 1):(mo + 1))))
    h  <- hist_d[hist_d$month %in% ms & hist_d$year >= y - 5 & hist_d$year < y, , drop = FALSE]
    v  <- h[[value_col]]
    n  <- sum(!is.na(v))
    mu <- mean(v, na.rm = TRUE)
    s  <- if (n >= 3) sd(v, na.rm = TRUE) else NA_real_
    has <- n >= 3 && !is.nan(mu) && !is.na(s)
    list(mu = mu, s = s, has_hist = has,
         exceeds2sd = has && !is.na(val) && val > 0 && val >= mu + 2 * s,
         exceeds1sd = has && !is.na(val) && val > 0 && val >= mu + s,
         abovemu    = has && !is.na(val) && val > 0 && val >= mu)
  }
  cb <- calc(cur_val, cur_year, cur_month)
  pb <- calc(prev_val, prev_year, prev_month)
  score <- if (!cb$has_hist) 0L
           else if (cb$exceeds2sd && pb$exceeds2sd) 3L
           else if (cb$exceeds2sd || cb$exceeds1sd) 2L
           else if (cb$abovemu) 1L
           else 0L
  label <- if (!cb$has_hist) "基準値なし" else
    c("0"="平均以下","1"="平均〜+1SD","2"="+1〜+2SD","3"="+2SD超過（2か月連続）")[as.character(score)]
  detail <- if (!cb$has_hist) "過去データ不足"
            else sprintf("%d件（基準 %.1f±%.1f）", as.integer(cur_val), cb$mu, cb$s)
  list(score = score, label = unname(label), detail = detail, method = "monthly", has_hist = cb$has_hist)
}
