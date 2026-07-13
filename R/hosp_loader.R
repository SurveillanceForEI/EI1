# ============================================================
# hosp_loader.R — 入院サーベイランス（基幹定点：インフルエンザ・
#                 新型コロナウイルス感染症 入院患者数）データ取得・解析
#
# データソース: 感染症発生動向調査週報（IDWR）PDF
#   https://id-info.jihs.go.jp/surveillance/idwr/idwr/{年}/idwr{年}-{号}.pdf
# 週報1号のPDF内に「報告数・疾病・都道府県別」という表が1つ（通常号）
# または2つ（2週分合併号）含まれており、都道府県別の入院患者報告数が
# 掲載されている。2023年5月の新型コロナ5類移行前は「インフルエンザ
# （入院患者）」列のみで、以降は「新型コロナウイルス感染症（入院患者）」
# 列が追加される。合併号では、各週の表が必ずしも隣接したページに
# あるとは限らないため、次の表見出しが現れるまで複数ページにわたって
# 都道府県行を探索する。
# ============================================================

library(httr)
library(dplyr)

HOSP_PDF_CACHE_DIR <- "data/cache_hosp_pdf"
HOSP_DATA_CACHE    <- "data/cache_hosp/hosp_data.rds"
IDWR_PDF_BASE      <- "https://id-info.jihs.go.jp/surveillance/idwr/idwr"

# ── 年別インデックスページから実在する号を取得 ──────────────────
# 号番号は通常 "18" のような単一数字だが、2週分合併号は
# "18-19" のようにハイフンでつながった号番号になる（例: idwr2026-18-19.pdf）。
# そのため号は文字列（issue token）として扱う。
fetch_idwr_pdf_index <- function(year, timeout_sec = 15) {
  url <- paste0(IDWR_PDF_BASE, "/", year, "/")
  tryCatch({
    resp <- GET(url, timeout(timeout_sec),
                add_headers("User-Agent" = "JapanSurveillanceDashboard/1.0"))
    if (status_code(resp) != 200) return(character(0))
    txt <- content(resp, "text", encoding = "UTF-8")
    m <- gregexpr(paste0("idwr", year, "-([0-9]+(?:-[0-9]+)?)\\.pdf"), txt, perl = TRUE)
    hits <- regmatches(txt, m)[[1]]
    if (length(hits) == 0) return(character(0))
    tokens <- gsub(paste0("^idwr", year, "-|\\.pdf$"), "", hits)
    # 号の先頭数字順にソート（"18-19" は 18 として扱う）
    ord <- as.integer(sub("-.*", "", tokens))
    unique(tokens[order(ord)])
  }, error = function(e) character(0))
}

# ── 単一号のPDFをダウンロード（キャッシュ済みならスキップ） ──────
# issue: fetch_idwr_pdf_index() が返す号トークン（例: "26", "18-19"）
download_idwr_pdf <- function(year, issue, force = FALSE, timeout_sec = 30) {
  if (!dir.exists(HOSP_PDF_CACHE_DIR)) dir.create(HOSP_PDF_CACHE_DIR, recursive = TRUE)
  path <- file.path(HOSP_PDF_CACHE_DIR, sprintf("%d-%s.pdf", year, issue))
  if (file.exists(path) && !force) return(path)
  url <- sprintf("%s/%d/idwr%d-%s.pdf", IDWR_PDF_BASE, year, year, issue)
  ok <- tryCatch({
    resp <- GET(url, timeout(timeout_sec),
                add_headers("User-Agent" = "JapanSurveillanceDashboard/1.0"),
                write_disk(path, overwrite = TRUE))
    status_code(resp) == 200
  }, error = function(e) FALSE)
  if (!ok) { if (file.exists(path)) unlink(path); return(NULL) }
  path
}

# 都道府県行かどうかを判定し、末尾の数値（"-"は0件）を取り出す
# 表の実データ行のみを拾うため、都道府県名の直後が空白・数字・"-"のみで
# 構成される行だけを対象とする（本文中で都道府県名に言及した文章行は除外）。
#
# 2013〜2018年頃のIDWR週報は、入院サーベイランス表が別の疾患表
# （感染性胃腸炎（ロタウイルス）等）と横並びの2列レイアウトになっており、
# pdftoolsのテキスト抽出では1行に両方の表の内容が連結されてしまう
# （例:「北海道　1　0.04　　北海道　25」＝ロタウイルス表の北海道行＋
# インフルエンザ表の北海道行が同一行に）。この場合、行頭からの単純な
# 都道府県名一致では左側の表（無関係な疾患）の数値と誤認識するか、
# 数値以外の文字（小数点・別の都道府県名）が混ざり判定に失敗して
# しまうため、都道府県名が複数回出現する行では「最後の出現位置」以降を
# 対象にする（入院サーベイランス表は2列レイアウトの右側に配置される
# ため）。都道府県名が1回だけの通常レイアウトでは従来どおりの動作。
.parse_pref_row <- function(line_trim, pref_names) {
  for (p in pref_names) {
    positions <- gregexpr(p, line_trim, fixed = TRUE)[[1]]
    if (positions[1] == -1) next
    if (positions[1] != 1) next  # 行頭が都道府県名で始まらない行は対象外
    last_pos <- positions[length(positions)]
    rest <- trimws(substring(line_trim, last_pos + nchar(p)))
    if (!grepl("^[0-9[:space:]-]+$", rest)) next
    nums <- regmatches(rest, gregexpr("-|[0-9]+", rest))[[1]]
    if (length(nums) == 0) next
    nums <- ifelse(nums == "-", 0L, suppressWarnings(as.integer(nums)))
    if (any(is.na(nums))) next
    return(list(pref = p, nums = nums))
  }
  NULL
}

# ── PDF1件から、含まれる全ての週次表を抽出する ───────────────
# 戻り値: data.frame(year, week, date, pref_name, flu_hosp, covid_hosp)
parse_idwr_hosp_pdf <- function(pdf_path) {
  pages <- tryCatch(pdftools::pdf_text(pdf_path), error = function(e) NULL)
  if (is.null(pages) || length(pages) == 0) return(NULL)

  pref_names <- PREF_MASTER$pref_name

  header_idx <- which(vapply(pages, function(p) grepl("報告数・疾病・都道府県別", p), logical(1)))
  if (length(header_idx) == 0) return(NULL)

  results <- list()
  for (hi in seq_along(header_idx)) {
    start_page <- header_idx[hi]
    end_page   <- if (hi < length(header_idx)) header_idx[hi + 1] - 1L else length(pages)

    # この表に対応する「YYYY年第WW週」ラベルを取得。
    # ページ上部のランニングヘッダー（例:「2026年第18週（4月27日〜5月3日）、
    # 2026年第19週（5月4日〜5月10日）：通巻第28巻第18・19合併号」）にも同じ
    # パターンの文字列が含まれ、合併号ではどちらの週の表かを誤認識してしまう
    # ため、「報告数・疾病・都道府県別」という見出し文字列より後ろの部分だけを
    # 対象に検索する。また、古い号（〜2013年頃）は「第」の文字がなく
    # 「YYYY年WW週」という表記のため、「第」は任意（省略可）として扱う
    header_block <- pages[[start_page]]
    heading_pos <- regexpr("報告数・疾病・都道府県別", header_block)
    search_block <- substring(header_block, heading_pos + attr(heading_pos, "match.length"))
    wk_m <- regmatches(search_block, regexpr("[0-9]{4}年第?[0-9]{1,2}週", search_block))
    if (length(wk_m) == 0) next
    yw <- regmatches(wk_m, regexec("([0-9]{4})年第?([0-9]{1,2})週", wk_m))[[1]]
    yr <- as.integer(yw[2]); wk <- as.integer(yw[3])

    collected <- list()
    for (pg in start_page:end_page) {
      lines <- strsplit(pages[[pg]], "\n")[[1]]
      for (ln in lines) {
        lt <- trimws(ln)
        if (lt == "") next
        row <- .parse_pref_row(lt, pref_names)
        if (!is.null(row) && is.null(collected[[row$pref]])) {
          collected[[row$pref]] <- row$nums
        }
      }
      if (length(collected) >= length(pref_names)) break
    }
    if (length(collected) == 0) next

    df <- do.call(rbind, lapply(names(collected), function(p) {
      nums <- collected[[p]]
      flu   <- nums[1]
      covid <- if (length(nums) >= 2) nums[2] else NA_integer_
      data.frame(pref_name = p, flu_hosp = flu, covid_hosp = covid, stringsAsFactors = FALSE)
    }))
    df$year <- yr
    df$week <- wk
    results[[length(results) + 1]] <- df
  }

  if (length(results) == 0) return(NULL)
  out <- bind_rows(results)
  out$date <- as.Date(paste0(out$year, "-01-01")) + (out$week - 1) * 7
  out %>% select(year, week, date, pref_name, flu_hosp, covid_hosp) %>%
    distinct(year, week, pref_name, .keep_all = TRUE)
}

# ── 指定年の全号を取得・解析し、既存キャッシュとマージする ──────
# force_latest_n: 直近n号は既にキャッシュ済みでも再取得する（速報値の訂正反映用）
update_hosp_data <- function(years, force_latest_n = 2) {
  old <- if (file.exists(HOSP_DATA_CACHE)) {
    tryCatch(readRDS(HOSP_DATA_CACHE), error = function(e) NULL)
  } else NULL

  .sort_issues <- function(x) x[order(as.integer(sub("-.*", "", x)))]

  all_new <- list()
  for (yr in years) {
    issues <- .sort_issues(fetch_idwr_pdf_index(yr))
    if (length(issues) == 0) next
    already <- if (!is.null(old) && "source_issue" %in% names(old)) {
      unique(as.character(old$source_issue[old$source_year == yr]))
    } else character(0)
    force_set <- tail(issues, force_latest_n)
    to_fetch <- .sort_issues(union(setdiff(issues, already), force_set))

    for (iss in to_fetch) {
      path <- download_idwr_pdf(yr, iss, force = iss %in% force_set)
      if (is.null(path)) next
      parsed <- tryCatch(parse_idwr_hosp_pdf(path), error = function(e) NULL)
      if (!is.null(parsed) && nrow(parsed) > 0) {
        parsed$source_year  <- yr
        parsed$source_issue <- iss
        all_new[[length(all_new) + 1]] <- parsed
      }
    }
  }

  new_df <- if (length(all_new) > 0) bind_rows(all_new) else NULL
  merged <- if (!is.null(new_df) && !is.null(old)) {
    bind_rows(new_df, old) %>% distinct(year, week, pref_name, .keep_all = TRUE)
  } else if (!is.null(new_df)) new_df else old

  if (!is.null(merged) && nrow(merged) > 0) {
    merged <- merged %>% arrange(date, pref_name)
    dir.create(dirname(HOSP_DATA_CACHE), showWarnings = FALSE, recursive = TRUE)
    saveRDS(merged, HOSP_DATA_CACHE)
  }
  merged
}

# ── キャッシュ済み入院サーベイランスデータの読み込み ──────────
load_hosp_cached <- function() {
  if (file.exists(HOSP_DATA_CACHE)) {
    tryCatch(readRDS(HOSP_DATA_CACHE), error = function(e) NULL)
  } else NULL
}

# ── 過去5年・同時期（±2週）平均±2SD帯を計算する ────────────
# 定点把握疾患の流行曲線（compute_ibs_band）と同様のロジック。
# main_df: 表示対象の週次データ（year, week, date, <value_col>列を含む）
# hist_df: 比較対象の過去データ（date_rangeスライダーに依存しない全期間データ）
# value_col: 対象列名（"flu_hosp" または "covid_hosp"）
compute_hosp_band <- function(main_df, hist_df, value_col) {
  if (is.null(main_df) || nrow(main_df) == 0) return(main_df)
  rows <- lapply(seq_len(nrow(main_df)), function(i) {
    w <- main_df$week[i]; y <- main_df$year[i]
    ws <- unique(pmax(1L, pmin(53L, (w - 2L):(w + 2L))))
    h <- hist_df[hist_df$week %in% ws & hist_df$year >= y - 5 & hist_df$year < y, , drop = FALSE]
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
