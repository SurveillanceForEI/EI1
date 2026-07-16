# ============================================================
# ari_pathogen_loader.R — ARIサーベイランス週報（PDF）の
#   図6「検体採取週ごとの病原体別報告数」（積み上げ棒グラフ）
#   図7「検体採取週ごとの病原体別陽性率」（折れ線グラフ）
#   をグラフ画像のピクセル解析により数値化する
#
# 出典: https://id-info.jihs.go.jp/surveillance/idss/target-diseases/
#        acute-respiratory-infection/weekly-report/index.html
#
# 注意: このPDFには図の元データの数値表が掲載されていないため、
# レンダリングした画像のピクセル色を凡例の色と照合して値を推定する
# （近似値。特に薄い色調の近い亜型間ではピクセル単位の誤差が生じうる）。
# 軸の最大値（図6=1700、図7=60%）は現時点のレポートで観測された値を
# 定数として用いている。将来的にJIHS側で軸スケールが変更された場合は
# 再調整が必要になる可能性がある。
# ============================================================

library(dplyr)

ARI_BASE <- "https://id-info.jihs.go.jp/surveillance/idss/target-diseases/acute-respiratory-infection/weekly-report"
ARI_PDF_CACHE_DIR  <- "data/cache_ari_pdf"
ARI_DATA_CACHE     <- "data/cache_ari/ari_pathogen_data.rds"

# 図6凡例（20カテゴリ、報告数=積み上げ棒グラフ）
ARI_COUNT_LEGEND <- c(
  flu_a_h1pdm09 = "#2171B5", flu_b_unknown  = "#C7E9C0", pi_1         = "#D94801", hmpv    = "#6A3D9A",
  flu_a_h3      = "#6BAED6", sars_cov2       = "#E0B84F", pi_2         = "#F16913", rhino_entero = "#BCBDDC",
  flu_a_unknown = "#C6DBEF", rsv_a           = "#C51B8A", pi_3         = "#FDAE6B", adeno   = "#4D4D4D",
  flu_b_victoria= "#238B45", rsv_b           = "#FA9FB5", pi_4         = "#FDD0A2", other   = "#C49A6C",
  flu_b_yamagata= "#65C1A3", rsv_unknown     = "#FCDFDC", pi_unknown   = "#FDE5CD", negative= "#E4E4E4"
)
ARI_COUNT_LABELS <- c(
  flu_a_h1pdm09="インフルエンザウイルスA/H1pdm09亜型", flu_a_h3="インフルエンザウイルスA/H3亜型",
  flu_a_unknown="インフルエンザウイルスA（亜型不明）",
  flu_b_victoria="インフルエンザウイルスB/ビクトリア系統", flu_b_yamagata="インフルエンザウイルスB/山形系統",
  flu_b_unknown="インフルエンザウイルスB（系統不明）", sars_cov2="SARS-CoV-2",
  rsv_a="RSウイルスA型", rsv_b="RSウイルスB型", rsv_unknown="RSウイルス（型不明）",
  pi_1="パラインフルエンザウイルス1型", pi_2="パラインフルエンザウイルス2型",
  pi_3="パラインフルエンザウイルス3型", pi_4="パラインフルエンザウイルス4型",
  pi_unknown="パラインフルエンザウイルス（型不明）",
  hmpv="ヒトメタニューモウイルス", rhino_entero="ライノ／エンテロウイルス",
  adeno="アデノウイルス", other="その他", negative="検出なし"
)
ARI_COUNT_ORDER <- names(ARI_COUNT_LEGEND)
ARI_COUNT_YMAX  <- 1700  # 現行レポートで観測された軸最大値（将来変更の可能性あり）

# 図7凡例（4カテゴリ、陽性率=折れ線グラフ）
ARI_POS_LEGEND <- c(flu_a = "#08519C", flu_b = "#74C476", sars_cov2 = "#E0B84F", rsv = "#AE017E")
ARI_POS_LABELS <- c(flu_a="インフルエンザウイルスA型", flu_b="インフルエンザウイルスB型",
                    sars_cov2="SARS-CoV-2", rsv="RSウイルス")
ARI_POS_YMAX <- 60  # %

# ── PDFレンダリング用ヘルパー ─────────────────────────────
.ari_bitmap_to_array <- function(bmp) {
  a <- aperm(array(as.integer(unclass(bmp)), dim = dim(bmp)), c(3, 2, 1))  # -> [H,W,4]
  a[,,1:3]
}
.ari_render_page <- function(pdf_path, page, dpi = 300) {
  bmp <- pdftools::pdf_render_page(pdf_path, page = page, dpi = dpi)
  .ari_bitmap_to_array(bmp)
}

# ── ページ内の水平黒線（軸ベースライン）を検出 ───────────────
.ari_find_baseline_row <- function(arr, row_range, col_range) {
  r <- arr[,,1]; g <- arr[,,2]; b <- arr[,,3]
  is_dark <- r < 130 & g < 130 & b < 130
  cnt <- rowSums(is_dark[row_range, col_range])
  best <- row_range[which.max(cnt)]
  if (max(cnt) < length(col_range) * 0.5) return(NA_integer_)
  best
}

# ── y軸ラベルのテキスト行（暗ピクセル帯）を検出 ─────────────
# label_col_range: ラベル文字が描画されているx範囲
.ari_find_text_row_bands <- function(arr, row_range, label_col_range, gap_tol = 15) {
  r <- arr[row_range, label_col_range, 1]
  g <- arr[row_range, label_col_range, 2]
  b <- arr[row_range, label_col_range, 3]
  is_dark <- r < 150 & g < 150 & b < 150
  row_counts <- rowSums(is_dark)
  nz <- which(row_counts > 1)
  if (length(nz) == 0) return(integer(0))
  grp <- cumsum(c(1, diff(nz) > gap_tol))
  bands <- split(nz, grp)
  sapply(bands, function(x) round(mean(range(x)))) + row_range[1] - 1
}

# ── 棒（週）のx列中心を検出 ────────────────────────────────
# 軸に近い行を横断してnon-white runをバーとして検出し、最初の2つ
# （y軸ラベル文字による欠け・分断アーティファクト＝最古週）は除外する
.ari_find_bar_centers <- function(arr, probe_row, col_range) {
  r <- arr[,,1]; g <- arr[,,2]; b <- arr[,,3]
  is_nonwhite <- !(r > 250 & g > 250 & b > 250)
  colvals <- is_nonwhite[probe_row, col_range]
  nzcols <- col_range[which(colvals)]
  if (length(nzcols) == 0) return(list(centers = numeric(0), skipped_first = FALSE))
  grp <- cumsum(c(1, diff(nzcols) > 2))
  runs <- split(nzcols, grp)
  widths <- sapply(runs, function(x) diff(range(x)) + 1)
  med_w <- median(widths)
  # 先頭1-2本が通常幅より大幅に細い場合はアーティファクトとして除外
  skip_first <- 0
  while (skip_first < length(runs) - 1 && widths[skip_first + 1] < med_w * 0.7) {
    skip_first <- skip_first + 1
  }
  runs2 <- if (skip_first > 0) runs[(skip_first + 1):length(runs)] else runs
  centers <- round(sapply(runs2, function(x) mean(range(x))))
  list(centers = unname(centers), skipped_first = skip_first > 0)
}

# ── 凡例スウォッチの色を検出（棒グラフ: 5行x4列の格子配置） ──
.ari_sample_color_grid <- function(arr, row_centers, col_centers) {
  out <- matrix(NA_character_, nrow = length(row_centers), ncol = length(col_centers))
  for (ri in seq_along(row_centers)) for (ci in seq_along(col_centers)) {
    px <- arr[row_centers[ri], col_centers[ci], ]
    out[ri, ci] <- sprintf("#%02X%02X%02X", px[1], px[2], px[3])
  }
  out
}

# ── PDFページ内で図6/図7のキャプションがあるページ番号を探す ──
.ari_find_fig_pages <- function(pdf_path) {
  pages <- tryCatch(pdftools::pdf_text(pdf_path), error = function(e) NULL)
  if (is.null(pages)) return(list(fig6 = NA_integer_, fig7 = NA_integer_, date_range = NULL))
  fig6 <- NA_integer_; fig7 <- NA_integer_
  for (i in seq_along(pages)) {
    if (is.na(fig6) && grepl("図\\s*6[:：]?\\s*検体採取週ごとの病原体別報告数", pages[i])) fig6 <- i
    if (is.na(fig7) && grepl("図\\s*7[:：]?\\s*検体採取週ごとの病原体別陽性率", pages[i])) fig7 <- i
  }
  # データ範囲テキスト（例: データ範囲: 2025年4月7日～2026年6月28日）を抽出
  date_range <- NULL
  for (i in seq_along(pages)) {
    pos <- regexpr(
      "データ範囲[:：]\\s*([0-9]{4})\\s*年\\s*([0-9]{1,2})\\s*月\\s*([0-9]{1,2})\\s*日\\s*[～~]\\s*([0-9]{4})\\s*年\\s*([0-9]{1,2})\\s*月\\s*([0-9]{1,2})\\s*日",
      pages[i], perl = TRUE)
    if (pos == -1) next
    m <- regmatches(pages[i], pos)
    if (length(m) > 0 && nchar(m) > 0) {
      nums <- regmatches(m, gregexpr("[0-9]+", m))[[1]]
      date_range <- list(
        start = as.Date(sprintf("%s-%02d-%02d", nums[1], as.integer(nums[2]), as.integer(nums[3]))),
        end   = as.Date(sprintf("%s-%02d-%02d", nums[4], as.integer(nums[5]), as.integer(nums[6])))
      )
      break
    }
  }
  list(fig6 = fig6, fig7 = fig7, date_range = date_range)
}

# ── メイン: 1つのARI週報PDFから図6・図7を抽出する ─────────
parse_ari_pathogen_pdf <- function(pdf_path, dpi = 300) {
  info <- .ari_find_fig_pages(pdf_path)
  if (is.na(info$fig6) || is.na(info$fig7) || is.null(info$date_range)) return(NULL)

  # ── 図6: 積み上げ棒グラフ ──────────────────────────────
  arr6 <- .ari_render_page(pdf_path, info$fig6, dpi = dpi)
  H <- dim(arr6)[1]; W <- dim(arr6)[2]
  search_rows <- round(H * 0.15):round(H * 0.6)
  row0 <- .ari_find_baseline_row(arr6, search_rows, round(W*0.1):round(W*0.95))
  if (is.na(row0)) return(NULL)
  # 図6には明確な上端の枠線がなく（本文段落テキストとの誤検出が起きやすい）、
  # レポートテンプレートのプロット領域の物理サイズ（軸範囲）は号ごとにほぼ一定
  # であるため、ベースライン(0)からの固定ピクセルオフセットで上端(1700)を算出する
  # （dpi=300で実測した値をスケーリング）
  axes_box_px_at_300dpi <- 1262.5
  row_top <- row0 - axes_box_px_at_300dpi * (dpi / 300)
  px_per_count <- (row0 - row_top) / ARI_COUNT_YMAX

  bar_info <- .ari_find_bar_centers(arr6, row0 - 11, round(W*0.09):round(W*0.99))
  centers6 <- bar_info$centers
  if (length(centers6) == 0) return(NULL)

  # 凡例色グリッド(5行x4列)を検出: 凡例は棒グラフ下、ページ高の58%〜68%付近
  leg_row_range <- round(H*0.575):round(H*0.685)
  r6 <- arr6[,,1]; g6 <- arr6[,,2]; b6 <- arr6[,,3]
  mx <- pmax(r6,g6,b6); mn <- pmin(r6,g6,b6); sat <- mx - mn
  is_colored <- sat > 25 & mx > 40
  row_counts <- rowSums(is_colored[leg_row_range, ])
  nzr <- leg_row_range[which(row_counts > 5)]
  if (length(nzr) < 5) return(NULL)
  grpr <- cumsum(c(1, diff(nzr) > 3))
  row_bands <- split(nzr, grpr)
  row_centers <- sapply(row_bands, function(x) round(mean(range(x))))
  if (length(row_centers) != 5) return(NULL)
  col_counts <- colSums(is_colored[row_bands[[1]], ])
  nzc <- which(col_counts > 3)
  grpc <- cumsum(c(1, diff(nzc) > 5))
  col_bands <- split(nzc, grpc)
  col_centers <- sapply(col_bands, function(x) round(mean(range(x))))
  if (length(col_centers) != 4) return(NULL)
  swatch_hex <- .ari_sample_color_grid(arr6, row_centers, col_centers)
  detected_legend <- as.vector(t(swatch_hex))  # row-major: matches ARI_COUNT_ORDER order
  names(detected_legend) <- ARI_COUNT_ORDER
  leg_rgb <- t(grDevices::col2rgb(detected_legend)); rownames(leg_rgb) <- ARI_COUNT_ORDER

  # 最初の検出済みバー(centers6[1])は、y軸ラベルによる欠け・分断アーティファクトを
  # 除いた「最初の完全なバー」であり、これはdate_range$start（最古週）に対応する
  # （ピクセル位置と週ラベル「33」等を突き合わせて実測・検証済み）。
  # そのため、centers6の要素数ぶんだけstartから週次に単純に日付を割り当てる
  n_weeks <- length(centers6)
  week_dates <- info$date_range$start + 7 * (0:(n_weeks - 1))

  classify_col6 <- function(x_center) {
    col_r <- r6[(row_top):(row0), x_center]
    col_g <- g6[(row_top):(row0), x_center]
    col_b <- b6[(row_top):(row0), x_center]
    n <- length(col_r)
    d_all <- sapply(seq_len(nrow(leg_rgb)), function(i)
      (col_r - leg_rgb[i,1])^2 + (col_g - leg_rgb[i,2])^2 + (col_b - leg_rgb[i,3])^2)
    is_white <- col_r > 250 & col_g > 250 & col_b > 250
    best <- apply(d_all, 1, which.min)
    labs <- ARI_COUNT_ORDER[best]
    labs[is_white] <- NA_character_
    labs
  }

  count_rows <- lapply(seq_along(centers6), function(i) {
    labs <- classify_col6(centers6[i])
    tab <- table(labs[!is.na(labs)])
    if (length(tab) == 0) return(NULL)
    counts <- round(as.numeric(tab) / px_per_count)
    tibble(date = week_dates[i], category = names(tab), reports = counts)
  })
  count_df <- bind_rows(count_rows)
  count_df$year <- as.integer(format(count_df$date, "%G"))
  count_df$week <- as.integer(format(count_df$date, "%V"))

  # ── 図7: 折れ線グラフ(陽性率) ──────────────────────────
  arr7 <- .ari_render_page(pdf_path, info$fig7, dpi = dpi)
  H7 <- dim(arr7)[1]; W7 <- dim(arr7)[2]
  row0_7 <- .ari_find_baseline_row(arr7, round(H7*0.28):round(H7*0.42), round(W7*0.1):round(W7*0.95))
  # "60"ラベルのテキスト行を検出（y軸ラベル列は左端付近）
  label_bands <- .ari_find_text_row_bands(arr7, round(H7*0.08):round(H7*0.20),
                                          round(W7*0.055):round(W7*0.09))
  if (is.na(row0_7) || length(label_bands) == 0) return(list(counts = count_df, positivity = NULL))
  row_top_7 <- label_bands[1]  # 最初(最上)のラベル="60"のはず
  px_per_pct <- (row0_7 - row_top_7) / ARI_POS_YMAX

  r7 <- arr7[,,1]; g7 <- arr7[,,2]; b7 <- arr7[,,3]
  pos_rgb <- t(grDevices::col2rgb(ARI_POS_LEGEND)); rownames(pos_rgb) <- names(ARI_POS_LEGEND)
  extract_pct <- function(x_center, target_rgb, tol2 = 3000) {
    rows <- row_top_7:row0_7
    col_r <- r7[rows, x_center]; col_g <- g7[rows, x_center]; col_b <- b7[rows, x_center]
    d <- (col_r-target_rgb[1])^2 + (col_g-target_rgb[2])^2 + (col_b-target_rgb[3])^2
    matched <- which(d < tol2)
    if (length(matched) == 0) return(NA_real_)
    row <- median(matched) + rows[1] - 1
    val <- (row0_7 - row) / px_per_pct
    max(0, val)
  }
  # 図6と同じ週間隔・週数のはずなので同じcenters6を流用
  pos_rows <- lapply(seq_along(centers6), function(i) {
    vals <- sapply(names(ARI_POS_LEGEND), function(nm) extract_pct(centers6[i], pos_rgb[nm,]))
    tibble(date = week_dates[i], category = names(vals), positivity = unname(vals))
  })
  pos_df <- bind_rows(pos_rows) %>% filter(!is.na(positivity))
  pos_df$year <- as.integer(format(pos_df$date, "%G"))
  pos_df$week <- as.integer(format(pos_df$date, "%V"))

  list(counts = count_df, positivity = pos_df)
}

# ── 週報アーカイブ一覧から最新PDFのURLを取得 ───────────────
fetch_latest_ari_report_url <- function() {
  idx_url <- paste0(ARI_BASE, "/index.html")
  text <- tryCatch({
    resp <- httr::GET(idx_url, httr::timeout(15))
    if (httr::status_code(resp) != 200) return(NULL)
    rawToChar(httr::content(resp, "raw"))
  }, error = function(e) NULL)
  if (is.null(text)) return(NULL)
  m <- regmatches(text, gregexpr('href="(\\./archive/[0-9]{4}/ARI_[0-9]{4}w[0-9]{2}\\.pdf)"', text, perl = TRUE))[[1]]
  if (length(m) == 0) return(NULL)
  rel <- sub('^href="(.*)"$', "\\1", m[1])
  list(url = paste0(ARI_BASE, "/", sub("^\\./", "", rel)), rel = rel)
}

download_ari_pdf <- function(rel_path, force = FALSE) {
  dir.create(ARI_PDF_CACHE_DIR, showWarnings = FALSE, recursive = TRUE)
  fname <- basename(rel_path)
  dest <- file.path(ARI_PDF_CACHE_DIR, fname)
  if (!force && file.exists(dest)) return(dest)
  url <- paste0(ARI_BASE, "/", sub("^\\./", "", rel_path))
  ok <- tryCatch({
    resp <- httr::GET(url, httr::write_disk(dest, overwrite = TRUE), httr::timeout(60))
    httr::status_code(resp) == 200
  }, error = function(e) FALSE)
  if (!ok) return(NULL)
  dest
}

# ── 最新レポートを取得・解析し、キャッシュへマージする ─────
update_ari_pathogen_data <- function(force = FALSE) {
  latest <- fetch_latest_ari_report_url()
  if (is.null(latest)) return(load_ari_pathogen_cached())
  path <- download_ari_pdf(latest$rel, force = force)
  if (is.null(path)) return(load_ari_pathogen_cached())
  parsed <- tryCatch(parse_ari_pathogen_pdf(path), error = function(e) {
    message("ARI病原体 PDF解析エラー: ", e$message); NULL
  })
  if (is.null(parsed) || is.null(parsed$counts)) return(load_ari_pathogen_cached())

  old <- if (file.exists(ARI_DATA_CACHE)) tryCatch(readRDS(ARI_DATA_CACHE), error = function(e) NULL) else NULL

  new_counts <- parsed$counts %>% mutate(source_report = basename(path))
  new_pos <- if (!is.null(parsed$positivity)) parsed$positivity %>% mutate(source_report = basename(path)) else NULL

  merged_counts <- if (!is.null(old) && !is.null(old$counts)) {
    bind_rows(new_counts, old$counts) %>% distinct(year, week, category, .keep_all = TRUE) %>% arrange(date)
  } else new_counts %>% arrange(date)

  merged_pos <- if (!is.null(new_pos)) {
    if (!is.null(old) && !is.null(old$positivity)) {
      bind_rows(new_pos, old$positivity) %>% distinct(year, week, category, .keep_all = TRUE) %>% arrange(date)
    } else new_pos %>% arrange(date)
  } else if (!is.null(old)) old$positivity else NULL

  merged <- list(counts = merged_counts, positivity = merged_pos)
  dir.create(dirname(ARI_DATA_CACHE), showWarnings = FALSE, recursive = TRUE)
  saveRDS(merged, ARI_DATA_CACHE)
  merged
}

load_ari_pathogen_cached <- function() {
  if (file.exists(ARI_DATA_CACHE)) tryCatch(readRDS(ARI_DATA_CACHE), error = function(e) NULL) else NULL
}
