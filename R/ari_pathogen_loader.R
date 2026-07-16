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

# 図8凡例（10カテゴリ・簡略版、全国＋地域別の積み上げ棒グラフ。図6と異なり
# インフルエンザ／パラインフルエンザ／RSウイルスの亜型は分けず1カテゴリにまとめている）
ARI_FIG8_LEGEND <- c(
  flu_a="#08519C", sars_cov2="#E0B84F", pi="#F28E2B", rhino_entero="#BCBDDC", other="#C49A6C",
  flu_b="#90D091", rsv="#FBB4C4", hmpv="#6A3D9A", adeno="#4D4D4D", negative="#E5E5E5"
)
ARI_FIG8_LABELS <- c(
  flu_a="インフルエンザウイルスA型", flu_b="インフルエンザウイルスB型", sars_cov2="SARS-CoV-2",
  rsv="RSウイルス", hmpv="ヒトメタニューモウイルス", pi="パラインフルエンザウイルス",
  rhino_entero="ライノ／エンテロウイルス", adeno="アデノウイルス", other="その他", negative="検出なし"
)
ARI_FIG8_ORDER <- names(ARI_FIG8_LEGEND)

# 図8の地域区分（全国＋8地域）
ARI_REGIONS <- c(
  national="全国", hokkaido_tohoku="北海道・東北", kanto="関東", hokuriku="北陸", tokai="東海",
  kinki="近畿", chugoku="中国", shikoku="四国", kyushu_okinawa="九州・沖縄"
)

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
.ari_find_bar_centers <- function(arr, probe_row, col_range, gap_thresh = 2) {
  r <- arr[,,1]; g <- arr[,,2]; b <- arr[,,3]
  is_nonwhite <- !(r > 250 & g > 250 & b > 250)
  colvals <- is_nonwhite[probe_row, col_range]
  nzcols <- col_range[which(colvals)]
  if (length(nzcols) == 0) return(list(centers = numeric(0), skipped_first = FALSE))
  grp <- cumsum(c(1, diff(nzcols) > gap_thresh))
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

# ── 図8用: OCRでy軸の最大値ラベルを読み取る ─────────────────
# 図8は全国＋8地域の9パネルで、パネルごとにy軸スケールが自動調整され
# 大きく異なる（60〜1500超）ため、図6/7のような固定オフセットが使えない。
# tesseract（ローカルのデータ更新時のみ使用。shinyapps.io本番では
# load_ari_pathogen_cached()でキャッシュ済みデータを読むだけなので不要）で
# 軸最上端のラベル文字を読み取る。OCRの誤読対策として、読み取った数字の
# 先頭から複数桁を試し、よくある「きりの良い」軸最大値と完全一致するものを
# 優先的に採用する
.ari_ocr_available <- function() {
  requireNamespace("tesseract", quietly = TRUE) && requireNamespace("magick", quietly = TRUE)
}

.ARI_NICE_AXIS_VALUES <- c(10,20,25,30,40,50,60,70,80,90,100,120,150,180,200,250,300,
                           350,400,500,600,700,800,900,1000,1200,1500,1800,2000,2500,3000)

.ari_ocr_top_axis_value <- function(arr, row_range, col_range) {
  if (!.ari_ocr_available()) return(NA_real_)
  crop <- arr[row_range, col_range, ]
  tmp1 <- tempfile(fileext = ".png"); tmp2 <- tempfile(fileext = ".png")
  on.exit(unlink(c(tmp1, tmp2)), add = TRUE)
  png::writePNG(crop / 255, tmp1)
  img <- magick::image_resize(magick::image_read(tmp1), "300%")
  magick::image_write(img, tmp2)
  eng <- tesseract::tesseract("eng", options = list(tessedit_char_whitelist = "0123456789,"))
  d <- tryCatch(tesseract::ocr_data(tmp2, engine = eng), error = function(e) NULL)
  if (is.null(d) || nrow(d) == 0) return(NA_real_)
  bbox <- do.call(rbind, lapply(strsplit(d$bbox, ","), as.numeric))
  raw <- d$word[which.min(bbox[, 2])]
  digits <- gsub("[^0-9]", "", raw)
  if (nchar(digits) == 0) return(NA_real_)
  for (len in nchar(digits):1) {
    cand <- suppressWarnings(as.numeric(substr(digits, 1, len)))
    if (!is.na(cand) && cand %in% .ARI_NICE_AXIS_VALUES) return(cand)
  }
  cand <- suppressWarnings(as.numeric(substr(digits, 1, min(3, nchar(digits)))))
  if (is.na(cand)) return(NA_real_)
  .ARI_NICE_AXIS_VALUES[which.min(abs(.ARI_NICE_AXIS_VALUES - cand))]
}

# ── 図8: 1パネル分（全国 or 1地域）を抽出する汎用関数 ──────
# panel_row_range/panel_col_range: パネル全体のおおよその範囲（ベースライン検出用）
# bar_col_range: 棒グラフ本体を探索するx範囲（実際のバー開始位置を含む広めの範囲でよい）
# dpi: y軸ラベルの相対オフセット（実測px値）をスケーリングするために使用
.ari_extract_fig8_panel <- function(arr, panel_row_range, panel_col_range,
                                     bar_col_range, leg_rgb, week_dates,
                                     gap_thresh = 2, dpi = 300) {
  row0 <- .ari_find_baseline_row(arr, panel_row_range, panel_col_range)
  if (is.na(row0)) return(NULL)

  bar_info <- .ari_find_bar_centers(arr, row0 - max(3, round((row0 - min(panel_row_range)) * 0.01)),
                                     bar_col_range, gap_thresh = gap_thresh)
  centers <- bar_info$centers
  if (length(centers) == 0) return(NULL)

  # y軸ラベルは実際に検出した最初のバー位置から見た相対オフセットで探す
  # （実測: 最初のバー中心から見て-220px〜-20px、dpi=300基準）
  bar_start <- centers[1]
  yt_off <- round(c(-220, -20) * (dpi / 300))
  ytick_col_range <- max(1, bar_start + yt_off[1]):max(2, bar_start + yt_off[2])
  ymax <- .ari_ocr_top_axis_value(arr, min(panel_row_range):row0, ytick_col_range)
  if (is.na(ymax)) return(NULL)
  # 直近側（週の並びの末尾）をweek_datesの長さに揃える
  n <- min(length(centers), length(week_dates))
  centers <- tail(centers, n)
  dates_use <- tail(week_dates, n)

  row_top <- min(panel_row_range)
  px_per_count <- (row0 - row_top) / ymax
  r <- arr[,,1]; g <- arr[,,2]; b <- arr[,,3]

  classify_col <- function(x_center) {
    col_r <- r[row_top:row0, x_center]; col_g <- g[row_top:row0, x_center]; col_b <- b[row_top:row0, x_center]
    d_all <- sapply(seq_len(nrow(leg_rgb)), function(i)
      (col_r - leg_rgb[i,1])^2 + (col_g - leg_rgb[i,2])^2 + (col_b - leg_rgb[i,3])^2)
    is_white <- col_r > 250 & col_g > 250 & col_b > 250
    best <- apply(d_all, 1, which.min)
    labs <- rownames(leg_rgb)[best]
    labs[is_white] <- NA_character_
    labs
  }

  rows_out <- lapply(seq_along(centers), function(i) {
    labs <- classify_col(centers[i])
    tab <- table(labs[!is.na(labs)])
    if (length(tab) == 0) return(NULL)
    counts <- round(as.numeric(tab) / px_per_count)
    tibble(date = dates_use[i], category = names(tab), reports = counts)
  })
  bind_rows(rows_out)
}

# ── PDFページ内で図6/図7のキャプションがあるページ番号を探す ──
.ari_find_fig_pages <- function(pdf_path) {
  pages <- tryCatch(pdftools::pdf_text(pdf_path), error = function(e) NULL)
  if (is.null(pages)) return(list(fig6 = NA_integer_, fig7 = NA_integer_, date_range = NULL))
  fig6 <- NA_integer_; fig7 <- NA_integer_; fig8 <- NA_integer_
  for (i in seq_along(pages)) {
    if (is.na(fig6) && grepl("図\\s*6[:：]?\\s*検体採取週ごとの病原体別報告数", pages[i])) fig6 <- i
    if (is.na(fig7) && grepl("図\\s*7[:：]?\\s*検体採取週ごとの病原体別陽性率", pages[i])) fig7 <- i
    if (is.na(fig8) && grepl("図\\s*8[:：]?\\s*検体採取週ごとの全国および地域別", pages[i])) fig8 <- i
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
  list(fig6 = fig6, fig7 = fig7, fig8 = fig8, date_range = date_range)
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

  # ── 図8: 全国＋地域別の積み上げ棒グラフ（9パネル） ─────────
  # ローカルのデータ更新時のみtesseract/magickが必要（未インストールなら
  # regional_dfはNULLになり、既存の全国合算データはそのまま利用可能）
  regional_df <- if (is.na(info$fig8) || !.ari_ocr_available()) {
    NULL
  } else {
    tryCatch(
      .ari_parse_fig8(pdf_path, info$fig8, week_dates, dpi = dpi),
      error = function(e) { message("図8解析エラー: ", e$message); NULL }
    )
  }

  list(counts = count_df, positivity = pos_df, regional = regional_df)
}

# ── 図8全体（全国＋8地域）を解析する ────────────────────────
.ari_parse_fig8 <- function(pdf_path, fig8_page, week_dates, dpi = 300) {
  leg_rgb_fig8 <- t(grDevices::col2rgb(ARI_FIG8_LEGEND)); rownames(leg_rgb_fig8) <- ARI_FIG8_ORDER

  # 全国パネル（図7と同じページの下部。凡例(2行x5列)の下にある）
  arr_nat <- .ari_render_page(pdf_path, fig8_page, dpi = dpi)
  H <- dim(arr_nat)[1]; W <- dim(arr_nat)[2]
  r <- arr_nat[,,1]; g <- arr_nat[,,2]; b <- arr_nat[,,3]
  mx <- pmax(r,g,b); mn <- pmin(r,g,b); sat <- mx - mn
  is_colored <- sat > 25 & mx > 40
  leg_search <- round(H*0.40):round(H*0.68)
  row_counts <- rowSums(is_colored[leg_search, ])
  nzr <- leg_search[which(row_counts > 5)]
  national_df <- NULL
  if (length(nzr) > 0) {
    grpr <- cumsum(c(1, diff(nzr) > 3))
    row_bands <- split(nzr, grpr)
    # 凡例の各行は十分な高さ（十数px以上）を持つはずなので、極小のノイズバンド
    # （チャート本体の先頭ピクセル等）を除外してから最後（最下段）を凡例とみなす
    row_bands <- Filter(function(x) diff(range(x)) >= 15, row_bands)
    if (length(row_bands) >= 2) {
      leg_bottom <- max(row_bands[[length(row_bands)]])
      national_df <- .ari_extract_fig8_panel(
        arr_nat,
        panel_row_range = (leg_bottom + 20):round(H*0.99),
        panel_col_range = round(W*0.09):round(W*0.98),
        bar_col_range   = round(W*0.09):round(W*0.99),
        leg_rgb = leg_rgb_fig8, week_dates = week_dates, gap_thresh = 2, dpi = dpi
      )
    }
  }
  if (!is.null(national_df)) national_df$region <- "national"

  # 地域パネル（2ページにまたがる2x2グリッド x 2ページ = 8地域）
  region_page1 <- names(ARI_REGIONS)[2:5]  # hokkaido_tohoku, kanto, hokuriku, tokai
  region_page2 <- names(ARI_REGIONS)[6:9]  # kinki, chugoku, shikoku, kyushu_okinawa
  quad_specs <- list(
    list(row = c(0.025, 0.30), col = c(0.05, 0.49), bar = c(0.09, 0.48)),
    list(row = c(0.025, 0.30), col = c(0.535, 0.99), bar = c(0.575, 0.965)),
    list(row = c(0.40, 0.565), col = c(0.05, 0.49), bar = c(0.09, 0.48)),
    list(row = c(0.40, 0.565), col = c(0.535, 0.99), bar = c(0.575, 0.965))
  )
  parse_region_page <- function(page_no, region_ids) {
    arr <- .ari_render_page(pdf_path, page_no, dpi = dpi)
    Hp <- dim(arr)[1]; Wp <- dim(arr)[2]
    out <- list()
    for (i in seq_along(region_ids)) {
      qs <- quad_specs[[i]]
      panel_row_range <- round(Hp*qs$row[1]):round(Hp*qs$row[2])
      panel_col_range <- round(Wp*qs$col[1]):round(Wp*qs$col[2])
      bar_col_range   <- round(Wp*qs$bar[1]):round(Wp*qs$bar[2])
      df <- tryCatch(
        .ari_extract_fig8_panel(arr, panel_row_range, panel_col_range,
                                bar_col_range, leg_rgb_fig8, week_dates, gap_thresh = 1, dpi = dpi),
        error = function(e) NULL)
      if (!is.null(df) && nrow(df) > 0) {
        df$region <- region_ids[i]
        out[[region_ids[i]]] <- df
      }
    }
    bind_rows(out)
  }

  df1 <- tryCatch(parse_region_page(fig8_page + 1, region_page1), error = function(e) NULL)
  df2 <- tryCatch(parse_region_page(fig8_page + 2, region_page2), error = function(e) NULL)

  bind_rows(national_df, df1, df2)
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
  new_regional <- if (!is.null(parsed$regional) && nrow(parsed$regional) > 0) {
    parsed$regional %>%
      mutate(year = as.integer(format(date, "%G")), week = as.integer(format(date, "%V")),
             source_report = basename(path))
  } else NULL

  # 週報は毎号、直近約64週分を再掲載する（「集計時点における報告数であるため、
  # 過去の週報で掲載された値とは必ずしも一致しない」との注記どおり、速報値が
  # 後日修正される）。そのため、新しく取得したnew_counts/new_pos/new_regionalを
  # 必ず先にbind_rowsし、distinct(.keep_all=TRUE)が先頭行（＝最新号の値）を
  # 残すことで、重複する過去週は常に最新号の値で上書きされるようにしている
  merged_counts <- if (!is.null(old) && !is.null(old$counts)) {
    bind_rows(new_counts, old$counts) %>% distinct(year, week, category, .keep_all = TRUE) %>% arrange(date)
  } else new_counts %>% arrange(date)

  merged_pos <- if (!is.null(new_pos)) {
    if (!is.null(old) && !is.null(old$positivity)) {
      bind_rows(new_pos, old$positivity) %>% distinct(year, week, category, .keep_all = TRUE) %>% arrange(date)
    } else new_pos %>% arrange(date)
  } else if (!is.null(old)) old$positivity else NULL

  # 図8（地域別）はtesseract/magick未インストール環境（本番サーバー等）では
  # NULLになりうるため、その場合は既存キャッシュのregionalをそのまま維持する
  merged_regional <- if (!is.null(new_regional)) {
    if (!is.null(old) && !is.null(old$regional)) {
      bind_rows(new_regional, old$regional) %>%
        distinct(year, week, category, region, .keep_all = TRUE) %>% arrange(date)
    } else new_regional %>% arrange(date)
  } else if (!is.null(old)) old$regional else NULL

  merged <- list(counts = merged_counts, positivity = merged_pos, regional = merged_regional)
  dir.create(dirname(ARI_DATA_CACHE), showWarnings = FALSE, recursive = TRUE)
  saveRDS(merged, ARI_DATA_CACHE)
  merged
}

load_ari_pathogen_cached <- function() {
  if (file.exists(ARI_DATA_CACHE)) tryCatch(readRDS(ARI_DATA_CACHE), error = function(e) NULL) else NULL
}
