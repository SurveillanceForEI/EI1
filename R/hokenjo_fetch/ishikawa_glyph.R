# 石川県週報PDFの疾患別ページ（画像埋め込み表）を、固定ビットマップフォントの
# テンプレートマッチングで読み取る。
#
# 表のレイアウト（300dpiレンダリング上の絶対座標）:
#   行: 石川県(県全体) → 金沢市 → 南加賀 → 石川中央 → 能登中部 → 能登北部 (e=0..5)
#   列: 直近5週 (c=0..4)
#   報告数セル（細字・中央揃え）:  中心 x = 874 + 333.4*c,  y = 610   + 122.4*e
#   定点あたりセル（太字・右揃え）: 右端 x = 1058 + 333.4*c, y = 668.5 + 122.4*e
# 画像が低解像度のためtesseractでは欠落・誤読が多いが、フォントは全週同一なので
# テンプレート（data/ishikawa_templates.rds、scripts/build_ishikawa_templates.Rで生成）
# との最近傍照合で安定して読める。照合距離が閾値を超えた桁はNAとする。

.ISK_H <- 20L; .ISK_W <- 12L
.ISK_RATE_SLOTS <- list(Z = c(-120, -100), A = c(-91, -71), B = c(-49, -29), C = c(-20, 0))

.isk_resize_area <- function(g, H = .ISK_H, W = .ISK_W) {
  nr <- nrow(g); nc <- ncol(g); ri <- floor(seq(0, H) * nr / H); ci <- floor(seq(0, W) * nc / W)
  out <- matrix(0, H, W)
  for (i in 1:H) for (j in 1:W) {
    r1 <- ri[i] + 1; r2 <- max(r1, ri[i + 1]); c1 <- ci[j] + 1; c2 <- max(c1, ci[j + 1])
    out[i, j] <- mean(g[r1:r2, c1:c2])
  }
  out
}

# PDFページを300dpiで描画し、グレースケール行列(行=y, 列=x, 0..1)にする
isk_page_gray <- function(pdf, p, dpi = 300) {
  b <- pdftools::pdf_render_page(pdf, page = p, dpi = dpi)
  a <- b[1, , ]; storage.mode(a) <- "integer"
  t(a) / 255
}

.isk_feat <- function(gg) c(as.vector(.isk_resize_area(gg)), ncol(gg) / 30, nrow(gg) / 40)

# 定点あたり報告数セルの1桁スロット画像 -> 特徴量（インク無しならNULL）
.isk_slot_feat <- function(m, thr = 0.55) {
  m[is.na(m)] <- 1; ink <- m < thr
  if (sum(ink) < 20) return(NULL)
  rows <- which(rowSums(ink) > 0); cols <- which(colSums(ink) > 0)
  .isk_feat(m[min(rows):max(rows), min(cols):max(cols), drop = FALSE])
}

# 報告数セルを、間隔5px以下を同一字としてグリフ分割 -> 特徴量リスト
.isk_count_glyphs <- function(g, e, c, thr = 0.8) {
  cx <- 874 + 333.4 * c; cy <- 610 + 122.4 * e
  m <- g[round(cy - 26):round(cy + 26), round(cx - 52):round(cx + 52), drop = FALSE]; m[is.na(m)] <- 1
  ink <- m < thr; colink <- colSums(ink) > 0
  if (!any(colink)) return(list())
  idx <- which(colink); grp <- cumsum(c(1, diff(idx) > 5))
  res <- lapply(split(idx, grp), function(ci) {
    sub <- m[, ci[1]:ci[length(ci)], drop = FALSE]; rows <- which(rowSums(sub < thr) > 0)
    if (length(rows) < 6) return(NULL)
    .isk_feat(sub[min(rows):max(rows), , drop = FALSE])
  })
  Filter(Negate(is.null), res)
}

.isk_classify <- function(f, templ, maxd = 2.2) {
  d <- sqrt(rowSums((templ$rep - matrix(f, nrow(templ$rep), length(f), byrow = TRUE))^2))
  j <- which.min(d)
  if (d[j] > maxd) NA_character_ else templ$label[j]
}

.isk_templates <- local({
  cache <- NULL
  function(path = "data/ishikawa_templates.rds") {
    if (is.null(cache)) {
      if (!file.exists(path)) stop("テンプレートが見つかりません: ", path, "（scripts/build_ishikawa_templates.R で生成）")
      cache <<- readRDS(path)
    }
    cache
  }
})

# 定点あたり報告数セル -> 数値（読めなければNA）
isk_read_rate <- function(g, e, c, templ = .isk_templates()$rate) {
  R <- 1058 + 333.4 * c; cy <- 668.5 + 122.4 * e
  ch <- vapply(.ISK_RATE_SLOTS, function(sl) {
    m <- g[round(cy - 24):round(cy + 24), round(R + sl[1] - 3):round(R + sl[2] + 3)]
    f <- .isk_slot_feat(m)
    if (is.null(f)) "" else .isk_classify(f, templ)
  }, character(1))
  if (anyNA(ch) || ch[["A"]] == "" || ch[["B"]] == "" || ch[["C"]] == "") return(NA_real_)
  as.numeric(paste0(ch[["Z"]], ch[["A"]], ".", ch[["B"]], ch[["C"]]))
}

# 報告数セル -> 整数（読めなければNA）
isk_read_count <- function(g, e, c, templ = .isk_templates()$count) {
  gl <- .isk_count_glyphs(g, e, c)
  if (length(gl) == 0 || length(gl) > 3) return(NA_real_)
  ch <- vapply(gl, .isk_classify, character(1), templ = templ)
  if (anyNA(ch)) return(NA_real_)
  as.numeric(paste(ch, collapse = ""))
}

# 1ページ(疾患1つ)の 6行×5週 を読み取り、県全体行を除いた保健所5行の count/rate 行列を返す
isk_read_page <- function(g) {
  cnt <- matrix(NA_real_, 6, 5); rate <- matrix(NA_real_, 6, 5)
  for (e in 0:5) for (c in 0:4) {
    rate[e + 1, c + 1] <- isk_read_rate(g, e, c)
    cnt[e + 1, c + 1] <- isk_read_count(g, e, c)
  }
  list(count = cnt, rate = rate)
}
