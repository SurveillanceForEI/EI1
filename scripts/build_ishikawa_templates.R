# 石川県週報PDF（疾患別ページの画像表）読み取り用の数字テンプレートを作る（一度実行すればよい）。
# 複数週のPDFをtesseractで全体OCRし、信頼度の高い数値トークンを正解ラベルとして
# セル内の桁画像と対応づけ、クラスタ多数決（純度90%以上）でテンプレートを確定する。
# 出力: data/ishikawa_templates.rds
setwd("//episynq-NAS1/public1/episynq/japan_surveillance")
source("R/hokenjo_fetch/ishikawa_glyph.R")

WEEKS <- 24:39
YEAR <- 2026
tmpdir <- tempfile("isk"); dir.create(tmpdir)
eng <- tesseract::tesseract("eng")

rate_f <- list(); rate_l <- character(0)
cnt_f <- list(); cnt_l <- character(0)

for (w in WEEKS) {
  f <- file.path(tmpdir, sprintf("%d.pdf", w))
  ok <- tryCatch({ download.file(sprintf("https://www.pref.ishikawa.lg.jp/hokan/kansenjoho/stock/%d/documents/%d-%d.pdf", YEAR, YEAR, w), f, mode = "wb", quiet = TRUE); TRUE },
                 error = function(e) FALSE, warning = function(e) FALSE)
  if (!ok) next
  txt <- pdftools::pdf_text(f)
  blank <- which(sapply(txt, function(p) nchar(trimws(gsub("[0-9]+", "", p))) < 3))
  pages <- blank[blank > 4]
  cat("week", w, "pages:", length(pages), "\n")
  for (p in pages) {
    g <- isk_page_gray(f, p)
    tf <- tempfile(fileext = ".png"); png::writePNG(g, tf)
    d <- tesseract::ocr_data(tf, engine = eng)
    bb <- do.call(rbind, strsplit(d$bbox, ","))
    d$xc <- (as.numeric(bb[, 1]) + as.numeric(bb[, 3])) / 2; d$x2 <- as.numeric(bb[, 3]); d$yc <- (as.numeric(bb[, 2]) + as.numeric(bb[, 4])) / 2
    d$conf <- as.numeric(d$confidence)
    d <- d[d$conf >= 80 & grepl("^[0-9.]+$", d$word) & d$yc > 580 & d$yc < 1330 & d$xc > 800, ]
    if (!nrow(d)) next
    # 定点あたり報告数（右揃え・小数2桁）
    rt <- d[grepl("^[0-9]{1,2}[.][0-9]{2}$", d$word), ]
    for (i in seq_len(nrow(rt))) {
      e <- round((rt$yc[i] - 668.5) / 122.4); c <- round((rt$x2[i] - 1058) / 333.4)
      if (e < 0 || e > 5 || c < 0 || c > 4 || abs(rt$yc[i] - (668.5 + 122.4 * e)) > 14 || abs(rt$x2[i] - (1058 + 333.4 * c)) > 16) next
      ch <- strsplit(sub("[.]", "", rt$word[i]), "")[[1]]
      slots <- if (length(ch) == 4) c("Z", "A", "B", "C") else c("A", "B", "C")
      R <- 1058 + 333.4 * c; cy <- 668.5 + 122.4 * e
      for (k in seq_along(slots)) {
        sl <- .ISK_RATE_SLOTS[[slots[k]]]
        ft <- .isk_slot_feat(g[round(cy - 24):round(cy + 24), round(R + sl[1] - 3):round(R + sl[2] + 3)])
        if (!is.null(ft)) { rate_f[[length(rate_f) + 1]] <- ft; rate_l <- c(rate_l, ch[k]) }
      }
    }
    # 報告数（細字・中央揃え・整数）
    ct <- d[grepl("^[0-9]{1,3}$", d$word), ]
    for (i in seq_len(nrow(ct))) {
      e <- round((ct$yc[i] - 610) / 122.4); c <- round((ct$xc[i] - 874) / 333.4)
      if (e < 0 || e > 5 || c < 0 || c > 4 || abs(ct$yc[i] - (610 + 122.4 * e)) > 14 || abs(ct$xc[i] - (874 + 333.4 * c)) > 45) next
      gl <- .isk_count_glyphs(g, e, c); ch <- strsplit(ct$word[i], "")[[1]]
      if (length(gl) == length(ch)) for (q in seq_along(gl)) { cnt_f[[length(cnt_f) + 1]] <- gl[[q]]; cnt_l <- c(cnt_l, ch[q]) }
    }
  }
}

make_templates <- function(feats, labs, thr = 1.6, min_n = 2, min_purity = 0.9) {
  F <- do.call(rbind, feats)
  reps <- matrix(0, 0, ncol(F)); lab <- integer(nrow(F))
  for (i in seq_len(nrow(F))) {
    if (nrow(reps) > 0) { d <- sqrt(rowSums((reps - matrix(F[i, ], nrow(reps), ncol(F), byrow = TRUE))^2)); j <- which.min(d) } else { d <- Inf; j <- 0 }
    if (nrow(reps) > 0 && d[j] <= thr) lab[i] <- j else { reps <- rbind(reps, F[i, ]); lab[i] <- nrow(reps) }
  }
  tab <- table(lab, labs)
  keep <- which(rowSums(tab) >= min_n & apply(tab, 1, max) / rowSums(tab) >= min_purity)
  list(rep = reps[as.integer(rownames(tab)[keep]), , drop = FALSE],
       label = colnames(tab)[apply(tab[keep, , drop = FALSE], 1, which.max)],
       n = rowSums(tab)[keep])
}
tr <- make_templates(rate_f, rate_l); tc <- make_templates(cnt_f, cnt_l)
cat("rate templates:", nrow(tr$rep), " digits covered:", paste(sort(unique(tr$label)), collapse = ""), "\n")
cat("count templates:", nrow(tc$rep), " digits covered:", paste(sort(unique(tc$label)), collapse = ""), "\n")
saveRDS(list(rate = tr, count = tc, built = Sys.time(), weeks = WEEKS), "data/ishikawa_templates.rds")
cat("saved data/ishikawa_templates.rds\n")
