setwd("C:/Users/kobayashi/Documents/R/japan_surveillance")

log_file <- "data/noise_check.log"
log <- function(...) {
  msg <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ...)
  cat(msg, "\n")
  cat(msg, "\n", file = log_file, append = TRUE)
}

log("=== EBS ノイズ記事チェック開始 ===")

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(httr)
  library(xml2)
  library(jsonlite)
})

source("R/ebs_rule_screening.R")
source("R/ebs_loader.R")

# ── キャッシュ読み込み ────────────────────────────────────────
cache_path <- "data/ebs_startup_cache.rds"
if (!file.exists(cache_path)) {
  log("キャッシュが存在しません。終了。")
  quit(status = 0)
}

d <- readRDS(cache_path)
log("キャッシュ総件数: ", nrow(d), " 件")

# ── ノイズ除外（is_noise_article）────────────────────────────
noise_flag <- mapply(is_noise_article, d$title, coalesce(d$summary, ""))
log("is_noise_article 除外対象: ", sum(noise_flag), " 件")

# ── 除外対象をキャッシュから除去して保存 ─────────────────────
if (sum(noise_flag) > 0) {
  d_clean <- d %>% filter(!noise_flag)
  saveRDS(d_clean, cache_path)
  log("ノイズ除去後キャッシュ保存: ", nrow(d_clean), " 件")
  d <- d_clean
} else {
  log("新規ノイズなし")
}

# ── disease_tags 分布 ─────────────────────────────────────────
if ("disease_tags" %in% names(d)) {
  tag_tbl <- sort(table(as.character(d$disease_tags)), decreasing = TRUE)
  log("disease_tags 分布 (上位10):")
  top10 <- head(tag_tbl, 10)
  for (nm in names(top10)) {
    log("  ", nm, ": ", top10[[nm]], " 件")
  }
}

# ── 疑わしい記事の抽出 ───────────────────────────────────────
# disease_tags が "other" のみ、かつ signal_level が FYI 以外 → 要確認
if ("signal_level" %in% names(d) && "disease_tags" %in% names(d)) {
  suspicious <- d %>%
    filter(
      as.character(disease_tags) %in% c("other", "general", "other,general") &
      as.character(signal_level) %in% c("Signal High", "Signal Low")
    )
  if (nrow(suspicious) > 0) {
    log("★ 疑わしい記事（other/general タグで Signal High/Low）: ", nrow(suspicious), " 件")
    for (i in seq_len(min(10, nrow(suspicious)))) {
      log("  [", as.character(suspicious$signal_level[i]), "] ",
          substr(suspicious$title[i], 1, 80))
    }
  } else {
    log("疑わしい記事（other/general で Signal High/Low）: なし")
  }
}

# ── disease_tags が other で過去7日以内の記事をサンプリング ──
recent_other <- d %>%
  filter(
    as.character(disease_tags) %in% c("other", "general"),
    !is.na(pub_date),
    pub_date >= Sys.Date() - 7
  )

if (nrow(recent_other) > 0) {
  log("直近7日 other/general 記事: ", nrow(recent_other), " 件")
  sample_n <- min(15, nrow(recent_other))
  sample_d <- recent_other[sample(seq_len(nrow(recent_other)), sample_n), ]
  log("サンプル（手動確認用）:")
  for (i in seq_len(nrow(sample_d))) {
    log("  ", substr(sample_d$title[i], 1, 90))
  }
} else {
  log("直近7日の other/general 記事: なし")
}

log("=== チェック完了 ===")
