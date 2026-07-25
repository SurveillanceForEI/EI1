# ============================================================
# rescreen_ebs_full.R — EBSキャッシュ全件の再スクリーニング（手動実行専用）
#
# screen_entry() / is_noise_article() 等の判定ロジックを変更した際に、
# 過去記事にもその変更を遡及適用するためのバッチスクリプト。
# 通常の自動更新（auto_update_deploy.R / app.Rの自動更新ロジック）は
# 新規記事のみをスクリーニングし、キャッシュ全件の再判定は行わない
# （日々の更新のたびに全件再判定するとコストが積み上がるため）。
#
# 実行方法:
#   Rscript scripts/rescreen_ebs_full.R
# ============================================================

setwd("C:/Users/kobayashi/Documents/R/japan_surveillance")

suppressPackageStartupMessages({
  library(dplyr)
})

source("R/data_loader.R")
source("R/ebs_rule_screening.R")
source("R/ebs_loader.R")

cache_path <- "data/ebs_startup_cache.rds"

cat("=== EBSキャッシュ全件再スクリーニング開始 ===\n")

old <- tryCatch(readRDS(cache_path), error = function(e) NULL)
if (is.null(old) || nrow(old) == 0) {
  stop("キャッシュが見つからないか空です: ", cache_path)
}
cat("対象件数:", nrow(old), "\n")

before <- old %>% select(title, signal_level, disease_tags)

rescreened <- rescreen_ebs_data(old) %>%
  dplyr::arrange(signal_level, dplyr::desc(pub_date))

after <- rescreened %>% select(title, signal_level, disease_tags)

# --- 差分レポート（signal_level / disease_tags の変化） ---
merged_diff <- before %>%
  rename(signal_level_before = signal_level, disease_tags_before = disease_tags) %>%
  left_join(
    after %>% rename(signal_level_after = signal_level, disease_tags_after = disease_tags),
    by = "title"
  )

changed_signal <- merged_diff %>%
  filter(as.character(signal_level_before) != as.character(signal_level_after))
changed_tags <- merged_diff %>%
  filter(coalesce(disease_tags_before, "") != coalesce(disease_tags_after, ""))

cat("\n=== signal_level が変化した記事:", nrow(changed_signal), "件 ===\n")
if (nrow(changed_signal) > 0) {
  for (i in seq_len(min(nrow(changed_signal), 50))) {
    r <- changed_signal[i, ]
    cat("[", as.character(r$signal_level_before), "->", as.character(r$signal_level_after), "]",
        r$title, "\n")
  }
  if (nrow(changed_signal) > 50) cat("...他", nrow(changed_signal) - 50, "件\n")
}

cat("\n=== disease_tags が変化した記事:", nrow(changed_tags), "件 ===\n")
if (nrow(changed_tags) > 0) {
  for (i in seq_len(min(nrow(changed_tags), 50))) {
    r <- changed_tags[i, ]
    cat(r$title, "\n  変更前:", r$disease_tags_before, " -> 変更後:", r$disease_tags_after, "\n")
  }
  if (nrow(changed_tags) > 50) cat("...他", nrow(changed_tags) - 50, "件\n")
}

# --- 保存前の確認プロンプト（対話実行時のみ） ---
if (interactive()) {
  ans <- readline("この結果でキャッシュを上書き保存しますか？ [y/N]: ")
  if (!tolower(trimws(ans)) %in% c("y", "yes")) {
    cat("保存をキャンセルしました。\n")
    quit(save = "no")
  }
}

saveRDS(rescreened, cache_path)
cat("\n=== 保存完了:", nrow(rescreened), "件 ===\n")
