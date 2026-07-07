# ============================================================
# correlation.R — EBS × IBS 時系列相関分析
# ============================================================

library(dplyr)
library(lubridate)

# ── EBS-IBS 統合データ構築 ─────────────────────────────────
build_ebs_ibs_combined <- function(ebs_data, ibs_data, disease_id) {
  # IBS: 全国週次平均
  ibs_weekly <- ibs_data %>%
    filter(disease == disease_id) %>%
    group_by(year, week) %>%
    summarise(ibs_value = mean(reports_per_site, na.rm=TRUE), .groups="drop") %>%
    mutate(date = as.Date(paste0(year, "-01-01")) + (week - 1) * 7)

  # EBS: 疾患フィルタ済み週次集計
  ebs_weekly <- aggregate_ebs_weekly(ebs_data, disease_filter=disease_id)

  # 結合（全IBS週を基準にleft join）
  combined <- ibs_weekly %>%
    left_join(ebs_weekly %>% select(year, week, ebs_count, ebs_signal_index,
                                    ebs_high_count, ebs_medium_count),
              by = c("year","week")) %>%
    mutate(
      ebs_count        = coalesce(ebs_count, 0L),
      ebs_signal_index = coalesce(ebs_signal_index, 0),
      ebs_high_count   = coalesce(ebs_high_count, 0L),
      ebs_medium_count = coalesce(ebs_medium_count, 0L)
    ) %>%
    arrange(date)

  combined
}

# ── 交差相関（CCF）計算 ───────────────────────────────────
compute_ccf <- function(combined, max_lag = 8) {
  x <- scale(combined$ebs_signal_index)
  y <- scale(combined$ibs_value)

  valid <- complete.cases(x, y)
  if (sum(valid) < 10) return(NULL)

  ccf_res <- ccf(x[valid], y[valid], lag.max = max_lag, plot = FALSE)

  tibble(
    lag = as.integer(ccf_res$lag),
    acf = as.numeric(ccf_res$acf),
    ci  = qnorm(0.975) / sqrt(sum(valid))
  )
}

# ── 最適ラグ検出 ──────────────────────────────────────────
find_optimal_lag <- function(ccf_df) {
  if (is.null(ccf_df) || nrow(ccf_df) == 0) return(NA_integer_)
  ccf_df %>%
    filter(lag <= 0) %>%   # EBSがIBSに先行する場合のみ（lag<0）
    slice_max(abs(acf), n=1) %>%
    pull(lag)
}

# ── 相関サマリー ──────────────────────────────────────────
correlation_summary <- function(combined, ccf_df) {
  if (is.null(ccf_df)) return(NULL)

  optimal_lag <- find_optimal_lag(ccf_df)
  corr_at_zero <- ccf_df %>% filter(lag == 0) %>% pull(acf) %>% first()
  corr_at_opt  <- ccf_df %>% filter(lag == optimal_lag) %>% pull(acf) %>% first()
  ci95 <- ccf_df$ci[1]

  # ラグオフセット適用後の散布図データ
  if (!is.na(optimal_lag) && optimal_lag < 0) {
    n <- nrow(combined)
    lag_abs <- abs(optimal_lag)
    scatter_df <- tibble(
      ebs_lagged = combined$ebs_signal_index[1:(n-lag_abs)],
      ibs_lead   = combined$ibs_value[(lag_abs+1):n],
      date       = combined$date[1:(n-lag_abs)]
    )
  } else {
    scatter_df <- tibble(
      ebs_lagged = combined$ebs_signal_index,
      ibs_lead   = combined$ibs_value,
      date       = combined$date
    )
  }

  list(
    optimal_lag  = optimal_lag,
    corr_zero    = round(corr_at_zero, 3),
    corr_optimal = round(corr_at_opt,  3),
    ci95         = round(ci95, 3),
    significant  = abs(corr_at_opt) > ci95,
    scatter_df   = scatter_df,
    n_obs        = nrow(combined)
  )
}

# ── 複数疾患横断ピアソン相関テーブル ─────────────────────
multi_disease_correlation_table <- function(ibs_data, disease_ids) {
  nat_weekly <- ibs_data %>%
    filter(disease %in% disease_ids) %>%
    group_by(disease, year, week) %>%
    summarise(val = mean(reports_per_site, na.rm=TRUE), .groups="drop") %>%
    mutate(date = as.Date(paste0(year, "-01-01")) + (week - 1) * 7)

  # ワイド変換
  wide <- nat_weekly %>%
    tidyr::pivot_wider(id_cols=date, names_from=disease, values_from=val)

  # 相関行列
  cor_mat <- cor(wide[, disease_ids], use="pairwise.complete.obs")
  as.data.frame(round(cor_mat, 3))
}
