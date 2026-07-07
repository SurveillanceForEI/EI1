# ============================================================
# rt_estimation.R
# 実効再生産数（Rt）推定
# ============================================================

library(dplyr)
library(tidyr)

# Cori法によるRt推定（7日窓）
# EpiEstim が使えない環境向けに簡易版を内包
estimate_rt_simple <- function(incidence_vec, si_mean, si_sd, window = 7) {
  n <- length(incidence_vec)
  na_tbl <- tibble(rt=rep(NA_real_,n), rt_lower=rep(NA_real_,n), rt_upper=rep(NA_real_,n))
  if (n < window + 1) return(na_tbl)
  if (is.null(si_mean) || is.null(si_sd) || is.na(si_mean) || is.na(si_sd)) return(na_tbl)

  # ガンマ分布でシリアルインターバルを近似
  k <- (si_mean / si_sd)^2
  theta <- si_sd^2 / si_mean
  weights <- pgamma(1:20, shape = k, scale = theta) - pgamma(0:19, shape = k, scale = theta)
  weights <- weights / sum(weights)

  rt_vec <- rep(NA_real_, n)
  rt_lower <- rep(NA_real_, n)
  rt_upper <- rep(NA_real_, n)

  for (t in (window + 1):n) {
    # 感染性プロファイルの畳み込み
    lambda <- sum(
      incidence_vec[max(1, t - length(weights)):(t - 1)] *
        rev(weights[1:min(length(weights), t - 1)])
    )
    if (is.na(lambda) || lambda <= 0) next

    # ガンマ事後分布（Cori et al. 2013）
    # 事前分布: Gamma(a=1, b=5) → 事後: Gamma(a+sum(I), b+sum(Lambda))
    a_prior <- 1; b_prior <- 5
    sum_I <- sum(incidence_vec[(t - window + 1):t])
    sum_L <- sum(
      sapply((t - window + 1):t, function(s) {
        sum(
          incidence_vec[max(1, s - length(weights)):(s - 1)] *
            rev(weights[1:min(length(weights), s - 1)])
        )
      })
    )
    a_post <- a_prior + sum_I
    b_post <- 1 / (b_prior + sum_L)

    rt_vec[t]   <- a_post * b_post
    rt_lower[t] <- qgamma(0.025, shape = a_post, scale = b_post)
    rt_upper[t] <- qgamma(0.975, shape = a_post, scale = b_post)
  }

  tibble(
    rt = rt_vec,
    rt_lower = rt_lower,
    rt_upper = rt_upper
  )
}

# 疾患・都道府県指定でRt系列を返す
compute_rt_series <- function(data, disease_id, pref_name_filter = NULL) {
  si <- SERIAL_INTERVALS[[disease_id]]
  if (is.null(si)) return(NULL)

  df <- data %>%
    filter(disease == disease_id)

  is_pref <- !is.null(pref_name_filter) &&
             length(pref_name_filter) == 1 &&
             pref_name_filter %in% PREF_MASTER$pref_name
  if (is_pref) {
    df <- df %>% filter(pref_name == pref_name_filter)
  } else {
    # 全国集計: 定点あたり平均
    df <- df %>%
      group_by(date, week, year) %>%
      summarise(reports_per_site = mean(reports_per_site, na.rm = TRUE), .groups = "drop")
  }

  df <- df %>%
    arrange(date) %>%
    filter(!is.na(reports_per_site), is.finite(reports_per_site))

  if (nrow(df) < 15) return(NULL)

  rt_df <- estimate_rt_simple(
    df$reports_per_site,
    si_mean = si$mean,
    si_sd   = si$sd
  )

  bind_cols(df %>% select(date, week, year, reports_per_site), rt_df)
}

# 全数把握疾患用 Rt 系列（件数ベース）
compute_rt_series_zensu <- function(data, disease_id, pref_name_filter = NULL) {
  si <- SERIAL_INTERVALS[[disease_id]]
  if (is.null(si)) return(NULL)

  df <- data %>% filter(disease == disease_id)

  is_pref <- !is.null(pref_name_filter) &&
             length(pref_name_filter) == 1 &&
             pref_name_filter %in% PREF_MASTER$pref_name
  if (is_pref) {
    df <- df %>% filter(pref_name == pref_name_filter) %>%
      group_by(date, week, year) %>%
      summarise(cases = sum(cases, na.rm = TRUE), .groups = "drop")
  } else {
    df <- df %>%
      group_by(date, week, year) %>%
      summarise(cases = sum(cases, na.rm = TRUE), .groups = "drop")
  }

  df <- df %>%
    arrange(date) %>%
    filter(!is.na(cases), is.finite(cases))

  if (nrow(df) < 15) return(NULL)

  rt_df <- estimate_rt_simple(df$cases, si_mean = si$mean, si_sd = si$sd)
  bind_cols(df %>% select(date, week, year, cases), rt_df)
}
