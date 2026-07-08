# ============================================================
# forecast_ts.R
# 流行曲線の短期予測（4週先まで、複数手法・アンサンブル対応）
# 追加パッケージを使わず base R（stats::HoltWinters等）のみで実装
# ============================================================

FORECAST_METHOD_LABELS <- c(
  ensemble = "アンサンブル（複数手法平均）",
  linear   = "線形トレンド外挿",
  holt     = "指数平滑法（Holt法）",
  rt       = "Rtベース（実効再生産数）"
)

# 線形トレンド外挿: 直近n_recent週の単純回帰直線を延長し、予測区間(80%)を付す
forecast_linear <- function(values, horizon = 4, n_recent = 8) {
  n <- length(values)
  if (n < 4 || all(is.na(values))) return(NULL)
  idx_use <- max(1, n - n_recent + 1):n
  x <- idx_use
  y <- values[idx_use]
  if (sum(!is.na(y)) < 3 || length(unique(na.omit(y))) < 2) return(NULL)
  fit <- tryCatch(lm(y ~ x), error = function(e) NULL)
  if (is.null(fit)) return(NULL)
  newx <- (n + 1):(n + horizon)
  pred <- tryCatch(
    predict(fit, newdata = data.frame(x = newx), interval = "prediction", level = 0.8),
    error = function(e) NULL)
  if (is.null(pred)) return(NULL)
  data.frame(
    step  = seq_len(horizon),
    value = pmax(0, pred[, "fit"]),
    lower = pmax(0, pred[, "lwr"]),
    upper = pmax(0, pred[, "upr"])
  )
}

# 指数平滑法（Holt線形トレンド法、季節成分なし）: stats::HoltWinters を使用
forecast_holt <- function(values, horizon = 4) {
  n <- length(values)
  if (n < 8 || all(is.na(values))) return(NULL)
  v <- values
  v[is.na(v)] <- 0
  ts_obj <- stats::ts(v, frequency = 1)
  hw <- tryCatch(stats::HoltWinters(ts_obj, gamma = FALSE), error = function(e) NULL)
  if (is.null(hw)) return(NULL)
  pred <- tryCatch(
    stats::predict(hw, n.ahead = horizon, prediction.interval = TRUE, level = 0.8),
    error = function(e) NULL)
  if (is.null(pred)) return(NULL)
  pred <- as.data.frame(pred)
  data.frame(
    step  = seq_len(horizon),
    value = pmax(0, pred$fit),
    lower = pmax(0, pred$lwr),
    upper = pmax(0, pred$upr)
  )
}

# Rtベース予測: 直近の実効再生産数が今後も一定と仮定した指数的な増減を延長
# 週あたり成長率 = Rt^(7/シリアルインターバル[日])
# Rt自体の推定誤差を反映するため、参考区間として±30%の簡易バンドを付す
forecast_rt <- function(last_value, rt_value, si_days, horizon = 4) {
  if (is.na(last_value) || is.na(rt_value) || is.na(si_days) || si_days <= 0) return(NULL)
  growth_per_week <- rt_value ^ (7 / si_days)
  vals <- last_value * growth_per_week ^ seq_len(horizon)
  data.frame(
    step  = seq_len(horizon),
    value = vals,
    lower = vals * 0.7,
    upper = vals * 1.3
  )
}

# アンサンブル: 利用可能な各手法の予測値・区間を単純平均
forecast_ensemble <- function(forecast_list) {
  forecast_list <- Filter(Negate(is.null), forecast_list)
  if (length(forecast_list) == 0) return(NULL)
  combined <- do.call(rbind, forecast_list)
  agg <- aggregate(cbind(value, lower, upper) ~ step, data = combined, FUN = mean, na.rm = TRUE)
  agg[order(agg$step), ]
}

# 予測ディスパッチャ: method に応じて手法を選択し、日付付きの予測データフレームを返す
# dates: 実測データの日付ベクトル（昇順）, values: 対応する値
compute_forecast <- function(dates, values, method, horizon = 4,
                              rt_value = NA_real_, si_days = NA_real_) {
  ord <- order(dates)
  dates  <- dates[ord]
  values <- values[ord]
  n <- length(values)
  if (n < 4) return(NULL)

  lin  <- forecast_linear(values, horizon)
  holt <- forecast_holt(values, horizon)
  rt   <- forecast_rt(values[n], rt_value, si_days, horizon)

  picked <- switch(method,
    linear   = lin,
    holt     = holt,
    rt       = rt,
    ensemble = forecast_ensemble(list(lin, holt, rt)),
    NULL
  )
  if (is.null(picked) || nrow(picked) == 0) return(NULL)

  last_date <- max(dates, na.rm = TRUE)
  picked$date <- last_date + lubridate::weeks(picked$step)
  picked
}
