# ============================================================
# forecast_ts.R
# 流行曲線の短期予測（4週先まで、複数手法・アンサンブル対応）
# 追加パッケージを使わず base R（stats::HoltWinters等）のみで実装
# ============================================================

FORECAST_METHOD_LABELS <- c(
  ensemble = "アンサンブル（複数手法平均）",
  poisson  = "ポアソン回帰（GLM）",
  holt     = "指数平滑法（Holt法）",
  rt       = "Rtベース（renewal equation）"
)

# 季節成分（週次・周期52週）をフィットするために必要な最低データ量（約2年分）
FORECAST_SEASONAL_MIN_WEEKS <- 104

# ポアソン回帰（準ポアソンGLM）: 感染症の週次報告数のような非負のカウントデータに対して
# 標準的に用いられる回帰手法。対数リンクにより乗法的な増減を仮定する。
# seasonal=TRUE かつ十分な長さのデータがある場合は、暦週に基づく調和項
# （sin/cos(2π×週/52)）を加え、直近3年分のデータで季節パターンを反映する。
# それ以外は直近n_recent週の単純なトレンドのみで延長する（従来どおり）。
forecast_poisson <- function(dates, values, horizon = 4, n_recent = 8, seasonal = FALSE) {
  n <- length(values)
  if (n < 4 || all(is.na(values))) return(NULL)

  if (isTRUE(seasonal) && n >= FORECAST_SEASONAL_MIN_WEEKS) {
    use_n   <- min(n, 156)  # 直近3年分
    idx_use <- (n - use_n + 1):n
    x  <- idx_use
    wk <- as.integer(format(dates[idx_use], "%V"))
    y  <- pmax(0, values[idx_use])
    if (sum(!is.na(y)) >= 20 && length(unique(na.omit(y))) >= 2) {
      s1 <- sin(2 * pi * wk / 52); c1 <- cos(2 * pi * wk / 52)
      fit <- tryCatch(glm(y ~ x + s1 + c1, family = quasipoisson()), error = function(e) NULL)
      if (!is.null(fit)) {
        newx  <- (n + 1):(n + horizon)
        newwk <- as.integer(format(dates[n] + lubridate::weeks(seq_len(horizon)), "%V"))
        news1 <- sin(2 * pi * newwk / 52); newc1 <- cos(2 * pi * newwk / 52)
        pred <- tryCatch(
          predict(fit, newdata = data.frame(x = newx, s1 = news1, c1 = newc1),
                  type = "link", se.fit = TRUE),
          error = function(e) NULL)
        if (!is.null(pred) && !any(is.na(pred$se.fit))) {
          z <- stats::qnorm(0.9)
          return(data.frame(
            step  = seq_len(horizon),
            value = exp(pred$fit),
            lower = exp(pred$fit - z * pred$se.fit),
            upper = exp(pred$fit + z * pred$se.fit)
          ))
        }
      }
    }
    # 季節版がフィットできない場合は非季節版にフォールバック
  }

  idx_use <- max(1, n - n_recent + 1):n
  x <- idx_use
  y <- pmax(0, values[idx_use])
  if (sum(!is.na(y)) < 3 || length(unique(na.omit(y))) < 2) return(NULL)
  fit <- tryCatch(glm(y ~ x, family = quasipoisson()), error = function(e) NULL)
  if (is.null(fit)) return(NULL)
  newx <- (n + 1):(n + horizon)
  pred <- tryCatch(
    predict(fit, newdata = data.frame(x = newx), type = "link", se.fit = TRUE),
    error = function(e) NULL)
  if (is.null(pred) || any(is.na(pred$se.fit))) return(NULL)
  z <- stats::qnorm(0.9)  # 80%区間
  data.frame(
    step  = seq_len(horizon),
    value = exp(pred$fit),
    lower = exp(pred$fit - z * pred$se.fit),
    upper = exp(pred$fit + z * pred$se.fit)
  )
}

# 指数平滑法: stats::HoltWinters を使用。
# seasonal=TRUE かつ十分な長さのデータがある場合は季節成分（周期52週）を含む
# 三重指数平滑法（Holt-Winters）を用い、それ以外はHolt線形トレンド法（季節成分なし）を使う。
forecast_holt <- function(values, horizon = 4, seasonal = FALSE) {
  n <- length(values)
  if (n < 8 || all(is.na(values))) return(NULL)
  v <- values
  v[is.na(v)] <- 0

  if (isTRUE(seasonal) && n >= FORECAST_SEASONAL_MIN_WEEKS) {
    ts_obj <- stats::ts(v, frequency = 52)
    hw <- tryCatch(stats::HoltWinters(ts_obj), error = function(e) NULL)
    if (!is.null(hw)) {
      pred <- tryCatch(
        stats::predict(hw, n.ahead = horizon, prediction.interval = TRUE, level = 0.8),
        error = function(e) NULL)
      if (!is.null(pred)) {
        pred <- as.data.frame(pred)
        return(data.frame(
          step  = seq_len(horizon),
          value = pmax(0, pred$fit),
          lower = pmax(0, pred$lwr),
          upper = pmax(0, pred$upr)
        ))
      }
    }
    # 季節版が収束しない場合は非季節版にフォールバック
  }

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

# Rtベース予測（renewal equation / 分枝過程モデル）:
# I(t) = Rt × Σ_s I(t-s)×w(s) で将来値を逐次生成する。w(s)はシリアルインターバル分布
# （Rt推定 estimate_rt_simple() と同一のガンマ分布近似）に基づく重みで、直近1点のみに
# 依存する単純な指数外挿と異なり、過去複数週の実績の形状を反映した予測になる。
# 直近の実効再生産数（Rt）が今後も一定と仮定し、Rt自体の推定誤差を踏まえた
# 参考区間として±30%の簡易バンドを付す。
forecast_rt <- function(values, rt_value, si_mean, si_sd, horizon = 4) {
  n <- length(values)
  if (n < 4 || is.na(rt_value) || is.na(si_mean) || is.na(si_sd) || si_mean <= 0 || si_sd <= 0) return(NULL)

  k     <- (si_mean / si_sd)^2
  theta <- si_sd^2 / si_mean
  w <- pgamma(1:20, shape = k, scale = theta) - pgamma(0:19, shape = k, scale = theta)
  w <- w / sum(w)
  L <- length(w)

  sim <- pmax(0, values)
  sim[is.na(sim)] <- 0

  preds <- numeric(horizon)
  for (h in seq_len(horizon)) {
    m <- length(sim)
    lags   <- seq_len(min(L, m))
    lambda <- sum(sim[m - lags + 1] * w[lags])
    next_val <- rt_value * lambda
    preds[h] <- next_val
    sim <- c(sim, next_val)
  }

  data.frame(
    step  = seq_len(horizon),
    value = preds,
    lower = preds * 0.7,
    upper = preds * 1.3
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
                              rt_value = NA_real_, si_mean = NA_real_, si_sd = NA_real_,
                              seasonal = FALSE, unit = "week") {
  ord <- order(dates)
  dates  <- dates[ord]
  values <- values[ord]
  n <- length(values)
  if (n < 4) return(NULL)

  poi  <- forecast_poisson(dates, values, horizon, seasonal = seasonal)
  holt <- forecast_holt(values, horizon, seasonal = seasonal)
  rt   <- forecast_rt(values, rt_value, si_mean, si_sd, horizon)

  picked <- switch(method,
    poisson  = poi,
    holt     = holt,
    rt       = rt,
    ensemble = forecast_ensemble(list(poi, holt, rt)),
    NULL
  )
  if (is.null(picked) || nrow(picked) == 0) return(NULL)

  last_date <- max(dates, na.rm = TRUE)
  picked$date <- if (identical(unit, "month")) {
    last_date %m+% months(picked$step)
  } else {
    last_date + lubridate::weeks(picked$step)
  }
  picked
}
