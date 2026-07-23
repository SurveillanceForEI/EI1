// ebs_trend_chart.js
// EBSトレンドタブの「国内EBSニュース 日別シグナル数」チャートを、
// 疾患切り替え時にサーバーへ問い合わせずクライアント側で再描画する。
// window.EBS_TREND_DATA は R側から1回（ebs_data()更新のたびに）渡される、
// 疾患ID毎の週次Signal High/Low件数の全件データ。
// 疾患セレクタ（#disease または #zensu_disease_ts）の変更を検知して
// Plotly.reactで即座に切り替える（サーバー往復なし）。

(function (global) {
  "use strict";

  var CONTAINER_ID = "ebs-trend-chart";
  var COLORS = { high: "#e74c3c", low: "#f39c12" };

  function currentDiseaseId() {
    var isZensu = $('input[name="ts_mode"]:checked').val() === "zensu";
    var id = isZensu ? $("#zensu_disease_ts").val() : $("#disease").val();
    return id;
  }

  function emptyPlot(text) {
    Plotly.react(
      CONTAINER_ID,
      [],
      {
        annotations: [{ text: text, showarrow: false, font: { size: 13, color: "#aaa" } }],
        paper_bgcolor: "transparent",
        plot_bgcolor: "transparent",
        margin: { t: 30, b: 30, l: 40, r: 10 },
      },
      { displayModeBar: false, responsive: true }
    );
  }

  function updateEbsTrendChart() {
    var el = document.getElementById(CONTAINER_ID);
    if (!el || !global.Plotly) return;
    var data = global.EBS_TREND_DATA;
    if (!data) return;

    var did = currentDiseaseId();
    var series = did ? data[did] : null;

    if (!series || series.length === 0) {
      emptyPlot(did ? "過去60日間にSignal High/Lowなし" : "データなし");
      return;
    }

    var xs = series.map(function (r) { return r.week; });
    var highYs = series.map(function (r) { return r.high; });
    var lowYs = series.map(function (r) { return r.low; });

    var today = new Date();
    var start = new Date(today.getTime() - 60 * 86400000);

    var traces = [
      { x: xs, y: highYs, name: "Signal High", type: "bar",
        marker: { color: COLORS.high }, width: 6 * 86400000 },
      { x: xs, y: lowYs, name: "Signal Low", type: "bar",
        marker: { color: COLORS.low }, width: 6 * 86400000 },
    ];

    var layout = {
      barmode: "stack",
      xaxis: {
        title: "", type: "date", tickformat: "%m/%d",
        dtick: 7 * 86400000,
        range: [start.toISOString(), new Date(today.getTime() + 0.5 * 86400000).toISOString()],
        gridcolor: "#eee",
      },
      yaxis: { title: "記事数", dtick: 1, gridcolor: "#eee", rangemode: "nonnegative" },
      legend: { orientation: "h", x: 0.5, xanchor: "center", y: 1.1, yanchor: "bottom" },
      paper_bgcolor: "transparent",
      plot_bgcolor: "rgba(250,250,250,0.5)",
      margin: { t: 30, b: 30, l: 40, r: 10 },
    };

    Plotly.react(CONTAINER_ID, traces, layout, { displayModeBar: false, responsive: true });
  }

  // ── Google Trendsチャート（gtrends-chart）──
  // window.GTRENDS_DATA は都道府県切替・「Trends 更新」ボタン時のみR側から
  // 再送信される（{series: {disease_id: {label,color,points:[{date,hits}]}}, geoLabel}）。
  // 疾患切替はここでもクライアント側のみで完結する
  var GTRENDS_CONTAINER_ID = "gtrends-chart";

  function updateGtrendsChart() {
    var el = document.getElementById(GTRENDS_CONTAINER_ID);
    if (!el || !global.Plotly) return;
    var payload = global.GTRENDS_DATA;
    if (!payload || !payload.series) {
      Plotly.react(GTRENDS_CONTAINER_ID, [], {
        annotations: [{ text: "データ取得中... 「Trends 更新」ボタンを押してください",
                         showarrow: false, font: { size: 13 } }],
      }, { displayModeBar: false, responsive: true });
      return;
    }

    var did = currentDiseaseId();
    var series = did ? payload.series[did] : null;

    if (!series || !series.points || series.points.length === 0) {
      Plotly.react(
        GTRENDS_CONTAINER_ID,
        [],
        {
          annotations: [{ text: "データなし", showarrow: false, font: { size: 14, color: "#aaa" } }],
          paper_bgcolor: "transparent", plot_bgcolor: "transparent",
        },
        { displayModeBar: false, responsive: true }
      );
      return;
    }

    var xs = series.points.map(function (p) { return p.date; });
    var ys = series.points.map(function (p) { return p.hits; });
    var lbl = series.label || did;

    var trace = {
      x: xs, y: ys, name: lbl, type: "scatter", mode: "lines",
      line: { color: series.color, width: 2 },
      hovertemplate: "%{x|%Y-%m-%d}　" + lbl + "　関心度: %{y}<extra></extra>",
    };

    var layout = {
      title: { text: "Google Trends — " + (payload.geoLabel || "日本全国"), font: { size: 13 }, x: 0 },
      xaxis: { title: "", showgrid: false },
      yaxis: { title: "検索関心度（最大=100）", gridcolor: "#eee", range: [0, 105] },
      legend: { orientation: "h", y: -0.15 },
      hovermode: "x unified",
      plot_bgcolor: "#fff", paper_bgcolor: "#fff",
      margin: { t: 30, b: 40, l: 55, r: 20 },
    };

    Plotly.react(GTRENDS_CONTAINER_ID, [trace], layout, { displayModeBar: false, responsive: true });
  }

  // 疾患セレクタ・表示モードの変更を検知して再描画（サーバー往復なし）
  $(document).on(
    "change",
    "#disease, #zensu_disease_ts, input[name=\"ts_mode\"]",
    function () {
      updateEbsTrendChart();
      updateGtrendsChart();
    }
  );

  global.updateEbsTrendChart = updateEbsTrendChart;
  global.updateGtrendsChart = updateGtrendsChart;
})(window);
