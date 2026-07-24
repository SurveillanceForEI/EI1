// ebs_cards.js
// EBSニュースカードをクライアント側で描画する。
// これまでR側(renderUI)で毎回すべてのtags$div/tags$aツリーを組み立てて
// いた処理をこちらに移し、サーバーはフィルタ済みデータをJSONで渡すだけに
// する（静的サイト化に向けた第一段階。将来的にはこの関数とJSONスキーマを
// そのまま静的ページに転用できる）。
//
// 呼び出し側(app.R)は以下のグローバル関数を window.renderEbsCards として
// 呼び出す想定:
//   renderEbsCards(containerId, cards, meta)
//     containerId: カードを描画する<div>のid
//     cards: カードオブジェクトの配列（下記フィールドを参照）
//     meta: { total: 全件数, pageSize: 表示件数, emptyMessage: 該当なし時の文言 }

(function (global) {
  "use strict";

  function esc(s) {
    if (s === null || s === undefined) return "";
    return String(s)
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;");
  }

  function diseaseBadgesHtml(tags) {
    if (!tags || tags.length === 0) return "";
    return tags
      .map(function (t) {
        return (
          '<span style="display:inline-block;background:#ecf0f1;color:#555;' +
          'border-radius:10px;padding:1px 8px;font-size:0.72em;margin:1px;">' +
          esc(t) +
          "</span>"
        );
      })
      .join("");
  }

  function criteriaBadgesHtml(labels) {
    if (!labels || labels.length === 0) return "";
    var inner = labels
      .map(function (l) {
        return (
          '<span style="display:inline-block;background:#27ae60;color:#fff;' +
          'border-radius:3px;padding:0 5px;font-size:0.65em;font-weight:700;margin:1px;">' +
          esc(l) +
          "</span>"
        );
      })
      .join("");
    return '<span style="margin-left:8px;">' + inner + "</span>";
  }

  function locationBadgeHtml(locationText) {
    if (!locationText) return "";
    return (
      '<span style="display:inline-block;background:#8e44ad;color:#fff;' +
      'border-radius:3px;padding:0 5px;font-size:0.65em;font-weight:600;margin:1px;">' +
      '<i class="fas fa-location-dot"></i> ' +
      esc(locationText) +
      "</span>"
    );
  }

  function idscRefHtml(idscRef) {
    if (!idscRef) return "";
    return (
      '<div style="font-size:0.72em;color:#999;margin-top:4px;font-style:italic;">' +
      '<i class="fas fa-circle-info"></i> 参考: 地域の公式情報 ' +
      '<a href="' + esc(idscRef.url) + '" target="_blank" rel="noopener noreferrer" ' +
      'style="color:#7f8c8d;text-decoration:underline;">' + esc(idscRef.label) + "</a>" +
      "（記事の情報源とは限りません）" +
      "</div>"
    );
  }

  function officialBadgeHtml(isOfficial) {
    if (!isOfficial) return "";
    return (
      '<span style="display:inline-block;background:#2980b9;color:#fff;' +
      'border-radius:3px;padding:0 5px;font-size:0.7em;font-weight:700;margin-left:5px;">' +
      '<i class="fas fa-landmark"></i> 公式</span>'
    );
  }

  function snsInfoHtml(retweet, like) {
    if (retweet === null || retweet === undefined) return "";
    return (
      '<span style="color:#aaa;font-size:0.75em;"> RT:' +
      esc(retweet) +
      " ♥:" +
      esc(like || 0) +
      "</span>"
    );
  }

  // 1ページに約10件収まる高さを目安にした1列・2行構成のコンパクト表示。
  // 要約本文は表示せず、タイトルのtitle属性（ホバー時のブラウザ標準ツールチップ）
  // に全文を保持するのみとする（ユーザー指示: 内容は全部見えなくてよい）
  function cardHtml(c) {
    var dateHtml = c.pubDate
      ? esc(c.pubDate)
      : '<span style="color:#e67e22;">日付不明</span>';
    var titleAttr = c.summary ? ' title="' + esc(c.summary) + '"' : "";

    return (
      '<div style="background:#fff;border-radius:5px;padding:6px 12px;' +
      "box-shadow:0 1px 2px rgba(0,0,0,0.06);border-left:4px solid " +
      esc(c.signalColor) +
      ';">' +
      '<div style="display:flex;justify-content:space-between;align-items:baseline;gap:8px;">' +
      '<a href="' +
      esc(c.link) +
      '" target="_blank" class="ebs-tr"' + titleAttr + ' style="flex:1;min-width:0;font-weight:700;' +
      "font-size:0.88em;color:#2c3e50;text-decoration:none;white-space:nowrap;" +
      'overflow:hidden;text-overflow:ellipsis;">' +
      esc(c.title) +
      "</a>" +
      '<span style="flex-shrink:0;background:' +
      esc(c.signalColor) +
      ';color:#fff;border-radius:10px;padding:1px 8px;font-size:0.68em;font-weight:700;">' +
      esc(c.signalLevel) +
      "</span>" +
      "</div>" +
      '<div style="font-size:0.74em;color:#888;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;">' +
      '<span style="font-weight:600;">' +
      esc(c.sourceName) +
      "</span>" +
      officialBadgeHtml(c.isOfficial) +
      " • " +
      dateHtml +
      snsInfoHtml(c.retweetCount, c.likeCount) +
      " " +
      diseaseBadgesHtml(c.diseaseTags) +
      criteriaBadgesHtml(c.criteriaLabels) +
      locationBadgeHtml(c.locationText) +
      "</div>" +
      "</div>"
    );
  }

  function renderEbsCards(containerId, cards, meta) {
    var root = document.getElementById(containerId);
    if (!root) return;
    meta = meta || {};

    if (!cards || cards.length === 0) {
      root.innerHTML =
        '<div class="demo-banner">' +
        esc(meta.emptyMessage || "該当なし。サイドバーの疾患を変更するか「すべて表示」を押してください。") +
        "</div>";
      return;
    }

    var grid =
      '<div style="display:flex;flex-direction:column;gap:4px;">' +
      cards.map(cardHtml).join("") +
      "</div>";

    var moreNote = "";
    if (meta.total && meta.pageSize && meta.total > meta.pageSize) {
      moreNote =
        '<div style="text-align:center;color:#888;font-size:0.85em;padding:10px;">' +
        esc(meta.pageSize) +
        " 件を表示中（全 " +
        esc(meta.total) +
        " 件）</div>";
    }

    root.innerHTML = grid + moreNote;
  }

  global.renderEbsCards = renderEbsCards;
})(window);
