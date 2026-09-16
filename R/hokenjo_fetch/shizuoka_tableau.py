# -*- coding: utf-8 -*-
"""
静岡県「定点報告感染症ダウンロード用（2015～2026）」Tableau Publicダッシュボードから
保健所別CSVをダウンロードするスクリプト。

Tableau Public側がJS側から動的にビューを構築する現行UIになっており、
tableauscraper等の静的HTML解析ライブラリ（HTMLに埋め込まれたJSON設定を前提とする）
ではデータを取得できなくなっていたため、実際にヘッドレスブラウザ（Playwright）で
「ダウンロード」ボタンの操作を再現する。

【重要な制約】このダッシュボードは名前に反して2026-09時点ではまだ2026年分の
データを含んでおらず、集計年フィルタは2015〜2025年までしか選択肢がない。
そのため本スクリプトは静岡県の「過去（2015〜2025年）」の保健所別データの
バックフィル用であり、現在の最新週データの取得は引き続きPDF方式の
fetch_shizuoka()/fetch_shizuoka_history()（R/hokenjo_fetch/shizuoka.R）に
依存する。ダッシュボード側が2026年分を含むよう更新されたら、このスクリプトを
定期実行に組み込むことで現在の週データもここから取得できるようになる見込み。

前提: Python 3 + playwright（`pip install playwright` 後
`playwright install chromium` が必要。requirements: shizuoka_tableau_requirements.txt）

使い方:
    python shizuoka_tableau.py <出力先CSVパス>

戻り値: 成功時はCSVファイルを書き出しexit code 0。失敗時はexit code 1でエラーメッセージをstderrへ。
出力CSVはUTF-16LE・タブ区切りのワイド形式（行=疾患×保健所×指標、列=年×週）。
R側でR/hokenjo_fetch/shizuoka_tableau_parse.Rのparse_shizuoka_tableau_csv()を使って
長形式に変換する。
"""
import sys
from pathlib import Path

from playwright.sync_api import sync_playwright

VIZ_URL = "https://public.tableau.com/app/profile/prefshizuoka/viz/20152026/sheet0"


def fetch_hokenjo_csv(out_path: str, timeout_ms: int = 60000) -> None:
    with sync_playwright() as p:
        browser = p.chromium.launch(headless=True)
        try:
            _fetch_hokenjo_csv_inner(browser, out_path, timeout_ms)
        finally:
            browser.close()


def _fetch_hokenjo_csv_inner(browser, out_path: str, timeout_ms: int) -> None:
        page = browser.new_page(accept_downloads=True)
        page.set_default_timeout(timeout_ms)

        page.goto(VIZ_URL, wait_until="load")
        page.wait_for_timeout(10000)

        # クッキー同意ポップアップ・宣伝モーダルを閉じる（複数回・force clickで確実に）
        for _ in range(3):
            for label in ["すべてのCookieを受け入れる", "Accept All Cookies"]:
                try:
                    page.get_by_text(label, exact=False).first.click(timeout=2000, force=True)
                    page.wait_for_timeout(500)
                except Exception:
                    pass
            try:
                page.mouse.click(872, 189)  # 宣伝モーダルの×ボタン固定座標
                page.wait_for_timeout(500)
            except Exception:
                pass
            try:
                page.keyboard.press("Escape")
                page.wait_for_timeout(300)
            except Exception:
                pass

        # ビューのiframe内にツールバーがある（読み込みタイミングにより数秒遅れることがあるためリトライする）
        frame = None
        for _ in range(20):
            for f in page.frames:
                if "views/20152026" in f.url:
                    frame = f
                    break
            if frame is not None:
                break
            page.wait_for_timeout(1000)
        if frame is None:
            raise RuntimeError("ビューのiframeが見つかりません")

        # ダッシュボード右下の「ダウンロード」ボタン（ダッシュボード作者が配置した
        # ダッシュボードアクションボタン。ツールバー本体のダウンロードボタンとは別物で、
        # クリックすると「クロス集計のダウンロード」ダイアログが直接開く）
        try:
            frame.get_by_text("ダウンロード", exact=True).last.click(timeout=5000)
        except Exception:
            frame.get_by_text("Download", exact=True).last.click(timeout=5000)
        page.wait_for_timeout(1500)

        # 「Download Crosstab」ダイアログはビューのiframe内に描画されるため、
        # page基準ではなくframe基準で要素を探す必要がある。
        # data-tb-test-id属性はUI言語(ja/en)に関係なく安定しているため、
        # テキストやroleの言語依存表記より優先して使う
        frame.locator('[data-tb-test-id="crosstab-options-dialog-radio-csv-Label"]').click(timeout=5000)
        page.wait_for_timeout(500)

        # ダイアログ内の「ダウンロード」実行ボタン
        with page.expect_download(timeout=timeout_ms) as dl_info:
            frame.locator('[data-tb-test-id="export-crosstab-export-Button"]').click(timeout=5000)
        download = dl_info.value
        download.save_as(out_path)


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("usage: python shizuoka_tableau.py <output_csv_path>", file=sys.stderr)
        sys.exit(1)
    out = sys.argv[1]
    Path(out).parent.mkdir(parents=True, exist_ok=True)
    try:
        fetch_hokenjo_csv(out)
        print(f"OK: saved to {out}")
    except Exception as e:
        print(f"ERROR: {e}", file=sys.stderr)
        sys.exit(1)
