# 保健所政令市（その他政令市）5市の感染症情報センター等リンク

地域保健法施行令第1条第3号に基づき、政令指定都市・中核市でないが独自に保健所を設置している5市（小樽市・町田市・藤沢市・茅ヶ崎市・四日市市）について調査した結果。

| 市名 | 感染症情報センター名(等) | センターURL | ニュースRSS/API候補URL | 備考(信頼度など) |
|---|---|---|---|---|
| 小樽市（北海道） | 専用センターなし。市サイトに「市内の感染症発生状況について（感染症発生動向調査）」ページがあり、北海道感染症情報センターの小樽市保健所管内データに委ねる形 | https://www.city.otaru.lg.jp/docs/2020101600662<br>（北海道側）https://www.iph.pref.hokkaido.jp/kansen/otaru/index.html | https://www.city.otaru.lg.jp/docs/index.rss | 高：RSSは取得検証済み（RSS 2.0、実際の新着記事タイトルを確認）。感染症情報は市独自センターがなく道の管内ページに依存している点に注意 |
| 町田市（東京都） | 町田市感染症週報（町田市保健所） | https://www.city.machida.tokyo.jp/iryo/hokenjo/kansen/kansensyosyuhou22.html | https://www.city.machida.tokyo.jp/rss_news.xml | 中：センターURLは市保健所の専用週報ページで内容確認済み。RSSはトップページ`<head>`内のlink relタグから検出したURLで、内容の実取得検証はできていない（要再確認） |
| 藤沢市（神奈川県） | 藤沢市感染症発生状況（藤沢市保健所） | https://www.city.fujisawa.kanagawa.jp/kenko/iryo/kansensho/kansensho/hassei/index.html | 候補なし（緊急情報RSS `https://www.city.fujisawa.kanagawa.jp/kinkyu/kinkyu.xml` は存在するが空で「緊急情報」専用のため新着/報道発表向けではない） | 低：センターページは年度別サーベイランス報告への入口として確認。一般新着情報のRSS/JSON配信は見つからず。新着情報ページ https://www.city.fujisawa.kanagawa.jp/shinchaku/index.html や 報道発表 https://www.city.fujisawa.kanagawa.jp/shise/koho/hodo/index.html を人手フォローするか、他のCMSパターンの追加調査が必要 |
| 茅ヶ崎市（神奈川県） | 感染症情報（週報）（茅ヶ崎市保健所） | https://www.city.chigasaki.kanagawa.jp/kenko/1022933/1038459/1046393/1046735.html | https://www.city.chigasaki.kanagawa.jp/news.rss | 高：RSSは取得検証済み（RSS 2.0、実際の新着記事タイトルを確認）。センターURLは感染症情報週報の専用ページ |
| 四日市市（三重県） | 専用の週報ページは未確認。「感染症（発生）情報」ページは啓発中心で、疫学週報の掲載は不明瞭。四日市市保健所 保健予防課が担当（三重県感染症情報センターへの依存の可能性あり） | https://www.city.yokkaichi.lg.jp/www/genre/1000100000448/index.html | https://www.city.yokkaichi.lg.jp/www/rss/news.rdf | 高：RSSは取得検証済み（RSS 1.0/RDF、実際の新着記事タイトルを確認、2026-07-17付の記事あり）。感染症センターURLは暫定で、県センターへの依存有無は要追加確認 |

## 補足
- 全市ともRSS/JSON APIの検証はWebFetchによる内容取得で行ったが、実際のRSSリーダーでの購読確認は未実施。
- 四日市市・藤沢市の感染症情報については、専用の「感染症情報センター」に相当する明確な名称のページが見当たらず、実態としては保健所の一部門ページに留まる可能性が高い。
