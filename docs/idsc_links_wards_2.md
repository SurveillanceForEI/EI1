# 東京23区 感染症情報ページ & 新着情報RSS候補（第2弾: 11区）

対象区: 渋谷区, 中野区, 杉並区, 豊島区, 北区, 荒川区, 板橋区, 練馬区, 足立区, 葛飾区, 江戸川区

| 区名 | 感染症情報ページ名(等) | URL | ニュースRSS/API候補URL | 備考(信頼度など) |
|---|---|---|---|---|
| 渋谷区 | 渋谷区感染症週報・月報 | https://www.city.shibuya.tokyo.jp/kenko/kansen/kansen-jokyo/kansen-doko.html （ユーザー確認済み） | RSS配信はないが、新着情報一覧 https://www.city.shibuya.tokyo.jp/contents/news/ （Nuxt.js SSRのため静的HTMLとして`ul.m-list-links li a > p.date/p.title`構造で取得可能）をHTMLスクレイピングで直接取得（fetch_shibuya_news）し、EBS_SOURCESに追加済み（id: city_shibuya）。 | RSSはなくHTML直接解析で解決。 |
| 中野区 | 中野区の感染症最新情報（感染症週報） | https://www.city.tokyo-nakano.lg.jp/kenko_hukushi/kansen/saishin.html | (RSS未発見) | 感染症情報ページURL: High（週報ページ、感染症情報の一覧 https://www.city.tokyo-nakano.lg.jp/dept/402000/d001716.html も参考）。RSS: Low — RSS配信ページ・フィードURLとも発見できず。中野区は現状RSS配信を提供していない可能性が高い。 |
| 杉並区 | 感染症（杉並保健所） | https://www.city.suginami.tokyo.jp/kenkou/kenkou/kansenshou/index.html | https://www.city.suginami.tokyo.jp/kenkou/shinchaku/shinchaku.xml （健康・医療・福祉 新着情報）／全区共通新着: https://www.city.suginami.tokyo.jp/news/oshirase.xml | 感染症情報ページURL: Medium — 感染症トップページは確認できたが、週報形式の専用ページは検索で見当たらず（コロナ関連ページが中心）。RSS: High — 公式「RSS配信について」ページ https://www.city.suginami.tokyo.jp/about/rss.html にカテゴリ別フィード一覧を確認（kenkou/shinchaku/shinchaku.xml 等、CMS定型パターン）。URL自体は未直接フェッチ検証（ページ記載内容からの抽出）。 |
| 豊島区 | 感染症情報 | https://www.city.toshima.lg.jp/543/kenko/kenko/kansensho/1803141318.html | https://www.city.toshima.lg.jp/oshirase/oshirase.xml | 感染症情報ページURL: Medium — 検索結果に出た2つのURL(543/…, 221/…)のうち221側は404だったため543側を採用したが未フェッチ確認、リンク切れの可能性あり。RSS: High（検証済み） — 実際にフェッチしXML(RDF/RSS1.0)形式であることを確認済み（`<?xml version="1.0"...?><rdf:RDF xmlns="http://purl.org/rss/1.0/"...`）。 |
| 北区 | 北区の感染症最新情報（感染症週報） | https://www.city.kita.tokyo.jp/kenko/kansensho/shuho/shuho2023-2025.html （感染症対策トップ: https://www.city.kita.tokyo.jp/kenko/kansensho/index.html） | https://www.city.kita.lg.jp/news.rss | 感染症情報ページURL: Medium — 検索結果からの週報ページ、直接フェッチ未検証。RSS: Medium — 公式「RSS配信について」ページ https://www.city.kita.lg.jp/about/rss.html にURL記載を確認したが、URL自体（news.rss）の実フェッチ検証はできず。北区サイトは city.kita.lg.jp と city.kita.tokyo.jp の2ドメインが混在している点に注意。 |
| 荒川区 | 感染症発生情報 | https://www.city.arakawa.tokyo.jp/a034/kenkouiryou/kansenshou/hasseidoukou.html （感染症トップ: https://www.city.arakawa.tokyo.jp/kenkouiryou/kansenshou/index.html、ユーザー確認済み） | RSS配信はないが、ユーザー提供の最新ニュース一覧 https://www.city.arakawa.tokyo.jp/kouhou/news/index.html （`table.list_table tr > td.date/td`構造）をHTMLスクレイピングで直接取得（fetch_arakawa_news）し、EBS_SOURCESに追加済み（id: city_arakawa）。 | RSSはなくHTML直接解析で解決。 |
| 板橋区 | 板橋区感染症ニュース | https://www.city.itabashi.tokyo.jp/kenko/kansensho/1014886.html | (RSS配信ページはあるが具体的feed URL未特定) | 感染症情報ページURL: High（フェッチ確認済み、2026年第28週の週報を確認）。RSS: Low — 「RSS配信」案内ページ https://www.city.itabashi.tokyo.jp/kusei/kouhou/1019887/about/rss.html は検索結果に表示されたがフェッチ時404、正しいURLパスを再特定できず。板橋区はRSS2.0でカテゴリ別配信をしている旨の言及はあるが、具体的なfeed URLは未確認。 |
| 練馬区 | 感染症発生動向調査 | https://www.city.nerima.tokyo.jp/hokenfukushi/hoken/kansensho/doko_chosa_2019.html | https://www.city.nerima.tokyo.jp/rss/oshirase/rss_news.xml （全区共通新着: https://www.city.nerima.tokyo.jp/rss_news.xml） | 感染症情報ページURL: High（フェッチ確認済み、令和8年第27週データを確認）。RSS: High — 公式「RSS配信の拡充について」ページ https://www.city.nerima.tokyo.jp/aboutweb/rss.html にてカテゴリ別フィードURL一覧を確認（お知らせ・イベント・保健福祉等）。URL自体の直接フェッチでの内容確認は未実施。 |
| 足立区 | 感染症 | https://www.city.adachi.tokyo.jp/fukushi-kenko/kenko/kansensho/index.html （感染症発生情報: https://www.city.adachi.tokyo.jp/kansensho/201609kansensyoujyouhou.html） | https://www.city.adachi.tokyo.jp/ku/koho/news/news.xml （新着: https://www.city.adachi.tokyo.jp/shinchaku/shinchaku.xml） | 感染症情報ページURL: Medium — トップページ・発生情報ページとも検索結果から確認、直接フェッチ未実施。RSS: High — 公式「RSS利用案内」ページ https://www.city.adachi.tokyo.jp/rss/index.html にてフィード一覧を確認（RSS1.0形式との記載あり）。個別URLの内容フェッチ検証は未実施。 |
| 葛飾区 | 葛飾区感染症週報 | https://www.city.katsushika.lg.jp/kenkou/1000050/1001797/1001959.html | https://www.city.katsushika.lg.jp/news.rss | 感染症情報ページURL: High（フェッチ確認済み、定点把握・全数把握疾患の週報ページであることを確認）。RSS: Medium — 公式RSSページ https://www.city.katsushika.lg.jp/about/rss.html にてnews.rssの記載を確認したが、フィード自体の内容フェッチ検証は未実施。 |
| 江戸川区 | 区内の感染症発生状況（インフルエンザ・新型コロナなど） | https://www.city.edogawa.tokyo.jp/e054/kenko/iryo/kansen/sonota/gurafu.html | https://www.city.edogawa.tokyo.jp/oshirase/oshirase.xml （新着情報: https://www.city.edogawa.tokyo.jp/shinchaku/shinchaku.xml／プレスリリース: https://www.city.edogawa.tokyo.jp/pressrelease/oshirase.xml） | 感染症情報ページURL: High（フェッチ確認済み、2026年第28週の手足口病等の定点報告データを確認）。RSS: High — 公式「RSS配信について」ページ https://www.city.edogawa.tokyo.jp/e004/aboutweb/rss.html にてカテゴリ別フィードURL一覧を確認（豊島区と同じCMSパターンでXML形式である可能性が高いが、URL自体の直接フェッチでの内容検証は未実施）。 |

## 追加検証（curlによる実データ取得確認・EBS_SOURCES登録済み）
中野区・杉並区・豊島区・北区・板橋区・練馬区・足立区・葛飾区・江戸川区の9区はcurlで200・XML形式・実記事タイトルまで確認し、`R/ebs_loader.R`のEBS_SOURCESに追加済み（江戸川区は当初の候補URLが404だったため、公式RSS案内ページから再調査し `https://www.city.edogawa.tokyo.jp/news/shinchaku.xml` を採用）。渋谷区・荒川区は複数パターンを試したがRSS配信が見つからず未収録（継続調査中）。

## 総評・注意点

- **東京都感染症情報センター (idsc.tokyo-eiken.go.jp)** に一本化して独自ページを持たない区は今回の11区の中では見当たらず、いずれも各区独自の感染症週報／発生状況ページを保有している。ただし多くの区が「全数把握疾患」については東京都感染症情報センターのデータを参照するようリンクしている（併用型）。
- **RSSフィードについて**: 実際にXMLであることをフェッチで直接検証できたのは **豊島区** (oshirase.xml, RDF/RSS1.0確認済み) のみ。他区は「RSS配信について」等の公式案内ページの存在・記載内容から候補URLを抽出したが、URL自体のcontent-typeフェッチ検証は行っていないため中〜高信頼度にとどまる。
- **渋谷区・中野区**はRSS配信の形跡が見当たらず、公式にRSSを提供していない可能性が高い（プッシュ通知・SNS中心の情報発信）。自動ニュース取得には、これら2区のみ別途スクレイピング等の代替手段を検討する必要がある。
- **板橋区・荒川区**はRSS案内ページの存在は示唆されるが、具体的なフィードURLを特定できなかった（板橋区は案内ページURLが404、荒川区はフィードURL自体が検索で見つからず）。追加調査が必要。
- **北区**は `city.kita.lg.jp` と `city.kita.tokyo.jp` の2ドメインが混在している点に注意（感染症情報は`.tokyo.jp`、RSS案内は`.lg.jp`で見つかった）。
