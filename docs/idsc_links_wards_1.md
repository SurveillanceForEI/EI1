# 東京23区 感染症情報ページ・RSS調査（1/2）

調査日: 2026年7月時点。以下は千代田区・中央区・港区・新宿区・文京区・台東区・墨田区・江東区・品川区・目黒区・大田区・世田谷区の12区について、(1) 感染症情報／週報ページのURLと (2) 新着情報RSS/APIの候補URLを、WebSearch/WebFetchで確認した結果。RSS URLは実際にXML/RSSとして取得できたかを検証し、信頼度を付した。ページ内容やURLは今後変更される可能性があるため、利用前に再確認を推奨。

| 区名 | 感染症情報ページ名(等) | URL | ニュースRSS/API候補URL | 備考(信頼度など) |
|---|---|---|---|---|
| 千代田区 | インフルエンザ情報（感染症情報のうち代表ページ。専用「感染症情報センター」は無く、東京都感染症情報センターへの言及が中心） | https://www.city.chiyoda.lg.jp/koho/kenko/kenko/kansensho/sonota/influenza.html | https://www.city.chiyoda.lg.jp/shinchaku/shinchaku.xml | RSS: 高信頼度。WebFetchでRDF/RSS1.0形式、50件のitemを確認済み。感染症ページは都センターへの誘導が多く、区独自の週報は限定的。 |
| 中央区 | 中央区感染症発生動向（週報） | https://www.city.chuo.lg.jp/kenkouiryou/iryou/kansen/index.html （動向詳細: https://www.city.chuo.lg.jp/a0031/kenkouiryou/iryou/kansen/chuokansendoukou.html） | https://www.city.chuo.lg.jp/shinchaku/shinchaku.xml | RSS: 高信頼度。RDF/RSS1.0形式で内容確認済み。感染症ページは区独自の発生動向調査を掲載しており充実。 |
| 港区 | 感染症発生動向調査（港区感染症週報） | https://www.city.minato.tokyo.jp/hokenyobou/doukou/ | https://www.city.minato.tokyo.jp/shinchaku/shinchaku.xml | RSS: 高信頼度。RDF/RSS1.0形式、50件超のitemを確認済み（ドメインはcity.minato.tokyo.jpが正、city-minato.tokyo.jpではない点に注意）。感染症週報は区独自で充実。 |
| 新宿区 | 新宿区感染症発生動向調査 週報 | https://www.city.shinjuku.lg.jp/kenkou/yobo01_002243.html | https://www.city.shinjuku.lg.jp/top_rss.rdf | RSS: 高信頼度。RDF形式、dc:creator/dc:date等含む正常なXMLを確認済み。感染症週報は区独自ページで充実。 |
| 文京区 | 感染症発生動向調査：文京区感染症週報 | https://www.city.bunkyo.lg.jp/b028/p002572.html | https://www.city.bunkyo.lg.jp/shinchaku/shinchaku.xml | RSS: 高信頼度。RDF/RSS1.0形式で内容確認済み（文京保健所発の項目を含む）。区独自の週報あり。 |
| 台東区 | 感染症情報／インフルエンザ発生状況 | https://www.city.taito.lg.jp/kenkohukusi/kenkokikikanrieisei/kansensho/kansenshoyobo/index.html （インフル: https://www.city.taito.lg.jp/kenkohukusi/kenkokikikanrieisei/kansensho/influenza/influenzaryuukou.html） | https://www.city.taito.lg.jp/rss_news.xml | RSS: 高信頼度。RSS2.0形式で内容確認済み（2026-07-17付の求人情報等含む）。感染症ページは区独自だが週報形式の詳細な発生動向データは限定的、都センター参照も併用されている模様。 |
| 墨田区 | 感染症の予防と対応／感染症についてのお知らせ | https://www.city.sumida.lg.jp/kenko_fukushi/kenko/kansensyou_yobou/index.html | https://www.city.sumida.lg.jp/rss_news.xml | RSS: 高信頼度。RSS2.0形式で内容確認済み。感染症ページは月次のお知らせ形式が中心で、都の週報・情報センターへの参照も多い。 |
| 江東区 | 保健予防課ページ（区独自の「感染症情報」特設ページは見当たらず、結核・感染症予防ページや保健予防課ページが窓口。区内流行の詳細週報は都センター参照が中心） | https://www.city.koto.lg.jp/fukushi/hoken/yobo/index.html | https://www.city.koto.lg.jp/shinchaku/shinchaku.xml | RSS: 高信頼度。RDF/RSS1.0形式で内容確認済み（2026-07付の職員懲戒処分公表等含む）。感染症情報は都センターへの依存度が高い可能性。 |
| 品川区 | 品川区の感染症流行状況（感染症発生動向調査） | https://www.city.shinagawa.tokyo.jp/PC/kenkou/kenkou-byouki/kenkou-byouki-oshirasenyuryoku/hpg000033447.html （ユーザー確認済み） | 未発見（rss_news.xml, shinchaku/shinchaku.xml, PC/shinchaku/shinchaku.xml等を試行したが404、ページ内にもRSSリンク未検出） | 感染症ページ: 高信頼度（区独自の発生動向調査ページ、ユーザーも確認済み）。RSS: 低信頼度/未発見。品川区議会サイト（gikai.city.shinagawa.tokyo.jp/rss-2）にはRSS案内があるが本庁サイトの一般ニュースRSSは未確認。追加調査（curl）でも発見できず、EBS_SOURCES未収録。 |
| 目黒区 | 目黒区感染症発生動向調査 週報 | https://www.city.meguro.tokyo.jp/hokenyobou/kenkoufukushi/iryou/haseidoukou.html | https://www.city.meguro.tokyo.jp/oshirase/rss_news.xml | RSS: 高信頼度。RDF/RSS1.0形式、56件のitemを確認済み。感染症週報は区独自で充実（緊急情報RSSも別途 https://www.city.meguro.tokyo.jp/kinkyu/rss_news.xml で存在）。 |
| 大田区 | 感染症発生動向調査（区内流行情報） | https://www.city.ota.tokyo.jp/seikatsu/hoken/kansen_taisaku/syuuhou.html | https://www.city.ota.tokyo.jp/oshirase/rss_news.xml | RSS: 高信頼度。RSS2.0形式で内容確認済み。感染症ページは区独自の週別グラフ（PDF）等を含み充実。募集情報用の別RSS（/boshu/rss_news.xml）も存在。 |
| 世田谷区 | 感染症発生動向調査 | https://www.city.setagaya.lg.jp/02015/3155.html | https://www.city.setagaya.lg.jp/shinchaku/shinchaku.xml | RSS: 高信頼度。RDF/RSS1.0形式で内容確認済み。感染症ページは区独自の定点医療機関データ（PDF等）を含み充実。RSS配信の説明ページ: https://www.city.setagaya.lg.jp/02002/7731.html |

## 追加検証（curlによる実データ取得確認・EBS_SOURCES登録済み）
千代田区・中央区・港区・新宿区・文京区・台東区・墨田区・江東区・目黒区・大田区・世田谷区の11区はcurlで200・XML形式・実記事タイトルまで確認し、`R/ebs_loader.R`のEBS_SOURCESに追加済み。品川区のみRSS配信が見つからず未収録（継続調査中）。
