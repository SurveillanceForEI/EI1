# ============================================================
# 感染症情報センター等リンク集データ
# 都道府県(47)・政令指定都市(20)・中核市(62)・保健所政令市(5)・
# 東京23特別区、計157の保健所設置自治体について、公式の
# 「感染症情報センター」または相当ページのURLをまとめたもの。
# 出典: docs/idsc_links_*.md（2026年7月調査）
# ============================================================

.idsc_city <- function(name, type, center, url, own = TRUE) {
  # own: 独自の感染症情報発信サイト/ページを持つか。FALSEの場合は都道府県センターへの
  # 参照として扱う（本文中で「独自センターなし」等と明記されている自治体）
  list(name = name, type = type, center = center, url = url, own = own)
}

IDSC_LINKS <- list(
  list(pref = "北海道", center = "北海道感染症情報センター",
       url = "https://www.iph.pref.hokkaido.jp/kansen/index.html",
       cities = list(
         .idsc_city("札幌市", "政令指定都市", "札幌市衛生研究所 感染症情報", "https://www.city.sapporo.jp/eiken/infect/index.html"),
         .idsc_city("旭川市", "中核市", "感染症", "https://www.city.asahikawa.hokkaido.jp/kurashi/135/136/150/index.html"),
         .idsc_city("函館市", "中核市", "市内の感染症発生状況について", "https://www.city.hakodate.hokkaido.jp/docs/2024020800073/"),
         .idsc_city("小樽市", "保健所政令市", "市内の感染症発生状況について（感染症発生動向調査）", "https://www.city.otaru.lg.jp/docs/2020101600662/")
       )),
  list(pref = "青森県", center = "青森県感染症情報ネット",
       url = "https://www.pref.aomori.lg.jp/soshiki/kenko/hoken/tubeculosis-kansen_home.html",
       cities = list(
         .idsc_city("青森市", "中核市", "青森市保健所（感染症の流行状況についてのお知らせ）", "https://www.city.aomori.aomori.jp/hukushi_kenkou/kenkou_iryou/1003125/1003152/1003153.html"),
         .idsc_city("八戸市", "中核市", "感染症流行状況", "https://www.city.hachinohe.aomori.jp/soshikikarasagasu/hokenyoboka/2/inf_alert/1959.html")
       )),
  list(pref = "岩手県", center = "岩手県感染症情報センター",
       url = "https://www2.pref.iwate.jp/~hp1353/kansen/",
       cities = list(
         .idsc_city("盛岡市", "中核市", "感染症発生状況", "https://www.city.morioka.iwate.jp/kenkou/hokenjo/kansen/1049440/index.html")
       )),
  list(pref = "宮城県", center = "宮城県結核・感染症情報センター",
       url = "https://www.pref.miyagi.jp/site/hokans/kansen-center.html",
       cities = list(
         .idsc_city("仙台市", "政令指定都市", "仙台市の感染症情報", "https://www.city.sendai.jp/kenkoanzen-kansen/kurashi/kenkotofukushi/kenkoiryo/kansensho/jokyo.html")
       )),
  list(pref = "秋田県", center = "秋田県感染症情報センター",
       url = "https://idsc.pref.akita.jp/",
       cities = list(
         .idsc_city("秋田市", "中核市", "秋田市公式サイト「感染症情報」", "https://www.city.akita.lg.jp/kurashi/kenko/1005371/1019138/index.html")
       )),
  list(pref = "山形県", center = "感染症情報センター",
       url = "https://www.eiken.yamagata.yamagata.jp/kansen.html",
       cities = list(
         .idsc_city("山形市", "中核市", "山形市「山形県感染症発生動向調査について」", "https://www.city.yamagata-yamagata.lg.jp/kenkofukushi/iryou/1006676/index.html")
       )),
  list(pref = "福島県", center = "福島県感染症情報センター",
       url = "https://www.pref.fukushima.lg.jp/sec/21910a/kansenshoujouhou.html",
       cities = list(
         .idsc_city("福島市", "中核市", "福島市「感染症の流行状況」", "https://www.city.fukushima.fukushima.jp/soshiki/9/1050/1/1/5429.html"),
         .idsc_city("郡山市", "中核市", "郡山市保健所 保健・感染症課", "https://www.city.koriyama.lg.jp/soshiki/71/116926.html"),
         .idsc_city("いわき市", "中核市", "いわき市役所「市内の感染症情報」", "https://www.city.iwaki.lg.jp/www/contents/1692257880453/index.html")
       )),
  list(pref = "茨城県", center = "感染症情報センター",
       url = "https://www.pref.ibaraki.jp/hokenfukushi/eiken/idwr/index.html",
       cities = list(
         .idsc_city("水戸市", "中核市", "水戸市 感染症流行情報（個別のお知らせページ、週報等の定期サーベイランスではない）", "https://www.city.mito.lg.jp/site/vaccination/2493.html")
       )),
  list(pref = "栃木県", center = "栃木県感染症情報センター（TIDC）",
       url = "https://www.pref.tochigi.lg.jp/e60/tidctop.html",
       cities = list(
         .idsc_city("宇都宮市", "中核市", "宇都宮市「感染症」ページ", "https://www.city.utsunomiya.lg.jp/kenko/iryo/kansensho/index.html")
       )),
  list(pref = "群馬県", center = "感染症情報トップページ",
       url = "https://www.pref.gunma.jp/page/3296.html",
       cities = list(
         .idsc_city("前橋市", "中核市", "感染症・病気", "https://www.city.maebashi.gunma.jp/kenko_fukushi/1/3/index.html"),
         .idsc_city("高崎市", "中核市", "高崎市 感染症情報（個別のお知らせページ、週報等の定期サーベイランスではない）", "https://www.city.takasaki.gunma.jp/page/62103.html")
       )),
  list(pref = "埼玉県", center = "感染症情報センター",
       url = "https://www.pref.saitama.lg.jp/b0714/surveillance/",
       cities = list(
         .idsc_city("さいたま市", "政令指定都市", "さいたま市感染症情報センター", "https://www.city.saitama.lg.jp/008/016/004/index.html"),
         .idsc_city("川越市", "中核市", "感染症ジャーナルかわごえ", "https://www.city.kawagoe.saitama.jp/kenko/iryo/1006178/1006180/1013879.html"),
         .idsc_city("川口市", "中核市", "川口市「最新の感染症流行状況」", "https://www.city.kawaguchi.lg.jp/soshiki/01090/020/hasseijyoukyou/20672.html"),
         .idsc_city("越谷市", "中核市", "越谷市保健所「感染症」ページ", "https://www.city.koshigaya.saitama.jp/kurashi_shisei/fukushi/hokenjo/kansensho/koshigaya_contents_20190829.html")
       )),
  list(pref = "千葉県", center = "千葉県感染症情報センター",
       url = "https://www.pref.chiba.lg.jp/eiken/c-idsc/index.html",
       cities = list(
         .idsc_city("千葉市", "政令指定都市", "千葉市感染症情報センター", "https://www.city.chiba.jp/hokenfukushi/iryoeisei/khoken/kkagaku/idsc/index.html"),
         .idsc_city("船橋市", "中核市", "ふなばし感染症情報", "https://www.city.funabashi.lg.jp/kenkou/kansenshou/001/p115700.html"),
         .idsc_city("柏市", "中核市", "柏市「感染症の発生状況」", "https://www.city.kashiwa.lg.jp/hokenyobo/shiseijoho/shisei/health_hospital/mainmenu/kansensho/hassejokyo.html")
       )),
  list(pref = "東京都", center = "東京都感染症情報センター",
       url = "https://idsc.tokyo-eiken.go.jp/",
       institute_name = "東京都健康安全研究センター（運営母体）",
       institute_url  = "https://www.tmiph.metro.tokyo.lg.jp/",
       cities = list(
         .idsc_city("八王子市", "中核市", "八王子市「感染症」ページ", "https://www.city.hachioji.tokyo.jp/kurashi/hoken/007/009/index.html"),
         .idsc_city("町田市", "保健所政令市", "町田市感染症週報", "https://www.city.machida.tokyo.jp/iryo/hokenjo/kansen/kansensyosyuhou22.html"),
         .idsc_city("千代田区", "特別区", "新型コロナウイルス感染症の発生状況（個別のお知らせページ、週報等の定期サーベイランスではない）", "https://www.city.chiyoda.lg.jp/koho/kenko/kenko/kansensho/coronavirus/hasseijokyo.html"),
         .idsc_city("中央区", "特別区", "中央区感染症発生動向（週報）", "https://www.city.chuo.lg.jp/kenkouiryou/iryou/kansen/index.html"),
         .idsc_city("港区", "特別区", "感染症発生動向調査（港区感染症週報）", "https://www.city.minato.tokyo.jp/hokenyobou/doukou/"),
         .idsc_city("新宿区", "特別区", "新宿区感染症発生動向調査 週報", "https://www.city.shinjuku.lg.jp/kenkou/yobo01_002243.html"),
         .idsc_city("文京区", "特別区", "文京区感染症週報", "https://www.city.bunkyo.lg.jp/b028/p002572.html"),
         .idsc_city("台東区", "特別区", "感染症情報 注目のお知らせ（個別のお知らせページ、週報等の定期サーベイランスではない）", "https://www.city.taito.lg.jp/kenkohukusi/kenkokikikanrieisei/kansensho/kansenshoyobo/chuumoku/01587234.html"),
         .idsc_city("墨田区", "特別区", "感染症のお知らせ（令和8年度、個別のお知らせページ、週報等の定期サーベイランスではない）", "https://www.city.sumida.lg.jp/kenko_fukushi/kenko/kansensyou_yobou/osirase/kosirase/r8.html"),
         .idsc_city("江東区", "特別区", "保健予防課ページ", "https://www.city.koto.lg.jp/fukushi/hoken/yobo/index.html"),
         .idsc_city("品川区", "特別区", "品川区の感染症流行状況", "https://www.city.shinagawa.tokyo.jp/PC/kenkou/kenkou-byouki/kenkou-byouki-oshirasenyuryoku/hpg000033447.html"),
         .idsc_city("目黒区", "特別区", "目黒区感染症発生動向調査 週報", "https://www.city.meguro.tokyo.jp/hokenyobou/kenkoufukushi/iryou/haseidoukou.html"),
         .idsc_city("大田区", "特別区", "感染症発生動向調査（区内流行情報）", "https://www.city.ota.tokyo.jp/seikatsu/hoken/kansen_taisaku/syuuhou.html"),
         .idsc_city("世田谷区", "特別区", "感染症発生動向調査", "https://www.city.setagaya.lg.jp/02015/3155.html"),
         .idsc_city("渋谷区", "特別区", "渋谷区感染症週報・月報", "https://www.city.shibuya.tokyo.jp/kenko/kansen/kansen-jokyo/kansen-doko.html"),
         .idsc_city("中野区", "特別区", "中野区の感染症最新情報", "https://www.city.tokyo-nakano.lg.jp/kenko_hukushi/kansen/saishin.html"),
         .idsc_city("杉並区", "特別区", "感染症", "https://www.city.suginami.tokyo.jp/kenkou/kenkou/kansenshou/index.html"),
         .idsc_city("豊島区", "特別区", "感染症情報", "https://www.city.toshima.lg.jp/543/kenko/kenko/kansensho/1803141318.html"),
         .idsc_city("北区", "特別区", "北区の感染症最新情報", "https://www.city.kita.tokyo.jp/kenko/kansensho/shuho/shuho2023-2025.html"),
         .idsc_city("荒川区", "特別区", "感染症発生情報", "https://www.city.arakawa.tokyo.jp/a034/kenkouiryou/kansenshou/hasseidoukou.html"),
         .idsc_city("板橋区", "特別区", "板橋区感染症ニュース", "https://www.city.itabashi.tokyo.jp/kenko/kansensho/1014886.html"),
         .idsc_city("練馬区", "特別区", "感染症発生動向調査", "https://www.city.nerima.tokyo.jp/hokenfukushi/hoken/kansensho/doko_chosa_2019.html"),
         .idsc_city("足立区", "特別区", "感染症", "https://www.city.adachi.tokyo.jp/fukushi-kenko/kenko/kansensho/index.html"),
         .idsc_city("葛飾区", "特別区", "葛飾区感染症週報", "https://www.city.katsushika.lg.jp/kenkou/1000050/1001797/1001959.html"),
         .idsc_city("江戸川区", "特別区", "区内の感染症発生状況", "https://www.city.edogawa.tokyo.jp/e054/kenko/iryo/kansen/sonota/gurafu.html")
       )),
  list(pref = "神奈川県", center = "感染症情報センター",
       url = "https://www.pref.kanagawa.jp/sys/eiken/003_center/03_center_main.htm",
       cities = list(
         .idsc_city("横浜市", "政令指定都市", "横浜市感染症情報センター", "https://www.city.yokohama.lg.jp/kenko-iryo-fukushi/kenko-iryo/eiken/idsc.html"),
         .idsc_city("川崎市", "政令指定都市", "川崎市感染症情報センター", "https://www.city.kawasaki.jp/kurashi/category/22-13-8-11-0-0-0-0-0-0.html"),
         .idsc_city("相模原市", "政令指定都市", "相模原市感染症情報センター", "https://www.city.sagamihara.kanagawa.jp/kosodate/kenko/1026625/kansenyobo/hassei_jokyo/1007129.html"),
         .idsc_city("横須賀市", "中核市", "感染症対策・予防接種", "https://www.city.yokosuka.kanagawa.jp/kenko/kenko/kansensho/index.html"),
         .idsc_city("藤沢市", "保健所政令市", "藤沢市感染症発生状況", "https://www.city.fujisawa.kanagawa.jp/kenko/iryo/kansensho/kansensho/hassei/index.html"),
         .idsc_city("茅ヶ崎市", "保健所政令市", "感染症情報（週報）", "https://www.city.chigasaki.kanagawa.jp/kenko/1022933/1038459/1046393/1046735.html")
       )),
  list(pref = "新潟県", center = "新潟県感染症情報（週報）",
       url = "https://www.pref.niigata.lg.jp/sec/kanyaku/1232482573101.html",
       cities = list(
         .idsc_city("新潟市", "政令指定都市", "新潟市感染症情報", "https://www.city.niigata.lg.jp/iryo/kenko/yobou_kansen/400kansen/index.html")
       )),
  list(pref = "富山県", center = "富山県感染症情報センター",
       url = "https://www.pref.toyama.jp/1279/kansen/",
       cities = list(
         .idsc_city("富山市", "中核市", "富山市 感染症発生状況", "https://www.city.toyama.lg.jp/health/kenshin/1010470/index.html")
       )),
  list(pref = "石川県", center = "石川県感染症情報センター",
       url = "https://www.pref.ishikawa.lg.jp/hokan/kansenjoho/top/top.html",
       cities = list(
         .idsc_city("金沢市", "中核市", "金沢市保健所地域保健課ページ", "https://www4.city.kanazawa.lg.jp/soshikikarasagasu/chiikihokenka/27342.html")
       )),
  list(pref = "福井県", center = "福井県感染症情報",
       url = "https://info.pref.fukui.lg.jp/kansensyou/",
       cities = list(
         .idsc_city("福井市", "中核市", "福井市「感染症発生動向調査について」", "https://www.city.fukui.lg.jp/fukusi/iryou/kensen/index.html")
       )),
  list(pref = "山梨県", center = "やまなし感染症ポータルサイト",
       url = "https://www.pref.yamanashi.jp/kansensho_portal/",
       cities = list(
         .idsc_city("甲府市", "中核市", "甲府市「感染症発生動向情報」", "https://www.city.kofu.yamanashi.jp/kenko/kenko/kansensyou/hasseidoko/index.html")
       )),
  list(pref = "長野県", center = "長野県感染症情報",
       url = "https://www.pref.nagano.lg.jp/shippei-kansen/kenko/kenko/kansensho/joho/index.html",
       cities = list(
         .idsc_city("長野市", "中核市", "長野市感染症情報", "https://www.city.nagano.nagano.jp/n106500/contents/p002025.html"),
         .idsc_city("松本市", "中核市", "松本市感染症情報", "https://www.city.matsumoto.nagano.jp/soshiki/230/1552.html")
       )),
  list(pref = "岐阜県", center = "感染症情報センター",
       url = "https://www.pref.gifu.lg.jp/page/9550.html",
       cities = list(
         .idsc_city("岐阜市", "中核市", "岐阜市 感染症情報", "https://www.city.gifu.lg.jp/kenko/kansensyou/1004430.html")
       )),
  list(pref = "静岡県", center = "感染症情報センター",
       url = "https://www.pref.shizuoka.jp/kenkofukushi/shippeikansensho/kansensho/1003065/index.html",
       cities = list(
         .idsc_city("静岡市", "政令指定都市", "静岡市感染症発生動向調査", "https://www.city.shizuoka.lg.jp/s2371/s003411.html"),
         .idsc_city("浜松市", "政令指定都市", "浜松市感染症情報センター", "https://www.city.hamamatsu.shizuoka.jp/hokanken/idwr/index.html")
       )),
  list(pref = "愛知県", center = "感染症の発生状況",
       url = "https://www.pref.aichi.jp/eiseiken/kansentop.html",
       cities = list(
         .idsc_city("名古屋市", "政令指定都市", "名古屋市感染症情報センター", "https://www.city.nagoya.jp/kenkofukushi/eisei/1015269/1015388/index.html"),
         .idsc_city("豊橋市", "中核市", "豊橋市「感染症発生動向調査」", "https://www.city.toyohashi.lg.jp/12751.htm"),
         .idsc_city("岡崎市", "中核市", "岡崎市「岡崎市内における各種感染症の発生状況」", "https://www.city.okazaki.lg.jp/1100/1107/1146/p005116.html"),
         .idsc_city("一宮市", "中核市", "一宮市「感染症発生動向調査情報ダッシュボード」", "https://www.city.ichinomiya.aichi.jp/hokenjo/hokenyobou/1044114/1000371/1063611.html"),
         .idsc_city("豊田市", "中核市", "豊田市「豊田市の感染症発生動向」", "https://www.city.toyota.aichi.jp/kurashi/kenkou/eisei/1069535/index.html")
       )),
  list(pref = "三重県", center = "三重県感染症情報センター",
       url = "https://www.kenkou.pref.mie.jp/",
       cities = list(
         .idsc_city("四日市市", "保健所政令市", "四日市市「感染症（発生）情報」", "https://www.city.yokkaichi.lg.jp/www/genre/1000100000448/index.html")
       )),
  list(pref = "滋賀県", center = "感染症情報センター",
       url = "https://www.pref.shiga.lg.jp/eiseikagaku/kansensyou/",
       cities = list(
         .idsc_city("大津市", "中核市", "大津市「感染症発生状況」", "https://www.city.otsu.lg.jp/soshiki/021/1443/g/kansensho/jokyo/index.html")
       )),
  list(pref = "京都府", center = "京都府感染症情報センター",
       url = "https://www.pref.kyoto.jp/idsc/",
       cities = list(
         .idsc_city("京都市", "政令指定都市", "京都市感染症情報センター", "https://www.city.kyoto.lg.jp/menu3/category/41-6-0-0-0-0-0-0-0-0.html")
       )),
  list(pref = "大阪府", center = "大阪府感染症情報センター",
       url = "https://www.iph.pref.osaka.jp/",
       institute_name = "大阪健康安全基盤研究所（運営母体）",
       institute_url  = "https://www.iph.osaka.jp/",
       cities = list(
         .idsc_city("大阪市", "政令指定都市", "大阪市感染症週報", "https://www.city.osaka.lg.jp/kenko/page/0000025741.html"),
         .idsc_city("堺市", "政令指定都市", "堺市感染症情報センター", "https://www.city.sakai.lg.jp/kenko/kenko/hokencenter/eiken/id_db/eiken.html"),
         .idsc_city("豊中市", "中核市", "豊中市「感染症流行状況」", "https://www.city.toyonaka.osaka.jp/kenko/kenko_hokeneisei/kekkaku_kansensho/sonota_kansensho/kansenryuukou.html"),
         .idsc_city("吹田市", "中核市", "吹田市「感染症サーベイランスシステムについて」", "https://www.city.suita.osaka.jp/kenko/1018600/1018623/1023375/index.html"),
         .idsc_city("高槻市", "中核市", "高槻市「感染症発生動向調査」", "https://www.city.takatsuki.osaka.jp/life/3/36/203/"),
         .idsc_city("枚方市", "中核市", "枚方市「感染症流行状況」", "https://www.city.hirakata.osaka.jp/0000050814.html"),
         .idsc_city("八尾市", "中核市", "八尾市保健所管内感染症情報", "https://www.city.yao.osaka.jp/kenkou_fukushi/iryou/1008460/1008461/1008462.html"),
         .idsc_city("寝屋川市", "中核市", "感染症発生状況詳細（定点把握疾患）", "https://www.city.neyagawa.osaka.jp/organization_list/kenkou/hokenyobou/kansensyo/survey/16720.html"),
         .idsc_city("東大阪市", "中核市", "東大阪市保健所 感染症対策課", "https://www.city.higashiosaka.lg.jp/0000041576.html")
       )),
  list(pref = "兵庫県", center = "兵庫県感染症情報センター",
       url = "https://web.pref.hyogo.lg.jp/iphs01/kansensho_jyoho/infectdis2.html",
       cities = list(
         .idsc_city("神戸市", "政令指定都市", "神戸市感染症統合情報システム（KMSS）", "https://kobecity-kmss.jp/"),
         .idsc_city("姫路市", "中核市", "姫路市 感染症発生動向調査", "https://www.city.himeji.lg.jp/kurashi/0000003629.html"),
         .idsc_city("尼崎市", "中核市", "尼崎市 感染症発生動向調査結果", "https://www.city.amagasaki.hyogo.jp/kurashi/kenko/kansensyo/049hasseidoukou.html"),
         .idsc_city("明石市", "中核市", "あかし保健所管内感染症情報", "https://www.city.akashi.lg.jp/hokensyo/h-yobou/kansen/kansenjyoho.html"),
         .idsc_city("西宮市", "中核市", "西宮市 感染症発生動向調査", "https://www.nishi.or.jp/kenko/hokenjojoho/kansensho/other/hasseidoukou.html")
       )),
  list(pref = "奈良県", center = "奈良県感染症情報センター",
       url = "https://www.pref.nara.jp/27874.htm",
       cities = list(
         .idsc_city("奈良市", "中核市", "奈良市 感染症情報", "https://www.city.nara.lg.jp/life/4/31/126/")
       )),
  list(pref = "和歌山県", center = "和歌山県感染症情報センター",
       url = "https://www.pref.wakayama.lg.jp/prefg/031801/idsw/d00153659.html",
       cities = list(
         .idsc_city("和歌山市", "中核市", "和歌山市感染症情報センター（独自ドメイン）", "https://www.kansen-wakayama.jp/")
       )),
  list(pref = "鳥取県", center = "感染症情報（週報）",
       url = "https://www.pref.tottori.lg.jp/60743.htm",
       cities = list(
         .idsc_city("鳥取市", "中核市", "鳥取市保健所 感染症情報", "https://www.city.tottori.lg.jp/site/kansen/4913.html")
       )),
  list(pref = "島根県", center = "島根県感染症情報提供システム",
       url = "https://www1.pref.shimane.lg.jp/contents/kansen/",
       cities = list(
         .idsc_city("松江市", "中核市", "島根県感染症情報提供システム（松江市保健所は県と共同設置）", "https://www1.pref.shimane.lg.jp/contents/kansen/")
       )),
  list(pref = "岡山県", center = "感染症情報センター",
       url = "https://www.pref.okayama.jp/soshiki/309/",
       cities = list(
         .idsc_city("岡山市", "政令指定都市", "岡山市感染症情報センター", "https://www.city.okayama.jp/kurashi/0000008327.html"),
         .idsc_city("倉敷市", "中核市", "倉敷市保健所 保健課 感染症情報", "https://www.city.kurashiki.okayama.jp/fukushi/health/1004741/1012618/index.html")
       )),
  list(pref = "広島県", center = "広島県感染症・疾病管理センター（ひろしまCDC）",
       url = "https://www.pref.hiroshima.lg.jp/site/hcdc/",
       cities = list(
         .idsc_city("広島市", "政令指定都市", "広島市感染症情報センター", "https://www.city.hiroshima.lg.jp/living/eisei/1003071/index.html"),
         .idsc_city("呉市", "中核市", "呉市 感染症発生動向調査（週報・月報）", "https://www.city.kure.lg.jp/soshiki/44/weekly.html"),
         .idsc_city("福山市", "中核市", "福山市 感染症発生状況", "https://www.city.fukuyama.hiroshima.jp/soshiki/hokenyobo/1881.html")
       )),
  list(pref = "山口県", center = "山口県感染症情報センター",
       url = "https://www.pref.yamaguchi.lg.jp/site/yidsc/",
       cities = list(
         .idsc_city("下関市", "中核市", "感染症", "https://www.city.shimonoseki.lg.jp/life/2/14/58/")
       )),
  list(pref = "徳島県", center = "徳島県感染症情報センター",
       url = "https://www.pref.tokushima.lg.jp/ippannokata/kenko/kansensho/2004062300038/",
       cities = list()),
  list(pref = "香川県", center = "感染症週報・月報",
       url = "https://www.pref.kagawa.lg.jp/kansensyo/kansensyoujouhou/hou/kfvn.html",
       cities = list(
         .idsc_city("高松市", "中核市", "高松市 感染症情報", "https://www.city.takamatsu.kagawa.jp/smph/kurashi/kenkou/kansensho/joho/index.html")
       )),
  list(pref = "愛媛県", center = "愛媛県感染症情報センター",
       url = "https://www.pref.ehime.jp/site/kanjyo/",
       cities = list(
         .idsc_city("松山市", "中核市", "松山市保健所 感染症対策", "https://www.city.matsuyama.ehime.jp/kurashi/iryo/hokenyobo/kansensho/index.html")
       )),
  list(pref = "高知県", center = "感染症に関する情報",
       url = "https://www.pref.kochi.lg.jp/doc/kansenshou/",
       cities = list(
         .idsc_city("高知市", "中核市", "高知市 感染症情報", "https://www.city.kochi.kochi.jp/life/27/167/1414/")
       )),
  list(pref = "福岡県", center = "福岡県感染症情報",
       url = "http://www.fihes.pref.fukuoka.jp/~idsc_fukuoka/",
       cities = list(
         .idsc_city("福岡市", "政令指定都市", "福岡市感染症発生報告数（定点報告）", "https://www.city.fukuoka.lg.jp/hofuku/hokensho/kansensho/kansenshojoho/chosa/teitenhoukoku.html"),
         .idsc_city("北九州市", "政令指定都市", "北九州市の感染症発生動向（定点報告）", "https://www.city.kitakyushu.lg.jp/contents/18300149.html"),
         .idsc_city("久留米市", "中核市", "久留米市 感染症ページ", "https://www.city.kurume.fukuoka.jp/1070kenkou/2040hokeneisei/3090kansensho/")
       )),
  list(pref = "佐賀県", center = "佐賀県感染症情報センター",
       url = "https://kansen.pref.saga.jp/",
       cities = list()),
  list(pref = "長崎県", center = "感染症情報速報(最新)",
       url = "https://www.pref.nagasaki.jp/doc/page-673274.html",
       cities = list(
         .idsc_city("長崎市", "中核市", "県感染症情報センターに準拠、市も発生動向ページあり", "https://www.city.nagasaki.lg.jp/page/4047.html"),
         .idsc_city("佐世保市", "中核市", "佐世保市 感染症ページ", "https://www.city.sasebo.lg.jp/hokenhukusi/kansen/20231011.html")
       )),
  list(pref = "熊本県", center = "感染症発生情報（週報）",
       url = "https://www.pref.kumamoto.jp/soshiki/30/51400.html",
       cities = list(
         .idsc_city("熊本市", "政令指定都市", "熊本市の感染症発生情報（健康危機管理課）", "https://www.city.kumamoto.jp/kiji0034156/index.html")
       )),
  list(pref = "大分県", center = "感染症情報",
       url = "https://www.pref.oita.jp/site/kansenpotal03/",
       cities = list(
         .idsc_city("大分市", "中核市", "大分市 感染症発生動向情報", "https://www.city.oita.oita.jp/o096/kenko/hoken/1137572759760.html")
       )),
  list(pref = "宮崎県", center = "宮崎県感染症情報センター",
       url = "https://www.pref.miyazaki.lg.jp/contents/org/fukushi/eikanken/center/",
       cities = list(
         .idsc_city("宮崎市", "中核市", "宮崎市 感染症の予防ページ", "https://www.city.miyazaki.miyazaki.jp/health/health/infection/12246.html")
       )),
  list(pref = "鹿児島県", center = "鹿児島県感染症情報",
       url = "http://www.pref.kagoshima.jp/kenko-fukushi/kenko-iryo/kansen/info/index.html",
       cities = list(
         .idsc_city("鹿児島市", "中核市", "鹿児島市感染症情報（週報・月報・四半期報）", "https://www.city.kagoshima.lg.jp/kenkofukushi/hokenjo/hoyobo-kan/kenko/kenko/ryuko/hasse.html")
       )),
  list(pref = "沖縄県", center = "沖縄県感染症情報センター",
       url = "https://www.pref.okinawa.lg.jp/site/hoken/eiken/kikaku/kansenjouhou/home.html",
       cities = list(
         .idsc_city("那覇市", "中核市", "那覇市の感染症発生状況（週報等）", "https://www.city.naha.okinawa.jp/nahahokenjyo/kansensyou/hassei/nahashijoukyou.html")
       ))
)

# ============================================================
# 国レベルの公的機関（国内）
# ============================================================
IDSC_NATIONAL <- list(
  list(name = "JIHS（国立健康危機管理研究機構）感染症情報提供サイト",
       org  = "国立健康危機管理研究機構（JIHS、旧NIID国立感染症研究所）",
       url  = "https://id-info.jihs.go.jp/")
)

# ============================================================
# 海外の公的機関・国際機関（サーベイランス情報を発信）
# ============================================================
.idsc_overseas <- function(name, org, url, region) {
  list(name = name, org = org, url = url, region = region)
}

IDSC_OVERSEAS <- list(
  # ── WHO本部・国際機関 ──────────────────────────────────────
  .idsc_overseas("WHO Disease Outbreak News", "世界保健機関（WHO）",
                 "https://www.who.int/emergencies/disease-outbreak-news", "国際機関"),
  .idsc_overseas("WHO EIOS（疫学情報収集ツール）", "世界保健機関（WHO）",
                 "https://www.who.int/initiatives/eios", "国際機関"),
  .idsc_overseas("ReliefWeb（人道情報・アウトブレイク速報）", "国連人道問題調整事務所（OCHA）",
                 "https://reliefweb.int/", "国際機関"),

  # ── アフリカ地域（AFRO）─────────────────────────────────────
  .idsc_overseas("Africa CDC（大陸統括機関）", "アフリカ疾病予防管理センター（Africa CDC）",
                 "https://africacdc.org/", "アフリカ地域"),
  .idsc_overseas("NCDC 感染症サーベイランス（Disease Situation Reports）", "ナイジェリア疾病予防管理センター（NCDC）",
                 "https://ncdc.gov.ng/diseases/sitreps", "アフリカ地域"),
  .idsc_overseas("IDSR（統合疾病サーベイランス・対応）ガイドライン・データ", "ケニア保健省",
                 "http://guidelines.health.go.ke/", "アフリカ地域"),
  .idsc_overseas("NICD（感染症サーベイランス）", "国立感染症研究所（南アフリカ）",
                 "https://www.nicd.ac.za/", "アフリカ地域"),
  .idsc_overseas("エチオピア公衆衛生研究所（公衆衛生緊急事態サーベイランス）", "エチオピア公衆衛生研究所（EPHI）",
                 "https://ephi.gov.et/", "アフリカ地域"),
  .idsc_overseas("Disease Outbreak Monitoring", "ガーナ保健サービス（GHS）",
                 "https://ghs.gov.gh/disease-outbreaks", "アフリカ地域"),
  .idsc_overseas("Ministère de la Santé Publique（感染症流行情報含む）", "コンゴ民主共和国保健省",
                 "https://sante.gouv.cd/", "アフリカ地域"),

  # ── 米州地域（AMRO/PAHO）───────────────────────────────────
  .idsc_overseas("PAHO Epidemiological Alerts and Updates", "汎米保健機構（PAHO/WHO）",
                 "https://www.paho.org/en/epidemiological-alerts-and-updates", "米州地域"),
  .idsc_overseas("CDC Outbreaks", "米国疾病予防管理センター（CDC）",
                 "https://www.cdc.gov/outbreaks/index.html", "米州地域"),
  .idsc_overseas("Canadian Notifiable Diseases Online", "カナダ公衆衛生庁（PHAC）",
                 "https://diseases.canada.ca/notifiable/", "米州地域"),
  .idsc_overseas("Boletim Epidemiológico（疫学速報）", "ブラジル保健省",
                 "https://www.gov.br/saude/pt-br/centrais-de-conteudo/publicacoes/boletins/epidemiologicos/ultimos", "米州地域"),
  .idsc_overseas("Boletín Epidemiológico（全国疫学速報）", "メキシコ保健省",
                 "https://www.gob.mx/salud/acciones-y-programas/direccion-general-de-epidemiologia-boletin-epidemiologico", "米州地域"),
  .idsc_overseas("Boletín Epidemiológico Nacional（全国疫学速報）", "アルゼンチン保健省",
                 "https://www.argentina.gob.ar/salud/boletin-epidemiologico-nacional", "米州地域"),
  .idsc_overseas("Instituto Nacional de Salud（SIVIGILA公衆衛生サーベイランス）", "コロンビア国立衛生研究所（INS）",
                 "https://www.ins.gov.co/", "米州地域"),

  # ── 東地中海地域（EMRO）─────────────────────────────────────
  .idsc_overseas("WHO EMRO（東地中海地域事務局）", "世界保健機関 東地中海地域事務局",
                 "https://www.emro.who.int/", "東地中海地域"),
  .idsc_overseas("感染症に関する健康啓発情報", "サウジアラビア保健省",
                 "https://www.moh.gov.sa/en/HealthAwareness/EducationalContent/Diseases/Infectious/Pages/default.aspx", "東地中海地域"),
  .idsc_overseas("National Institutes of Health（感染症サーベイランス指針）", "パキスタン国立衛生研究所（NIH）",
                 "https://www.nih.org.pk/", "東地中海地域"),
  .idsc_overseas("感染症対策（Combatting Communicable Diseases）", "アラブ首長国連邦政府",
                 "https://u.ae/en/information-and-services/health-and-fitness/combatting-communicable-diseases", "東地中海地域"),

  # ── 欧州地域（EURO）─────────────────────────────────────────
  .idsc_overseas("ECDC Surveillance and disease data", "欧州疾病予防管理センター（ECDC）",
                 "https://www.ecdc.europa.eu/en/surveillance-and-disease-data", "欧州地域"),
  .idsc_overseas("UK Health Security Agency", "英国健康安全保障庁（UKHSA）",
                 "https://www.gov.uk/government/organisations/uk-health-security-agency", "欧州地域"),
  .idsc_overseas("Robert Koch-Institut（感染症サーベイランス）", "ロベルト・コッホ研究所（ドイツ）",
                 "https://www.rki.de/DE/Themen/Infektionskrankheiten/infektionskrankheiten_node.html", "欧州地域"),
  .idsc_overseas("Santé publique France", "フランス公衆衛生局",
                 "https://www.santepubliquefrance.fr/", "欧州地域"),
  .idsc_overseas("EpiCentro（疫学サーベイランスポータル）", "イタリア国立衛生研究所（ISS）",
                 "https://www.epicentro.iss.it/", "欧州地域"),
  .idsc_overseas("RIVM Infectious Disease Control", "オランダ国立公衆衛生環境研究所（RIVM）",
                 "https://www.rivm.nl/en/infectious-disease-control", "欧州地域"),
  .idsc_overseas("RENAVE（国家公衆衛生サーベイランスネットワーク）", "スペイン カルロス3世保健研究所（ISCIII）",
                 "https://www.isciii.es/en/servicios/vigilancia-salud-publica-renave", "欧州地域"),

  # ── 東南アジア地域（SEARO）──────────────────────────────────
  .idsc_overseas("IDSP（統合疾病サーベイランスプログラム）", "インド国立疾病予防管理センター（NCDC）",
                 "https://idsp.mohfw.gov.in/", "東南アジア地域"),
  .idsc_overseas("Infeksi Emerging（新興感染症情報）", "インドネシア保健省",
                 "https://infeksiemerging.kemkes.go.id/", "東南アジア地域"),
  .idsc_overseas("Department of Disease Control（DDC）", "タイ保健省 疾病管理局",
                 "https://ddc.moph.go.th/en/", "東南アジア地域"),
  .idsc_overseas("IEDCR（疫学・疾病対策研究所）", "バングラデシュ保健省 IEDCR",
                 "https://iedcr.gov.bd/", "東南アジア地域"),
  .idsc_overseas("Epidemiology Unit（週報 Weekly Epidemiological Return）", "スリランカ保健省 疫学ユニット",
                 "https://www.epid.gov.lk/", "東南アジア地域"),
  # ミャンマー保健省(moh.gov.mm/mohs.gov.mm)は接続不可のため見送り（継続調査中）

  # ── 西太平洋地域（WPRO）─────────────────────────────────────
  .idsc_overseas("台湾 CDC（衛生福利部疾病管制署）", "台湾 衛生福利部疾病管制署",
                 "https://www.cdc.gov.tw/", "西太平洋地域"),
  .idsc_overseas("中国 CDC（中国疾病預防控制中心）", "中国疾病預防控制中心",
                 "https://www.chinacdc.cn/", "西太平洋地域"),
  .idsc_overseas("香港 CHP（衛生防護中心）", "香港衛生署 衛生防護中心",
                 "https://www.chp.gov.hk/", "西太平洋地域"),
  .idsc_overseas("Australian CDC — NNDSS（全国届出感染症サーベイランスシステム）", "オーストラリア疾病予防管理センター（CDC Australia）",
                 "https://www.cdc.gov.au/diseases/surveillance-systems-and-networks/national-notifiable-diseases-surveillance-system-nndss", "西太平洋地域"),
  .idsc_overseas("PHF Science（旧ESR）感染症サーベイランス", "ニュージーランド公衆衛生・法科学研究所（PHF Science）",
                 "https://www.esr.cri.nz/expertise/public-health/infectious-disease-intelligence-surveillance/", "西太平洋地域"),
  .idsc_overseas("KDCA 感染症サーベイランスシステム", "韓国疾病管理庁（KDCA）",
                 "https://www.kdca.go.kr/eng/4354/subview.do", "西太平洋地域"),
  .idsc_overseas("CDA（感染症庁）", "シンガポール感染症庁（Communicable Diseases Agency）",
                 "https://www.cda.gov.sg/", "西太平洋地域"),
  .idsc_overseas("DOH 週間疾病サーベイランス報告", "フィリピン保健省 疫学局（Epidemiology Bureau, DOH）",
                 "https://doh.gov.ph/health-statistics/weekly-disease-surveillance-report/", "西太平洋地域"),
  .idsc_overseas("KKMNOW（保健省オープンデータ・感染症統計）", "マレーシア保健省（MOH Malaysia）",
                 "https://data.moh.gov.my/", "西太平洋地域"),
  .idsc_overseas("保健省（感染症サーベイランス専用ページは未確認）", "ベトナム保健省（Ministry of Health）",
                 "https://moh.gov.vn/", "西太平洋地域")
)

.idsc_overseas_region_color <- function(region) {
  c("国際機関"="#7f8c8d", "アフリカ地域"="#d35400", "米州地域"="#2980b9",
    "東地中海地域"="#16a085", "欧州地域"="#8e44ad", "東南アジア地域"="#f39c12",
    "西太平洋地域"="#c0392b")[region]
}

# タイプ別バッジ色
.idsc_type_color <- function(type) {
  c("政令指定都市"="#2980b9", "中核市"="#27ae60", "保健所政令市"="#e67e22",
    "特別区"="#8e44ad")[type]
}

# 都道府県を8地方区分にグルーピングするための境界（IDSC_LINKSはJIS標準順）
.IDSC_REGION_BOUNDS <- list(
  "北海道"     = c(1, 1),
  "東北"       = c(2, 7),
  "関東"       = c(8, 14),
  "中部"       = c(15, 23),
  "近畿"       = c(24, 30),
  "中国"       = c(31, 35),
  "四国"       = c(36, 39),
  "九州・沖縄" = c(40, 47)
)

# 参考リンクタブのUIを生成（アコーディオンは使わず、独自サイトを持つ市区は
# 都道府県と並列のフラットな一覧として表示。独自サイトを持たない市区は
# 都道府県行に「参照」として自治体名のみ注記する）
# 縦に長いページのため、上部に地方区分別クイックジャンプと検索ボックス、
# 下部に「トップに戻る」ボタンを設置してアクセス性を確保している
render_idsc_links_ui <- function() {
  rows <- lapply(seq_along(IDSC_LINKS), function(i) {
    p <- IDSC_LINKS[[i]]
    own_cities  <- Filter(function(c) isTRUE(c$own),  p$cities)
    ref_cities  <- Filter(function(c) !isTRUE(c$own), p$cities)

    pref_row <- tags$tr(
      id = paste0("pref-", i),
      tags$td(style="font-weight:bold;color:#2c3e50;padding:6px 8px;border-top:2px solid #dfe6e9;white-space:nowrap;scroll-margin-top:70px;",
              p$pref),
      tags$td(style="padding:6px 8px;border-top:2px solid #dfe6e9;",
              tags$span(class="badge", style="background:#7f8c8d;color:#fff;font-size:0.75em;padding:2px 6px;border-radius:3px;", "都道府県")),
      tags$td(style="padding:6px 8px;border-top:2px solid #dfe6e9;",
        tags$a(href = p$url, target = "_blank", rel = "noopener noreferrer", p$center),
        if (!is.null(p$institute_url))
          tags$span(style="color:#555;font-size:0.85em;margin-left:8px;",
            "／ ", tags$a(href = p$institute_url, target = "_blank", rel = "noopener noreferrer", p$institute_name)),
        if (length(ref_cities) > 0)
          tags$div(style="color:#888;font-size:0.82em;margin-top:2px;",
            "（参照: ", paste(vapply(ref_cities, function(c) c$name, character(1)), collapse="・"), "）")
      )
    )

    city_rows <- lapply(own_cities, function(c) {
      tags$tr(
        tags$td(style="padding:5px 8px 5px 24px;color:#2c3e50;white-space:nowrap;", c$name),
        tags$td(style="padding:5px 8px;",
          tags$span(class="badge", style=paste0("background:", .idsc_type_color(c$type), ";color:#fff;font-size:0.75em;padding:2px 6px;border-radius:3px;"), c$type)),
        tags$td(style="padding:5px 8px;",
          tags$a(href = c$url, target = "_blank", rel = "noopener noreferrer", c$center))
      )
    })

    c(list(pref_row), city_rows)
  })

  overseas_rows <- lapply(IDSC_OVERSEAS, function(o) {
    tags$tr(
      tags$td(style="padding:5px 8px;",
        tags$span(class="badge", style=paste0("background:", .idsc_overseas_region_color(o$region), ";color:#fff;font-size:0.75em;padding:2px 6px;border-radius:3px;"), o$region)),
      tags$td(style="padding:5px 8px;color:#2c3e50;white-space:nowrap;", o$org),
      tags$td(style="padding:5px 8px;",
        tags$a(href = o$url, target = "_blank", rel = "noopener noreferrer", o$name))
    )
  })

  # 地方区分ごとのクイックジャンプ用チップを生成
  region_nav <- lapply(names(.IDSC_REGION_BOUNDS), function(region) {
    rng <- .IDSC_REGION_BOUNDS[[region]]
    chips <- lapply(seq(rng[1], rng[2]), function(i) {
      tags$a(href = paste0("#pref-", i),
             style="display:inline-block;background:#fff;border:1px solid #b8d4e8;color:#2980b9;font-size:0.8em;padding:2px 8px;border-radius:12px;margin:2px;text-decoration:none;",
             IDSC_LINKS[[i]]$pref)
    })
    tags$div(style="margin-bottom:4px;",
      tags$span(style="font-weight:bold;color:#2c3e50;font-size:0.82em;margin-right:6px;", region, "："),
      chips
    )
  })

  tags$div(id = "idsc-top", style="padding:20px;max-width:1000px;",
    tags$div(
      style="background:#eaf4fb;border-left:4px solid #2980b9;border-radius:4px;padding:14px 18px;margin-bottom:20px;font-size:0.9em;",
      tags$p(style="margin:0;",
        "国内の保健所設置自治体（都道府県47・政令指定都市20・中核市62・保健所政令市5・東京23特別区、計157自治体）および国の公的機関、",
        "海外の感染症サーベイランス情報等発信公的機関のリンク集です。なお、保健所設置市の一部のサイトは注意喚起等情報の発信のみで、",
        "サーベイランスデータは都道府県等の感染症情報センターのサイトを参照するものもあります"),
      tags$p(style="margin:6px 0 0;color:#555;",
        icon("triangle-exclamation"),
        " URLはリンク切れ・ページ移転の可能性があります。2026年7月時点")
    ),

    # ── クイックジャンプ・検索（縦に長いページのためのナビゲーション）──
    tags$div(
      style="position:sticky;top:0;background:#fff;z-index:5;border:1px solid #e0e0e0;border-radius:6px;padding:10px 14px;margin-bottom:20px;box-shadow:0 1px 4px rgba(0,0,0,0.08);",
      tags$div(style="margin-bottom:8px;",
        tags$input(type="text", id="idsc-search", placeholder="自治体名・センター名で検索...",
                   style="width:100%;max-width:360px;padding:6px 10px;border:1px solid #ccc;border-radius:4px;font-size:0.9em;",
                   oninput="idscFilterTable(this.value)")
      ),
      region_nav,
      tags$div(style="margin-top:4px;",
        tags$a(href="#idsc-overseas", style="font-size:0.82em;color:#8e44ad;font-weight:bold;text-decoration:none;", "→ 海外セクションへ")
      )
    ),
    tags$script(HTML("
      function idscFilterTable(term) {
        term = term.toLowerCase();
        document.querySelectorAll('#idsc-table tbody tr').forEach(function(tr) {
          var show = term === '' || tr.textContent.toLowerCase().indexOf(term) !== -1;
          tr.style.display = show ? '' : 'none';
        });
      }
    ")),

    tags$h4(style="border-bottom:2px solid #2980b9;padding-bottom:4px;color:#2c3e50;margin-top:0;",
            icon("flag"), " 国内"),
    tags$div(style="margin-bottom:16px;",
      lapply(IDSC_NATIONAL, function(n) {
        tags$div(style="background:#fdf6e3;border-left:4px solid #e67e22;border-radius:4px;padding:10px 14px;margin-bottom:8px;",
          tags$a(href = n$url, target = "_blank", rel = "noopener noreferrer",
                 style="font-weight:bold;", n$name),
          tags$span(style="color:#777;font-size:0.85em;", paste0(" ― ", n$org))
        )
      })
    ),
    tags$table(id = "idsc-table", style="width:100%;border-collapse:collapse;font-size:0.92em;margin-bottom:30px;",
      tags$thead(
        tags$tr(style="border-bottom:2px solid #2c3e50;",
          tags$th(style="text-align:left;padding:6px 8px;", "自治体"),
          tags$th(style="text-align:left;padding:6px 8px;", "種別"),
          tags$th(style="text-align:left;padding:6px 8px;", "感染症情報センター等")
        )
      ),
      tags$tbody(rows)
    ),

    tags$h4(id = "idsc-overseas", style="border-bottom:2px solid #8e44ad;padding-bottom:4px;color:#2c3e50;scroll-margin-top:70px;",
            icon("globe"), " 海外"),
    tags$table(style="width:100%;border-collapse:collapse;font-size:0.92em;",
      tags$thead(
        tags$tr(style="border-bottom:2px solid #2c3e50;",
          tags$th(style="text-align:left;padding:6px 8px;", "地域"),
          tags$th(style="text-align:left;padding:6px 8px;", "機関"),
          tags$th(style="text-align:left;padding:6px 8px;", "サーベイランス情報ページ")
        )
      ),
      tags$tbody(overseas_rows)
    ),

    tags$a(href = "#idsc-top",
      style="position:fixed;bottom:24px;right:24px;background:#2980b9;color:#fff;width:42px;height:42px;",
      class="idsc-back-to-top",
      title="トップに戻る",
      tags$div(style="display:flex;align-items:center;justify-content:center;width:100%;height:100%;border-radius:50%;box-shadow:0 2px 6px rgba(0,0,0,0.3);",
        icon("arrow-up"))
    )
  )
}
