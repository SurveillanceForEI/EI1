# ============================================================
# 都道府県別「保健所管轄別（定点把握）データ」入手可能性 調査結果
# ------------------------------------------------------------
# 2026年8月、47都道府県の感染症情報センター等サイトを実地調査
# （PDF/CSV/Excel/専用システムをpdftools等で直接確認）した結果を
# 構造化データとして保持する。将来的な「都道府県・疾患選択→保健所
# 単位のMap・グラフ表示」機能のデータ取得層（パーサー）を実装する
# 際の設計図・優先順位付けに用いる。
#
# 注意：
# - `sample_url` は調査時点（2026年8月）で実際に確認した週報の
#   URLで、年週が変わればパスが変わるものが大半。`url_pattern` に
#   分かる範囲でプレースホルダ化したテンプレートを記載しているが、
#   未確認のものは NA とし、後日の実装時に個別解析が必要。
# - `archive_from` は「確認できた最も古い年」。"要確認" は、
#   アーカイブページの存在は確認したが最古年までは検証していない
#   ことを示す（実際はより古くまで遡れる可能性が高い）。
# - `rating`: "◎"=ほぼ全疾患×複数保健所の数値表が毎週入手可能、
#            "○"=上位疾患のみ／広域地区単位／グラフ形式のみ、
#            "△"=定性的文章のみ（今回の調査では最終的に該当なし）
# ============================================================

.hj <- function(pref, rating, format, hokenjo_n, hokenjo_names = character(0),
                sample_url, url_pattern = NA_character_,
                archive_from = NA_character_, notes = "") {
  list(pref = pref, rating = rating, format = format,
       hokenjo_n = hokenjo_n, hokenjo_names = hokenjo_names,
       sample_url = sample_url, url_pattern = url_pattern,
       archive_from = archive_from, notes = notes)
}

HOKENJO_DATA_SOURCES <- list(

  # ── 北海道・東北 ──────────────────────────────────────────
  .hj("北海道", "◎", "地図（インタラクティブ）", 31,
      sample_url = "https://www.iph.pref.hokkaido.jp/kansen/area.html",
      archive_from = "2020",
      notes = "保健所別ページで疾患別・定点別の発生状況を個別表示。週報PDFは別途 weekly2026-NNw.pdf 系"),

  .hj("青森県", "◎", "表（PDF）", 6,
      hokenjo_names = c("東津軽・青森市", "中南", "三戸・八戸市", "西北", "上北", "下北"),
      sample_url = "https://www.pref.aomori.lg.jp/soshiki/kenko/hoken/files/wr2025w13.pdf",
      url_pattern = "https://www.pref.aomori.lg.jp/soshiki/kenko/hoken/files/wr{YEAR}w{WEEK}.pdf",
      archive_from = "2004",
      notes = "全数把握疾患も保健所別記載。A型/B型別・年齢区分別表もあり"),

  .hj("岩手県", "◎", "地図（GIF画像・実数値付き）", 10,
      hokenjo_names = c("二戸", "久慈", "宮古", "釜石", "県央", "盛岡市", "中部", "大船渡", "奥州", "一関"),
      sample_url = "https://www2.pref.iwate.jp/~hp1353/kansen/image/imagemenu.html",
      archive_from = "2022",
      notes = "疾患別に色分け地図＋実数値（例: 手足口病 二戸3.50等）。15疾患。画像は image/imageNN/img-XXXX.gif"),

  .hj("宮城県", "○", "グラフ（棒グラフ、数値表なし）", 6,
      hokenjo_names = c("仙南", "塩釜", "大崎", "石巻", "気仙沼", "仙台"),
      sample_url = "https://www.pref.miyagi.jp/documents/1967/syuho202631w.pdf",
      archive_from = "2013",
      notes = "ARI・手足口病・ヘルパンギーナ・水痘の4疾患のみ管内集計区分別グラフ。数値表ではない"),

  .hj("秋田県", "◎", "表（PDF）", 9,
      hokenjo_names = c("秋田市", "大館", "北秋田", "能代", "秋田中央", "由利本荘", "大仙", "横手", "湯沢"),
      sample_url = "https://idsc.pref.akita.jp/kss/RAPIDS.pdf",
      archive_from = "2024（要再確認）",
      notes = "バックナンバー idsc.pref.akita.jp/kss/back.asp、RAPIDS_YYMM.pdf 形式"),

  .hj("山形県", "◎", "表（PDF）", 5,
      hokenjo_names = c("山形市", "村山", "最上", "置賜", "庄内"),
      sample_url = "https://www.eiken.yamagata.yamagata.jp/pdfshuho/2026/202631.pdf",
      url_pattern = "https://www.eiken.yamagata.yamagata.jp/pdfshuho/{YEAR}/{YEAR}{WEEK}.pdf",
      archive_from = "2016",
      notes = "17疾患前後、警報/注意報レベル表示付き。全数把握疾患も保健所別集計表あり"),

  .hj("福島県", "◎", "表（PDF）", 9,
      hokenjo_names = c("県北", "県中", "県南", "会津", "南会津", "相双", "いわき市", "福島市", "郡山市"),
      sample_url = "https://www.pref.fukushima.lg.jp/uploaded/attachment/759652.pdf",
      archive_from = "2025（週報PDFとしては短いが年報は2008年まで）",
      notes = ""),

  # ── 関東 ──────────────────────────────────────────────────
  .hj("茨城県", "◎", "表（PDF）", 9,
      hokenjo_names = c("中央", "日立", "潮来", "竜ケ崎", "土浦", "つくば", "筑西", "古河", "水戸市"),
      sample_url = "https://www.pref.ibaraki.jp/hokenfukushi/eiken/idwr/weekly/documents/2026idwr31.pdf",
      url_pattern = "https://www.pref.ibaraki.jp/hokenfukushi/eiken/idwr/weekly/documents/{YEAR}idwr{WEEK}.pdf",
      archive_from = "2008",
      notes = "全数・定点ともに保健所別。バックナンバーが特に長期。「ひたちなか」は定点医療機関数の表にのみ登場し、実患者報告数は日立に合算計上（境界データもひたちなか分を日立に統合済み）"),

  .hj("栃木県", "◎", "表（PDF）", 6,
      hokenjo_names = c("宇都宮市", "県西", "県東", "県南", "県北", "安足"),
      sample_url = "https://www.pref.tochigi.lg.jp/e60/tidc/documents/intidwr202631.pdf",
      archive_from = "2019",
      notes = "警報・注意報基準も併記"),

  .hj("群馬県", "◎", "表（PDF、地域区分）", 4,
      hokenjo_names = c("北毛（渋川・吾妻・利根沼田）", "西毛（高崎市・藤岡・富岡・安中）",
                         "中毛（前橋市・伊勢崎）", "東毛（館林・桐生・太田）"),
      sample_url = "https://www.pref.gunma.jp/uploaded/attachment/710182.pdf",
      archive_from = "2020",
      notes = "保健所単位ではなく4広域地域区分だが、ARI・小児科・眼科・基幹定点の全疾患を網羅。地域↔保健所対応表あり。境界データは12保健所を地域区分単位で結合（data/geo/hokenjo_boundaries/gunma.geojson）。実保健所12分割版は gunma_official12.geojson.bak に保存"),

  .hj("埼玉県", "◎", "表（PDF）", 10,
      sample_url = "https://www.pref.saitama.lg.jp/documents/277313/2026_31w.pdf",
      archive_from = "1999",
      notes = "1999〜2024年の全数・定点データがExcel形式でもアーカイブ提供"),

  .hj("千葉県", "◎", "表（PDF）", 15,
      sample_url = "https://www.pref.chiba.lg.jp/eiken/c-idsc/documents/wr2632-2.pdf",
      archive_from = "2012",
      notes = "年齢階級別・保健所別の詳細表、全5ページ構成"),

  .hj("東京都", "◎", "表（PDF）", 30,
      sample_url = "https://idsc.tmiph.metro.tokyo.lg.jp/assets/weekly/2026/31.pdf",
      url_pattern = "https://idsc.tmiph.metro.tokyo.lg.jp/assets/weekly/{YEAR}/{WEEK}.pdf",
      archive_from = "要確認",
      notes = "23区＋市部、独自システム「東京都感染症情報センター」"),

  .hj("神奈川県", "◎", "表（PDF）", NA,
      sample_url = "https://www.pref.kanagawa.jp/sys/eiken/003_center/0001_weekly/pdf/wrR08_31.pdf",
      archive_from = "2001",
      notes = "保健福祉事務所別。表1（国還元データ）・表2（速報）の2系統"),

  # ── 中部 ──────────────────────────────────────────────────
  .hj("新潟県", "○〜◎", "表（PDF）", 14,
      sample_url = "https://www.pref.niigata.lg.jp/uploaded/attachment/506688.pdf",
      archive_from = "要確認",
      notes = "地域振興局等管内別"),

  .hj("富山県", "◎", "表（PDF）", 5,
      hokenjo_names = c("砺波", "高岡", "富山市", "中部", "新川"),
      sample_url = "https://www.pref.toyama.jp/documents/32640/teiten_hc_2632w.pdf",
      archive_from = "要確認",
      notes = "厚生センター管内別、全18疾患超、全11ページ"),

  .hj("石川県", "◎", "表（PDF）", 5,
      hokenjo_names = c("金沢市", "南加賀", "石川中央", "能登中部", "能登北部"),
      sample_url = "https://www.pref.ishikawa.lg.jp/hokan/kansenjoho/stock/2026/documents/2026-32.pdf",
      url_pattern = "https://www.pref.ishikawa.lg.jp/hokan/kansenjoho/stock/{YEAR}/documents/{YEAR}-{WEEK}.pdf",
      archive_from = "要確認（stock/配下に年別アーカイブ）",
      notes = "19疾患＋ARI別途"),

  .hj("福井県", "◎", "表（PDF）", 7,
      hokenjo_names = c("福井市", "福井", "坂井", "奥越", "丹南", "二州", "若狭"),
      sample_url = "https://kansensyou-joho.pref.fukui.lg.jp/image18/ih23202631.pdf",
      url_pattern = "https://kansensyou-joho.pref.fukui.lg.jp/image18/ih23{YEAR}{WEEK}.pdf",
      archive_from = "2018年度（メニューページに記載）",
      notes = "旧URL info.pref.fukui.lg.jp は301リダイレクト"),

  .hj("山梨県", "◎", "表（PDF）", 5,
      hokenjo_names = c("中北", "峡東", "峡南", "富士東部", "甲府市"),
      sample_url = "https://www.pref.yamanashi.jp/documents/101494/202632w.pdf",
      archive_from = "要確認",
      notes = ""),

  .hj("長野県", "◎", "表（PDF）", 12,
      hokenjo_names = c("佐久", "上田", "諏訪", "伊那", "飯田", "木曽", "松本", "大町",
                         "長野", "北信", "長野市", "松本市"),
      sample_url = "https://www.pref.nagano.lg.jp/shippei-kansen/kenko/kenko/kansensho/joho/documents/2026-32w_data_07m.pdf",
      archive_from = "2023",
      notes = "週報・月報合併号、年齢階級別・保健所別定点数表も付属。最も詳細度が高い県の一つ"),

  .hj("岐阜県", "◎", "表（PDF）", 8,
      hokenjo_names = c("岐阜市", "岐阜", "西濃", "関", "可茂", "東濃", "恵那", "飛騨"),
      sample_url = "https://www.pref.gifu.lg.jp/uploaded/attachment/510301.pdf",
      archive_from = "要確認（GIDWRアーカイブページあり）",
      notes = ""),

  # ── 東海・近畿 ────────────────────────────────────────────
  .hj("静岡県", "◎", "表（PDF）", 9,
      hokenjo_names = c("賀茂", "熱海", "東部", "御殿場", "富士", "静岡市", "中部", "西部", "浜松市"),
      sample_url = "https://www.pref.shizuoka.jp/_res/projects/default_project/_page_/001/081/723/2026idwr31-2.pdf",
      archive_from = "2019",
      notes = "月報版の保健所別集計表も別途あり"),

  .hj("愛知県", "◎", "表（PDF）", NA,
      sample_url = "https://www.pref.aichi.jp/eiseiken/kansen/2026/202632.pdf",
      url_pattern = "https://www.pref.aichi.jp/eiseiken/kansen/{YEAR}/{YEAR}{WEEK}.pdf",
      archive_from = "要確認",
      notes = "名古屋市含む保健所別、年齢別表もあり"),

  .hj("三重県", "◎", "表（PDF）", 9,
      hokenjo_names = c("桑名", "四日市市", "鈴鹿", "津", "松阪", "伊勢", "伊賀", "尾鷲", "熊野"),
      sample_url = "https://www.kenkou.pref.mie.jp/M_products/WR_New.pdf",
      archive_from = "要確認",
      notes = "「定点把握感染症の保健所別3週間の推移」ページも別途あり"),

  .hj("滋賀県", "◎", "地図＋表（PDF）", 7,
      hokenjo_names = c("大津市", "草津", "甲賀", "東近江", "彦根", "長浜", "高島"),
      sample_url = "https://www.pref.shiga.lg.jp/file/attachment/5628767.pdf",
      archive_from = "要確認",
      notes = "手足口病等の圏域別マップ（色分け）＋全数・定点の累積患者数表（人口10万人当たり換算あり）。Map機能の参考実装として最優良"),

  .hj("京都府", "◎", "CSV（機械可読・構造化データ）", 12,
      hokenjo_names = c("北・左京", "上京・中京・下京", "東山・山科", "南・伏見", "右京・西京",
                         "乙訓", "山城北", "山城南", "南丹", "中丹西", "中丹東", "丹後"),
      sample_url = "https://www.pref.kyoto.jp/idsc/data/week/area-table/2026/documents/202631_2-2-2.csv",
      url_pattern = "https://www.pref.kyoto.jp/idsc/data/week/area-table/{YEAR}/documents/{YEAR}{WEEK}_2-2-2.csv",
      archive_from = "2018",
      notes = "12地域×全疾患×報告数/定点当たり数の両方をCSVで毎週提供。実装に最も扱いやすい形式。Shift-JISエンコード"),

  .hj("大阪府", "◎", "表（HTML）", 11,
      hokenjo_names = c("豊能", "三島", "北河内", "中河内", "南河内", "堺市", "泉州",
                         "大阪市北部", "大阪市西部", "大阪市東部", "大阪市南部"),
      sample_url = "https://www.iph.pref.osaka.jp/infection/surv26/surv31t.html",
      url_pattern = "https://www.iph.pref.osaka.jp/infection/surv{YEAR2}/surv{WEEK}t.html",
      archive_from = "2011",
      notes = "8ブロック+大阪市4区、報告数・定点当たり報告数の両方をHTML表で提供。年齢別報告数表も同ページ内。境界データは大阪府保健医療計画の二次医療圏定義＋大阪市4基本保健医療圏（区単位）で再構築（data/geo/hokenjo_boundaries/osaka.geojson）。厚労省ベースの実保健所18分割版は osaka_official18.geojson.bak に保存"),

  .hj("兵庫県", "◎", "Excel（機械可読・オープンデータ）", NA,
      sample_url = "https://web.pref.hyogo.lg.jp/iphs01/kansensho_jyoho/download/documents/weekly_2026-31w_t3201-t3203.xlsx",
      url_pattern = "https://web.pref.hyogo.lg.jp/iphs01/kansensho_jyoho/download/documents/weekly_{YEAR}-{WEEK}w_t3201-t3203.xlsx",
      archive_from = "2015（オープンデータExcel、〜2024年分）",
      notes = "神戸市・尼崎市・姫路市・西宮市等、全保健所×全定点疾患(20疾患)を機械可読Excelで提供。シート:保健所別/年齢階級別/週別。2015〜2024年オープンデータあり"),

  .hj("奈良県", "◎", "表（PDF）", 6,
      hokenjo_names = c("奈良市", "郡山", "中和（東）", "中和（西）", "吉野", "県全体"),
      sample_url = "https://www.pref.nara.lg.jp/documents/4352/0831.pdf",
      archive_from = "要確認",
      notes = "北部/中部/南部の3ブロックの中に6細分。全数把握も保健所別内訳あり"),

  .hj("和歌山県", "◎", "表（PDF）", 9,
      hokenjo_names = c("和歌山市", "海南", "岩出", "橋本", "湯浅", "御坊", "田辺", "新宮", "串本"),
      sample_url = "https://www.pref.wakayama.lg.jp/prefg/031801/idsw/khdc/d00153694_d/fil/WIDR202631.pdf",
      archive_from = "要確認",
      notes = "「保健所別の患者報告数」表（p.11付近）に全19疾患。警報・注意報レベル状況の週次推移表も別途あり"),

  # ── 中国・四国 ────────────────────────────────────────────
  .hj("鳥取県", "○", "表（PDF、地域区分）", 3,
      hokenjo_names = c("東部", "中部", "西部"),
      sample_url = "https://www.pref.tottori.lg.jp/secure/519458/R8W32_hp.pdf",
      archive_from = "要確認",
      notes = "ユーザー確認済み：地域区分（3地区）で妥当とのこと。全国比較値も掲載"),

  .hj("島根県", "◎", "専用システム（DIDSS）", NA,
      sample_url = "https://pref.shimane.didss.dsvc.jp/files/report/week/weeklyreport_y2026w32.pdf",
      archive_from = "2006",
      notes = "保健所別集計表＋分布マップ。山口県と同系統のシステム"),

  .hj("岡山県", "◎", "表（PDF、マップ連動）", 7,
      hokenjo_names = c("岡山市", "倉敷市", "備前", "備中", "備北", "真庭", "美作"),
      sample_url = "https://www.pref.okayama.jp/uploaded/life/1050510_10173723_misc.pdf",
      archive_from = "要確認",
      notes = "「岡山県地区別感染症マップ」用のレベル判定（赤=レベル3/黄=レベル2）色分け表あり。全15ページ構成"),

  .hj("広島県", "◎", "表（PDF）", 7,
      hokenjo_names = c("西部", "西部東", "東部", "北部", "広島市", "呉市", "福山市"),
      sample_url = "https://www.pref.hiroshima.lg.jp/uploaded/attachment/678620.pdf",
      archive_from = "2001",
      notes = "5類定点20疾患＋全数把握疾患すべてに地区別数値。最長の遡及可能性を確認できた県の一つ"),

  .hj("山口県", "◎", "専用システム（DIDSS）", 9,
      hokenjo_names = c("下関", "岩国", "柳井", "周南", "防府", "山口", "宇部", "長門", "萩"),
      sample_url = "https://pref.yamaguchi.didss.dsvc.jp/files/report/week/weeklyreport_y2026w32.pdf",
      archive_from = "2006",
      notes = "PDF/CSVダウンロード可、島根県と同系統システム"),

  .hj("徳島県", "◎", "表（PDF）", 6,
      hokenjo_names = c("徳島", "阿南", "美波", "吉野川", "美馬", "三好"),
      sample_url = "https://www.pref.tokushima.lg.jp/file/attachment/1070872.pdf",
      archive_from = "要確認",
      notes = "ARI・COVID等について保健所別数値表、上位3位の小児科定点疾患も同様"),

  .hj("香川県", "◎", "表（PDF）", 5,
      hokenjo_names = c("高松市", "小豆", "東讃", "中讃", "西讃"),
      sample_url = "https://www.pref.kagawa.lg.jp/documents/7135/2026syuuhou31.pdf",
      archive_from = "要確認（過去10年分の列が週報内にあり）",
      notes = "地区別定点数の内訳（小児科/ARI/眼科/基幹）も明記、警報・注意報地区の色分けあり"),

  .hj("愛媛県", "◎", "表（PDF）", 7,
      hokenjo_names = c("四国中央", "西条", "今治", "松山市", "中予", "八幡浜", "宇和島"),
      sample_url = "https://www.pref.ehime.jp/uploaded/attachment/187603.pdf",
      archive_from = "要確認",
      notes = "患者報告数・定点当たり報告数の両方を保健所別表で提供。年齢階級別内訳表も同ページ"),

  .hj("高知県", "◎", "表（PDF）", 6,
      hokenjo_names = c("安芸", "中央東", "高知市", "中央西", "須崎", "幡多"),
      sample_url = "https://www.pref.kochi.lg.jp/doc/2026011400132/file_contents/file_2026864114034_1.pdf",
      archive_from = "要確認",
      notes = "p.5「疾病別・地域別報告数」にARI・小児科・眼科・基幹定点の全19疾患の保健所別表"),

  # ── 九州・沖縄 ────────────────────────────────────────────
  .hj("福岡県", "◎", "表（HTML、保健所別ページ）", 12,
      hokenjo_names = c("北九州市", "福岡市", "久留米市", "筑紫", "粕屋", "糸島", "宗像・遠賀",
                         "嘉穂・鞍手", "田川", "北筑後", "南筑後", "京築"),
      sample_url = "https://www.fihes.pref.fukuoka.jp/~idsc_fukuoka/idwr/table_t01/01.html",
      url_pattern = "https://www.fihes.pref.fukuoka.jp/~idsc_fukuoka/idwr/table_t01/{NN}.html",
      archive_from = "要確認",
      notes = "保健所ごとに専用の週次数値表ページ（過去5週分×全定点疾患）。プルダウンで選択（idwr-f2.html）"),

  .hj("佐賀県", "◎", "表（PDF、API経由生成）", 5,
      hokenjo_names = c("佐賀中部", "鳥栖", "唐津", "伊万里", "杵藤"),
      sample_url = "https://kansen.pref.saga.jp/api/report/openPdf?schemaname=public&yw=202631",
      url_pattern = "https://kansen.pref.saga.jp/api/report/openPdf?schemaname=public&yw={YEAR}{WEEK}",
      archive_from = "2010",
      notes = "p.3「定点報告：五類感染症」に5保健福祉事務所別×全19疾患。年週一覧は https://kansen.pref.saga.jp/api/report で機械的に取得可能（JSON）"),

  .hj("長崎県", "◎", "表（PDF）", 10,
      hokenjo_names = c("佐世保市", "長崎市", "壱岐", "西彼", "県央", "県南", "県北", "五島", "上五島", "対馬"),
      sample_url = "https://www.pref.nagasaki.jp/fs/3/3/4/7/0/_/2026__31__7_27___8_2______.pdf",
      archive_from = "要確認",
      notes = "p.4「疾病別・保健所管内別発生状況」に全19疾患、警報/注意報レベルの色分け付き"),

  .hj("熊本県", "◎", "表（PDF）", 11,
      hokenjo_names = c("熊本市", "山鹿", "菊池", "阿蘇", "御船", "八代", "水俣", "人吉",
                         "有明", "宇城", "天草"),
      sample_url = "https://www.pref.kumamoto.jp/uploaded/attachment/316243.pdf",
      archive_from = "要確認",
      notes = "p.6「保健所別発生状況」に全20疾患、年齢別発生状況表も別ページ"),

  .hj("大分県", "◎", "表（PDF、男女別）", NA,
      sample_url = "https://www.pref.oita.jp/uploaded/attachment/2274543.pdf",
      archive_from = "要確認",
      notes = "全9ページ構成、疾病・保健所別報告数を男女別・総数で提供"),

  .hj("宮崎県", "◎", "表（PDF）", 9,
      hokenjo_names = c("宮崎市", "都城", "延岡", "日南", "小林", "高鍋", "高千穂", "日向", "中央"),
      sample_url = "https://www.pref.miyazaki.lg.jp/contents/org/fukushi/eikanken/center/infectious/pdf/202631.pdf",
      url_pattern = "https://www.pref.miyazaki.lg.jp/contents/org/fukushi/eikanken/center/infectious/pdf/{YEAR}{WEEK}.pdf",
      archive_from = "要確認",
      notes = "最終ページ(p.5)に9保健所×全16疾患の完全な数値表。警報基準値超過保健所の一覧表も別途あり"),

  .hj("鹿児島県", "◎", "表（PDF）", 15,
      hokenjo_names = c("鹿児島市", "指宿", "加世田", "伊集院", "川薩", "出水", "大口", "姶良",
                         "志布志", "鹿屋", "西之表", "屋久島", "名瀬", "徳之島"),
      sample_url = "https://www.pref.kagoshima.jp/ae06/kenko-fukushi/kenko-iryo/kansen/hasseidoko/week/documents/125377_20260806134914-1.pdf",
      archive_from = "要確認",
      notes = "p.5「疾病別保健所別患者報告数及び定点当たり報告数」に全20疾患。旧ドメイン kagoshima-pref.jp は不可、pref.kagoshima.jp を使用"),

  .hj("沖縄県", "○", "グラフ（折れ線、PDF）", 6,
      hokenjo_names = c("北部", "中部", "那覇市", "南部", "宮古", "八重山"),
      sample_url = "https://www.pref.okinawa.lg.jp/_res/projects/default_project/_page_/001/006/484/syuuho0831.pdf",
      archive_from = "2013（年報アーカイブ）",
      notes = "週報(全38ページ)の各疾患ページに保健所別グラフ。数値表ではなくグラフ形式のため読み取り精度に限界あり。年報は2013〜2024年分あり")
)

# ============================================================
# ヘルパー関数
# ============================================================

# 判定（◎/○/△）でフィルタ
hokenjo_sources_by_rating <- function(rating) {
  Filter(function(x) x$rating == rating, HOKENJO_DATA_SOURCES)
}

# 都道府県名から1件取得
hokenjo_source_for_pref <- function(pref) {
  Find(function(x) x$pref == pref, HOKENJO_DATA_SOURCES)
}

# ============================================================
# 「掲載ページのURLは固定だが、最新週のPDF添付ファイルのURLは
# 毎週変わる」都道府県が多いことが判明したため、掲載ページを毎回
# スクレイピングして最新週のPDFリンクを自動解決する汎用リゾルバ。
#
# 従来はHOKENJO_DATA_SOURCESのsample_urlやHOKENJO_REFRESH_DISPATCH
# （scripts/refresh_hokenjo_data.R）に特定週のURLを直書きしており、
# 該当週を過ぎると自動更新が止まってしまっていた（宮城県・千葉県・
# 群馬県で実際に発覚）。
#
# pick="latest_week": リンクテキストから週番号（最初に現れる整数）を
#   抽出し、最大のものを採用する（宮城県・千葉県のように、過去の
#   全週分がアーカイブとして1ページに並ぶ場合。掲載順が新しい順/古い
#   順のどちらでも週番号自体で判定するため安全）。
# pick="first": リンクテキストパターンに最初にマッチしたものを採用
#   する（群馬県のように、ページ自体が最新週のみを毎週差し替えて
#   掲載する場合。週番号がリンクテキストに含まれない場合はこちら）。
# ============================================================

resolve_hokenjo_pdf_url <- function(landing_url, link_text_pattern, pick = c("latest_week", "first", "latest_week_href"),
                                     href_must_contain = NA_character_, file_ext = "pdf") {
  pick <- match.arg(pick)
  if (!requireNamespace("rvest", quietly = TRUE)) stop("rvest パッケージが必要です")
  doc <- rvest::read_html(landing_url)
  links <- rvest::html_elements(doc, "a")
  hrefs <- rvest::html_attr(links, "href")
  # 広島県のように全角数字（「第３２週」）で週番号を表記するページがあるため、
  # マッチング・週番号抽出の前に全角数字を半角に正規化しておく
  texts <- chartr("０１２３４５６７８９", "0123456789", rvest::html_text(links))
  is_target_ext <- grepl(paste0("\\.", file_ext, "$"), hrefs, ignore.case = TRUE)
  matched_idx <- which(is_target_ext & !is.na(hrefs) & grepl(link_text_pattern, texts))
  # 一覧ページが複数年分のアーカイブを1ページに掲載している場合、
  # 週番号だけでは古い年の「第52週」等を誤って最新扱いしてしまう
  # ことがある（宮城県で実際に発覚）。href_must_containでファイル名
  # 側に対象年が含まれることを要求し、年をまたいだ誤選択を防ぐ
  if (!is.na(href_must_contain)) {
    matched_idx <- matched_idx[grepl(href_must_contain, hrefs[matched_idx])]
  }
  if (length(matched_idx) == 0) {
    stop(sprintf("resolve_hokenjo_pdf_url: '%s' にマッチする%sリンクが見つかりません（%s）",
                 link_text_pattern, file_ext, landing_url))
  }
  best <- if (pick == "latest_week") {
    wk_txt <- regmatches(texts[matched_idx], regexpr("[0-9]+", texts[matched_idx]))
    wk <- suppressWarnings(as.integer(wk_txt))
    matched_idx[which.max(wk)]
  } else if (pick == "latest_week_href") {
    # リンクテキストに週番号が含まれない場合（例: 富山県のZIPリンクは
    # 「（ZIP：9KB）」等のファイルサイズ表記のみ）、ファイル名側から
    # 「年+週」の数字列を拾って比較する（例: teiten_hc_202633w.zip → 202633）
    wk_txt <- regmatches(hrefs[matched_idx], regexpr("[0-9]{6}", hrefs[matched_idx]))
    wk <- suppressWarnings(as.integer(wk_txt))
    matched_idx[which.max(wk)]
  } else {
    matched_idx[1]
  }
  xml2::url_absolute(hrefs[best], landing_url)
}

# 都道府県ごとの「掲載ページURL + リンクテキストの正規表現パターン
# （+ 選択方法）」対応表。resolve_hokenjo_pdf_url_for_pref()で使う
HOKENJO_LANDING_PAGES <- list(
  "群馬県" = list(url = "https://www.pref.gunma.jp/page/3304.html",
                pattern = "地域別疾病報告状況", pick = "first"),
  "宮城県" = list(url = "https://www.pref.miyagi.jp/site/hokans/surveypdf-shuho.html",
                pattern = "第[0-9]+週", pick = "latest_week", href_must_contain = "2026"),
  "千葉県" = list(url = "https://www.pref.chiba.lg.jp/eiken/c-idsc/wr2026.html",
                pattern = "^[0-9]+週", pick = "latest_week"),
  "富山県" = list(url = "https://www.pref.toyama.jp/1279/kansen/#c-1",
                pattern = "厚生センター（保健所）管内別", pick = "latest_week_href",
                href_must_contain = "2026", file_ext = "zip"),
  "広島県" = list(url = "https://www.pref.hiroshima.lg.jp/site/hcdc/hidsc-kanzya-zyouhou-syuukaiseki.html",
                # バックナンバー一覧の先頭に出る「令和８年第３２週」のような
                # 令和年号付きの表記が最新号で、それ以降は「第１週」等の
                # 過去アーカイブが年号無しで並ぶ。年号付きパターンに限定する
                # ことで、IDがファイル名に年を含まない（連番CMS ID）この
                # サイトでも確実に最新号だけを拾える
                pattern = "令和[0-9]+年第[0-9]+週", pick = "first")
)

resolve_hokenjo_pdf_url_for_pref <- function(pref) {
  cfg <- HOKENJO_LANDING_PAGES[[pref]]
  if (is.null(cfg)) stop(sprintf("%s: HOKENJO_LANDING_PAGESに未登録です", pref))
  href_must_contain <- if (is.null(cfg$href_must_contain)) NA_character_ else cfg$href_must_contain
  file_ext <- if (is.null(cfg$file_ext)) "pdf" else cfg$file_ext
  resolve_hokenjo_pdf_url(cfg$url, cfg$pattern, pick = cfg$pick,
                           href_must_contain = href_must_contain, file_ext = file_ext)
}

# ============================================================
# 「掲載ページのスクレイピングでは解決できないが、URL自体はyear/week
# を埋め込むテンプレート（url_pattern）で機械的に組み立てられる」
# 都道府県（京都府・兵庫県・佐賀県・山形県等）向けのヘルパー。
#
# 今日の日付からISO週（月曜始まり）の年・週番号を計算する。
# ただし実際の公表は数日〜1週間程度遅れることが多いため、
# probe_latest_week_fetch()と組み合わせ、計算した週から必要なら
# 1週ずつ遡って実際に取得できる週を探すのが基本の使い方。
# ============================================================

current_iso_year_week <- function(d = Sys.Date()) {
  monday_of_week1 <- function(y) {
    b <- as.Date(sprintf("%d-01-04", y))
    b - (as.integer(format(b, "%u")) - 1)
  }
  yr <- as.integer(format(d, "%Y"))
  if (d < monday_of_week1(yr)) yr <- yr - 1L
  if (d >= monday_of_week1(yr + 1L)) yr <- yr + 1L
  wk <- as.integer(floor(as.numeric(d - monday_of_week1(yr)) / 7)) + 1L
  list(year = yr, week = wk)
}

# fetch_week_fn(year, week) を、計算上の最新週から必要なら最大max_back
# 週分だけ遡りながら試し、最初に成功（エラー無し・1行以上）した結果を
# 返す。実際の公表ラグ（速報の反映が数日〜1週間遅れる）に対応するため。
probe_latest_week_fetch <- function(fetch_week_fn, max_back = 3, start_year_week = NULL) {
  yw <- if (is.null(start_year_week)) current_iso_year_week() else start_year_week
  last_err <- NULL
  for (back in 0:max_back) {
    wk <- yw$week - back
    yr <- yw$year
    if (wk < 1) { yr <- yr - 1L; wk <- wk + 52L }
    res <- tryCatch(fetch_week_fn(yr, wk), error = function(e) { last_err <<- e; NULL })
    if (!is.null(res) && nrow(res) > 0) return(res)
  }
  if (!is.null(last_err)) stop(last_err)
  stop("probe_latest_week_fetch: 直近", max_back + 1, "週分とも取得できませんでした")
}

# data.frame化（実装時の一覧確認・CSV出力用）
hokenjo_sources_df <- function() {
  do.call(rbind, lapply(HOKENJO_DATA_SOURCES, function(x) {
    data.frame(
      pref = x$pref,
      rating = x$rating,
      format = x$format,
      hokenjo_n = ifelse(is.null(x$hokenjo_n) || is.na(x$hokenjo_n), NA, x$hokenjo_n),
      hokenjo_names = paste(x$hokenjo_names, collapse = "、"),
      sample_url = x$sample_url,
      url_pattern = ifelse(is.na(x$url_pattern), "", x$url_pattern),
      archive_from = x$archive_from,
      notes = x$notes,
      stringsAsFactors = FALSE
    )
  }))
}
