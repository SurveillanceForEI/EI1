# EBS ルールベース自動スクリーニング
# Claude APIキー不要・完全オフライン動作
#
# 使い方（単体テスト）:
#   source("ebs_rule_screening.R")
#   result <- screen_entry(title = "Cholera – Benin", summary = "...")
#
# Shinyへの組み込み:
#   source("ebs_rule_screening.R")
#   result <- screen_entry(title = input$title, summary = input$summary)

# ============================================================
# 疾患データベース（疾患名・分類リストシートより）
# ============================================================

DISEASE_DB <- list(
  # name_patterns: タイトル/概要にマッチさせるキーワード（英語・日本語）
  # category: 疾患分類
  # name_en / name_ja: 標準名称

  list(patterns = c("smallpox","variola","痘そう","天然痘"),
       category = "Bioterrorism agents", name_en = "Smallpox", name_ja = "痘そう"),
  list(patterns = c("ebola","エボラ"),
       category = "Viral haemorrhagic fever", name_en = "Ebola", name_ja = "エボラ出血熱"),
  list(patterns = c("crimean.congo","cchf","クリミア","コンゴ出血熱"),
       category = "Viral haemorrhagic fever", name_en = "Crimean-Congo hemorrhagic fever", name_ja = "クリミア・コンゴ出血熱"),
  list(patterns = c("marburg","マールブルグ"),
       category = "Viral haemorrhagic fever", name_en = "Marburg disease", name_ja = "マールブルグ病"),
  list(patterns = c("lassa","ラッサ"),
       category = "Viral haemorrhagic fever", name_en = "Lassa fever", name_ja = "ラッサ熱"),
  list(patterns = c("\\bplague\\b","ペスト"),
       category = "Zoonotic diseases", name_en = "Plague", name_ja = "ペスト"),
  list(patterns = c("tuberculosis","結核","抗酸菌","結核菌"),
       category = "Respiratory diseases", name_en = "Tuberculosis", name_ja = "結核"),
  list(patterns = c("\\bsars\\b","重症急性呼吸器症候群"),
       category = "Respiratory diseases", name_en = "SARS", name_ja = "SARS"),
  list(patterns = c("\\bmers\\b","中東呼吸器症候群"),
       category = "Zoonotic diseases", name_en = "MERS", name_ja = "MERS"),
  list(patterns = c("polio","ポリオ","急性灰白髄炎"),
       category = "Vaccine-preventable diseases", name_en = "Poliomyelitis", name_ja = "ポリオ"),
  list(patterns = c("diphtheria","ジフテリア"),
       category = "Vaccine-preventable diseases", name_en = "Diphtheria", name_ja = "ジフテリア"),
  list(patterns = c("avian.?flu","bird.?flu","avian influenza","h5n1","h7n9","h5n2","h5n6","h5n8","鳥インフル","鳥flu","禽流感"),
       category = "Zoonotic diseases", name_en = "Avian influenza", name_ja = "鳥インフルエンザ"),
  list(patterns = c("swine.?flu","豚インフル","h1n1"),
       category = "Zoonotic diseases", name_en = "Swine influenza", name_ja = "豚インフルエンザ"),
  list(patterns = c("\\bcholera\\b","コレラ"),
       category = "Food-and-water-borne diseases", name_en = "Cholera", name_ja = "コレラ"),
  list(patterns = c("shigella","shigellosis","細菌性赤痢"),
       category = "Food-and-water-borne diseases", name_en = "Shigellosis", name_ja = "細菌性赤痢"),
  list(patterns = c("\\behec\\b","e\\.coli o157","腸管出血性大腸菌","o157"),
       category = "Food-and-water-borne diseases", name_en = "EHEC", name_ja = "腸管出血性大腸菌感染症"),
  list(patterns = c("typhoid","腸チフス"),
       category = "Food-and-water-borne diseases", name_en = "Typhoid fever", name_ja = "腸チフス"),
  list(patterns = c("\\bmpox\\b","monkeypox","サル痘","エムポックス"),
       category = "Sexually transmitted diseases", name_en = "Mpox", name_ja = "エムポックス"),
  list(patterns = c("\\banthrax\\b","炭疽"),
       category = "Bioterrorism agents", name_en = "Anthrax", name_ja = "炭疽"),
  list(patterns = c("botulism","ボツリヌス"),
       category = "Bioterrorism agents", name_en = "Botulism", name_ja = "ボツリヌス症"),
  list(patterns = c("hepatitis.?e","E型肝炎"),
       category = "Food-and-water-borne diseases", name_en = "Hepatitis E", name_ja = "E型肝炎"),
  list(patterns = c("hepatitis.?a","A型肝炎"),
       category = "Food-and-water-borne diseases", name_en = "Hepatitis A", name_ja = "A型肝炎"),
  list(patterns = c("yellow.?fever","黄熱"),
       category = "Mosquito-borne diseases", name_en = "Yellow fever", name_ja = "黄熱"),
  list(patterns = c("legionella","legionnaires","レジオネラ"),
       category = "Food-and-water-borne diseases", name_en = "Legionellosis", name_ja = "レジオネラ症"),
  list(patterns = c("west.?nile","ウエストナイル"),
       category = "Mosquito-borne diseases", name_en = "West Nile fever", name_ja = "ウエストナイル熱"),
  list(patterns = c("\\bzika\\b","ジカ"),
       category = "Mosquito-borne diseases", name_en = "Zika virus infection", name_ja = "ジカウイルス感染症"),
  list(patterns = c("chikungunya","チクングニア"),
       category = "Mosquito-borne diseases", name_en = "Chikungunya fever", name_ja = "チクングニア熱"),
  list(patterns = c("\\bdengue\\b","デング"),
       category = "Mosquito-borne diseases", name_en = "Dengue", name_ja = "デング熱"),
  list(patterns = c("\\bmalaria\\b","マラリア"),
       category = "Mosquito-borne diseases", name_en = "Malaria", name_ja = "マラリア"),
  list(patterns = c("rift.?valley","リフトバレー"),
       category = "Mosquito-borne diseases", name_en = "Rift Valley fever", name_ja = "リフトバレー熱"),
  list(patterns = c("\\bsfts\\b","重症熱性血小板減少症候群"),
       category = "Tick-borne diseases", name_en = "SFTS", name_ja = "重症熱性血小板減少症候群"),
  list(patterns = c("tick.?borne encephalitis","ダニ脳炎"),
       category = "Tick-borne diseases", name_en = "Tick-borne encephalitis", name_ja = "ダニ媒介性脳炎"),
  list(patterns = c("\\bq.?fever\\b","Qフィーバー","コクシエラ"),
       category = "Tick-borne diseases", name_en = "Q fever", name_ja = "Q熱"),
  list(patterns = c("\\brabies\\b","狂犬病"),
       category = "Zoonotic diseases", name_en = "Rabies", name_ja = "狂犬病"),
  list(patterns = c("nipah","ニパ"),
       category = "Zoonotic diseases", name_en = "Nipah virus infection", name_ja = "ニパウイルス感染症"),
  list(patterns = c("brucellosis","ブルセラ"),
       category = "Zoonotic diseases", name_en = "Brucellosis", name_ja = "ブルセラ症"),
  list(patterns = c("leptospirosis","レプトスピラ"),
       category = "Zoonotic diseases", name_en = "Leptospirosis", name_ja = "レプトスピラ症"),
  list(patterns = c("tularemia","野兎病"),
       category = "Bioterrorism agents", name_en = "Tularemia", name_ja = "野兎病"),
  list(patterns = c("melioidosis","類鼻疽"),
       category = "Bioterrorism agents", name_en = "Melioidosis", name_ja = "類鼻疽"),
  list(patterns = c("glanders","鼻疽"),
       category = "Bioterrorism agents", name_en = "Glanders", name_ja = "鼻疽"),
  list(patterns = c("\\bcre\\b","carbapenem.resistant","カルバペネム耐性"),
       category = "Antimicrobial resistant diseases", name_en = "CRE", name_ja = "カルバペネム耐性腸内細菌"),
  list(patterns = c("\\bmrsa\\b","メチシリン耐性黄色ブドウ球菌"),
       category = "Antimicrobial resistant diseases", name_en = "MRSA infection", name_ja = "MRSA感染症"),
  list(patterns = c("hiv","aids","エイズ"),
       category = "Sexually transmitted diseases", name_en = "HIV/AIDS", name_ja = "HIV/AIDS"),
  list(patterns = c("\\bsyphilis\\b","梅毒"),
       category = "Sexually transmitted diseases", name_en = "Syphilis", name_ja = "梅毒"),
  list(patterns = c("\\bmeasles\\b","麻疹","はしか"),
       category = "Vaccine-preventable diseases", name_en = "Measles", name_ja = "麻疹"),
  list(patterns = c("\\brubella\\b","風疹"),
       category = "Vaccine-preventable diseases", name_en = "Rubella", name_ja = "風疹"),
  list(patterns = c("\\bpertussis\\b","whooping.?cough","百日咳"),
       category = "Vaccine-preventable diseases", name_en = "Pertussis", name_ja = "百日咳"),
  list(patterns = c("\\btetanus\\b","破傷風"),
       category = "Vaccine-preventable diseases", name_en = "Tetanus", name_ja = "破傷風"),
  list(patterns = c("\\binfluenza\\b","インフルエンザ"),
       category = "Respiratory diseases", name_en = "Influenza", name_ja = "インフルエンザ"),
  list(patterns = c("covid","sars.?cov","coronavirus","新型コロナ","コロナウイルス"),
       category = "Respiratory diseases", name_en = "COVID-19", name_ja = "COVID-19"),
  list(patterns = c("\\bstss\\b","劇症型溶連菌","streptococcal toxic shock"),
       category = "Respiratory diseases", name_en = "STSS", name_ja = "劇症型溶連菌感染症"),
  list(patterns = c("\\bimd\\b","invasive meningococcal","髄膜炎菌"),
       category = "Respiratory diseases", name_en = "IMD", name_ja = "侵襲性髄膜炎菌感染症"),
  list(patterns = c("\\bnorovirus\\b","ノロウイルス"),
       category = "Food-and-water-borne diseases", name_en = "Norovirus", name_ja = "ノロウイルス感染症"),
  list(patterns = c("earthquake","地震","被災"),
       category = "Disaster", name_en = "Earthquake", name_ja = "地震"),
  list(patterns = c("\\bflooding\\b","flood","洪水","浸水"),
       category = "Disaster", name_en = "Flooding", name_ja = "洪水"),
  list(patterns = c("typhoon","台風","hurricane","cyclone"),
       category = "Disaster", name_en = "Typhoon/Hurricane", name_ja = "台風・ハリケーン"),
  list(patterns = c("unknown","undiagnosed","不明","原因不明","未診断"),
       category = "Undiagnosed", name_en = "Undiagnosed", name_ja = "原因不明疾患")
)

# ============================================================
# 国・地域データベース（国・地域リストシートより抜粋）
# ============================================================

COUNTRY_DB <- list(
  # Asia
  list(en = c("Afghanistan"), ja = c("アフガニスタン"), region = "アジア (Asia)"),
  list(en = c("Bangladesh"), ja = c("バングラデシュ"), region = "アジア (Asia)"),
  list(en = c("China","Chinese"), ja = c("中国","中華人民共和国","中国大陸"), region = "アジア (Asia)"),
  list(en = c("Hong Kong"), ja = c("香港"), region = "アジア (Asia)"),
  list(en = c("India","Indian"), ja = c("インド"), region = "アジア (Asia)"),
  list(en = c("Indonesia","Indonesian"), ja = c("インドネシア"), region = "アジア (Asia)"),
  list(en = c("Cambodia"), ja = c("カンボジア","カンボディア"), region = "アジア (Asia)"),
  list(en = c("Korea","Republic of Korea","South Korea"), ja = c("韓国","大韓民国"), region = "アジア (Asia)"),
  list(en = c("Laos","Lao"), ja = c("ラオス"), region = "アジア (Asia)"),
  list(en = c("Malaysia"), ja = c("マレーシア"), region = "アジア (Asia)"),
  list(en = c("Myanmar","Burma"), ja = c("ミャンマー"), region = "アジア (Asia)"),
  list(en = c("Nepal"), ja = c("ネパール"), region = "アジア (Asia)"),
  list(en = c("Pakistan"), ja = c("パキスタン"), region = "アジア (Asia)"),
  list(en = c("Philippines","Filipino"), ja = c("フィリピン"), region = "アジア (Asia)"),
  list(en = c("Singapore"), ja = c("シンガポール"), region = "アジア (Asia)"),
  list(en = c("Sri Lanka"), ja = c("スリランカ"), region = "アジア (Asia)"),
  list(en = c("Taiwan","Taiwanese"), ja = c("台湾"), region = "アジア (Asia)"),
  list(en = c("Thailand","Thai"), ja = c("タイ"), region = "アジア (Asia)"),
  list(en = c("Vietnam","Viet Nam","Vietnamese"), ja = c("ベトナム"), region = "アジア (Asia)"),
  list(en = c("Japan","Japanese"), ja = c("日本"), region = "その他 (Others)"),
  list(en = c("Mongolia"), ja = c("モンゴル"), region = "アジア (Asia)"),
  list(en = c("Russia","Russian"), ja = c("ロシア"), region = "アジア (Asia)"),
  list(en = c("Kazakhstan"), ja = c("カザフスタン"), region = "アジア (Asia)"),
  list(en = c("Uzbekistan"), ja = c("ウズベキスタン"), region = "アジア (Asia)"),
  # Middle East
  list(en = c("Saudi Arabia","Saudi"), ja = c("サウジアラビア"), region = "中東(Middle East)"),
  list(en = c("UAE","United Arab Emirates"), ja = c("アラブ首長国連邦"), region = "中東(Middle East)"),
  list(en = c("Iran","Iranian"), ja = c("イラン"), region = "中東(Middle East)"),
  list(en = c("Iraq","Iraqi"), ja = c("イラク"), region = "中東(Middle East)"),
  list(en = c("Israel","Israeli"), ja = c("イスラエル"), region = "中東(Middle East)"),
  list(en = c("Jordan"), ja = c("ヨルダン"), region = "中東(Middle East)"),
  list(en = c("Kuwait"), ja = c("クウェート"), region = "中東(Middle East)"),
  list(en = c("Lebanon"), ja = c("レバノン"), region = "中東(Middle East)"),
  list(en = c("Oman"), ja = c("オマーン"), region = "中東(Middle East)"),
  list(en = c("Qatar"), ja = c("カタール"), region = "中東(Middle East)"),
  list(en = c("Syria","Syrian"), ja = c("シリア"), region = "中東(Middle East)"),
  list(en = c("Turkey","Turkish"), ja = c("トルコ"), region = "中東(Middle East)"),
  list(en = c("Yemen"), ja = c("イエメン"), region = "中東(Middle East)"),
  # Africa
  list(en = c("Angola"), ja = c("アンゴラ"), region = "アフリカ (Africa)"),
  list(en = c("Benin"), ja = c("ベナン"), region = "アフリカ (Africa)"),
  list(en = c("Burkina Faso"), ja = c("ブルキナファソ"), region = "アフリカ (Africa)"),
  list(en = c("Cameroon"), ja = c("カメルーン"), region = "アフリカ (Africa)"),
  list(en = c("Chad"), ja = c("チャド"), region = "アフリカ (Africa)"),
  list(en = c("Congo","DRC","Democratic Republic"), ja = c("コンゴ","コンゴ民主共和国","コンゴ共和国"), region = "アフリカ (Africa)"),
  list(en = c("Egypt","Egyptian"), ja = c("エジプト"), region = "アフリカ (Africa)"),
  list(en = c("Ethiopia","Ethiopian"), ja = c("エチオピア"), region = "アフリカ (Africa)"),
  list(en = c("Ghana"), ja = c("ガーナ"), region = "アフリカ (Africa)"),
  list(en = c("Guinea"), ja = c("ギニア"), region = "アフリカ (Africa)"),
  list(en = c("Kenya","Kenyan"), ja = c("ケニア"), region = "アフリカ (Africa)"),
  list(en = c("Libya"), ja = c("リビア"), region = "アフリカ (Africa)"),
  list(en = c("Madagascar"), ja = c("マダガスカル"), region = "アフリカ (Africa)"),
  list(en = c("Mali"), ja = c("マリ"), region = "アフリカ (Africa)"),
  list(en = c("Morocco"), ja = c("モロッコ"), region = "アフリカ (Africa)"),
  list(en = c("Mozambique"), ja = c("モザンビーク"), region = "アフリカ (Africa)"),
  list(en = c("Niger"), ja = c("ニジェール"), region = "アフリカ (Africa)"),
  list(en = c("Nigeria","Nigerian"), ja = c("ナイジェリア"), region = "アフリカ (Africa)"),
  list(en = c("Rwanda"), ja = c("ルワンダ"), region = "アフリカ (Africa)"),
  list(en = c("Senegal"), ja = c("セネガル"), region = "アフリカ (Africa)"),
  list(en = c("Sierra Leone"), ja = c("シエラレオネ"), region = "アフリカ (Africa)"),
  list(en = c("Somalia"), ja = c("ソマリア"), region = "アフリカ (Africa)"),
  list(en = c("South Africa"), ja = c("南アフリカ"), region = "アフリカ (Africa)"),
  list(en = c("South Sudan"), ja = c("南スーダン"), region = "アフリカ (Africa)"),
  list(en = c("Sudan"), ja = c("スーダン"), region = "アフリカ (Africa)"),
  list(en = c("Tanzania"), ja = c("タンザニア"), region = "アフリカ (Africa)"),
  list(en = c("Uganda"), ja = c("ウガンダ"), region = "アフリカ (Africa)"),
  list(en = c("Zambia"), ja = c("ザンビア"), region = "アフリカ (Africa)"),
  list(en = c("Zimbabwe"), ja = c("ジンバブエ"), region = "アフリカ (Africa)"),
  # Europe
  list(en = c("France","French"), ja = c("フランス"), region = "ヨーロッパ (Europe)"),
  list(en = c("Germany","German"), ja = c("ドイツ"), region = "ヨーロッパ (Europe)"),
  list(en = c("Italy","Italian"), ja = c("イタリア"), region = "ヨーロッパ (Europe)"),
  list(en = c("Spain","Spanish"), ja = c("スペイン"), region = "ヨーロッパ (Europe)"),
  list(en = c("UK","United Kingdom","Britain","British","England"), ja = c("英国","イギリス"), region = "ヨーロッパ (Europe)"),
  list(en = c("Netherlands","Dutch"), ja = c("オランダ"), region = "ヨーロッパ (Europe)"),
  list(en = c("Belgium","Belgian"), ja = c("ベルギー"), region = "ヨーロッパ (Europe)"),
  list(en = c("Denmark","Danish"), ja = c("デンマーク"), region = "ヨーロッパ (Europe)"),
  list(en = c("Norway"), ja = c("ノルウェー"), region = "ヨーロッパ (Europe)"),
  list(en = c("Sweden","Swedish"), ja = c("スウェーデン"), region = "ヨーロッパ (Europe)"),
  list(en = c("Poland","Polish"), ja = c("ポーランド"), region = "ヨーロッパ (Europe)"),
  list(en = c("Ukraine","Ukrainian"), ja = c("ウクライナ"), region = "ヨーロッパ (Europe)"),
  list(en = c("Greece","Greek"), ja = c("ギリシャ","ギリシア"), region = "ヨーロッパ (Europe)"),
  list(en = c("Portugal","Portuguese"), ja = c("ポルトガル"), region = "ヨーロッパ (Europe)"),
  list(en = c("Czech","Czechia","Czech Republic"), ja = c("チェコ","チェコ共和国"), region = "ヨーロッパ (Europe)"),
  list(en = c("Slovakia","Slovak"), ja = c("スロバキア"), region = "ヨーロッパ (Europe)"),
  list(en = c("Hungary","Hungarian"), ja = c("ハンガリー"), region = "ヨーロッパ (Europe)"),
  list(en = c("Austria","Austrian"), ja = c("オーストリア"), region = "ヨーロッパ (Europe)"),
  list(en = c("Switzerland","Swiss"), ja = c("スイス"), region = "ヨーロッパ (Europe)"),
  list(en = c("Romania","Romanian"), ja = c("ルーマニア"), region = "ヨーロッパ (Europe)"),
  list(en = c("Bulgaria","Bulgarian"), ja = c("ブルガリア"), region = "ヨーロッパ (Europe)"),
  list(en = c("Serbia","Serbian"), ja = c("セルビア"), region = "ヨーロッパ (Europe)"),
  list(en = c("Croatia","Croatian"), ja = c("クロアチア"), region = "ヨーロッパ (Europe)"),
  list(en = c("Finland","Finnish"), ja = c("フィンランド"), region = "ヨーロッパ (Europe)"),
  list(en = c("Ireland","Irish"), ja = c("アイルランド"), region = "ヨーロッパ (Europe)"),
  # Americas
  list(en = c("USA","United States","America","American"), ja = c("米国","アメリカ"), region = "北米 (North America)"),
  list(en = c("Canada","Canadian"), ja = c("カナダ"), region = "北米 (North America)"),
  list(en = c("Brazil","Brazilian"), ja = c("ブラジル"), region = "中南米・カリブ (Central & South America/Caribbean)"),
  list(en = c("Mexico","Mexican"), ja = c("メキシコ"), region = "中南米・カリブ (Central & South America/Caribbean)"),
  list(en = c("Argentina"), ja = c("アルゼンチン"), region = "中南米・カリブ (Central & South America/Caribbean)"),
  list(en = c("Colombia"), ja = c("コロンビア"), region = "中南米・カリブ (Central & South America/Caribbean)"),
  list(en = c("Peru","Peruvian"), ja = c("ペルー"), region = "中南米・カリブ (Central & South America/Caribbean)"),
  list(en = c("Venezuela"), ja = c("ベネズエラ"), region = "中南米・カリブ (Central & South America/Caribbean)"),
  list(en = c("Ecuador"), ja = c("エクアドル"), region = "中南米・カリブ (Central & South America/Caribbean)"),
  list(en = c("Haiti"), ja = c("ハイチ"), region = "中南米・カリブ (Central & South America/Caribbean)"),
  # Oceania
  list(en = c("Australia","Australian"), ja = c("オーストラリア"), region = "オセアニア (Oceania)"),
  list(en = c("New Zealand"), ja = c("ニュージーランド"), region = "オセアニア (Oceania)"),
  list(en = c("Papua New Guinea"), ja = c("パプアニューギニア"), region = "オセアニア (Oceania)")
)

# ============================================================
# ユーティリティ
# ============================================================

# テキストを小文字・正規化して検索用文字列を作成
normalize_text <- function(...) {
  texts <- c(...)
  texts <- texts[!is.na(texts) & nchar(trimws(texts)) > 0]
  tolower(paste(texts, collapse = " "))
}

# パターンリストのいずれかがテキストにマッチするか
any_match <- function(text, patterns) {
  # 複数パターンを1つのOR正規表現に結合して高速化
  combined <- paste0("(?i)(", paste(patterns, collapse = "|"), ")")
  grepl(combined, text, perl = TRUE)
}

# ============================================================
# 疾患分類
# ============================================================

classify_disease <- function(title, summary = "") {
  text <- normalize_text(title, summary)

  for (d in DISEASE_DB) {
    if (any_match(text, d$patterns)) {
      return(list(
        category = d$category,
        name_en  = d$name_en,
        name_ja  = d$name_ja
      ))
    }
  }
  list(category = "Other", name_en = "Unknown", name_ja = "不明")
}

# ============================================================
# 国・地域分類
# ============================================================

# 起動時に1回だけ正規表現パターンをコンパイル
.escape_re <- function(x) gsub("([][.(){}^$*+?|\\\\])", "\\\\\\1", x, perl = TRUE)

# カタカナ複合語（例:「サーベイランス」「マドラーズ」）の中に国名の一部が偶然
# 埋め込まれてしまう誤検出（例:「サーベイランス」→「イラン」）を防ぐため、
# カタカナのみの国名は前後がカタカナで連続していない場合のみマッチさせる
# （カタカナの単語境界チェック）。英字の国名は通常の\bで単語境界を取る。
.is_katakana_only <- function(s) grepl("^[゠-ヿー]+$", s, perl = TRUE)
.is_ascii_alpha    <- function(s) grepl("^[A-Za-z ]+$", s, perl = TRUE)

.build_country_name_pattern <- function(nm) {
  esc <- .escape_re(nm)
  if (.is_katakana_only(nm)) {
    paste0("(?<![゠-ヿ])", esc, "(?![゠-ヿ])")
  } else if (.is_ascii_alpha(nm)) {
    paste0("\\b", esc, "\\b")
  } else {
    esc
  }
}

COUNTRY_PATTERNS <- lapply(COUNTRY_DB, function(ctry) {
  all_names <- c(ctry$en, ctry$ja)
  parts <- vapply(all_names, .build_country_name_pattern, character(1))
  list(
    pattern  = paste0("(?i)(", paste(parts, collapse = "|"), ")"),
    location = ctry$en[1],
    region   = ctry$region
  )
})

GLOBAL_PATTERN <- paste0("(?i)(",
  paste(c("global","worldwide","international","multiple countries","several countries",
          "グローバル","世界","複数国","国際的"), collapse = "|"),
  ")")

# 疫学分野で頻出する語の中に、偶然そのまま国名が埋め込まれてしまっているものを
# 国名マッチングの前に除去しておく（例:「サーベイランス」→「イラン」を誤検出する）。
.COUNTRY_MATCH_FALSE_POSITIVE_WORDS <- c("サーベイランス")

classify_location <- function(title, summary = "") {
  text <- normalize_text(title, summary)
  text_for_country <- text
  for (w in .COUNTRY_MATCH_FALSE_POSITIVE_WORDS) {
    text_for_country <- gsub(w, "", text_for_country, fixed = TRUE)
  }

  # 「日本政府がコンゴのエボラ流行に資金協力」のように、実際のアウトブレイク国と無関係に
  # 「日本」という語（日本政府・日本人渡航者等）を含む記事が多いため、COUNTRY_PATTERNSの
  # 出現順に関わらず「日本」以外の国名を優先的にチェックする。他国名が見つからない場合に
  # のみ「日本（その他）」と判定する。
  for (cp in COUNTRY_PATTERNS) {
    if (identical(cp$location, "Japan")) next
    if (grepl(cp$pattern, text_for_country, perl = TRUE)) {
      return(list(location = cp$location, region = cp$region))
    }
  }
  for (cp in COUNTRY_PATTERNS) {
    if (identical(cp$location, "Japan") && grepl(cp$pattern, text_for_country, perl = TRUE)) {
      return(list(location = cp$location, region = cp$region))
    }
  }

  if (grepl(GLOBAL_PATTERN, text, perl = TRUE)) {
    return(list(location = "Global", region = "グローバル (Global)"))
  }

  list(location = "Unknown", region = "不明")
}

# ============================================================
# スクリーニング基準の判定
# ============================================================

# 1. Unusual/unexpected
# 定義: 変か？ベースラインと異なる状況か？（動物における大規模アウトブレイクも含む）
check_unusual <- function(text) {
  any_match(text, c(
    # 通常と異なる・初めての状況
    "novel","new variant","new strain","new pathogen","unusual","unprecedented",
    "unexpected","rare","first.?time","never before","first.*case","first.*report",
    "emerging","re.?emerging","unknown.*pathogen","undiagnosed","mystery.*illness",
    "new.*serotype","new.*genotype","atypical","anomalous",
    # ベースラインからの逸脱
    "surge","spike","dramatic increase","sharp rise","sudden increase",
    "above.*baseline","exceed.*threshold","historic.*high","record.*high",
    # 動物における大規模アウトブレイク（Zoonosis）
    "animal.*outbreak","livestock.*outbreak","poultry.*outbreak","wildlife.*outbreak",
    "mass.*die","mass death.*animal","死.*家禽","家畜.*感染拡大","野生動物.*アウトブレイク",
    "zoonotic spillover","spillover.*human","スピルオーバー",
    # 日本語
    "新規","新型","新たな株","初確認","初めて.*報告","初.*症例","珍しい","予想外",
    "未知","変異株","異常増加","過去最高","ベースライン.*超","クレード.*新"
  ))
}

# 2. Serious PH impact in the country
# 感染症文脈かどうかの基本チェック（非感染症記事を除外するゲート）
INFECTIOUS_CONTEXT_KW <- c(
  # 感染症固有の語（アレルギー・食品リコールでは通常出ない）
  "outbreak","epidemic","pandemic","infection","infectious","contagious",
  "pathogen","virus","bacteria","bacterial","fungal","parasite",
  "transmit","transmission","contagion","communicable",
  "hospitalized.*infection","icu.*patient.*infect",
  "感染症","アウトブレイク","流行","パンデミック","病原","ウイルス","細菌",
  "感染者","患者.*感染","集団感染","ヒト-ヒト",
  # 食中毒・集団発生（感染症文脈として明示追加）
  "食中毒","集団食中毒","食品媒介","food.*poison","foodborne","food.?borne",
  "集団発生","集団感染","集団下痢","ノロ","norovirus","サルモネラ","salmonella",
  "カンピロバクター","campylobacter","o157","ehec","腸管出血性"
)
# 非感染症除外キーワード（これが含まれ感染症語がない → 除外）
NON_INFECT_KW <- c(
  "allergy alert","allergen","undeclared.*peanut","undeclared.*milk",
  "undeclared.*wheat","undeclared.*soy","food recall","product recall",
  "voluntary recall","consumer advisory.*food","アレルギー.*リコール",
  "食品リコール","自主回収",
  # サイバー・コンピューターウイルス（感染症と無関係）
  "コンピュータウイルス","コンピューターウイルス","computer virus",
  "マルウェア","malware","ランサムウェア","ransomware","cyberattack",
  "サイバー攻撃","サイバーセキュリティ","情報セキュリティ",
  "フィッシング","phishing","不正アクセス","脆弱性","vulnerability",
  "usbウイルス","usb.*ウイルス","ウイルス.*usb",
  "悪意.*ウイルス","不正.*ウイルス",
  "セキュリティパッチ","トロイの木馬","trojan","スパイウェア"
)

is_infectious_disease_context <- function(text) {
  # 非感染症キーワードが明示的にあれば除外
  if (any_match(text, NON_INFECT_KW)) return(FALSE)
  # 疾患DB（DISEASE_DB）にマッチ、または感染症固有語があればOK
  any_match(text, INFECTIOUS_CONTEXT_KW)
}

check_serious_country <- function(text, is_known_disease = FALSE) {
  # 感染症文脈必須（疾患DBに一致している場合はゲートをスキップ）
  if (!is_known_disease && !is_infectious_disease_context(text)) return(FALSE)

  # 定義: その国にとって公衆衛生上のインパクトがあるか（国内の広域事例含む）
  # → 死者・重症・大規模流行・広域感染・国家的緊急事態
  any_match(text, c(
    # 死者・重篤
    "deaths?.*outbreak","fatalities.*case","killed.*disease","mortality.*high",
    "死亡.*感染","死者.*報告","致死率.*高",
    # 広域・大規模
    "widespread.*outbreak","nationwide.*spread","country.?wide.*case",
    "large.?scale.*outbreak","major.*epidemic","thousands.*infected",
    "全国.*感染","広域.*流行","大規模.*アウトブレイク","数千.*感染者",
    # 医療逼迫
    "hospital.*overwhelm","healthcare.*system.*strain","bed.*shortage.*hospital",
    "医療.*逼迫","病床.*不足.*感染","医療崩壊",
    # 緊急事態宣言
    "public health emergency","state of emergency.*health","PHEIC",
    "national emergency.*disease","公衆衛生上の緊急事態","国家緊急事態.*感染",
    # 大規模症例数（100例以上の報告）英語
    "[1-9][0-9]{2,}\\s*(cases?|patients?|deaths?).*report",
    "report.*[1-9][0-9]{2,}\\s*(cases?|patients?|deaths?)",
    "[1-9][0-9]{2,}\\s*(cases?|deaths?).*this\\s+week",
    "cases?.*rising","rising.*cases?","surge.*cases?","cases?.*surging",
    # 大規模症例数（100例以上の報告）日本語
    "[1-9][0-9]{2,}.*例.*死亡","死亡.*[1-9][0-9]{2,}.*例",
    "[1-9][0-9]{2,}例.*報告","報告.*[1-9][0-9]{2,}例",
    "死亡者.*[1-9][0-9]{2,}","死者.*[1-9][0-9]{2,}",
    # 公衆衛生機関の対応
    "public health.*response","health.*authorities.*respond",
    "response.*ongoing","outbreak.*response","outbreak.*control",
    "アウトブレイク.*死亡","死亡.*アウトブレイク",
    "集団感染.*拡大","感染拡大.*続"
  ))
}

# 日本国内ソース（source_id または source_name で判定）
# lang="ja" のソースはすべて日本向けだが、source_id でも明示管理する
JAPAN_SOURCE_IDS <- c(
  "mhlw","jihs","nhk","nhk_sci","nhk_soc","nhk_pol",
  "asahi_h","asahi_s","asahi_n","mainichi","jiji",
  "yahoo_s","yahoo_d","nikkei_s","hokkaido","miyanichi"
)
# Japan Times は英語だが日本向けソース
JAPAN_SOURCE_IDS_EN <- c("jptimes", "reliefweb")

# 国際メディアの日本語版（日本語だが海外事象を報じる → 日本国内ソース扱いしない）
# 健康情報・医療解説系メディア（アウトブレイク報告ではなく一般向け解説）
HEALTH_INFO_MEDIA <- c(
  "メディカルドック","medical doc","medicalook","メディカルノート","medical note",
  "healthline","webmd","mayo clinic","everydayhealth","healthcarejapan",
  "あすけん","スポーツ栄養","ヘルスケア大学","介護のほんね","みんなの介護",
  "病気スコープ","薬の窓口","薬価サーチ","お薬110番"
)

# 健康情報・解説記事パターン（アウトブレイク報告と区別）
HEALTH_ADVICE_PATTERNS <- c(
  "医師が解説","専門家が解説","医師.*監修","専門家.*監修",
  "解説.*医師","監修.*医師","医師.*コメント",
  "症状と対策","予防.*ポイント","重症化しやすい人","かかりやすい人",
  "注意.*ポイント","気をつけるべき","対処法","セルフケア",
  "治療法.*解説","受診の目安","病院に行く.*タイミング",
  "ワクチン.*効果.*解説","接種.*メリット.*デメリット"
)

is_health_advice_article <- function(title, text) {
  t <- tolower(paste(title, text))
  sn_title <- tolower(trimws(as.character(title)))
  # タイトル末尾のメディア名チェック
  media_from_title <- ""
  m <- regmatches(sn_title, regexpr("[-–—]\\s*([^-–—]+)$", sn_title, perl=TRUE))
  if (length(m) > 0) media_from_title <- tolower(trimws(gsub("^[-–—]\\s*","",m)))

  is_health_media <- any(sapply(HEALTH_INFO_MEDIA,
    function(k) grepl(k, media_from_title, fixed=TRUE) ||
                grepl(k, t, fixed=TRUE)))
  has_advice_pattern <- any_match(t, HEALTH_ADVICE_PATTERNS)

  is_health_media || has_advice_pattern
}

INTL_MEDIA_JA <- c(
  "cnn","reuters","bbc","bloomberg","ap ","afp","wsj","nytimes","guardian",
  "washington post","fox news","abc news","nbc news","cbs news",
  "the economist","time ","newsweek","foreign policy","nature","science",
  "who ","ecdc","cdc ","promed","cidrap","lancet","nejm","bmj",
  # アジア系国際メディア
  "cgtn","xinhua","新華","global times","south china morning",
  "straits times","channel news asia","cna","korea herald","yonhap",
  "nhk world",  # NHK Worldは英語国際放送（国内向けのnhkとは別）
  # 在外・外国情報の日本語メディア
  "vietjo","vietnamplus","vietnam news","asia nikkei","nikkei asia",
  "jakarta post","bangkok post","philippine daily","myanmar times",
  "india times","hindustan times","dawn ","arab news","gulf news",
  "kyodo news","共同通信.*english","jiji.*english",
  # 韓国メディア
  "매일경제","조선일보","중앙일보","한국일보","연합뉴스","경향신문","한겨레",
  "korea joongang","chosun","donga","maeil business","mk news",
  # 中国語・台湾メディア
  "自由時報","蘋果日報","中央社","聯合新聞","中時","大紀元",
  # アラビア語・中東メディア
  "al jazeera","aljazeera","al arabiya","middle east eye",
  # その他アジア
  "theprint","the wire ","scroll.in","rappler","coconuts"
)

is_japan_source <- function(source_id = "", source_name = "", lang = "", title = "") {
  # Google Newsタイトル末尾の「- メディア名」を抽出
  title_media <- ""
  if (nchar(trimws(as.character(title))) > 0) {
    m <- regmatches(title, regexpr("[-–—]\\s*([^-–—]+)$", title, perl = TRUE))
    if (length(m) > 0)
      title_media <- tolower(trimws(gsub("^[-–—]\\s*", "", m)))
  }

  # 判定対象：source_name と タイトル末尾メディア名 の両方をチェック
  sn    <- tolower(trimws(as.character(source_name)))
  check <- paste(sn, title_media)  # 両方まとめて照合

  # タイトル末尾メディア名にハングル・アラビア文字・キリル文字等が含まれれば海外メディア
  if (nchar(title_media) > 0 &&
      grepl("[가-힣؀-ۿЀ-ӿऀ-ॿ฀-๿]",
            title_media, perl = TRUE)) {
    return(FALSE)
  }

  # 国際メディアが含まれていれば除外
  is_intl_media <- any(sapply(INTL_MEDIA_JA, function(k) grepl(k, check, fixed = TRUE)))
  if (is_intl_media) return(FALSE)

  # source_id が既知の日本国内ソース
  sid <- trimws(tolower(as.character(source_id)))
  if (sid %in% c(JAPAN_SOURCE_IDS, JAPAN_SOURCE_IDS_EN)) return(TRUE)

  # source_name / タイトル末尾が既知の日本国内メディア
  if (nchar(check) > 0 &&
      any(sapply(c("nhk","mhlw","jihs","厚生労働","朝日新聞","毎日新聞","時事通信",
                   "読売新聞","産経新聞","北海道新聞","宮崎日日","yahoo.*ニュース",
                   "japan times","reliefweb"),
                 function(k) grepl(k, check, perl = TRUE)))) return(TRUE)

  # lang="ja" かつ国際メディアでない場合は国内ソースとみなす
  identical(trimws(as.character(lang)), "ja")
}

# 3. Serious PH impact to Japan
# 日本への直接影響を示すパターン（ソース種別に関わらず共通）
JAPAN_IMPACT_PATTERNS <- c(
  "japan.*confirmed.*case","japan.*reported.*case","japan.*detected",
  "japan.*outbreak","outbreak.*in.*japan",
  "case.*in.*japan","cases.*in.*japan",
  "spread.*to.*japan","japan.*imported.*case","imported.*case.*japan",
  "日本.*輸入例","日本.*感染者.*確認","日本.*症例.*報告",
  "日本.*アウトブレイク","日本.*発生確認",
  "帰国者.*感染確認","在留日本人.*感染","邦人.*感染",
  "国内.*感染確認","国内.*発生"
)

check_serious_japan <- function(text, source_id = "", source_name = "", lang = "", title = "",
                               is_known_disease = FALSE) {
  # 非感染症記事は除外（疾患DBに一致している場合はゲートをスキップ）
  if (!is_known_disease && !is_infectious_disease_context(text)) return(FALSE)

  # 日本への直接影響パターンがある場合は無条件でTrue（全ソース共通）
  if (any_match(text, JAPAN_IMPACT_PATTERNS)) return(TRUE)

  # タイトル・本文に外国地名が含まれるか確認
  # classify_location はタイトル+本文から主要地名を抽出
  loc <- classify_location(title)  # タイトルだけで判定（より確実）
  is_foreign_location <- !is.null(loc$location) &&
                         !is.na(loc$location) &&
                         !(loc$location %in% c("Unknown", "Japan", "Global")) &&
                         loc$location != "Unknown"

  # 外国地名が主題にある場合：日本への明示的な言及なし → FALSE
  # （国内ソースでも海外事象を報じている場合は除外）
  if (is_foreign_location) return(FALSE)

  # 外国地名なし：国内ソースであれば True、海外ソースは False
  is_japan_source(source_id, source_name, lang, title)
}

# 4. Epidemic-prone
# 定義: クラスターや地域流行につながるもの／パンデミック可能性病原体／動物-ヒト界面
check_epidemic_prone <- function(text) {
  any_match(text, c(
    # ヒト-ヒト感染・持続伝播
    "human.?to.?human","person.?to.?person","sustained transmission",
    "community.?spread","community transmission","secondary.*case",
    "ヒト-ヒト感染","ヒトからヒトへ","持続的.*感染","市中感染","二次感染",
    # クラスター・地域流行
    "cluster","clusters","local.*outbreak","regional.*spread",
    "クラスター","地域.*流行","集積",
    # パンデミック可能性
    "pandemic.*potential","pandemic.*risk","pandemic.*threat",
    "パンデミック","流行拡大.*世界","世界的.*感染拡大",
    # 高感染性・高伝播性
    "highly contagious","highly transmissible","rapid.*spread",
    "感染性が高","高い伝播性",
    # Animal-human interface（定義に明示）
    "avian.?flu","bird.?flu","avian influenza","h5n1","h5n2","h5n6","h5n8","h7n9",
    "swine.?flu","swine influenza","h1n1","variant.*influenza",
    "\\bmers\\b","camel.*mers","mers.?cov",
    "\\bnipah\\b","bat.*virus.*human","animal.?human.*interface",
    "zoonotic.*human","zoonosis.*outbreak",
    "鳥インフルエンザ","豚インフルエンザ","動物.*ヒト.*感染","人獣共通.*感染"
  ))
}

# 5. Mass exposure
# 定義: 集団への曝露の可能性があるか
check_mass_exposure <- function(text) {
  any_match(text, c(
    # 食品・水系集団曝露
    "food.*poisoning","foodborne.*illness","food.?borne.*outbreak",
    "contaminated.*food","contaminated.*water","water.*contamination",
    "waterborne.*outbreak","water.*supply.*contaminat",
    "食中毒","集団食中毒","食品.*汚染","水道.*汚染","水系.*感染",
    # 共通曝露・集団発生
    "common.*source.*outbreak","single.*source.*outbreak",
    "mass.*exposure","mass.*poisoning","mass.*casualty",
    "集団曝露","同一.*曝露源","共通.*感染源",
    # 施設・イベントでの集団発生
    "school.*outbreak","nursing home.*outbreak","hospital.*outbreak",
    "restaurant.*outbreak","workplace.*outbreak","cruise.*ship.*outbreak",
    "wedding.*outbreak","festival.*case","gathering.*infection",
    "学校.*集団感染","施設.*集団発生","給食.*感染","クルーズ船.*感染",
    "職場.*集団","老人ホーム.*感染","イベント.*集団感染"
  ))
}

# 6. High profile
# 定義: 国際機関・Directorsの関心が高いもの／応用疫学センターとして対応可能性
#       原因不明疾患（重症・多数・日本への影響）
check_high_profile <- function(text) {
  any_match(text, c(
    # 国際機関の関与・勧告（機関名単体ではなく「勧告・警告・懸念」とセット）
    "WHO.*warn","WHO.*alert","WHO.*concern","WHO.*emergency","WHO.*monitor",
    "ECDC.*warn","ECDC.*alert","ECDC.*concern",
    "CDC.*health alert","CDC.*warn","CDC.*advisory",
    "ProMED.*alert","FAO.*warn","UNICEF.*outbreak",
    "MSF","Doctors Without Borders","médecins sans frontières",
    "IHR","PHEIC","international health regulations","国際保健規則",
    # 国際的関心・懸念
    "international.*concern","global.*concern","worldwide.*spread",
    "global.*health.*threat","international.*alert",
    "国際的.*懸念","世界的.*感染拡大","グローバル.*リスク",
    # 日本への注目（High profileの定義に「日本への影響」含む）
    "japan.*risk","risk.*japan","japan.*concern","concern.*japan",
    "日本.*リスク","日本.*懸念","日本への影響",
    # 原因不明疾患（重症・多数）
    "undiagnosed.*severe","unknown.*pathogen.*death","mystery.*illness.*death",
    "unexplained.*death","unexplained.*cluster",
    "原因不明.*重症","原因不明.*死亡","不明.*病原体.*死",
    # 大規模国際イベント
    "olympic","paralympic","G7","G20","APEC","world cup",
    "オリンピック","パラリンピック","ワールドカップ"
  ))
}

# 7. Special pathogen / Bioterrorism
check_special_pathogen <- function(text) {
  # 一類感染症
  class1 <- c(
    "ebola","marburg","lassa","crimean.congo","south american hemorrhagic fever",
    "smallpox","variola","plague","\\banthrax\\b","botulism","tularemia",
    "glanders","melioidosis",
    "エボラ","マールブルグ","ラッサ","クリミア.コンゴ","南米出血熱",
    "痘そう","天然痘","ペスト","炭疽","ボツリヌス","野兎病","鼻疽","類鼻疽",
    "一類感染症","bioterrorism","bio.?terror","バイオテロ","生物兵器",
    "biological weapon","weaponized"
  )
  any_match(text, class1)
}

# ============================================================
# 結核を除く二類感染症・即時対応疾患キーワード（国内Signal High判定用）
# ============================================================
CLASS2_IMMEDIATE_KW <- c(
  # 二類感染症（結核除く）
  "麻疹","はしか","measles",
  "風疹","rubella",
  "侵襲性髄膜炎菌","髄膜炎菌","meningococcal","invasive meningococcal","\\bimd\\b",
  "劇症型溶連菌","stss","streptococcal toxic shock",
  "重症熱性血小板減少症候群","\\bsfts\\b",
  "ジフテリア","diphtheria",
  "急性灰白髄炎","ポリオ","polio",
  # 一類感染症（国内発生はすべてSignal High）
  "エボラ","ebola","マールブルグ","marburg","ラッサ","lassa",
  "クリミア.コンゴ","痘そう","天然痘","smallpox","ペスト","plague",
  "炭疽","\\banthrax\\b","ボツリヌス","botulism","野兎病","tularemia",
  "鼻疽","類鼻疽",
  # バイオテロ
  "バイオテロ","bioterrorism","bio.?terror","生物兵器","biological weapon"
)

# 大規模集団発生キーワード（100例以上の食中毒等）
LARGE_OUTBREAK_PATTERNS <- c(
  # 100例以上の数値パターン
  "[1-9][0-9]{2,}\\s*(人|名|例|件|cases?|patients?).*?(食中毒|集団感染|感染|outbreak|poisoning)",
  "(食中毒|集団感染|outbreak|poisoning).*?[1-9][0-9]{2,}\\s*(人|名|例|件|cases?|patients?)",
  # 明示的な大規模表現
  "大規模.*食中毒","集団食中毒.*多数","食中毒.*大規模",
  "large.?scale.*food.*poison","mass.*food.*poison","mass.*outbreak",
  "数百.*感染","数千.*感染","hundreds.*infected","thousands.*infected"
)

is_large_scale_outbreak <- function(text) {
  any_match(text, LARGE_OUTBREAK_PATTERNS)
}

# ============================================================
# Signal の重みづけ判定（国内・海外で分岐）
# ============================================================

calc_signal_weight <- function(unusual, serious_c, serious_j, epidemic,
                                mass, high, special,
                                is_overseas = FALSE, text = "") {

  # ══════════════════════════════════════════════════════════
  # 【海外記事】その国・国際社会にとっての重大性で判定
  # ══════════════════════════════════════════════════════════
  if (is_overseas) {
    # Signal High: その国で重大インパクト + 異常性/一類/大規模/国際機関警告
    is_event <-
      (serious_c && unusual)  ||   # 海外で重大 + 異常性
      (serious_c && special)  ||   # 海外で重大 + 一類相当
      (serious_c && high)     ||   # 海外で重大 + 国際的関心
      (serious_c && mass)     ||   # 海外で重大 + 集団曝露
      (special   && epidemic) ||   # 一類 + 伝播可能性
      (unusual   && high)          # 原因不明 + 国際的関心

    if (is_event) return("Signal High")

    # Signal Low: 海外での重大事象、または伝播可能性あり、または一類感染症
    is_signal <-
      special              ||   # 一類感染症・バイオテロ
      serious_c            ||   # 海外で公衆衛生上の重大インパクト
      (epidemic && unusual) ||  # 伝播可能性 + 異常性
      (epidemic && serious_c)|| # 伝播可能性 + 海外重大事象
      (mass && unusual)         # 集団曝露 + 異常性

    if (is_signal) return("Signal Low")
    return("FYI")
  }

  # ══════════════════════════════════════════════════════════
  # 【国内記事】日本にとっての公衆衛生上の重大性で判定
  # ══════════════════════════════════════════════════════════

  # 二類感染症（結核除く）・一類感染症の国内発生
  has_class2_immediate <- any_match(text, CLASS2_IMMEDIATE_KW)

  # 大規模集団発生（食中毒100例以上等）
  is_large <- is_large_scale_outbreak(text)

  # ─── Signal High ─────────────────────────────────────────
  # 日本への直接影響が直ちにある・大きいもの
  is_event <-
    (serious_j && has_class2_immediate) ||  # 一類・二類の国内発生
    (serious_j && unusual && !epidemic) ||  # 原因不明重症（epidemic単独を除く）
    (serious_j && is_large)             ||  # 大規模集団発生（100例以上等）
    (serious_j && high)                 ||  # 政治的・国際的関心が高い国内事象
    (special   && serious_j)            ||  # 一類感染症 + 日本への影響
    (unusual   && high && serious_j)        # 原因不明 + 高関心 + 国内影響

  if (is_event) return("Signal High")

  # ─── Signal Low ──────────────────────────────────────────
  # 人の健康に危機を与えうる事例・疫学情報を含み
  # 公衆衛生機関がリスク評価・何らかの対応を要する可能性があるもの
  is_signal <-
    serious_j                      ||  # 日本への影響あり（High未満）
    (serious_c && epidemic)        ||  # 海外重大事象 + 伝播可能性（輸入リスク）
    (special && !serious_j)        ||  # 一類（海外事例でもモニタリング対象）
    (epidemic && unusual)          ||  # 新型・変異 + 伝播可能性
    (mass && serious_c)            ||  # 海外集団曝露 + 重大影響
    (has_class2_immediate && epidemic) # 二類疾患 + 伝播可能性

  if (is_signal) return("Signal Low")

  "FYI"
}

# ============================================================
# メイン関数: 1記事を評価
# ============================================================

screen_entry <- function(title, summary = "", source_id = "", source_name = "",
                         lang = "", is_overseas = FALSE) {
  text <- normalize_text(title, summary)

  # 疾患DBに一致するか（最も確実な感染症判定）
  disease  <- classify_disease(title, summary)
  is_known_disease <- disease$category != "Other"

  # 感染症文脈か（疾患DB一致 OR 感染症固有語）
  is_infect <- is_known_disease || is_infectious_disease_context(text)

  # 非感染症記事 または 健康情報・解説記事 → FYI
  if (!is_infect || is_health_advice_article(title, text)) {
    location <- classify_location(title, summary)
    return(list(
      unusual_unexpected = "", serious_ph_country = "", serious_ph_japan   = "",
      epidemic_prone     = "", mass_exposure      = "", high_profile        = "",
      special_pathogen   = "", signal_weight      = "FYI",
      disease_category   = disease$category, disease_name_en = disease$name_en,
      disease_name_ja    = disease$name_ja,  location = location$location,
      region             = location$region
    ))
  }

  # 感染症記事：各基準を評価
  unusual    <- check_unusual(text)
  serious_c  <- check_serious_country(text, is_known_disease = is_known_disease)
  serious_j  <- check_serious_japan(text, source_id = source_id,
                                    source_name = source_name, lang = lang,
                                    title = title, is_known_disease = is_known_disease)
  epidemic   <- check_epidemic_prone(text)
  mass       <- check_mass_exposure(text)
  high       <- check_high_profile(text)
  special    <- check_special_pathogen(text)

  # Signal重みづけ（国内・海外で分岐）
  weight <- calc_signal_weight(unusual, serious_c, serious_j, epidemic,
                                mass, high, special,
                                is_overseas = is_overseas, text = text)

  # 国・地域分類
  location <- classify_location(title, summary)

  list(
    unusual_unexpected = if (unusual)   "✓" else "",
    serious_ph_country = if (serious_c) "✓" else "",
    serious_ph_japan   = if (serious_j) "✓" else "",
    epidemic_prone     = if (epidemic)  "✓" else "",
    mass_exposure      = if (mass)      "✓" else "",
    high_profile       = if (high)      "✓" else "",
    special_pathogen   = if (special)   "✓" else "",
    signal_weight      = weight,
    disease_category   = disease$category,
    disease_name_en    = disease$name_en,
    disease_name_ja    = disease$name_ja,
    location           = location$location,
    region             = location$region
  )
}

# ============================================================
# ノイズ記事除外（感染症と無関係な記事を落とす）
# ebs_loader.R と app.R の両方から呼ばれる共有関数
# ============================================================

is_noise_article <- function(title, summary = "") {
  tl <- tolower(paste(coalesce(as.character(title), ""),
                      coalesce(as.character(summary), ""), sep = " "))

  # ── 1. 無条件除外: サイバー・コンピューターセキュリティ ─────────────
  cyber_fixed <- c(
    "コンピュータウイルス","コンピューターウイルス","computer virus",
    "マルウェア","malware","ランサムウェア","ransomware",
    "サイバー攻撃","cyberattack","サイバーセキュリティ","情報セキュリティ",
    "フィッシング","phishing","スパイウェア","spyware",
    "ハッキング","hacking","不正アクセス","セキュリティパッチ",
    "脆弱性","vulnerability","トロイの木馬","trojan",
    "usbウイルス","usb virus","悪意のあるソフト","不正プログラム"
  )
  if (any(sapply(cyber_fixed, function(k) grepl(k, tl, fixed = TRUE)))) return(TRUE)
  if (any(sapply(c(
    "usb.{0,20}(ウイルス|virus|マルウェア|malware)",
    "(ウイルス|virus).{0,20}usb",
    "悪意.{0,15}ウイルス",
    "不正.{0,10}ウイルス",
    "(自衛隊|防衛省|総務省|警察庁).{0,50}(ウイルス|マルウェア|サイバー)",
    "情報漏(えい|洩)"
  ), function(p) grepl(p, tl, perl = TRUE)))) return(TRUE)

  # ── 2. 無条件除外: TB ストレージ（テラバイト）誤ヒット ─────────────
  if (grepl("テラバイト|terabyte", tl, fixed = FALSE)) return(TRUE)
  if (grepl("\\d+\\s*tb\\b|tb.{0,5}(ssd|hdd|ストレージ|ディスク|容量|byte|gb|mb)", tl, perl = TRUE)) return(TRUE)

  # ── 3. 無条件除外: スポーツ・芸能 ────────────────────────────────
  if (grepl("wimbledon|ウィンブルドン|全仏オープン|全米オープン|全豪オープン", tl, perl = TRUE) &&
      !grepl("感染|infectious|outbreak|epidemic", tl, perl = TRUE)) return(TRUE)
  if (grepl("tbs.{0,10}(スペシャル|アスリート|アンバサダー|番組)", tl, perl = TRUE)) return(TRUE)

  # ── 4. 無条件除外: AI・テクノロジービジネス ─────────────────────
  if (grepl(paste0("sovereign.*ai|japan.*ai.*robot|ai.*frenzy|bots.*buying|",
                   "furniture.*bot|furniture.*ai|fear.*anger.*meta.*ai|",
                   "ai era.*bots"), tl, perl = TRUE)) return(TRUE)

  # ── 5. 無条件除外: 金融・為替 ────────────────────────────────────
  if (grepl("traders.*worst.case.*yen|worst.case.*currency|yen.*currency.*crisis|為替.*危機", tl, perl = TRUE)) return(TRUE)

  # ── 5b. 無条件除外: 社説（タイトルに社説マーカーがある場合は内容問わず除外） ──
  if (grepl("＜社説＞|〔社説〕|【社説】|^社説[　 ：:]|\\[社説\\]", tl, perl = TRUE)) return(TRUE)

  # ── 5c. 無条件除外: 過去の流行を理由とした中止・中断の回顧言及 ──────────
  # 「新型コロナウイルス感染拡大の影響で中止」のように「感染拡大」等の語自体は
  # has_outbreak_core にも合致してしまうため、その判定より前に無条件除外する。
  # スポーツ大会の沿革紹介等、現在の流行とは無関係な過去の出来事の説明であることが
  # 明確なパターンに限定する。
  if (grepl(paste0(
    "感染拡大の影響で(中止|中断|延期|開催できなかった)|",
    "新型コロナウイルスの影響で定着した|コロナ禍で定着した|",
    "コロナ.{0,5}影響で.{0,10}(リモートワーク|在宅勤務|テレワーク)"
  ), tl, perl = TRUE)) return(TRUE)

  # ── 6. 感染症アウトブレイク固有の文脈があるか判定 ──────────────────
  # これが TRUE なら以下の条件付き除外は適用しない
  # 注: 「患者.{0,5}[0-9]」等の緩いギャップ許容パターンは、「患者様向け対策1位」の
  # ような無関係な数字（ランキング等）まで症例数の言及と誤認識してしまうため、
  # 数字の直後に「人・名・例」等の症例カウント接尾語を要求する厳密な形に統一する。
  has_outbreak_core <- grepl(
    paste0("outbreak(?!.*study|.*training)|epidemic(?!.*study|.*economic)|",
           "pandemic|アウトブレイク|感染拡大|クラスター|集団感染|集団発生|",
           "感染者[0-9]+\\s*(人|名|例)|[0-9]+\\s*(人|名|例).{0,3}(の)?感染者|",
           "患者[0-9]+\\s*(人|名|例)|[0-9]+\\s*(人|名|例).{0,3}(の)?患者|",
           "[0-9]+\\s*(人|名|例).{0,10}(発症|感染|罹患)|",
           "(発症|感染|罹患).{0,10}[0-9]+\\s*(人|名|例)|",
           "感染者数|陽性者数|発生動向調査|サーベイランス.{0,5}(速報|データ|結果|強化)|",
           "surveillance.{0,5}(data|report|result|update)|",
           "感染確認|感染症.*発生.*確認|感染症.*発生.*報告|発生.*確認.*感染|感染症が発生"),
    tl, perl = TRUE
  )
  if (has_outbreak_core) return(FALSE)

  # ── 以下はアウトブレイク文脈がない場合のみ除外 ────────────────────

  # 7. 自然災害・人道支援（感染症でなく災害主体）
  if (grepl(paste0("earthquake|terremoto|flood(?!.*disease|.*outbreak)|",
                   "drought(?!.*disease)|tsunami|cyclone|hurricane(?!.*disease)|",
                   "地震(?!.*感染)|洪水(?!.*感染)|台風(?!.*感染)|干ばつ"),
            tl, perl = TRUE)) return(TRUE)
  if (grepl(paste0("(situation report|sitrep|humanitarian).{0,40}",
                   "(earthquake|flood|drought|migration|displacement|search and rescue|sar team)"),
            tl, perl = TRUE)) return(TRUE)
  if (grepl(paste0("(earthquake|flood|drought|displacement|migration).{0,40}",
                   "(situation report|sitrep|humanitarian)"),
            tl, perl = TRUE)) return(TRUE)
  # ReliefWeb 난민・移住・選挙・経済系
  if (grepl(paste0("migration.{0,30}(report|bulletin|monthly|update)|",
                   "flukset.*migratore|monitoreo.*estacional|lluvia|",
                   "nutrition cluster|food.*security.*bulletin|",
                   "elections.*peace|responsible use.*ai.*elections"),
            tl, perl = TRUE)) return(TRUE)

  # 8. 政治・行政・社会統計（非感染症）
  if (grepl(paste0("有効求人倍率|障害者雇用率|育児休業|少子化(?!.*感染)|",
                   "出生率.*過去最低|子どもが減る社会|人口.*最少|",
                   "被爆者.*平均年齢|被爆者.*9万|原爆.*式典|",
                   "平和の鐘.*追悼|戦争映画.*鑑賞|らい予防法.*廃止|",
                   "審議拒否|都構想.*副首都|副首都.*構想|",
                   "ストーカー規制法.*有罪|厚労省係長.*ストーカー"),
            tl, perl = TRUE)) return(TRUE)

  # 9. 薬事・医療機器承認（アウトブレイクと無関係）
  if (grepl(paste0("製造販売承認.{0,10}(取り消し|取消|勧告)|",
                   "自由診療.{0,20}(再生医療|幹細胞|美容)|",
                   "再生医療.*妥当性評価|",
                   "(痩せ薬|ダイエット|美容目的).{0,20}(薬|注射|クリニック)|",
                   "(薬|注射).{0,20}(ダイエット|痩せ|美容目的)"),
            tl, perl = TRUE)) return(TRUE)

  # 10. サプリメント・健康食品（リコール・一般注意喚起）
  if (grepl(paste0("サプリ.{0,20}(健康被害|副作用|届け出|義務)|",
                   "健康食品.*被害|紅麹.*サプリ|",
                   "コーヒー.{0,20}(リスク低下|健康効果)|",
                   "食べ物.*がん.*予防|食品.*がん.*リスク"),
            tl, perl = TRUE)) return(TRUE)

  # 11. 学術論文・方法論研究（日本アウトブレイク文脈なし）
  if (grepl(paste0("genomic.*landscape|phylogenetic.*insight|",
                   "biosynthetic.*locus|lipopolysaccharide.*clonal|",
                   "estimation.*sample.*size.*detect|",
                   "validation.*risk.*model.*predict|",
                   "epidemiological.*economic.*effect.*modell|",
                   "autopsy.*practices.*guideline|",
                   "global burden.*disease.*study|",
                   "preference.driven.*implementation.*modelling"),
            tl, perl = TRUE)) return(TRUE)

  # 12. 病院・医療機関ビジネス・行政（感染症対応外）
  if (grepl(paste0("連携協定.{0,20}(地域医療|病院)|",
                   "(地域医療|病院).{0,20}連携協定|",
                   "移行期医療.*支援センター|小児.*成人.*連携(?!.*感染)|",
                   "付き添い.*入院.*負担|有床診療所.*活用|",
                   "創薬産業.*成長戦略|医療.*成長戦略.*維新"),
            tl, perl = TRUE)) return(TRUE)

  # 13. 人事・着任・栄転（感染症センター以外）
  if (grepl(paste0("(センター長|所長|院長|副院長|部長|教授).{0,20}(栄転|退任)|",
                   "栄転.{0,20}(センター長|所長|院長)|",
                   "コロナ禍活躍.{0,20}感染症専門医.{0,20}栄転"),
            tl, perl = TRUE)) return(TRUE)

  # 14. 労働・雇用・経済統計
  if (grepl(paste0("有効求人倍率|コカイン.*使用.*推計|薬物.*使用.*最多|",
                   "investors.*yen|traders.*yen|worst-case.*scenario.*yen|",
                   "英国家具.*ai|sofa.*bot"),
            tl, perl = TRUE)) return(TRUE)

  # 15. 非感染症疾患の一般健康記事（アウトブレイク文脈なし）
  ncd_fixed <- c(
    "花粉症","スギ花粉","ヒノキ花粉","アレルギー性鼻炎","アトピー性皮膚炎",
    "生活習慣病","糖尿病","高血圧","脂質異常症","動脈硬化","心筋梗塞",
    "脳梗塞","脳卒中","認知症","アルツハイマー","パーキンソン",
    "骨粗しょう症","変形性膝","うつ病","統合失調症","双極性障害","摂食障害",
    "繰り返す.*ぎっくり腰","ぎっくり腰.*クワバタ"
  )
  if (any(sapply(ncd_fixed, function(k) grepl(k, tl, fixed = TRUE)))) return(TRUE)
  if (grepl("熱中症.*(予防|対策|警戒|搬送|症状|リスク)", tl, perl = TRUE)) return(TRUE)

  # 16. がん（感染性でないもの）
  if (grepl(paste0("がん.*(検診|保険|免疫療法|手術)(?!.*感染)|",
                   "(乳がん|前立腺がん|大腸がん|肺がん|胃がん).*(検診|治療|予防)|",
                   "防止法.*がん|血管炎.*薬.*承認"),
            tl, perl = TRUE)) return(TRUE)

  # 17. 芸能人・有名人の個人健康エピソード（流行と無関係）
  if (grepl(paste0("(芸人|タレント|俳優|歌手|アイドル).{0,30}",
                   "(感染|腸炎|骨折|入院|手術).{0,30}",
                   "(漫才|ライブ|舞台|活動|復帰|告白|語る)"),
            tl, perl = TRUE)) return(TRUE)

  # 18. 食中毒予防・衛生管理の一般啓発（集団発症報告でない）
  if (grepl(paste0("食中毒.*(予防|気をつける|対策|注意.*[夏季]|衛生管理)|",
                   "衛生管理.*細心.*注意|弁当.*加熱.*菌"),
            tl, perl = TRUE)) return(TRUE)

  # 19. ペット・動物病院（人獣共通感染症の言及なし）
  if (grepl("(犬|猫|ペット|動物病院).{0,20}(病気|治療|手術|投薬)", tl, perl = TRUE) &&
      !grepl("人|ヒト|human|zoonot|人獣共通|公衆衛生", tl, perl = TRUE)) return(TRUE)

  # 20. 市場調査・産業レポート・ビジネス予測
  if (grepl(paste0("市場.{0,20}(成長見通し|規模.*[0-9]億|予測.*[0-9]億|急成長)|",
                   "(成長見通し|市場規模|市場分析|市場調査).{0,20}(市場|億|ドル)|",
                   "[0-9]+(億|兆).*(市場|ドル|成長)|",
                   "市場.*[0-9]+(億|兆).*成長"),
            tl, perl = TRUE)) return(TRUE)

  # 21. 原爆・戦争・社会史・文化（感染症と無関係）
  if (grepl(paste0("原爆.*(式典|慰霊|追悼)|被爆者.*(平均年齢|9万|記憶の継承)|",
                   "太宰治.*(温泉|療養|文人)|石垣.*(平和|追悼)|沖縄.*(平和の|追悼式)|",
                   "ハンセン病.*(差別|廃止|名誉回復)|らい予防法"),
            tl, perl = TRUE)) return(TRUE)

  # 22. 個人の病魔・体験談（流行事象でない）
  if (grepl(paste0("病魔の正体|年金ぐら.*命を落とす|苦しさ癒やした湯|",
                   "ぎっくり腰.*(クワバタ|芸人|タレント)|",
                   "(芸人|タレント).{0,20}ぎっくり腰"),
            tl, perl = TRUE)) return(TRUE)

  # 23. 人口・少子化・被爆者統計
  if (grepl(paste0("出生率.*(過去最低|最低)|被爆者.*(9万|平均年齢)|",
                   "子どもが減る社会|少子化.*(対策|統計)|",
                   "有効求人倍率|コカイン.*使用.*推計|薬物.*使用.*最多"),
            tl, perl = TRUE)) return(TRUE)

  # 24. 気象・環境（熱波・猛暑の死者数）
  if (grepl(paste0("(熱波|heat wave|heatwave).*(死亡|死者|多く死亡)|",
                   "(記録的.*熱波|猛暑).*(死亡|死者)|",
                   "europe.*drought|drought.*europe"),
            tl, perl = TRUE)) return(TRUE)

  # 25. 医療手術・移植（感染症でない）
  if (grepl(paste0("異種移植|ブタ.*(腎臓|心臓|臓器).*移植|",
                   "xenotransplant|pig.*kidney.*transplant"),
            tl, perl = TRUE)) return(TRUE)

  # 26. 健康研究（リスク低下・予防効果の研究論文）
  if (grepl(paste0("(コーヒー|緑茶|食品|栄養素|運動|睡眠).{0,30}(リスク.*低下|低下.*リスク|予防効果)|",
                   "(摂取|飲食).{0,20}(がん.*リスク|肝疾患.*リスク|心臓病.*リスク)"),
            tl, perl = TRUE)) return(TRUE)

  # 27. 社会福祉・育児施設（感染症と無関係）
  if (grepl(paste0("赤ちゃんポスト|こうのとりのゆりかご|",
                   "病児.*保護者.*負担|付き添い.*入院.*負担"),
            tl, perl = TRUE)) return(TRUE)

  # 28. 医療事故・医療訴訟（感染症と無関係）
  if (grepl(paste0("(死亡問題|医療事故).*(再開|検討|方針)|",
                   "髄くう内注射.*(問題|再開|死亡)|",
                   "医療センター.*死亡問題|小児.*死亡.*注射"),
            tl, perl = TRUE)) return(TRUE)

  # 29. ADHD・ディスレクシア・発達障害（感染症と無関係）
  if (grepl(paste0("adhd|ディスレクシア|発達障害.*(起業|就労|社会)|",
                   "目に見えない障害"),
            tl, perl = TRUE)) return(TRUE)

  # 30. ドクターヘリ・救急体制（感染症と無関係）
  if (grepl(paste0("ドクターヘリ.*(運休|検討|対応策)|",
                   "ヘリ.*運休.*厚労|救急ヘリ.*運休"),
            tl, perl = TRUE)) return(TRUE)

  # 31. 社説・コラム（政治・行政テーマ）
  if (grepl(paste0("＜社説＞|〔社説〕|【社説】|社説[　 ]|",
                   "社説.*都構想|社説.*維新|社説.*政治"),
            tl, perl = TRUE)) return(TRUE)

  # 32. 平和・戦争・ハンセン病（歴史・社会テーマ）
  if (grepl(paste0("ハンセン病.*(患者.*沖縄|沖縄戦|高校生|差別)|",
                   "知事.*平和宣言|平和宣言.*全文|玉城知事.*平和|",
                   "避難壕.*地上戦|沖縄.*地上戦"),
            tl, perl = TRUE)) return(TRUE)

  # 33. 薬害訴訟・補償（感染症の流行でなく賠償・法的問題）
  if (grepl(paste0("薬害エイズ.*(救済|充実|原告|厚労相)|",
                   "薬害.*原告団.*厚労|b型肝炎.*訴訟"),
            tl, perl = TRUE)) return(TRUE)

  # 34. 犯罪・詐欺・事故（感染症と無関係）
  if (grepl(paste0("詐欺.*(防ぐ|防止|被害)|コンビニ.*詐欺|特殊詐欺|",
                   "誤ってスプレー|郵便局.*スプレー.*搬送|化学物質.*搬送(?!.*感染)|",
                   "ストーカー.*逮捕|ストーカー.*有罪"),
            tl, perl = TRUE)) return(TRUE)

  # 35. 考古学・人類史（腸内細菌・歴史研究）
  if (grepl(paste0("ミイラ研究所.*腸内細菌|腸内細菌.*人類史.*古代|",
                   "古代人.*腸内|腸内.*読み解く.*人類史"),
            tl, perl = TRUE)) return(TRUE)

  # 36. がん治療薬・抗がん剤承認（感染症と無関係）
  if (grepl(paste0("ベネトクラクス|cll.*(承認|治療)|白血病.*(承認|新薬)|",
                   "抗がん剤.*(承認|取得)|欧州委員会.*承認.*(cll|白血病|腫瘍)"),
            tl, perl = TRUE)) return(TRUE)

  # 37. 戦争・記念コラム（沖縄空襲体験談・証言）
  if (grepl(paste0("戦争の記憶.*どう伝える|戦争.*記憶.*知恵を絞|",
                   "戦争の記憶.*局面|",
                   "うまんちゅの戦争体験|[0-9]+・[0-9]+空襲.*爆弾|",
                   "沖縄.*戦争体験.*証言"),
            tl, perl = TRUE)) return(TRUE)

  # 38. 最低賃金・労働経済
  if (grepl(paste0("最低賃金.*(議論|引き上げ|改定|審議|スタート)|",
                   "賃金引き上げ.*焦点"),
            tl, perl = TRUE)) return(TRUE)

  # 39. 生態系・野生動物管理（感染症なし）
  if (grepl(paste0("生態系.*対策.*イエネコ|希少種.*捕食.*追加|",
                   "環境省.*イエネコ"),
            tl, perl = TRUE)) return(TRUE)

  # 40. 啓発音楽イベント（感染症名+音楽・コンサート）
  if (grepl(paste0("音楽でつながる.*肝炎|肝炎プロジェクト.*ロマンディスコ|",
                   "知って.*肝炎.*プロジェクト.*(×|コラボ|音楽)"),
            tl, perl = TRUE)) return(TRUE)

  # 41. 食中毒季節コラム（論説・注意喚起コラム）
  if (grepl(paste0("観国之光.*食中毒|食中毒シーズン.*衛生管理.*細心|",
                   "食中毒.*論説委員|観光経済新聞.*論説.*食中毒"),
            tl, perl = TRUE)) return(TRUE)

  # 42. 慰霊の日・戦争慰霊コラム（沖縄）
  if (grepl(paste0("慰霊の日.*ことし|慰霊の日.*暑|八重山.*戦争.*慰霊|",
                   "沖縄.*慰霊の日.*[0-9]"),
            tl, perl = TRUE)) return(TRUE)

  # 43. 肝癌・非感染性腫瘍の一次予防・疫学
  if (grepl(paste0("prevention of liver cancer.*obesity|",
                   "肝癌.*肥満.*予防|非感染性.*腫瘍.*疫学"),
            tl, perl = TRUE)) return(TRUE)

  # 44. 患者向けQ&A・爪水虫・皮膚科単発
  if (grepl(paste0("病気のq.?a.*爪水虫|爪水虫.*薬が効かない|爪.*水虫.*q.?a"),
            tl, perl = TRUE)) return(TRUE)

  # 45. 母乳・乳児栄養研究
  if (grepl(paste0("母乳オリゴ糖|ヒトミルクオリゴ糖.*細胞|培養細胞.*母乳|",
                   "乳児.*母乳.*オリゴ糖"),
            tl, perl = TRUE)) return(TRUE)

  # 46. ウィルソン病・代謝性疾患ガイドライン
  if (grepl(paste0("wilson'?s disease.*guidance|ウィルソン病.*診療|",
                   "apasl.*wilson"),
            tl, perl = TRUE)) return(TRUE)

  # 47. 病理談話会・学術カンファレンス案内（感染症でない）
  if (grepl(paste0("病理談話会|病理.*カンファレンス.*案内|",
                   "臨床病理.*研究会.*[0-9]+回"),
            tl, perl = TRUE)) return(TRUE)

  # 48. 慢性疾患サミット・生活習慣病イベント
  if (grepl(paste0("me-byo.*サミット|未病サミット|生活習慣病.*サミット"),
            tl, perl = TRUE)) return(TRUE)

  # 49. 食物アレルギー警告（感染症でない）
  if (grepl(paste0("allergy alert.*undeclared|アレルギー.*表示漏れ.*回収|",
                   "食物アレルギー.*リコール"),
            tl, perl = TRUE)) return(TRUE)

  # 50. Lancet等医学誌批評・エビデンス批評コラム
  if (grepl(paste0("lancet.*印籠|医学誌.*乱用.*エビデンス|",
                   "lancet.*権威.*コロナ禍"),
            tl, perl = TRUE)) return(TRUE)

  # 51. 医療コラム・批評（感染対策器具の是非など）
  if (grepl(paste0("シューカバー.*合理的|感染対策.*やりすぎ.*シューカバー|",
                   "dr.*イワケン.*感染症のリアル"),
            tl, perl = TRUE)) return(TRUE)

  # 52. 病名が過去の背景・文脈としてのみ言及される記事（現在の流行と無関係）
  # 例:「鳥インフルエンザなどの衛生リスク」等、疾患名は登場するが記事の主題は別
  # （企業のビジネス戦略等）であるケース。「感染拡大の影響で中止」等の
  # より強いパターンは5cで無条件除外済み。
  if (grepl(
    "(鳥インフルエンザ|新型コロナ|インフルエンザ).{0,10}などの.{0,5}(衛生)?リスク",
    tl, perl = TRUE)) return(TRUE)

  # 53. PRニュースワイヤー等の受賞・式典告知（感染症と無関係な企業/文化イベント）
  if (grepl("(prnewswire|kyodonewsprwire).*(賞|award|ceremony)|(賞|award).*(prnewswire|kyodonewsprwire)",
            tl, perl = TRUE)) return(TRUE)

  FALSE
}

# ============================================================
# バッチ処理
# ============================================================

screen_batch <- function(df, title_col = "Signalタイトル", summary_col = "概要") {
  results <- lapply(seq_len(nrow(df)), function(i) {
    summary_text <- if (summary_col %in% names(df)) df[[summary_col]][i] else ""
    summary_text <- ifelse(is.na(summary_text), "", summary_text)
    screen_entry(title = df[[title_col]][i], summary = summary_text)
  })

  result_df <- do.call(rbind, lapply(results, as.data.frame, stringsAsFactors = FALSE))
  cbind(df, result_df)
}

# ============================================================
# テスト実行（直接実行時のみ）
# ============================================================

if (sys.nframe() == 0) {
  test_cases <- list(
    list(
      title   = "Cholera – Benin",
      summary = "2021年9月から2022年1月に1430例報告、うち20例死亡（CFR 1.4%）。WASH対策不十分な地域でのアウトブレイク。"
    ),
    list(
      title   = "Denmark SSI Reports Novel Swine Influenza A(H1N2) Virus",
      summary = "デンマークで新型豚インフルエンザA(H1N2)ウイルスが報告された。ヒト感染例あり。ヒト-ヒト感染は未確認。"
    ),
    list(
      title   = "Ebola outbreak – DRC",
      summary = "コンゴ民主共和国でエボラ出血熱のアウトブレイク。10例報告、うち4例死亡。"
    ),
    list(
      title   = "Food poisoning at school – Japan",
      summary = "日本の小学校で集団食中毒が発生。給食を食べた児童80名が嘔吐・下痢を訴えた。"
    )
  )

  cat("=== EBS ルールベーススクリーニング テスト ===\n\n")
  for (tc in test_cases) {
    cat("タイトル:", tc$title, "\n")
    r <- screen_entry(tc$title, tc$summary)
    cat("  Unusual:        ", r$unusual_unexpected, "\n")
    cat("  Serious (国):   ", r$serious_ph_country, "\n")
    cat("  Serious (Japan):", r$serious_ph_japan, "\n")
    cat("  Epidemic-prone: ", r$epidemic_prone, "\n")
    cat("  Mass exposure:  ", r$mass_exposure, "\n")
    cat("  High profile:   ", r$high_profile, "\n")
    cat("  Special path:   ", r$special_pathogen, "\n")
    cat("  Signal重みづけ: ", r$signal_weight, "\n")
    cat("  疾患分類:       ", r$disease_category, "/", r$disease_name_en, "/", r$disease_name_ja, "\n")
    cat("  場所:           ", r$location, "(", r$region, ")\n")
    cat("\n")
  }
}
