# ============================================================
# 日本感染症サーベイランスダッシュボード
# IBS（指標ベース）+ EBS（イベントベース）統合
# ============================================================

library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(sf)
library(DT)
library(lubridate)
library(scales)
library(RColorBrewer)
library(shinyjs)

source("R/data_loader.R")
source("R/rt_estimation.R")
source("R/ebs_rule_screening.R")   # ebs_loader より先にロード（screen_entry を使うため）
source("R/ebs_loader.R")
source("R/zensu_loader.R")
source("R/iasr_loader.R")
source("R/ari_pathogen_loader.R")
source("R/hosp_loader.R")
source("R/std_loader.R")
source("R/change_tracker.R")
source("R/forecast_ts.R")
source("R/idsc_links_data.R")

# shinyapps.io 上での実行かどうかを判定
# R_CONFIG_ACTIVE, HOME パス, またはアプリIDのいずれかで判定
IS_SHINYAPPS <- nchar(Sys.getenv("SHINYAPPS_APPLICATION_ID")) > 0 ||
                grepl("^/srv/", getwd()) ||
                nchar(Sys.getenv("R_CONFIG_ACTIVE")) > 0

# 国内・海外タブの振り分けロジック（is_overseas_article, detect_pref, strip_gnews_suffix,
# JAPAN_KEYWORDS/OVERSEAS_KEYWORDS相当）は R/ebs_loader.R 側の定義（.JAPAN_KW_LOADER /
# .OVERSEAS_KW_LOADER）に一本化している。以前はここに同名の重複定義があり、
# source("R/ebs_loader.R") の後に評価されるためこちらが常に上書きしてしまい、
# ebs_loader.R側の修正（Google Newsメディア名サフィックス除去等）が反映されない
# バグの原因になっていた。

cat("IBS データ読み込み中（キャッシュ）...\n")
SURV_DATA_RAW <- load_all_cached()
# JIHS週報CSVの「総数」行（全国の定点あたり報告数の公式値）はpref_name=="全国"として
# 別枠に分離する。SURV_DATA自体は従来通り47都道府県のみを保持し、既存の
# 都道府県別集計・Rt推定等のロジックに影響を与えないようにする
SURV_DATA_NATIONAL <- if (!is.null(SURV_DATA_RAW)) SURV_DATA_RAW %>% dplyr::filter(pref_name == "全国") else NULL
SURV_DATA <- if (!is.null(SURV_DATA_RAW)) SURV_DATA_RAW %>% dplyr::filter(pref_name != "全国") else SURV_DATA_RAW
IS_REAL_DATA <- !is.null(SURV_DATA) && nrow(SURV_DATA) > 0
tryCatch(
  record_data_change("ibs", compute_recent_signature(SURV_DATA, "date", 90)),
  error = function(e) NULL
)
cat("地図データ読み込み中...\n")
JAPAN_MAP <- tryCatch({
  # キャッシュ済み地図データを優先使用（rnaturalearthhires不要）
  if (file.exists("data/japan_map.rds")) {
    readRDS("data/japan_map.rds")
  } else {
    m <- rnaturalearth::ne_states(country="Japan", returnclass="sf")
    m %>% mutate(pref_name=name_ja) %>% select(pref_name, geometry)
  }
}, error = function(e) NULL)
cat("EBS データ読み込み中（キャッシュ）...\n")
EBS_STARTUP_CACHE <- "data/ebs_startup_cache.rds"
EBS_CACHE <- local({
  d <- if (file.exists(EBS_STARTUP_CACHE)) {
    tryCatch(readRDS(EBS_STARTUP_CACHE), error=function(e) make_demo_ebs())
  } else {
    make_demo_ebs()
  }
  # 非感染症記事をキャッシュから除去（is_noise_article は ebs_rule_screening.R で定義）
  if (!is.null(d) && nrow(d) > 0) {
    d <- d %>% dplyr::filter(
      !mapply(is_noise_article, title, coalesce(summary, ""))
    )
  }

  # 旧分類（高/中/低/参考）を新分類（Event/Signal/FYI）に変換
  if (!is.null(d) && "signal_level" %in% names(d)) {
    lv <- as.character(d$signal_level)
    lv <- dplyr::case_when(
      lv == "高"   ~ "Signal High",
      lv %in% c("中","低") ~ "Signal Low",
      lv == "参考" ~ "FYI",
      lv == "Event"  ~ "Signal High",
      lv == "Signal" ~ "Signal Low",
      lv %in% c("Signal High","Signal Low","FYI") ~ lv,
      TRUE ~ "FYI"
    )
    d$signal_level <- factor(lv, levels = c("Signal High","Signal Low","FYI"))
    d$signal_weight <- sapply(lv, function(x) c("Signal High"=3,"Signal Low"=2,FYI=0.5)[x])
  }
  d
})

# EBSデータにスクリーニング列を付与するヘルパー
apply_ebs_screening <- function(d) {
  if (is.null(d) || nrow(d) == 0) return(d)
  screens <- lapply(seq_len(nrow(d)), function(i) {
    screen_entry(
      title       = coalesce(d$title[i],       ""),
      summary     = coalesce(d$summary[i],     ""),
      source_id   = coalesce(d$source_id[i],   ""),
      source_name = coalesce(d$source_name[i], ""),
      lang        = coalesce(d$lang[i],        "")
      # title は screen_entry 内で直接参照
    )
  })
  sc <- do.call(rbind, lapply(screens, as.data.frame, stringsAsFactors = FALSE))
  d$ebs_unusual   <- sc$unusual_unexpected
  d$ebs_serious_c <- sc$serious_ph_country
  d$ebs_serious_j <- sc$serious_ph_japan
  d$ebs_epidemic  <- sc$epidemic_prone
  d$ebs_mass      <- sc$mass_exposure
  d$ebs_high      <- sc$high_profile
  d$ebs_special   <- sc$special_pathogen
  d$signal_level  <- factor(sc$signal_weight, levels = c("Signal High","Signal Low","FYI"))
  d$signal_weight <- sapply(sc$signal_weight,
                            function(x) c("Signal High"=3,"Signal Low"=2,FYI=0.5)[x])
  d
}

# キャッシュにスクリーニング列がなければ付与（新規取得済みデータは不要）
if (!is.null(EBS_CACHE) && !"ebs_unusual" %in% names(EBS_CACHE)) {
  cat("EBSスクリーニング適用中...\n")
  EBS_CACHE <- apply_ebs_screening(EBS_CACHE)
}
tryCatch(
  record_data_change("ebs", compute_recent_signature(EBS_CACHE, "pub_date", 60)),
  error = function(e) NULL
)

# EBSキャッシュの保持日数。EBSスコア評価系（zensu_ibs_band・ebs_score_for等）の
# 「評価不能」判定もこの値に連動させ、実際にキャッシュが持つ範囲までは評価可能にする
# （数日分のバッファを見て少し短めに設定）
EBS_CACHE_KEEP_DAYS      <- 365
EBS_SCORE_EVALUABLE_DAYS <- 360

# ローリングキャッシュ管理（過去1年保持・重複削除）
merge_ebs_cache <- function(new_data, cache_path = EBS_STARTUP_CACHE, keep_days = EBS_CACHE_KEEP_DAYS) {
  cutoff <- Sys.Date() - keep_days
  old <- tryCatch(readRDS(cache_path), error = function(e) NULL)
  combined <- if (!is.null(old) && nrow(old) > 0) {
    dplyr::bind_rows(old, new_data)
  } else {
    new_data
  }
  combined <- combined %>%
    dplyr::filter(is.na(pub_date) | pub_date >= cutoff) %>%
    dplyr::distinct(title, source_id, .keep_all = TRUE)
  # フィード表示範囲外に出て再取得されなくなった過去記事にも、screen_entry() の
  # ロジック修正が反映されるよう、マージ後の全件を再スクリーニングする
  combined <- rescreen_ebs_data(combined)
  combined %>% dplyr::arrange(signal_level, dplyr::desc(pub_date))
}

save_ebs_cache <- function(data, cache_path = EBS_STARTUP_CACHE) {
  tryCatch({
    dir.create(dirname(cache_path), showWarnings = FALSE, recursive = TRUE)
    saveRDS(data, cache_path)
  }, error = function(e) message("キャッシュ保存エラー: ", e$message))
}
cat("全数把握 データ取得中...\n")
ZENSU_DATA <- tryCatch(load_all_zensu_cached(), error=function(e) { message("全数 ERROR:", e$message); NULL })
tryCatch(
  record_data_change("zensu", compute_recent_signature(ZENSU_DATA, "date", 90)),
  error = function(e) NULL
)
cat("IASR データ取得中...\n")
IASR_DATA <- tryCatch(load_all_iasr(), error=function(e) { message("IASR ERROR:", e$message); NULL })
tryCatch(
  record_data_change("iasr", compute_recent_signature(IASR_DATA, "date", 180)),
  error = function(e) NULL
)
cat("ARI病原体 データ読み込み中（キャッシュ）...\n")
ARI_PATHOGEN_DATA <- tryCatch(load_ari_pathogen_cached(), error=function(e) { message("ARI病原体 ERROR:", e$message); NULL })
tryCatch(
  record_data_change("ari_pathogen", compute_recent_signature(ARI_PATHOGEN_DATA$counts, "date", 180)),
  error = function(e) NULL
)
cat("入院サーベイランス データ読み込み中（キャッシュ）...\n")
HOSP_DATA <- tryCatch(load_hosp_cached(), error=function(e) { message("入院サーベイランス ERROR:", e$message); NULL })
tryCatch(
  record_data_change("hosp", compute_recent_signature(HOSP_DATA, "date", 180)),
  error = function(e) NULL
)
cat("月報疾患 データ読み込み中（キャッシュ）...\n")
STD_DATA <- tryCatch(load_std_cached(), error=function(e) { message("月報疾患 ERROR:", e$message); NULL })
tryCatch(
  record_data_change("std", compute_recent_signature(STD_DATA, "date", 400)),
  error = function(e) NULL
)
cat("準備完了\n")

CURRENT_YEAR <- if (!is.null(SURV_DATA)) max(SURV_DATA$year, na.rm=TRUE) else as.integer(format(Sys.Date(),"%Y"))
CURRENT_WEEK <- if (!is.null(SURV_DATA)) { SURV_DATA %>% filter(year==CURRENT_YEAR) %>% pull(week) %>% max(na.rm=TRUE) } else 1L
ALL_DISEASE_IDS <- names(DISEASE_CONFIG)

DISEASE_GROUPS <- list(
  respiratory = list(label="呼吸器系",
    ids=c("flu","rsv","phar_conj","strep","mycop","chlamydia","covid","ari")),
  digestive   = list(label="消化器系",
    ids=c("gi","gi_rota")),
  pediatric   = list(label="小児感染症",
    ids=c("hfmd","erythema","roseola","herp_ang","varicella","mumps")),
  eye         = list(label="眼科系",
    ids=c("hem_conj","epid_conj")),
  neuro       = list(label="神経系",
    ids=c("bact_mening","asep_mening"))
)
# 実際にDISEASE_CONFIGに存在する疾患のみに絞る
DISEASE_GROUPS <- lapply(DISEASE_GROUPS, function(g) {
  list(label=g$label, ids=g$ids[g$ids %in% names(DISEASE_CONFIG)])
})

# 定点把握 Rt 対象疾患（SIまたは潜伏期間推定値あり）
RT_DISEASE_IDS <- c(
  "flu","rsv","gi","varicella","hfmd","mumps","mycop","covid",
  "phar_conj","strep","erythema","roseola","herp_ang",
  "hem_conj","epid_conj","bact_mening","asep_mening",
  "chlamydia","gi_rota","ari"
)
RT_DISEASE_IDS <- RT_DISEASE_IDS[RT_DISEASE_IDS %in% names(DISEASE_CONFIG) &
                                   RT_DISEASE_IDS %in% names(SERIAL_INTERVALS)]
RT_DISEASE_CHOICES <- setNames(RT_DISEASE_IDS,
  sapply(RT_DISEASE_IDS, function(x) DISEASE_CONFIG[[x]]$label))

# 全数把握 Rt 対象疾患（SIまたは潜伏期間推定値あり）
RT_ZENSU_IDS <- c("measles","rubella","pertussis","mpox","hep_a","dengue","igas","ehec")
RT_ZENSU_IDS <- RT_ZENSU_IDS[RT_ZENSU_IDS %in% names(ZENSU_DISEASE_CONFIG) &
                               RT_ZENSU_IDS %in% names(SERIAL_INTERVALS)]
RT_ZENSU_CHOICES <- setNames(RT_ZENSU_IDS,
  sapply(RT_ZENSU_IDS, function(x) ZENSU_DISEASE_CONFIG[[x]]$label))

# 「定点把握疾患」選択肢に混在する月次報告（性感染症・薬剤耐性菌）の疾患ID一覧
# （UI側のconditionalPanelで週次/月次表示を振り分けるためのJS配列リテラル）
std_ids_js <- paste0("[", paste0("'", names(STD_DISEASE_CONFIG), "'", collapse = ","), "]")

# ============================================================
# UI
# ============================================================
ui <- dashboardPage(
  title = "EI活動支援 IBS/EBS統合ダッシュボード",
  skin = "blue",
  dashboardHeader(
    title = tags$span(style="white-space:normal;line-height:1.25;text-align:center;",
      "Epidemic Intelligence活動支援", tags$br(), "IBS/EBS統合ダッシュボード"),
    titleWidth = 420,
    tags$li(class="dropdown",
      tags$li(class="dropdown",
        uiOutput("auto_refresh_status", inline=TRUE)
      )
    )
  ),

  dashboardSidebar(width=240,
    tags$div(class="sidebar-section-title", style="padding-left:15px", "表示モード"),
    # ── ts_mode（定点/全数の切り替え）を2箇所に分割配置 ──────────
    # 通常のradioButtons()は1つの<div>にまとめて描画されるため2箇所に
    # 分割できない。native radioはname属性が同じであればDOM上の位置に
    # 関係なく1つのグループとして排他選択される性質を利用し、素の
    # <input type="radio" name="ts_mode">を手書きして離れた場所に配置する。
    # Shiny側へはonchangeでShiny.setInputValue()を呼んで反映する。
    tags$div(style="padding-left:15px;",
      tags$label(class="radio-inline",
        tags$input(type="radio", name="ts_mode", value="teiten", checked=NA,
          onchange="Shiny.setInputValue('ts_mode', this.value, {priority:'event'});"),
        "定点把握疾患"
      )
    ),

    selectInput("disease", NULL,
      choices = list(
        "週次報告" = setNames(names(DISEASE_CONFIG), sapply(DISEASE_CONFIG, `[[`, "label")),
        "月次報告（性感染症・薬剤耐性菌）" = setNames(names(STD_DISEASE_CONFIG), sapply(STD_DISEASE_CONFIG, `[[`, "label"))
      ),
      selected="flu"),

    tags$div(style="padding-left:15px;",
      tags$label(class="radio-inline",
        tags$input(type="radio", name="ts_mode", value="zensu",
          onchange="Shiny.setInputValue('ts_mode', this.value, {priority:'event'});"),
        "全数把握疾患"
      )
    ),
    tags$script(HTML(
      "$(document).one('shiny:sessioninitialized', function() { Shiny.setInputValue('ts_mode', 'teiten'); });"
    )),
    tags$div(style="padding:2px 8px 4px;",
      radioButtons("zensu_class", NULL,
        choices = c("全て","1類","2類","3類","4類","5類"),
        selected = "全て", inline = TRUE)
    ),
    selectInput("zensu_disease_ts", NULL,
      choices = local({
        classes <- c("1類","2類","3類","4類","5類全数")
        class_labels <- c("1類感染症","2類感染症","3類感染症","4類感染症","5類感染症（全数）")
        grp <- setNames(vector("list", length(classes)), class_labels)
        for (i in seq_along(classes)) {
          ids <- names(Filter(function(x) x$class == classes[i], ZENSU_DISEASE_CONFIG))
          grp[[class_labels[i]]] <- setNames(ids, sapply(ids, function(x) ZENSU_DISEASE_CONFIG[[x]]$label))
        }
        grp
      }),
      selected="measles", width="100%"),

    tags$div(class="sidebar-section-title", style="padding-left:15px", "期間"),
    sliderInput("date_range", NULL,
      min   = as.Date("2001-01-01"),
      max   = as.Date(paste0(format(Sys.Date(), "%Y"), "-12-31")),
      value = c(Sys.Date() - 365*3, Sys.Date()),
      timeFormat = "%Y/%m", step = 7, width = "100%"),
    tags$div(style="padding:0 15px 6px 15px;text-align:center;",
      actionButton("date_range_reset", "最新表示に戻す", icon=icon("rotate-right"),
        class="btn-default btn-sm", style="font-size:0.78em;padding:3px 10px;")),

    tags$div(class="sidebar-section-title", style="padding-left:15px", "都道府県"),
    selectInput("pref_filter",NULL,
      choices=c("全国", PREF_MASTER$pref_name), selected="全国"),


    hr(),
    tags$div(class="sidebar-section-title", style="padding-left:15px", "データ更新"),
    tags$div(style="padding:4px 15px;",
      # ローカル実行時のみ更新ボタンを表示
      if (!IS_SHINYAPPS) tagList(
        actionButton("surv_refresh", "定点データ更新", icon=icon("rotate"),
          class="btn-warning btn-sm btn-block", style="margin-bottom:4px;"),
        actionButton("zensu_refresh", "全数データ更新", icon=icon("rotate"),
          class="btn-warning btn-sm btn-block", style="margin-bottom:4px;"),
        actionButton("ebs_refresh", "EBSニュース更新", icon=icon("newspaper"),
          class="btn-primary btn-sm btn-block", style="margin-bottom:4px;"),
        actionButton("gtrends_refresh", "Google Trends 更新", icon=icon("arrow-trend-up"),
          class="btn-info btn-sm btn-block", style="margin-bottom:4px;"),
        actionButton("hosp_update", "入院データ更新", icon=icon("rotate"),
          class="btn-warning btn-sm btn-block"),
        tags$hr(style="margin:6px 0;")
      ),
      uiOutput("data_fetch_times")
    )
  ),

  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$link(rel="stylesheet", href="custom.css"),
      tags$script(src="js/ebs_cards.js"),
      tags$script(src="js/ebs_trend_chart.js"),
      tags$link(href="https://fonts.googleapis.com/css2?family=Noto+Sans+JP:wght@400;700&display=swap",
                rel="stylesheet"),
      # Notes内の計算式を数式として表示するためのMathJax（\( \) と \[ \] のみをmath区切りとして使用し、
      # 本文中の "$" 記号との衝突を避ける）。設定スクリプトは本体の読み込みより前に置く必要がある。
      tags$script(HTML("
        window.MathJax = {
          tex: { inlineMath: [['\\\\(', '\\\\)']], displayMath: [['\\\\[', '\\\\]']] }
        };
      ")),
      tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/3.2.2/es5/tex-mml-chtml.js"),
      tags$script(HTML('
/* ── 状態の保存・復元（localStorage） ── */
var _survSaveTimer;
var _survRestored = false;

function survCollectState() {
  var s = {};
  // ラジオボタン
  var tsMode = document.querySelector("input[name=\\"ts_mode\\"]:checked");
  if (tsMode) s.ts_mode = tsMode.value;
  // セレクト
  ["disease","zensu_disease_ts","pref_filter",
   "ebs_signal","ebs_disease_filter","ebs_page_size",
   "ebs_ov_signal","ebs_ov_disease","ebs_ov_page_size",
   "multi_group","multi_view","rt_disease","rt_zensu_disease"
  ].forEach(function(id) {
    var el = document.getElementById(id);
    if (el) s[id] = el.value;
  });
  // チェックボックス
  ["ebs_recent_only","ebs_show_pubmed","ebs_ov_recent_only",
   "ebs_official_only","ebs_ov_official_only"].forEach(function(id) {
    var el = document.getElementById(id);
    if (el) s[id] = el.checked;
  });
  // アクティブタブ（ネストしたタブがあるため、外側main_tabsのnavに限定して取得）
  var activeTab = document.querySelector("#main_tabs > .nav-tabs > li.active > a");
  if (activeTab) s.main_tabs = activeTab.getAttribute("data-value");
  return s;
}

function survSaveState() {
  try {
    localStorage.setItem("japanSurv_state", JSON.stringify(survCollectState()));
  } catch(e) {}
}

// 入力変化をデバウンスして保存
$(document).on("change", "input, select", function() {
  clearTimeout(_survSaveTimer);
  _survSaveTimer = setTimeout(survSaveState, 600);
});
// タブクリックも保存
$(document).on("click", ".nav-tabs a", function() {
  clearTimeout(_survSaveTimer);
  _survSaveTimer = setTimeout(survSaveState, 400);
});

// 初回接続時に復元
$(document).on("shiny:sessioninitialized", function() {
  if (_survRestored) return;
  _survRestored = true;
  try {
    var saved = localStorage.getItem("japanSurv_state");
    if (saved) {
      Shiny.setInputValue("_restored_state", JSON.parse(saved), {priority: "event"});
    }
  } catch(e) {}
});

// ── EBSカード翻訳 ──
async function ebsTranslateCards(containerId) {
  var els = Array.from(document.querySelectorAll("#" + containerId + " .ebs-tr"));
  await Promise.all(els.map(async function(el) {
    var text = (el.getAttribute("data-orig") || el.textContent).trim();
    if (!text) return;
    el.setAttribute("data-orig", text);
    try {
      var r = await fetch(
        "https://translate.googleapis.com/translate_a/single?client=gtx&sl=auto&tl=ja&dt=t&q=" +
        encodeURIComponent(text)
      );
      var d = await r.json();
      el.textContent = d[0].map(function(x){ return x[0]; }).join("");
    } catch(e) { console.warn("translate error", e); }
  }));
}
function goToNotes(anchorId) {
  var tabLink = document.querySelector("a[data-value=\'Notes\']");
  if (tabLink) { tabLink.click(); }
  setTimeout(function() {
    var el = document.getElementById(anchorId);
    if (el) el.scrollIntoView({ behavior: "smooth", block: "start" });
  }, 350);
}
function ebsUntranslateCards(containerId) {
  Array.from(document.querySelectorAll("#" + containerId + " .ebs-tr")).forEach(function(el) {
    var orig = el.getAttribute("data-orig");
    if (orig) { el.textContent = orig; el.removeAttribute("data-orig"); }
  });
}
      '))
    ),

    # Plotly.jsの依存読み込みを常に先読みさせるためのダミー出力（非表示）。
    # 静的化したPlotly.reactベースのグラフ（EBSトレンド等）は、タブ内に
    # plotlyOutputが1つも無いとPlotly.js自体が遅延ロードされず初回描画に
    # 失敗するため、常にレンダリングされる位置にこれを置いて確実に読み込む
    tags$div(style="display:none;", plotlyOutput("plotly_dep_loader", height="1px")),

    uiOutput("data_source_banner"),

    # ── KPIカード ──────────────────────────────────────────
    # 1行目: 疾患・地域 + IBS・EBS統合活動レベル
    # 2行目: IBS総合判定 + IBS3枚（報告数・Rt・基準値）+ EBS（トレンド）
    fluidRow(
      column(3, uiOutput("kpi_pref")),
      column(9, uiOutput("kpi_integrated"))
    ),
    fluidRow(
      column(2, uiOutput("kpi_alert")),
      column(2, uiOutput("kpi_national")),
      column(2, uiOutput("kpi_rt")),
      column(3, uiOutput("kpi_threshold")),
      column(3, uiOutput("kpi_ebs_trend"))
    ),

    # ── タブ群 ─────────────────────────────────────────────
    tabBox(width=12, id="main_tabs",

      # ── 活動レベル一覧 ─────────────────────────────────────
      tabPanel("活動レベル一覧", icon=icon("th"),
        tabsetPanel(id="sub_levels", type="tabs",
          tabPanel("疾患別",
            tags$div(style="padding:10px 6px 4px;",
              uiOutput("all_levels_header"),
              uiOutput("all_levels_ui")
            )
          ),
          tabPanel("都道府県別",
            tags$div(style="padding:10px 6px 4px;",
              uiOutput("pref_levels_header"),
              uiOutput("pref_levels_ui")
            )
          )
        )
      ),

      # ── 患者サーベイランス ─────────────────────────────────
      tabPanel("患者サーベイランス", icon=icon("chart-line"),
      tabsetPanel(id="sub_patient", type="tabs",

      # ── 地図（地域別比較） ─────────────────────────────────
      tabPanel("地域別比較", icon=icon("map"),
        tags$div(style="text-align:right;font-size:0.78em;margin:2px 4px 0;",
          tags$a(href="javascript:void(0)", onclick="goToNotes('notes-ibs')",
            style="color:#888;text-decoration:none;",
            icon("circle-info"), " 注意事項・データソース")),
        uiOutput("filter_bar_map"),
        fluidRow(
          column(12,
            uiOutput("map_week_selector_ui")
          )
        ),
        fluidRow(
          column(8,
            uiOutput("map_source_bar"),
            leafletOutput("choropleth_map", height="480px")
          ),
          column(4,
            uiOutput("ranking_title_ui"),
            DTOutput("ranking_table")
          )
        ),
        # 都道府県別ヒートマップ・地域別比較（定点把握疾患。週次/月次報告は
        # input$diseaseの選択IDで振り分ける）
        conditionalPanel(sprintf("input.ts_mode === 'teiten' && %s.indexOf(input.disease) === -1", std_ids_js),
          fluidRow(
            column(6, tags$h5("都道府県別ヒートマップ",style="font-weight:700;margin-top:16px"),
                   plotlyOutput("heatmap_plot", height="340px")),
            column(6, tags$h5("地域別比較",style="font-weight:700;margin-top:16px"),
                   plotlyOutput("region_plot", height="340px"))
          )
        ),
        conditionalPanel(sprintf("input.ts_mode === 'teiten' && %s.indexOf(input.disease) !== -1", std_ids_js),
          fluidRow(
            column(6, tags$h5("都道府県別ヒートマップ",style="font-weight:700;margin-top:16px"),
                   plotlyOutput("std_heatmap_plot", height="340px")),
            column(6, tags$h5("地域別比較",style="font-weight:700;margin-top:16px"),
                   plotlyOutput("std_region_plot", height="340px"))
          )
        ),
        # 都道府県別ヒートマップ・地域別比較（全数把握疾患）
        conditionalPanel("input.ts_mode === 'zensu'",
          fluidRow(
            column(6, tags$h5("都道府県別ヒートマップ",style="font-weight:700;margin-top:16px"),
                   plotlyOutput("zensu_heatmap_plot", height="340px")),
            column(6, tags$h5("地域別比較",style="font-weight:700;margin-top:16px"),
                   plotlyOutput("zensu_region_plot", height="340px"))
          )
        )
      ),

      # ── 流行曲線 ───────────────────────────────────────────
      tabPanel("流行曲線", icon=icon("chart-line"),
        tags$div(style="text-align:right;font-size:0.78em;margin:2px 4px 0;",
          tags$a(href="javascript:void(0)", onclick="goToNotes('notes-ibs')",
            style="color:#888;text-decoration:none;",
            icon("circle-info"), " 定点把握 注意事項"),
          tags$span(style="color:#ccc;margin:0 4px;", "|"),
          tags$a(href="javascript:void(0)", onclick="goToNotes('notes-zensu')",
            style="color:#888;text-decoration:none;",
            icon("circle-info"), " 全数把握 注意事項")),
        uiOutput("filter_bar_ts"),
        tags$div(style="display:flex;align-items:center;gap:12px;flex-wrap:wrap;margin:2px 4px 6px;padding:6px 10px;background:#f4f4f4;border-radius:6px;",
          tags$div(style="display:flex;align-items:center;gap:6px;",
            checkboxInput("show_forecast", "予測を表示（4週後まで）", value = FALSE, width = "auto")
          ),
          conditionalPanel("input.show_forecast",
            tags$div(style="display:flex;align-items:center;gap:6px;",
              tags$label("予測手法:", style="font-size:0.85em;margin:0;color:#555;"),
              selectInput("forecast_method", NULL,
                choices = c("アンサンブル（推奨）"="ensemble", "ポアソン回帰（GLM）"="poisson",
                            "指数平滑法（Holt法）"="holt", "Rtベース（実効再生産数）"="rt"),
                selected = "ensemble", width = "230px")
            )
          ),
          tags$a(href="javascript:void(0)", onclick="goToNotes('notes-forecast')",
            style="font-size:0.78em;color:#888;text-decoration:none;",
            icon("circle-info"), " 予測手法について"),
          tags$div(style="margin-left:auto;",
            downloadButton("download_csv", "CSVダウンロード", class="btn-xs btn-default", icon=icon("download"))
          )
        ),
        # 定点把握（線グラフ＋年別重ね合わせ）。「定点把握疾患」選択肢には
        # 週次報告の疾患と月次報告（性感染症・薬剤耐性菌）の疾患が混在するため、
        # 選択中の疾患IDで週次/月次のどちらの表示にするかをJS側で振り分ける
        # （都道府県別ヒートマップ・地域別比較は「地域別比較」タブに移設）
        conditionalPanel(sprintf("input.ts_mode === 'teiten' && %s.indexOf(input.disease) === -1", std_ids_js),
          fluidRow(column(12,
            tags$h5("定点把握疾患 — 週次報告数（定点あたり）", style="font-weight:700;margin-bottom:2px"),
            uiOutput("ts_source_bar"),
            plotlyOutput("timeseries_plot", height="300px"),
            uiOutput("timeseries_legend")
          )),
          fluidRow(column(12,
            tags$h5("年別重ね合わせ（週次・定点あたり報告数）", style="font-weight:700;margin-top:16px"),
            plotlyOutput("yearly_overlay_plot", height="320px")
          ))
        ),
        # 全数把握（流行曲線バーグラフ）
        conditionalPanel("input.ts_mode === 'zensu'",
          fluidRow(column(12,
            uiOutput("zensu_ts_title_ui"),
            plotlyOutput("zensu_ts_plot", height="380px"),
            uiOutput("zensu_ts_legend")
          )),
          fluidRow(column(12,
            tags$h5("年別重ね合わせ（週次報告数）", style="font-weight:700;margin-top:16px"),
            plotlyOutput("zensu_yearly_overlay_plot", height="320px")
          ))
        ),
        # 月次報告（性感染症・薬剤耐性菌）
        conditionalPanel(sprintf("input.ts_mode === 'teiten' && %s.indexOf(input.disease) !== -1", std_ids_js),
          tags$div(style="text-align:right;font-size:0.78em;margin:2px 4px 0;",
            tags$a(href="javascript:void(0)", onclick="goToNotes('notes-std')",
              style="color:#888;text-decoration:none;",
              icon("circle-info"), " 注意事項・データソース")),
          fluidRow(column(12,
            uiOutput("std_ts_title_ui"),
            plotlyOutput("std_ts_plot", height="380px"),
            uiOutput("std_ts_legend")
          )),
          fluidRow(column(12,
            tags$h5("年別重ね合わせ（月次報告数）", style="font-weight:700;margin-top:16px"),
            plotlyOutput("std_yearly_overlay_plot", height="320px")
          ))
        )
      ),

      # ── 複数疾患比較（新） ───────────────────────────────
      tabPanel("複数疾患比較（定点）", icon=icon("layer-group"),
        tags$div(style="text-align:right;font-size:0.78em;margin:2px 4px 0;",
          tags$a(href="javascript:void(0)", onclick="goToNotes('notes-ibs')",
            style="color:#888;text-decoration:none;",
            icon("circle-info"), " 注意事項・データソース")),
        uiOutput("filter_bar_multi"),
        fluidRow(
          column(12,
            tags$div(class="data-source-bar",
              "全疾患の同時比較（全国平均定点あたり報告数）"),
            fluidRow(
              column(3,
                selectInput("multi_group","疾患グループ:",
                  choices=c("すべて"="all",
                    setNames(names(DISEASE_GROUPS),
                             sapply(DISEASE_GROUPS, `[[`, "label"))),
                  selected="all", width="100%")
              ),
              column(9,
                checkboxGroupInput("multi_diseases","比較する疾患:",
                  choices=setNames(names(DISEASE_CONFIG),
                                   sapply(DISEASE_CONFIG,`[[`,"label")),
                  selected=names(DISEASE_CONFIG), inline=TRUE)
              )
            ),
            selectInput("multi_view","表示形式:",
              choices=c("重ね合わせ"="overlay_raw",
                        "2×2 グリッド"="facet"),
              selected="overlay_raw", width="250px")
          )
        ),
        fluidRow(column(12, plotlyOutput("multi_disease_plot", height="420px"))),
        fluidRow(
          column(6,
            tags$h5("現在週 疾患横断サマリー",style="font-weight:700;margin-top:12px"),
            DTOutput("multi_summary_table")
          ),
          column(6)
        )
      ),

      # ── 実効再生産数 ─────────────────────────────────────
      tabPanel("実効再生産数 Rt", icon=icon("chart-area"),
        tags$div(style="text-align:right;font-size:0.78em;margin:2px 4px 0;",
          tags$a(href="javascript:void(0)", onclick="goToNotes('notes-rt')",
            style="color:#888;text-decoration:none;",
            icon("circle-info"), " 注意事項・データソース")),
        uiOutput("filter_bar_rt"),
        fluidRow(
          column(4,
            # 定点把握モード
            conditionalPanel("input.ts_mode === 'teiten'",
              selectInput("rt_disease", "定点把握疾患:",
                choices = RT_DISEASE_CHOICES, selected = "flu", width = "100%")
            ),
            # 全数把握モード
            conditionalPanel("input.ts_mode === 'zensu'",
              selectInput("rt_zensu_disease", "全数把握疾患:",
                choices = RT_ZENSU_CHOICES, selected = "measles", width = "100%")
            )
          ),
          column(8,
            tags$div(class="data-source-bar",
              "Cori法 週次Rt推定 | 95%CI | Rt > 1 の継続は流行拡大を示します")
          )
        ),
        fluidRow(column(12,
          plotlyOutput("rt_plot_single", height="400px")
        )),
        fluidRow(column(12,
          uiOutput("rt_si_info")
        ))
      )

      ) # sub_patient
      ),

      # ── 病原体サーベイランス ───────────────────────────────
      tabPanel("病原体サーベイランス", icon=icon("flask"),
      tabsetPanel(id="sub_pathogen", type="tabs",

      tabPanel("病原体検出", icon=icon("flask"),
        tags$div(style="text-align:right;font-size:0.78em;margin:2px 4px 0;",
          tags$a(href="javascript:void(0)", onclick="goToNotes('notes-iasr')",
            style="color:#888;text-decoration:none;",
            icon("circle-info"), " 注意事項・データソース")),
        fluidRow(style="margin:8px 0 4px;",
          column(3,
            selectInput("iasr_category", "病原体カテゴリ",
              choices = setNames(names(IASR_CATEGORIES),
                                 sapply(IASR_CATEGORIES, `[[`, "label")),
              selected = "influenza", width = "100%")
          ),
          column(2,
            selectInput("iasr_time_type", "表示期間",
              choices = c("月別" = "monthly", "年別" = "annual"),
              selected = "monthly", width = "100%")
          ),
          column(3,
            selectInput("iasr_virus_filter", "病原体絞り込み（複数可）",
              choices = NULL, multiple = TRUE, width = "100%")
          ),
          column(2,
            selectInput("iasr_chart_type", "グラフ種別",
              choices = c("積み上げ棒グラフ" = "bar", "折れ線グラフ" = "line"),
              selected = "bar", width = "100%")
          ),
          column(2,
            tags$div(style="padding-top:25px;font-size:0.78em;color:#888;",
              uiOutput("iasr_last_updated"))
          )
        ),
        fluidRow(
          column(12, plotlyOutput("iasr_plot", height = "420px"))
        ),
        fluidRow(style="margin-top:8px;",
          column(12,
            tags$div(style="font-size:0.75em;color:#666;padding:4px 8px;",
              "出典: 病原微生物検出情報（IASR）/ 地方衛生研究所 月別・年別集計　",
              tags$a(href="https://id-info.jihs.go.jp/surveillance/iasr/graph/summary-table-of-virus/index.html",
                     target="_blank", "JIHS IASR"))
          )
        ),
        fluidRow(style="margin-top:4px;",
          column(12,
            div(style="text-align:right; margin-bottom:4px;",
              downloadButton("iasr_table_dl", "CSVダウンロード",
                             class="btn-xs btn-default", icon=icon("download"))
            ),
            DTOutput("iasr_table", height="300px")
          )
        )
      ),

      # ── ARI病原体（急性呼吸器感染症サーベイランス週報）──────────
      tabPanel("ARI病原体", icon=icon("virus"),
        tags$div(style="text-align:right;font-size:0.78em;margin:2px 4px 0;",
          tags$a(href="javascript:void(0)", onclick="goToNotes('notes-ari-pathogen')",
            style="color:#888;text-decoration:none;",
            icon("circle-info"), " 注意事項・データソース")),
        tags$div(style="font-size:0.75em;color:#a15c00;background:#fff8e6;border:1px solid #f0d38a;border-radius:4px;padding:6px 10px;margin:4px;",
          icon("triangle-exclamation"), " このタブのグラフはJIHS ARIサーベイランス週報PDFに掲載された",
          "グラフ画像を色解析により数値化した近似値です。元データの数値表は公表されていないため、",
          "特に色調の近い亜型間では誤差が生じる場合があります。正確な値は",
          tags$a(href="https://id-info.jihs.go.jp/surveillance/idss/target-diseases/acute-respiratory-infection/weekly-report/index.html",
                 target="_blank", "JIHS公式週報PDF"), "をご確認ください。"),
        fluidRow(style="margin:8px 4px 4px;",
          column(12, uiOutput("ari_last_updated"))
        ),
        fluidRow(column(12,
          tags$h5("検体採取週ごとの病原体別報告数", style="font-weight:700;margin-bottom:2px"),
          plotlyOutput("ari_count_plot", height="420px")
        )),
        fluidRow(column(12,
          tags$h5("検体採取週ごとの病原体別陽性率", style="font-weight:700;margin-top:16px"),
          plotlyOutput("ari_positivity_plot", height="340px")
        )),
        fluidRow(style="margin-top:16px;",
          column(12,
            tags$div(style="display:flex;align-items:center;gap:10px;flex-wrap:wrap;",
              tags$h5("検体採取週ごとの全国および地域別、病原体別報告数", style="font-weight:700;margin:0;"),
              selectInput("ari_region", NULL,
                choices = setNames(names(ARI_REGIONS), ARI_REGIONS),
                selected = "national", width = "220px")
            ),
            plotlyOutput("ari_regional_plot", height="380px")
          )
        ),
        fluidRow(style="margin-top:8px;",
          column(12,
            div(style="text-align:right; margin-bottom:4px;",
              downloadButton("ari_table_dl", "CSVダウンロード",
                             class="btn-xs btn-default", icon=icon("download"))
            ),
            DTOutput("ari_table", height="300px")
          )
        )
      )

      ) # sub_pathogen
      ),

      # ── 重症サーベイランス（入院、IDWR週報PDF）────────────────
      tabPanel("重症サーベイランス", icon=icon("bed-pulse"),
      tabsetPanel(id="sub_severe", type="tabs",
        tabPanel("入院サーベイランス",
          tags$div(style="text-align:right;font-size:0.78em;margin:2px 4px 0;",
            tags$a(href="javascript:void(0)", onclick="goToNotes('notes-hosp')",
              style="color:#888;text-decoration:none;",
              icon("circle-info"), " 注意事項・データソース")),
          fluidRow(style="margin:8px 0 4px;",
            column(4,
              checkboxGroupInput("hosp_disease", "対象疾患",
                choices = c("インフルエンザ" = "flu", "新型コロナウイルス感染症" = "covid"),
                selected = c("flu", "covid"))
            )
          ),
          tags$div(class="data-source-bar", "基幹定点 入院サーベイランス — 週次入院患者報告数"),
          uiOutput("hosp_last_updated"),
          plotlyOutput("hosp_plot", height="380px"),
          uiOutput("hosp_legend"),
          tags$hr(),
          DTOutput("hosp_table"),
          downloadButton("hosp_table_dl", "CSVダウンロード", class="btn-sm", style="margin-top:8px;")
        )
      ) # sub_severe
      ),

      # ── EBS ──────────────────────────────────────────────
      tabPanel("EBS", icon=icon("newspaper"),
      tabsetPanel(id="sub_ebs", type="tabs",

      tabPanel("国内", icon=icon("newspaper"),
        tags$div(style="text-align:right;font-size:0.78em;margin:2px 4px 0;",
          tags$a(href="javascript:void(0)", onclick="goToNotes('notes-ebs')",
            style="color:#888;text-decoration:none;",
            icon("circle-info"), " 注意事項・データソース")),
        fluidRow(
          column(3,
            tags$div(class="data-source-bar","EBSニュース（国内）"),
            tags$div(style="font-size:0.85em;color:#555;margin-bottom:6px;",
              icon("info-circle"), " 疾患・都道府県はサイドバーで選択"),
            selectInput("ebs_page_size","表示件数",
              choices=c("10件"=10,"50件"=50,"100件"=100,"全部"=9999), selected=50),
            radioButtons("ebs_sort", "並び順",
              choices = c("シグナル優先"="signal", "日付順（新しい順）"="date"),
              selected = "date"),
            actionButton("ebs_show_all", "すべて表示",
              icon=icon("list"), class="btn btn-default btn-sm",
              style="width:100%;margin-top:4px;"),
            tags$div(style="font-size:0.75em;color:#888;margin-top:4px;line-height:1.4;",
              "押すと疾患フィルターを解除して全疾患の記事を表示します"),
            checkboxInput("ebs_official_only",
              tags$span(icon("landmark"), " 公式情報のみ表示"),
              value = FALSE),
            tags$div(style="font-size:0.75em;color:#888;margin-top:-8px;line-height:1.4;",
              "厚労省・都道府県等の自治体公式サイトからの情報のみに絞り込みます")
          ),
          column(9,
            uiOutput("ebs_signal_summary"),
            tags$div(style="text-align:right;margin-bottom:6px;",
              radioButtons("ebs_translate", NULL,
                choices = c("そのまま表示" = "off", "🌐 日本語訳で読む" = "on"),
                selected = "off", inline = TRUE)
            ),
            uiOutput("ebs_news_feed")
          )
        )
      ),

      tabPanel("国外", icon=icon("globe"),
        tags$div(style="text-align:right;font-size:0.78em;margin:2px 4px 0;",
          tags$a(href="javascript:void(0)", onclick="goToNotes('notes-ebs-overseas')",
            style="color:#888;text-decoration:none;",
            icon("circle-info"), " 注意事項・データソース")),
        fluidRow(
          column(3,
            tags$div(class="data-source-bar","EBSニュース（海外）"),
            tags$div(style="font-size:0.85em;color:#555;margin-bottom:6px;",
              icon("info-circle"), " 疾患はサイドバーで選択"),
            selectInput("ebs_ov_page_size","表示件数",
              choices=c("10件"=10,"50件"=50,"100件"=100,"全部"=9999), selected=50),
            radioButtons("ebs_ov_sort", "並び順",
              choices = c("シグナル優先"="signal", "日付順（新しい順）"="date"),
              selected = "date"),
            actionButton("ebs_ov_show_all", "すべて表示",
              icon=icon("list"), class="btn btn-default btn-sm",
              style="width:100%;margin-top:4px;"),
            tags$div(style="font-size:0.75em;color:#888;margin-top:4px;line-height:1.4;",
              "押すと疾患フィルターを解除して全疾患の記事を表示します"),
            checkboxInput("ebs_ov_official_only",
              tags$span(icon("landmark"), " 公式情報のみ表示"),
              value = FALSE),
            tags$div(style="font-size:0.75em;color:#888;margin-top:-8px;line-height:1.4;",
              "WHO・海外保健当局等の公式サイトからの情報のみに絞り込みます"),
            tags$div(style="font-size:0.8em;color:#888;margin-top:8px;",
              icon("info-circle"),
              " 流行トレンド評価には含まれません。都道府県フィルタ対象外。")
          ),
          column(9,
            uiOutput("ebs_ov_signal_summary"),
            tags$div(style="text-align:right;margin-bottom:6px;",
              radioButtons("ebs_ov_translate", NULL,
                choices = c("そのまま表示" = "off", "🌐 日本語訳で読む" = "on"),
                selected = "off", inline = TRUE)
            ),
            uiOutput("ebs_ov_news_feed")
          )
        )
      ),

      tabPanel("EBSトレンド", icon=icon("arrow-trend-up"),
        tags$div(style="text-align:right;font-size:0.78em;margin:2px 4px 0;",
          tags$a(href="javascript:void(0)", onclick="goToNotes('notes-ebs-trends')",
            style="color:#888;text-decoration:none;",
            icon("circle-info"), " 注意事項・データソース")),
        uiOutput("filter_bar_trends"),
        fluidRow(
          column(12,
            # ── EBS 日別記事数チャート ──
            tags$div(class="data-source-bar", "国内EBSニュース 日別シグナル数（過去60日）"),
            tags$div(id = "ebs-trend-chart", style = "height:220px;"),
            uiOutput("ebs_trend_data_script"),
            tags$hr(),
            # ── Google Trends ──
            tags$div(class="data-source-bar",
              "Google Trends | 過去12ヶ月 | 100=最大関心度"),
            tags$div(id = "gtrends-chart", style = "height:400px;"),
            uiOutput("gtrends_data_script")
          )
        )
      ),

      # ── 文献（PubMed）────────────────────────────────────
      tabPanel("文献", icon=icon("book-open"),
        fluidRow(
          column(3,
            tags$div(class="data-source-bar", "PubMed"),
            tags$div(style="font-size:0.85em;color:#555;margin-bottom:6px;",
              icon("info-circle"), " 疾患はサイドバーで選択"),
            selectInput("pubmed_page_size","表示件数",
              choices=c("10件"=10,"50件"=50,"100件"=100,"全部"=9999), selected=50),
            actionButton("pubmed_show_all", "すべて表示",
              icon=icon("list"), class="btn btn-default btn-sm",
              style="width:100%;margin-top:4px;")
          ),
          column(9,
            tags$div(style="text-align:right;margin-bottom:6px;",
              radioButtons("pubmed_translate", NULL,
                choices=c("そのまま表示"="off","🌐 日本語訳で読む"="on"),
                selected="off", inline=TRUE)
            ),
            uiOutput("pubmed_news_feed")
          )
        )
      )

      ) # sub_ebs
      ),

      # ── その他 ────────────────────────────────────────────
      tabPanel("その他", icon=icon("ellipsis"),
      tabsetPanel(id="sub_other", type="tabs",

      # ── Notes ────────────────────────────────────────────
      tabPanel("Notes", icon=icon("circle-info"),
        tags$div(style="padding:20px;max-width:960px;font-size:0.9em;line-height:1.8;",

          # ── 目的 ──────────────────────────────────────────
          tags$div(
            style="background:#eaf4fb;border-left:4px solid #2980b9;border-radius:4px;padding:14px 18px;margin-bottom:20px;",
            tags$p(style="margin:0;",
              "本ダッシュボードは感染症サーベイランス情報の統合・可視化を目的とした研究用ツールです。",
              "公開データを自動集計し、定点把握・全数把握・病原体検出・EBSニュース・EBS Trendsを横断的に確認できる環境を提供しています。",
              "暫定的なデータを用いているため、公式発表の代替ではありません。",
              tags$strong("Epidemic Intelligence"), "活動としての、情報収集・分析の補助ツールとして活用されることを想定しています。"
            )
          ),

          # ── タブ構成 ──────────────────────────────────────
          tags$h4("■ タブ構成", style="border-bottom:2px solid #7f8c8d;padding-bottom:4px;color:#2c3e50;"),
          tags$p(style="font-size:0.85em;color:#888;margin-bottom:6px;",
            "関連するタブを6つのグループにまとめ、グループ内はサブタブで切り替えます。"),
          tags$ul(
            tags$li(tags$strong("活動レベル一覧:"),
              tags$ul(
                tags$li(tags$strong("疾患別:"), "サイドバーで選択中の都道府県・表示モードについて、全疾患の統合活動レベルをタイル表示"),
                tags$li(tags$strong("都道府県別:"), "選択中の疾患について、全47都道府県の統合活動レベルをデフォルメ日本地図上にタイル表示")
              )),
            tags$li(tags$strong("患者サーベイランス:"),
              tags$ul(
                tags$li(tags$strong("地域別比較:"), "都道府県別コロプレスマップ（直近週）＋都道府県ランキング＋都道府県別ヒートマップ＋地域別比較（定点把握。週次/月次報告に対応）"),
                tags$li(tags$strong("流行曲線:"), "週次報告数推移・年別重ね合わせ（定点把握・全数把握）／月次報告数推移・年別重ね合わせ（月報疾患：性感染症・薬剤耐性菌）。CSVダウンロードもこのタブ内で可能"),
                tags$li(tags$strong("複数疾患比較（定点）:"), "複数疾患の定点報告数を重ね合わせ表示"),
                tags$li(tags$strong("実効再生産数 Rt:"), "Cori法によるRt推定")
              )),
            tags$li(tags$strong("病原体サーベイランス:"),
              tags$ul(
                tags$li(tags$strong("病原体検出:"), "IASR（病原微生物検出情報）に基づく病原体検出状況"),
                tags$li(tags$strong("ARI病原体:"), "ARIサーベイランス週報に基づく病原体別報告数・陽性率（グラフ画像からの近似値）")
              )),
            tags$li(tags$strong("重症サーベイランス:"),
              tags$ul(
                tags$li(tags$strong("入院サーベイランス:"), "IDWR週報PDFに基づく、インフルエンザ・新型コロナウイルス感染症の入院患者数の推移")
              )),
            tags$li(tags$strong("EBS:"),
              tags$ul(
                tags$li(tags$strong("国内・国外:"), "報道・行政発表等のニュース記事をシグナルレベル別に一覧表示"),
                tags$li(tags$strong("EBSトレンド:"), "EBSニュースとGoogle Trendsを統合した短期トレンド評価"),
                tags$li(tags$strong("文献:"), "PubMed収載論文の一覧")
              )),
            tags$li(tags$strong("その他:"),
              tags$ul(
                tags$li(tags$strong("Notes:"), "このページ（データソース・算出方法・注意事項の説明）")
              ))
          ),
          tags$br(),

          # ── IBS（定点把握）────────────────────────────────
          tags$h4(id="notes-ibs", "■定点把握疾患", style="border-bottom:2px solid #3498db;padding-bottom:4px;color:#2c3e50;"),
          tags$h5("データソース"),
          tags$p("感染症発生動向調査週報（IDWR）速報／感染症発生動向調査事業年報　国立健康危機管理研究機構（JIHS）"),
          tags$ul(
            tags$li("定点医療機関からの週次報告データ（定点あたり報告数）"),
            tags$li("2001年〜概ね前年分までは事業年報の確定値、当年の未確定期間はIDWR週報の速報値（暫定値）を使用しています"),
            tags$li("速報値は後日、事業年報の確定値で修正される場合があります（画面右上の「⚠ 速報値」「✓ 確定値」表示で判別できます）"),
            tags$li("集計週は月曜〜日曜、報告週は翌週月曜基準で公表"),
            tags$li("JIHSサイトでの公表: 原則毎週火曜日（祝祭日等の影響でずれる場合あり）"),
            tags$li("本ダッシュボードのデータ更新: 毎日午前3時に自動取得（新週が公表されていれば差分取得）")
          ),
          tags$h5("定点の種類（定点種別）"),
          tags$p(
            "定点把握疾患は、全国に指定された医療機関（定点）から報告されます。疾患ごとに報告を担う定点の",
            "種別が異なり、それぞれ全国の指定医療機関数（報告定点数）もおおむね決まっています（数はIDWR公表時点により変動）。"
          ),
          tags$ul(
            tags$li(tags$strong("急性呼吸器感染症定点（内科系、全国約3,700カ所）: "),
              "インフルエンザ・新型コロナウイルス感染症・急性呼吸器感染症（ARI）等"),
            tags$li(tags$strong("小児科定点（全国約2,200カ所）: "),
              "手足口病・感染性胃腸炎・水痘・伝染性紅斑・突発性発しん・ヘルパンギーナ・",
              "流行性耳下腺炎・RSウイルス感染症・A群溶血性レンサ球菌咽頭炎等"),
            tags$li(tags$strong("眼科定点（全国約600カ所）: "),
              "咽頭結膜熱・流行性角結膜炎・急性出血性結膜炎"),
            tags$li(tags$strong("基幹定点（300床以上の病院を中心に指定、全国約500カ所）: "),
              "マイコプラズマ肺炎・クラミジア肺炎等（このほか、入院サーベイランスタブで扱う",
              "インフルエンザ・新型コロナウイルス感染症の入院患者数も基幹定点からの報告）")
          ),
          tags$h5("注意報・警報基準値について"),
          tags$div(style="background:#fff8e1;border-left:4px solid #f39c12;padding:8px 12px;margin-bottom:8px;font-size:0.9em;",
            tags$strong("注意:"),
            " 以下の基準値は、厚生労働科学研究「効果的な感染症サーベイランスの評価ならびに改良に関する研究」の研究班報告書（定点あたり報告数）に基づく",
            tags$strong("研究班報告ベースの基準値"), "であり、国（厚生労働省・JIHS等）が定める",
            tags$strong("公式の注意報・警報基準ではありません"), "。",
            "都道府県が実際に注意報・警報を発信する際は、これに加えて「基準値を超えた保健所の管轄人口の合計が都道府県全体人口の30%を超えた場合」等の人口按分条件も加味されるため、",
            "本ダッシュボードの判定はあくまで研究班報告ベースの簡易的な参考情報であり、公式の注意報・警報発令そのものではありません。"
          ),
          tags$h5("「注意報」「警報」「終息」の解釈"),
          tags$ul(
            tags$li(tags$strong("注意報: "),
              "流行の発生前であれば「今後4週間以内に大きな流行が発生する可能性が高いこと」、",
              "発生後であれば「流行が継続していると疑われること」を示します。"),
            tags$li(tags$strong("警報: "),
              "「大きな流行が発生または継続しつつあると疑われること」を示します。"),
            tags$li(tags$strong("警報終息基準値: "),
              "警報が発令された後、報告数がこの値を下回ると流行が収束しつつあると判断される水準です。")
          ),
          tags$h5("判定方法（注意報・警報基準値・Rt・過去5年比較の統合、共線性に配慮した2段階加重平均）"),
          tags$p(
            "流行フェーズ（基準以下／流行期／注意／警戒の4段階）は、次の3つの指標をそれぞれ0〜3点でスコア化して算出しています。"
          ),
          tags$ul(
            tags$li(tags$strong("① 注意報・警報基準値（研究班報告ベース、定点あたり報告数）: "),
              "報告数が警報開始基準値以上→3点（警戒相当）、注意報開始基準値以上（設定がある疾患のみ）→2点（注意相当）、警報終息基準値以上→1点（流行期相当）、それ未満→0点。",
              "研究班報告に注意報開始基準値の記載がない疾患は②の判定を省略しています（表中「―」）。基準値自体が設定されていない疾患はこの指標を評価から除外しています。"),
            tags$li(tags$strong("② 実効再生産数（Rt）: "),
              "Rt≧2.0→3点、Rt≧1.5→2点、Rt≧1.0→1点、Rt<1.0→0点。SI（シリアルインターバル/潜伏期間推定値）が定義されていない疾患はこの指標を評価から除外しています。"),
            tags$li(tags$strong("③ IBS方式・過去5年比較（±SD）: "),
              "疾患ごとに季節性の強さを自動判定し、評価方式を切り替えています。季節性ありと判定された疾患は、同一週±2週・過去5年平均に対して+2SD以上→3点、+1SD以上→2点、平均以上→1点、平均未満→0点。",
              "定点あたり報告数が少なく季節性が乏しい疾患（咽頭結膜熱・手足口病など）は、この固定的な5年比較では基準値自体が不安定になるため、直近の推移と比較するEARS C2ライクな方式に自動的に切り替わります（詳細は",
              tags$a(href="javascript:void(0)", onclick="goToNotes('notes-zensu-ibs')", "こちら"), "）。過去データが不足する場合はこの指標を評価から除外しています。")
          ),
          tags$h5("計算式（現在の流行フェーズカード）"),
          tags$div(style="background:#f8f9fa;border:1px solid #ddd;border-radius:6px;padding:12px 16px;margin-bottom:8px;",
            tags$div(style="font-size:0.8em;color:#666;margin-bottom:4px;",
              "ステップ1: 水準スコア（①③が利用可能な場合のみ。片方だけならその値をそのまま使用）"),
            tags$div(HTML("\\[ S_{\\text{level}} = \\dfrac{0.3 \\cdot S_{①} + 0.4 \\cdot S_{③}}{0.3 + 0.4} \\]")),
            tags$div(style="font-size:0.8em;color:#666;margin-bottom:4px;",
              "ステップ2: 最終スコア（水準スコア・②Rtが利用可能な場合のみ。片方だけならその値をそのまま使用）"),
            tags$div(HTML("\\[ S_{\\text{final}} = \\mathrm{round}\\bigl(0.5 \\cdot S_{\\text{level}} + 0.5 \\cdot S_{②} \\bigr) \\]")),
            tags$div(style="font-size:0.82em;color:#555;",
              "レベル対応: ", tags$code("0"), "=基準以下　", tags$code("1"), "=流行期（レベル1）　",
              tags$code("2"), "=注意（レベル2）　", tags$code("3"), "=警戒（レベル3）",
              tags$br(), "①②③すべて欠損の場合は「―」（判定対象外）")
          ),
          tags$div(style="background:#f4f4f4;border-left:4px solid #7f8c8d;padding:8px 12px;margin-bottom:8px;font-size:0.9em;",
            tags$strong("共線性への配慮: "),
            "①参考基準値と③IBS過去5年比較は、いずれも「現在の報告数の水準」を異なる基準（固定閾値 vs 季節調整済み平均）で",
            "評価したものであり、相関が強く同じ情報を二重にカウントしやすい性質があります。",
            "そこで①③をまず1つの「水準スコア」（内部配分: ①30% : ③40%）に統合し、",
            "感染動態（増加率）に基づく独立性の高い②Rt（動態スコア）と",
            tags$strong("1:1（水準50%・Rt50%）"),
            "で組み合わせる2段階方式を採用しています。"
          ),
          tags$p(
            "評価可能な指標が一部のみの疾患は、残りの指標だけで重みを再正規化して判定します（各段階内で正規化）。",
            "①③②のいずれも欠損している疾患は「―」（判定対象外）と表示されます。",
            tags$br(),
            "この方式により、報告数自体はまだ基準値未満でも、Rtの上昇や過去5年からの逸脱が大きい場合には早期に高いフェーズとして表示されます。"
          ),
          tags$p(style="font-size:0.85em;color:#888;",
            "以下の基準値は研究班報告書に記載された値であり、",
            tags$strong("国の公式基準ではありません"), "（詳細は上記「注意報・警報基準値について」参照）。"
          ),
          tags$table(class="table table-bordered table-sm", style="font-size:0.85em;",
            tags$thead(tags$tr(tags$th("疾患"), tags$th("警報開始"), tags$th("警報終息"), tags$th("注意報開始"), tags$th("出典"))),
            tags$tbody(
              tags$tr(tags$td("インフルエンザ"), tags$td("30"), tags$td("10"), tags$td("10"), tags$td("研究班報告書")),
              tags$tr(tags$td("咽頭結膜熱"), tags$td("3"), tags$td("1"), tags$td("―"), tags$td("研究班報告書")),
              tags$tr(tags$td("A群溶血性レンサ球菌咽頭炎"), tags$td("8"), tags$td("4"), tags$td("―"), tags$td("研究班報告書")),
              tags$tr(tags$td("感染性胃腸炎"), tags$td("20"), tags$td("12"), tags$td("―"), tags$td("研究班報告書")),
              tags$tr(tags$td("水痘"), tags$td("7"), tags$td("4"), tags$td("4"), tags$td("研究班報告書")),
              tags$tr(tags$td("手足口病"), tags$td("5"), tags$td("2"), tags$td("―"), tags$td("研究班報告書")),
              tags$tr(tags$td("伝染性紅斑"), tags$td("2"), tags$td("1"), tags$td("―"), tags$td("研究班報告書")),
              tags$tr(tags$td("ヘルパンギーナ"), tags$td("6"), tags$td("2"), tags$td("―"), tags$td("研究班報告書")),
              tags$tr(tags$td("流行性耳下腺炎"), tags$td("6"), tags$td("2"), tags$td("3"), tags$td("研究班報告書")),
              tags$tr(tags$td("急性出血性結膜炎"), tags$td("6"), tags$td("0.1"), tags$td("―"), tags$td("研究班報告書")),
              tags$tr(tags$td("流行性角結膜炎"), tags$td("8"), tags$td("4"), tags$td("―"), tags$td("研究班報告書")),
              tags$tr(tags$td("その他の疾患"), tags$td("―"), tags$td("―"), tags$td("―"), tags$td("基準値なし"))
            )
          ),
          tags$p(style="font-size:0.85em;color:#888;",
            "※ 研究班報告書には百日咳（警報開始1／終息0.1）も掲載されていますが、現行の感染症法では百日咳は全数把握対象疾患のため、",
            "本ダッシュボードの定点把握基準値としては適用していません。"
          ),
          tags$br(),

          # ── 全数把握疾患 ──────────────────────────────────
          tags$h4(id="notes-zensu", "■ 全数把握疾患（地図・流行曲線）", style="border-bottom:2px solid #27ae60;padding-bottom:4px;color:#2c3e50;"),
          tags$h5("データソース"),
          tags$p("感染症発生動向調査　国立健康危機管理研究機構（JIHS）"),
          tags$ul(
            tags$li("全数把握対象疾患の医師による全例報告（感染症法届出）"),
            tags$li("報告は診断した医師が保健所経由で届け出"),
            tags$li("週次集計で公表"),
            tags$li("2001年〜概ね前年分までは事業年報の確定値、当年の未確定期間はIDWR週報の速報値（暫定値）を使用しています"),
            tags$li("JIHSサイトでの公表: 原則毎週火曜日（祝祭日等の影響でずれる場合あり）"),
            tags$li("本ダッシュボードのデータ更新: 毎日午前3時に自動取得（新週が公表されていれば差分取得）")
          ),
          tags$h5("対象疾患（本ダッシュボード掲載）"),
          tags$p("麻疹・風疹・百日咳・梅毒・腸管出血性大腸菌感染症（O157等）・デング熱・エムポックス・結核・劇症型溶血性レンサ球菌感染症・侵襲性肺炎球菌感染症・レジオネラ症・A型肝炎・SFTS・エボラ出血熱"),

          tags$h5(id="notes-zensu-ibs", "IBS評価方式の季節性自動判定（定点把握・全数把握 共通）"),
          tags$p(
            "定点把握疾患・全数把握疾患のいずれにも、インフルエンザ様の反復流行を示す疾患と、",
            "咽頭結膜熱・手足口病や麻疹・風疹のように定点あたり報告数（全数把握では報告数）がほぼ0〜数件で",
            "散発的にしか発生しない疾患が混在しています。",
            "後者に「同時期×過去5年平均」という季節比較方式を一律適用すると、",
            "基準となる過去データ自体がほぼ0件のため判定が不安定・不正確になります。",
            "そこで本ダッシュボードでは、疾患ごとの実データから季節性の強さを自動判定し、評価方式を切り替えています。"
          ),
          tags$h5("① 季節性の判定方法"),
          tags$p("疾患ごとに「暦週（week of year）別の平均報告数」を過去データから算出し、その変動係数（CV）を求めます。"),
          tags$div(HTML("\\[ CV = \\dfrac{\\mathrm{SD}(\\bar{v}_{\\text{week}})}{\\mathrm{mean}(\\bar{v}_{\\text{week}})} \\]")),
          tags$p(tags$em("（", HTML("\\(\\bar{v}_{\\text{week}}\\)"), " は暦週ごとの過去全年平均報告数）")),
          tags$ul(
            tags$li("暦週別平均のデータが10週分未満、または報告のある年が3年未満の場合 → 「季節性なし」と判定"),
            tags$li("年平均報告数が10件未満の場合 → CVの値によらず「季節性なし」と判定（回帰熱等、報告数が極めて少ない疾患では、",
                    "数少ない散発例がたまたま特定の週に集中しただけでCVが高くなり、季節性ありと誤判定されやすいための追加ガード）"),
            tags$li(HTML("CV &gt; 0.6 &rarr; 「季節性あり」と判定（特定の時期に発生が集中している）")),
            tags$li("それ以外 → 「季節性なし」と判定")
          ),
          tags$h5("② 季節性ありと判定された疾患の評価方式"),
          tags$p("直近週の値を「同時期（±2週）×過去5年」の平均・標準偏差（SD）と比較する、従来からの方式です。"),
          tags$div(HTML("\\[ \\text{score} = \\begin{cases} 3 & (\\text{値} \\geq \\mu + 2\\sigma\\ \\text{が2週連続}) \\\\ 2 & (\\text{値} \\geq \\mu + \\sigma) \\\\ 1 & (\\text{値} \\geq \\mu) \\\\ 0 & (\\text{それ以外}) \\end{cases} \\]")),
          tags$h5("③ 季節性なしと判定された疾患の評価方式（散発疾患向け）"),
          tags$p(
            "米国CDCのEARS（Early Aberration Reporting System）C2法を簡略化した方式を使用しています。",
            "年単位の周期比較ができない散発疾患のため、直近の推移（ベースライン）との比較で判定しています。",
            "直近2週間をガードバンド（進行中の増加を汚染させないための除外期間）として除き、その前の7週間をベースラインとしています。"
          ),
          tags$div(HTML("\\[ \\mu = \\mathrm{mean}(\\text{直近2週を除く過去7週}) \\]")),
          tags$div(HTML("\\[ \\sigma = \\sqrt{\\max(\\mu,\\ 1)} \\quad \\text{（ポアソン近似: 分散} \\approx \\text{平均、下限1）} \\]")),
          tags$div(HTML("\\[ \\text{score} = \\begin{cases} 3 & (\\text{値} \\geq \\mu + 3\\sigma,\\ \\text{急増}) \\\\ 2 & (\\text{値} \\geq \\mu + 2\\sigma,\\ \\text{増加}) \\\\ 1 & (\\text{値} > \\mu,\\ \\text{やや増加}) \\\\ 0 & (\\text{それ以外、平常}) \\end{cases} \\]")),
          tags$p(tags$em(
            "参考: CDC EARS法（C1/C2/C3）— 感染症サーベイランスにおける異常値検知の標準的手法の一つ。本ダッシュボードでは簡略化したC2ライクな実装を使用しています。",
            tags$br(),
            "出典: Hutwagner L, et al. \"The bioterrorism preparedness and response Early Aberration Reporting System (EARS).\" J Urban Health. 2003;80(2 Suppl 1):i89-96. ",
            tags$a(href="https://pubmed.ncbi.nlm.nih.gov/12791774/", target="_blank", "PubMed 12791774")
          )),
          tags$br(),

          # ── 月報疾患 ────────────────────────────────────
          tags$h4(id="notes-std", "■ 月報疾患（性感染症・薬剤耐性菌）", style="border-bottom:2px solid #9c27b0;padding-bottom:4px;color:#2c3e50;"),
          tags$h5("データソース"),
          tags$p("感染症発生動向調査週報（IDWR）　国立健康危機管理研究機構（JIHS）"),
          tags$ul(
            tags$li("性器クラミジア感染症・性器ヘルペスウイルス感染症・尖圭コンジローマ・淋菌感染症は定点把握（性感染症定点）、",
              "メチシリン耐性黄色ブドウ球菌感染症・ペニシリン耐性肺炎球菌感染症は基幹定点からの報告"),
            tags$li("週報（毎週発行）とは別に、月1回程度「月報」として都道府県別報告数がまとめて掲載される"),
            tags$li("月報の掲載号・掲載ページは月によって変動するため、該当月のデータが見つかるまで各号を探索して取得している"),
            tags$li("薬剤耐性緑膿菌感染症は2026年4月6日より全数把握対象疾患（多剤耐性緑膿菌感染症）に変更されたため、",
              "それ以降の月報には掲載されず、本タブでは扱っていません（全数把握タブの「多剤耐性緑膿菌感染症」を参照）")
          ),
          tags$p(style="font-size:0.85em;color:#888;",
            "※ 月報が発行されなかった月・未取得の月はグラフ上で欠測（線が途切れる）となります。",
            "過去5年比較の帯は、同月・前後1か月分・過去5年間のデータのうち3年分以上取得できている月のみ表示しています。"),
          tags$br(),

          # ── 実効再生産数 Rt ────────────────────────────────
          tags$h4(id="notes-rt", "■ 実効再生産数（Rt）", style="border-bottom:2px solid #e74c3c;padding-bottom:4px;color:#2c3e50;"),
          tags$h5("推定方法"),
          tags$p("Cori法（Wallinga & Teunis, 2004 改良版）に基づくベイズ推定を使用しています。"),
          tags$ul(
            tags$li("推定窓: 7日間（週次データのため1週）"),
            tags$li("事前分布: ガンマ分布（shape=1, rate=5）"),
            tags$li("最低データ要件: 15週以上の連続データ"),
            tags$li("Rt > 1.0: 流行拡大傾向、Rt < 1.0: 流行縮小傾向")
          ),
          tags$h5("シリアルインターバル（SI）の出典（実際に使用しているデータ）"),
          tags$table(class="table table-bordered table-sm", style="font-size:0.8em;",
            tags$thead(tags$tr(tags$th("疾患"), tags$th("平均(日)"), tags$th("SD(日)"), tags$th("区分"), tags$th("出典"))),
            tags$tbody(
              lapply(names(SERIAL_INTERVALS), function(id) {
                si  <- SERIAL_INTERVALS[[id]]
                lbl <- if (id %in% names(DISEASE_CONFIG)) DISEASE_CONFIG[[id]]$label
                       else if (id %in% names(ZENSU_DISEASE_CONFIG)) ZENSU_DISEASE_CONFIG[[id]]$label
                       else id
                tags$tr(
                  tags$td(lbl),
                  tags$td(sprintf("%.1f", si$mean)),
                  tags$td(sprintf("%.1f", si$sd)),
                  tags$td(if (isTRUE(si$published)) "文献SI" else "潜伏期間からの推定値"),
                  tags$td(si$source)
                )
              })
            )
          ),
          tags$h5("潜伏期間代用について"),
          tags$p(
            "査読済みのシリアルインターバル推定値が存在しない疾患については、潜伏期間データをSIの代替値として使用しています。",
            "この場合、Rt推定画面に「（潜伏期間より推定）」と表示されます。",
            tags$br(),
            "対象疾患（主なもの）: デング熱（ヒト潜伏期間＋Aedes蚊外因性潜伏期間の合計）、腸管出血性大腸菌（EHEC）、咽頭結膜熱、伝染性紅斑、急性出血性結膜炎、流行性角結膜炎など",
            tags$br(),
            tags$em("注: ベクター媒介感染症のSIはヒト間での直接伝播とは異なる解釈が必要です。")
          ),
          tags$h5("Rt解釈が限定的な疾患"),
          tags$ul(
            tags$li(tags$strong("腸管出血性大腸菌（EHEC）: "), "症例の約80%は食品媒介（共通感染源）。Rtが1を超えても食品汚染源を反映している可能性があり、ヒト間伝播の指標としては過大推定になりやすい。"),
            tags$li(tags$strong("細菌性髄膜炎: "), "散発性・稀少疾患のためCori法による週次Rt推定は統計的に不安定。接触者予防投薬により二次感染がほぼ防止される。"),
            tags$li(tags$strong("劇症型溶血性レンサ球菌感染症: "), "家庭内二次アタックレート<0.22%と非常に低く、週次サーベイランスデータからのRt推定は解釈に注意が必要。"),
            tags$li(tags$strong("伝染性紅斑（リンゴ病）: "), "発疹出現時にはすでに感染性を失っている。報告データは過去2–3週の伝播を遅れて反映する。")
          ),
          tags$h5("注意事項"),
          tags$ul(
            tags$li("Rt推定には週次集計データを使用しており、日次データに比べて分解能が低くなります"),
            tags$li("95%信頼区間が広い場合はデータ不足の可能性があります"),
            tags$li("SI値が未定義の疾患は「推定対象外」と表示されます"),
            tags$li("「潜伏期間より推定」と表示される疾患のRt値は参考値として扱ってください")
          ),
          tags$br(),

          # ── 短期予測 ────────────────────────────────────
          tags$h4(id="notes-forecast", "■ 短期予測（4週先まで）", style="border-bottom:2px solid #8e44ad;padding-bottom:4px;color:#2c3e50;"),
          tags$p(
            "流行曲線タブの「予測を表示」をオンにすると、直近の実測データから4週先までの参考予測を",
            "紫色の破線・帯（予測区間の目安）で重ねて表示します。追加のパッケージを使わず、",
            "以下のいずれかの手法（プルダウンで選択）により算出しています。"
          ),
          tags$ul(
            tags$li(tags$strong("ポアソン回帰（GLM）: "),
              "感染症の週次報告数のような非負のカウントデータに標準的に用いられる回帰手法です。",
              "直近8週の実測値に対数リンクの準ポアソン回帰（quasipoisson GLM）をあてはめ、",
              "乗法的な増減として延長します（過分散に対応、80%予測区間つき）。",
              "単純な直線回帰と異なり、値が負にならず、増減が指数的に働く点が実際の流行動態に近い挙動です。"),
            tags$li(tags$strong("指数平滑法（Holt法）: "),
              "水準とトレンド（増減の勢い）を指数的に重み付けしながら逐次更新するHolt線形トレンド法",
              "（季節成分なし）です。直近のデータほど重視しつつ、過去の推移も緩やかに反映します。"),
            tags$li(tags$strong("Rtベース（renewal equation）: "),
              "感染症の数理モデルにおける更新方程式（renewal equation／分枝過程モデル）に基づく手法です。",
              tags$code("I(t) = Rt × Σ I(t-s)×w(s)"),
              "の式で、過去の実測値をシリアルインターバル分布（Rt推定と同じガンマ分布近似）で",
              "重み付け合成し、直近の実効再生産数（Rt）が今後も一定と仮定して1週ずつ将来値を生成します。",
              "直近1点だけを外挿する単純な指数近似と異なり、過去複数週の推移の形状を踏まえた予測になります。",
              "Rt自体の推定誤差を踏まえ、参考区間として±30%の簡易バンドを付しています。",
              "Rtが推定できない疾患ではこの手法は使用できません。"),
            tags$li(tags$strong("アンサンブル（推奨）: "),
              "上記3手法のうち算出できたものの単純平均です。個々の手法の誤差や前提の偏りを緩和し、",
              "より安定した予測になる傾向があります。")
          ),
          tags$h5("季節性の反映について"),
          tags$p(
            "IBS方式・過去5年比較の季節性自動判定（",
            tags$a(href="javascript:void(0)", onclick="goToNotes('notes-zensu-ibs')", "詳細"),
            "）と同じロジックで疾患ごとに季節性の有無を判定し、季節性ありと判定された疾患",
            "（かつ約2年分以上のデータがある場合）は、ポアソン回帰・指数平滑法の2手法に",
            "暦週（week of year）に基づく季節成分を追加します。凡例に「・季節性反映」と表示されます。"
          ),
          tags$ul(
            tags$li(tags$strong("ポアソン回帰: "),
              "直近3年分のデータで、暦週の調和項（sin/cos(2π×週/52)）をトレンド項に追加してフィットします。"),
            tags$li(tags$strong("指数平滑法: "),
              "Holt線形トレンド法から、季節成分（周期52週）を含む三重指数平滑法（Holt-Winters）に切り替えます。"),
            tags$li(tags$strong("Rtベース（renewal equation）: "),
              "季節成分は追加しません。Rtは実効再生産数＝リアルタイムの伝播動態そのものであり、",
              "暦の季節性をさらに重ねると二重に加味することになるため、現状のまま据え置いています。")
          ),
          tags$div(style="background:#fff8e1;border-left:4px solid #f39c12;padding:8px 12px;margin-bottom:8px;font-size:0.9em;",
            tags$strong("注意: "),
            "いずれの手法も統計的な外挿にすぎず、変異株の出現、施策変更、報告体制の変化等の",
            "予測不能な要因は反映されません。特に報告数が少ない疾患では区間が大きく広がる、",
            "またはRtベース手法が使用できない場合があります。予測値は参考情報としてのみ利用し、",
            "実際の判断は最新の実測データと専門家の評価に基づいて行ってください。"
          ),
          tags$br(),

          # ── EBS ──────────────────────────────────────────
          tags$h4(id="notes-ebs", "■ EBS（イベントベースサーベイランス）", style="border-bottom:2px solid #f39c12;padding-bottom:4px;color:#2c3e50;"),
          tags$h5("データソース（全ソース共通）"),
          tags$table(class="table table-bordered table-sm", style="font-size:0.85em;",
            tags$thead(tags$tr(tags$th("ソース"), tags$th("カテゴリ"), tags$th("言語"))),
            tags$tbody(
              tags$tr(tags$td("厚生労働省（MHLW）"), tags$td("行政"), tags$td("日本語")),
              tags$tr(tags$td("JIHS（国立健康危機管理研究機構）"), tags$td("研究機関"), tags$td("日本語")),
              tags$tr(tags$td("WHO Disease Outbreak News"), tags$td("国際"), tags$td("英語")),
              tags$tr(tags$td("ProMED Mail"), tags$td("国際"), tags$td("英語")),
              tags$tr(tags$td("ECDC"), tags$td("国際"), tags$td("英語")),
              tags$tr(tags$td("CDC Health Alerts"), tags$td("国際"), tags$td("英語")),
              tags$tr(tags$td("ReliefWeb（日本関連）"), tags$td("国際"), tags$td("英語")),
              tags$tr(tags$td("CIDRAP"), tags$td("国際"), tags$td("英語")),
              tags$tr(tags$td("Outbreak News Today"), tags$td("国際"), tags$td("英語")),
              tags$tr(tags$td("NHK（健康・科学・社会・政治）"), tags$td("メディア"), tags$td("日本語")),
              tags$tr(tags$td("朝日新聞（健康・科学・社会）"), tags$td("メディア"), tags$td("日本語")),
              tags$tr(tags$td("毎日新聞 / 時事通信"), tags$td("メディア"), tags$td("日本語")),
              tags$tr(tags$td("Yahoo!ニュース（科学・国内）"), tags$td("メディア"), tags$td("日本語")),
              tags$tr(tags$td("日本経済新聞 / Japan Times"), tags$td("メディア"), tags$td("日本語/英語")),
              tags$tr(tags$td("北海道新聞 / 宮崎日日新聞"), tags$td("地方紙"), tags$td("日本語")),
              tags$tr(tags$td("Google News（疾患別キーワード検索）"), tags$td("ニュース"), tags$td("日本語/英語")),
              tags$tr(tags$td("PubMed（文献）"), tags$td("学術"), tags$td("英語"))
            )
          ),
          tags$p(style="font-size:0.85em;color:#888;", "※ PubMed論文はEBSニュース（海外）タブに表示。流行トレンド評価には含まない。"),
          tags$h5("国内・海外タブの振り分け"),
          tags$ul(
            tags$li("情報ソースではなく、", tags$strong("記事のタイトル・本文中の地名・国名"), "で国内・海外を判定"),
            tags$li("都道府県名・政令市・市区町村名・保健所名が含まれる記事 → ", tags$strong("EBSニュース（国内）")),
            tags$li("海外の国名・地名のみ含まれる記事 → ", tags$strong("EBSニュース（海外）")),
            tags$li("どちらも検出されない場合は国内扱い")
          ),
          tags$h5("シグナルレベル（WHO EBS 7基準による自動判定）"),
          tags$table(class="table table-bordered table-sm", style="font-size:0.85em;",
            tags$thead(tags$tr(tags$th("レベル"), tags$th("意味"), tags$th("重み"))),
            tags$tbody(
              tags$tr(tags$td(tags$span(style="color:#e74c3c;font-weight:700;", "Signal High")),
                      tags$td("日本への直接影響あり・原因不明かつ国際的関心・一類感染症＋国内影響"),
                      tags$td("3")),
              tags$tr(tags$td(tags$span(style="color:#e67e22;font-weight:700;", "Signal Low")),
                      tags$td("海外での重大事象・一類感染症（海外）・伝播可能性あり"),
                      tags$td("2")),
              tags$tr(tags$td(tags$span(style="color:#95a5a6;font-weight:700;", "FYI")),
                      tags$td("感染症関連情報・参考情報（流行評価には含めない）"),
                      tags$td("―"))
            )
          ),
          tags$h5("公式情報のみ表示フィルタ"),
          tags$ul(
            tags$li("国内・海外タブそれぞれのサイドバーにある「公式情報のみ表示」チェックボックスで、",
                    "行政機関・国際機関の公式サイト/公式RSSから取得した記事のみに絞り込めます"),
            tags$li("対象: 厚生労働省・JIHS・全47都道府県の公式サイト、WHO・ECDC・UKHSA・ロベルトコッホ研究所・",
                    "台湾/中国CDC・香港CHP等の海外公的機関"),
            tags$li("対象外（公式情報として扱わない）: NHK・新聞社等の報道機関、Google Newsによる",
                    "ニュース集約、PubMed（学術論文）、SNS"),
            tags$li("公式情報源からの記事にはカード上に", tags$strong("「公式」"), "バッジが表示されます")
          ),
          tags$h5("都道府県判定（EBSニュース（国内））"),
          tags$ul(
            tags$li("メディア名・URL → 都道府県マッピング（例: 神奈川新聞→神奈川県）"),
            tags$li("都道府県名・政令市・市区町村名（全国主要400市区）"),
            tags$li("保健所名パターン（「○○保健所」「○○保健センター」等）"),
            tags$li("都道府県フィルターで絞り込み可能")
          ),
          tags$h5("データ更新"),
          tags$ul(
            tags$li("毎日午前3時に自動取得・過去1年分をキャッシュ保持（PubMed含む全ソース共通）"),
            tags$li("PubMed: 直近1年以内に公開された論文を毎回取得し、累積1年分を保持"),
            tags$li("重複記事は自動除去（タイトル・日付・ソースで判定）")
          ),
          tags$br(),

          # ── EBSニュース（海外）──────────────────────────────
          tags$h4(id="notes-ebs-overseas", "■ EBSニュース（海外）タブ", style="border-bottom:2px solid #2980b9;padding-bottom:4px;color:#2c3e50;"),
          tags$ul(
            tags$li("全ソースのうち、タイトル・本文から海外の地名・国名のみ検出された記事を表示"),
            tags$li("Signal High / Signal Low / FYI の判定は国内タブと同じ基準"),
            tags$li(tags$strong("EBS/Trends 流行トレンド評価には含まれない")),
            tags$li("都道府県フィルターの影響を受けない")
          ),
          tags$br(),

          # ── EBS Trends ─────────────────────────────────────
          tags$h4(id="notes-ebs-trends", "■ EBS Trends タブ", style="border-bottom:2px solid #9b59b6;padding-bottom:4px;color:#2c3e50;"),
          tags$h5("EBSニュース 日別シグナル数グラフ"),
          tags$ul(
            tags$li("EBSニュース（国内）のSignal High・Signal Lowの週別記事数を過去60日分積み上げ棒グラフで表示"),
            tags$li("サイドバーの疾患・都道府県選択に連動"),
            tags$li("PubMed・海外記事は除外")
          ),
          tags$h5("Google Trends グラフ"),
          tags$p("Google Trends API（gtrendsRパッケージ経由）"),
          tags$ul(
            tags$li("検索関心度を0〜100に正規化（100=期間内最大値）、週次・過去12ヶ月"),
            tags$li("サイドバーの疾患・都道府県選択に連動"),
            tags$li("データは毎日午前3時に自動更新（レート制限のため取得できない場合はキャッシュを表示）")
          ),
          tags$br(),

          # ── EBS/Trends トレンドカード ──────────────────────
          tags$h4("■ EBS/Trends 流行トレンドカード", style="border-bottom:2px solid #e67e22;padding-bottom:4px;color:#2c3e50;"),
          tags$h5("評価方法"),
          tags$p("EBSニュース（過去14日）とGoogle Trendsを統合してトレンドを評価しています。"),
          tags$table(class="table table-bordered table-sm", style="font-size:0.85em;",
            tags$thead(tags$tr(tags$th("判定"), tags$th("基準"))),
            tags$tbody(
              tags$tr(tags$td("急上昇"),   tags$td("EBSスコア9以上 または Trendsが50%超上昇")),
              tags$tr(tags$td("上昇傾向"), tags$td("EBSスコア4〜8 かつ/または Trendsが20〜50%上昇")),
              tags$tr(tags$td("やや上昇"), tags$td("EBSスコア1〜3 かつ/または Trendsが20%未満上昇")),
              tags$tr(tags$td("横ばい"),   tags$td("EBSシグナルなし かつ Trends変化20%未満")),
              tags$tr(tags$td("低下傾向"), tags$td("Trendsが20%超低下"))
            )
          ),
          tags$p(style="color:#888;font-size:0.85em;",
            "※ EBSスコア = Signal High（重み3）× 件数 + Signal Low（重み2）× 件数の合計。FYIは評価に含めない。"),
          tags$p(style="color:#888;font-size:0.85em;",
            "※ 都道府県フィルター選択時はその都道府県に関連する記事のみで評価。"),
          tags$div(style="background:#fff8e1;border-left:4px solid #f39c12;padding:8px 12px;margin-bottom:8px;font-size:0.9em;",
            tags$strong("注意（少数記事の前週比について）: "),
            "感染症発生動向調査（IDWR）は週1回の公表のため、その報道を伝える記事が公表日周辺に定例的にまとまって",
            "出やすい傾向があります。記事数が少ない週は、1〜2件の増減だけで前週比が大きく振れやすく",
            "（例: 1件→2件で+100%）、この定例的な増減を実際の流行拡大のシグナルと誤判定しやすいため、",
            "直近1週間の記事数が3件未満の場合は前週比によらず「横ばい」扱いとしています。"
          ),
          tags$br(),

          # ── 統合活動レベル（IBS + EBS）─────────────────────
          # IBS（定点把握・注意報警報基準値・Rt・過去5年比較）とEBSの両方を用いるため、
          # 両方の説明が出そろった後にまとめて配置する
          tags$h4(id="notes-integrated", "■ 統合活動レベルカード（IBS + EBS）", style="border-bottom:2px solid #6c3483;padding-bottom:4px;color:#2c3e50;"),
          tags$p(
            "地図・流行曲線タブ上部の「統合活動レベル」カードは、上記の定点把握（IBS）データとEBSニュースの", tags$strong("両方"), "を用いて算出しています。",
            "IBS成分（95%）には「現在の流行フェーズカード」と同じ2段階加重平均ロジック（注意報・警報基準値・Rt・過去5年比較の統合）を適用し、",
            "EBS成分（5%）と合算して最終的な活動レベル（1〜4）を決定しています。"
          ),
          tags$h5("計算式"),
          tags$div(style="background:#f8f9fa;border:1px solid #ddd;border-radius:6px;padding:12px 16px;margin-bottom:8px;",
            tags$div(style="font-size:0.8em;color:#666;margin-bottom:4px;",
              "IBS成分（0〜3点）: 「現在の流行フェーズカード」と同じ2段階加重平均で算出",
              tags$br(),
              "（③は直近2週の過去5年比較±SD、2週連続+2SD超過のみ3点。①②が利用可能ならさらに統合）"),
            tags$div(style="font-size:0.8em;color:#666;margin-bottom:4px;",
              "EBSスコア（0〜2点）: 今週シグナル（高×3＋低×2）と先週シグナルの変化率 ",
              tags$em("r"), " から算出（", tags$em("r"), " < 20%→0点、20% ≤ ", tags$em("r"), " < 50%→1点、",
              tags$em("r"), " ≥ 50%→2点。今週・先週とも記事なしの場合は0点）"),
            tags$div(HTML("\\[ S_{\\text{EBS}}' = \\min(3,\\ 1.5 \\cdot S_{\\text{EBS}}) \\]")),
            tags$div(HTML("\\[ S_{\\text{combined}} = 0.95 \\cdot S_{\\text{IBS}} + 0.05 \\cdot S_{\\text{EBS}}' \\]")),
            tags$div(HTML("\\[ \\text{活動レベル} = \\min\\bigl(4,\\ \\max(1,\\ \\mathrm{round}(S_{\\text{combined}}) + 1) \\bigr) \\]")),
            tags$div(style="font-size:0.82em;color:#555;",
              "レベル対応: ", tags$code("1"), "=通常　", tags$code("2"), "=注意　",
              tags$code("3"), "=警戒　", tags$code("4"), "=流行")
          ),
          tags$br(),

          # ── 活動レベル一覧（疾患別／都道府県別） ──────────────
          tags$h4(id="notes-levels", "■ 活動レベル一覧（疾患別・都道府県別）", style="border-bottom:2px solid #6c3483;padding-bottom:4px;color:#2c3e50;"),
          tags$p(
            "上記「統合活動レベルカード」と同じ計算方法で実施した結果を、疾患ごと・都道府県ごとに一括表示するタブです。",
            "「活動レベル一覧（疾患別）」はサイドバーで選択中の都道府県・表示モードについて全疾患のレベルをタイルで並べ、",
            "「活動レベル一覧（都道府県別）」は選択中の疾患について全47都道府県のレベルをタイルで表示しています。"
          ),
          tags$ul(
            tags$li("全数把握疾患のIBSスコアは、季節性の有無で評価方式が自動的に切り替わります（詳細は",
              tags$a(href="javascript:void(0)", onclick="goToNotes('notes-zensu-ibs')", "こちら"), "）。"),
            tags$li("いずれのタブも、期間スライダー（date_range）で選択した期間の末尾週を評価対象とします。"),
            tags$li("タイルをクリックすると、その疾患・都道府県（もう一方の軸は現在の選択を維持）に絞り込んだ「流行曲線」タブへ移動します。")
          ),
          tags$br(),

          # ── 入院サーベイランス ──────────────────────────────
          tags$h4(id="notes-hosp", "■ 入院サーベイランスタブ", style="border-bottom:2px solid #2980b9;padding-bottom:4px;color:#2c3e50;"),
          tags$h5("データソース"),
          tags$p("感染症発生動向調査週報（IDWR）　国立健康危機管理研究機構（JIHS）"),
          tags$ul(
            tags$li("基幹定点（300床以上の病院を中心に指定、全国約500カ所。定点の種類については",
              tags$a(href="javascript:void(0)", onclick="goToNotes('notes-ibs')", "こちら"),
              "も参照）からの、インフルエンザ・新型コロナウイルス感染症による入院患者数（都道府県別）"),
            tags$li("定点あたり報告数ではなく、基幹定点から報告された入院患者数の実数（都道府県別合計）を集計したもの"),
            tags$li("新型コロナウイルス感染症（入院患者）は2023年5月8日の5類移行後に開始された集計のため、それ以前はデータがありません")
          ),
          tags$p(style="font-size:0.85em;color:#888;",
            "※ 報告医療機関数が少数の基幹定点からの実数であるため、地域の医療体制・報告状況によって",
            "ばらつきが大きい点にご留意ください。"),
          tags$br(),

          # ── 病原体検出（IASR）────────────────────────────────
          tags$h4(id="notes-iasr", "■ 病原体検出タブ", style="border-bottom:2px solid #16a085;padding-bottom:4px;color:#2c3e50;"),
          tags$h5("データソース"),
          tags$p("病原微生物検出情報（IASR）　国立健康危機管理研究機構（JIHS）/ 地方衛生研究所"),
          tags$ul(
            tags$li("地方衛生研究所等からの月別・年別ウイルス・細菌分離・検出報告数を集計したもの"),
            tags$li("月別データ（直近）: 2023年〜直近（JIHS速報集計表）"),
            tags$li("月別データ（過去）: 2012年〜2022年（IASRアーカイブ）"),
            tags$li("年別データ: 年次集計（複数年スパン）"),
            tags$li("データ更新: 毎日午前3時に自動取得・更新（24時間キャッシュ）")
          ),
          tags$h5("病原体カテゴリ"),
          tags$ul(
            tags$li(tags$strong("インフルエンザ&呼吸器ウイルス:"), "インフルエンザ各型・RSV・SARS-CoV-2・ヒトメタニューモウイルス等（2012年〜）"),
            tags$li(tags$strong("エンテロウイルス(1)(2):"), "エンテロウイルス・コクサッキーウイルス・ポリオウイルス等（2012年〜）"),
            tags$li(tags$strong("胃腸炎ウイルス:"), "ノロウイルス・ロタウイルス・アストロウイルス等（2012年〜）"),
            tags$li(tags$strong("アデノウイルス:"), "アデノウイルス各型（2012年〜）"),
            tags$li(tags$strong("ヘルペス群ウイルス&その他:"), "HSV・VZV・EBV・CMV・HHV等（2012年〜）"),
            tags$li(tags$strong("腸管出血性大腸菌（VTEC）:"), "血清型別週次データを月別集計（2012年〜）"),
            tags$li(tags$strong("腸管感染症（赤痢・チフス・コレラ）:"), "国内例・海外例を月別集計（2012年〜）"),
            tags$li(tags$strong("食中毒菌:"), "サルモネラ・カンピロバクター・腸炎ビブリオ等（2012年〜）")
          ),
          tags$h5("注意事項"),
          tags$ul(
            tags$li("報告数は分離・検出件数であり、患者数・感染者数ではありません"),
            tags$li("報告は参加施設（地方衛生研究所等）からの自発的報告に基づくため、全数を反映しているわけではありません"),
            tags$li("施設数・検査体制の変化により経年比較には注意が必要です"),
            tags$li("速報値のため、後日修正される場合があります"),
            tags$li("直近のデータは今後遅れて報告される可能性があります（グラフ上では直近1か月をグレーの網掛けで示しています）"),
            tags$li("グラフ下の表はCSVダウンロードが可能です（現在の絞り込み・期間設定を反映）")
          ),
          tags$br(),

          # ── ARI病原体 ────────────────────────────────────────
          tags$h4(id="notes-ari-pathogen", "■ ARI病原体タブ", style="border-bottom:2px solid #16a085;padding-bottom:4px;color:#2c3e50;"),
          tags$h5("データソース"),
          tags$p("急性呼吸器感染症（ARI）サーベイランス週報　国立健康危機管理研究機構（JIHS）"),
          tags$ul(
            tags$li("ARI病原体定点（基幹定点）における検体採取週ごとの病原体別報告数・陽性率・全国および地域別報告数（タブ内のプルダウンで地域を切り替え）"),
            tags$li(tags$a(href="https://id-info.jihs.go.jp/surveillance/idss/target-diseases/acute-respiratory-infection/weekly-report/index.html",
                           target="_blank", "JIHS ARIサーベイランス週報（原本PDF）")),
            tags$li("データ更新: 毎日午前3時に最新週報PDFを自動取得・再集計")
          ),
          tags$h5("注意事項（重要）"),
          tags$ul(
            tags$li(tags$strong("このタブの数値は近似値です。"), "週報PDFには図（グラフ画像）のみが掲載され、",
                    "元データの数値表は公表されていません。グラフをレンダリングした画像を、凡例の色と照合して",
                    "数値化（ピクセル解析）しているため、元の値と完全には一致しない場合があります"),
            tags$li("特に色調の近い亜型間（パラインフルエンザウイルス1〜4型、RSウイルスA/B/型不明、",
                    "インフルエンザ各亜型など）では、誤差が生じやすい点にご留意ください"),
            tags$li("報告週ではなく検体採取週で集計。集計時点における報告数のため、過去の週報と値が変わる場合があります"),
            tags$li("正確な数値が必要な場合は、必ず出典元のJIHS公式週報PDFをご確認ください")
          ),
          tags$br(),

          # ── データ更新スケジュール ─────────────────────────
          tags$h4("■ データ更新スケジュール", style="border-bottom:2px solid #1abc9c;padding-bottom:4px;color:#2c3e50;"),
          tags$table(class="table table-bordered table-sm", style="font-size:0.85em;",
            tags$thead(tags$tr(tags$th("データ"), tags$th("更新頻度"), tags$th("取得元"))),
            tags$tbody(
              tags$tr(tags$td("定点把握（IBS）"), tags$td("毎日 03:00 自動"), tags$td("JIHS IDWR")),
              tags$tr(tags$td("全数把握"), tags$td("毎日 03:00 自動"), tags$td("JIHS 感染症発生動向調査")),
              tags$tr(tags$td("EBSニュース"), tags$td("毎日 03:00 自動（1年キャッシュ）"), tags$td("各RSS・Google News")),
              tags$tr(tags$td("Google Trends"), tags$td("毎日 03:00 自動"), tags$td("gtrendsR API")),
              tags$tr(tags$td("病原体検出（IASR）"), tags$td("毎日 03:00 自動（24時間キャッシュ）"), tags$td("JIHS IASR")),
              tags$tr(tags$td("入院サーベイランス"), tags$td("毎日 03:00 自動（直近3号分を再取得）"), tags$td("JIHS IDWR週報PDF")),
              tags$tr(tags$td("ARI病原体"), tags$td("毎日 03:00 自動（最新週報を再取得）"), tags$td("JIHS ARIサーベイランス週報PDF"))
            )
          ),
          tags$br(),

          # ── 免責事項 ──────────────────────────────────────
          tags$h4("■ 免責事項", style="border-bottom:2px solid #95a5a6;padding-bottom:4px;color:#2c3e50;"),
          tags$div(style="background:#f8f9fa;border:1px solid #dee2e6;border-radius:6px;padding:12px;",
            tags$p("本ダッシュボードは感染症サーベイランス情報の統合・可視化を目的とした研究用ツールです。"),
            tags$ul(
              tags$li("掲載情報は公開データをもとに自動集計したものであり、公式発表とは異なる場合があります"),
              tags$li("Rt推定値・シグナルレベルはアルゴリズムによる自動算出であり、専門家の判断を代替するものではありません"),
              tags$li("本ダッシュボードの情報に基づく判断・行動については利用者自身の責任において行ってください"),
              tags$li("データの正確性・完全性・最新性について保証するものではありません")
            )
          )
        )
      ),

      # ── 参考リンク（感染症情報センター等）────────────────
      tabPanel("参考リンク", icon=icon("link"),
        render_idsc_links_ui()
      )

      ) # sub_other
      )
    ),

    # ── フッター ────────────────────────────────────────────
    tags$div(
      style = "background:#2c3e50;color:#bdc3c7;font-size:0.78em;text-align:center;padding:10px 20px;width:100%;margin-top:4px;",
      "文部科学省科学研究費助成事業「IBSとEBSの統合解析によるEpidemic Intelligence活動支援モデル構築（研究代表：小林祐介）」により開発し、試行運用しています。",
      tags$br(),
      "本ダッシュボードは公開情報のみを使用しています。"
    )
  )
)

# ============================================================
# SERVER
# ============================================================
EBS_DLABEL <- c(
  # 定点把握
  flu="インフルエンザ", covid="COVID-19", rsv="RSウイルス", ari="急性呼吸器感染症",
  hfmd="手足口病", varicella="水痘", mumps="流行性耳下腺炎", mycop="マイコプラズマ肺炎",
  gi="感染性胃腸炎", strep="A群溶血性レンサ球菌咽頭炎",
  # 1類
  ebola="エボラ出血熱", crimean_congo="クリミア・コンゴ出血熱", smallpox="痘そう",
  south_am_hem="南米出血熱", plague="ペスト", marburg="マールブルグ病", lassa="ラッサ熱",
  # 2類
  polio="急性灰白髄炎", tb="結核", diphtheria="ジフテリア",
  sars="重症急性呼吸器症候群", mers="中東呼吸器症候群",
  avian_h5n1="鳥インフルエンザ（H5N1）", avian_h7n9="鳥インフルエンザ（H7N9）",
  # 3類
  cholera="コレラ", dysentery="細菌性赤痢", ehec="腸管出血性大腸菌感染症",
  typhoid="腸チフス", paratyphoid="パラチフス",
  # 4類
  hep_e="E型肝炎", west_nile="ウエストナイル熱", hep_a="A型肝炎",
  echinococcus="エキノコックス症", mpox="エムポックス", yellow_fever="黄熱",
  psittacosis="オウム病", omsk_hem="オムスク出血熱", relapsing_f="回帰熱",
  kyasanur="キャサヌル森林病", q_fever="Q熱", rabies="狂犬病",
  coccidioides="コクシジオイデス症", zika="ジカウイルス感染症",
  sfts="重症熱性血小板減少症候群", hfrs="腎症候性出血熱", wee="西部ウマ脳炎",
  tick_enceph="ダニ媒介脳炎", anthrax="炭疽", chikungunya="チクングニア熱",
  scrub="つつが虫病", dengue="デング熱", eee="東部ウマ脳炎",
  avian_other="鳥インフルエンザ(H5N1除く)", nipah="ニパウイルス感染症",
  spotted_f="日本紅斑熱", japanese_enc="日本脳炎", hps="ハンタウイルス肺症候群",
  b_virus="Bウイルス病", glanders="鼻疽", brucella="ブルセラ症",
  vee="ベネズエラウマ脳炎", hendra="ヘンドラウイルス感染症", typhus="発しんチフス",
  botulism="ボツリヌス症", malaria="マラリア", tularemia="野兎病",
  lyme="ライム病", lyssavirus="リッサウイルス感染症", rift_valley="リフトバレー熱",
  melioidosis="類鼻疽", legionella="レジオネラ症", leptospira="レプトスピラ症",
  rocky_mtn="ロッキー山紅斑熱",
  # 5類全数
  ameba="アメーバ赤痢", hep_viral="ウイルス性肝炎",
  cre="カルバペネム耐性腸内細菌目細菌感染症", acute_flaccid="急性弛緩性麻痺",
  encephalitis="急性脳炎", cryptospor="クリプトスポリジウム症",
  cjd="クロイツフェルト・ヤコブ病", igas="劇症型溶血性レンサ球菌感染症",
  aids="後天性免疫不全症候群", giardia="ジアルジア症",
  invasive_hib="侵襲性インフルエンザ菌感染症", invasive_mening="侵襲性髄膜炎菌感染症",
  invasive_pneu="侵襲性肺炎球菌感染症", varicella_hosp="水痘（入院例）",
  crs="先天性風しん症候群", mdra="多剤耐性緑膿菌感染症", syphilis="梅毒",
  crypto_dissem="播種性クリプトコックス症", tetanus="破傷風",
  vrsa="バンコマイシン耐性黄色ブドウ球菌感染症", vre="バンコマイシン耐性腸球菌感染症",
  pertussis="百日咳", rubella="風しん", measles="麻しん",
  dra="薬剤耐性アシネトバクター感染症",
  general="感染症全般", other="その他"
)

# 定点把握疾患の「全国」系列を取得する。JIHS週報CSVの「総数」行（全国の定点あたり
# 報告数の公式値）が存在する週はそれを使い、存在しない週（旧キャッシュ等）は
# 都道府県の単純平均で補完するフォールバック方式
national_series_for <- function(did, dr = NULL) {
  avg <- SURV_DATA %>% filter(disease == did, region %in% unique(PREF_MASTER$region))
  if (!is.null(dr)) avg <- avg %>% filter(date >= dr[1], date <= dr[2])
  avg <- avg %>%
    group_by(date, year, week) %>%
    summarise(avg_val = mean(reports_per_site, na.rm = TRUE), .groups = "drop") %>%
    arrange(date)

  off <- if (is.null(SURV_DATA_NATIONAL) || nrow(SURV_DATA_NATIONAL) == 0) {
    tibble(date = as.Date(character()), reports_per_site = numeric())
  } else {
    SURV_DATA_NATIONAL %>% filter(disease == did)
  }
  if (!is.null(dr)) off <- off %>% filter(date >= dr[1], date <= dr[2])
  off <- off %>% select(date, official_val = reports_per_site)

  avg %>%
    left_join(off, by = "date") %>%
    mutate(reports_per_site = dplyr::coalesce(official_val, avg_val)) %>%
    select(date, year, week, reports_per_site) %>%
    arrange(date)
}

server <- function(input, output, session) {

  # Plotly.js依存の先読み確保用（UI側の非表示div参照、詳細はUI定義部のコメント参照）。
  # 非表示(display:none)のままだとShinyのデフォルト挙動(suspendWhenHidden)で
  # このoutputが評価されず依存関係も読み込まれないため、明示的に無効化する
  output$plotly_dep_loader <- renderPlotly({ plot_ly() })
  outputOptions(output, "plotly_dep_loader", suspendWhenHidden = FALSE)

  # ── 初回アクセス時の注意事項ポップアップ ─────────────────
  showModal(modalDialog(
    title = "ご利用にあたっての注意事項",
    tags$div(style="line-height:1.8;font-size:0.95em;",
      tags$p(
        "本サイトは、文部科学省科学研究費助成事業「IBSとEBSの統合解析によるEpidemic Intelligence活動支援モデル構築（研究代表：小林祐介）」により開発し、試行運用しています。"
      ),
      tags$p(
        "試験運用中のため、サイト内の内容、レイアウト、データソース等は変更される可能性があります。"
      ),
      tags$p(
        "本ダッシュボードは公開情報のみを使用していますが一部暫定データを利用しているため、データの正確性・完全性・最新性について保証するものではありません。最新データは公式サイトを参照してください。"
      ),
      tags$p(
        "本サイトで用いている評価は試験的なアルゴリズムによる判定であり、情報に基づく判断は専門家を代替するものではありません。必ず、元データを参照し、利用者自身の責任において利用してください。"
      )
    ),
    footer = modalButton("同意して閉じる"),
    easyClose = TRUE,
    size = "m"
  ))

  # ── 状態復元（localStorage → Shiny inputs） ───────────────
  observeEvent(input[["_restored_state"]], {
    s <- input[["_restored_state"]]
    if (is.null(s)) return()
    # ラジオボタン
    if (!is.null(s$ts_mode))
      updateRadioButtons(session, "ts_mode", selected = s$ts_mode)
    # サイドバー セレクト
    if (!is.null(s$disease))
      updateSelectInput(session, "disease", selected = s$disease)
    if (!is.null(s$zensu_disease_ts))
      updateSelectInput(session, "zensu_disease_ts", selected = s$zensu_disease_ts)
    if (!is.null(s$pref_filter))
      updateSelectInput(session, "pref_filter", selected = s$pref_filter)
    # EBSニュース（国内）
    if (!is.null(s$ebs_signal))
      updateSelectInput(session, "ebs_signal", selected = s$ebs_signal)
    if (!is.null(s$ebs_disease_filter))
      updateSelectInput(session, "ebs_disease_filter", selected = s$ebs_disease_filter)
    if (!is.null(s$ebs_recent_only))
      updateCheckboxInput(session, "ebs_recent_only", value = isTRUE(s$ebs_recent_only))
    if (!is.null(s$ebs_show_pubmed))
      updateCheckboxInput(session, "ebs_show_pubmed", value = isTRUE(s$ebs_show_pubmed))
    if (!is.null(s$ebs_official_only))
      updateCheckboxInput(session, "ebs_official_only", value = isTRUE(s$ebs_official_only))
    if (!is.null(s$ebs_page_size))
      updateSelectInput(session, "ebs_page_size", selected = s$ebs_page_size)
    # EBSニュース（海外）
    if (!is.null(s$ebs_ov_signal))
      updateSelectInput(session, "ebs_ov_signal", selected = s$ebs_ov_signal)
    if (!is.null(s$ebs_ov_disease))
      updateSelectInput(session, "ebs_ov_disease", selected = s$ebs_ov_disease)
    if (!is.null(s$ebs_ov_recent_only))
      updateCheckboxInput(session, "ebs_ov_recent_only", value = isTRUE(s$ebs_ov_recent_only))
    if (!is.null(s$ebs_ov_official_only))
      updateCheckboxInput(session, "ebs_ov_official_only", value = isTRUE(s$ebs_ov_official_only))
    if (!is.null(s$ebs_ov_page_size))
      updateSelectInput(session, "ebs_ov_page_size", selected = s$ebs_ov_page_size)
    # 複数疾患比較
    if (!is.null(s$multi_group))
      updateSelectInput(session, "multi_group", selected = s$multi_group)
    if (!is.null(s$multi_view))
      updateSelectInput(session, "multi_view", selected = s$multi_view)
    # 実効再生産数
    # rt_disease はサイドバーの疾患選択と双方向同期しているため、ここで独立に
    # 復元すると保存時の値とずれて起動直後にプルダウンが勝手に切り替わる原因に
    # なる。disease の復元（上記）に伴う同期に任せ、ここでは復元しない。
    if (!is.null(s$rt_zensu_disease))
      updateSelectInput(session, "rt_zensu_disease", selected = s$rt_zensu_disease)
    # アクティブタブ
    if (!is.null(s$main_tabs))
      updateTabsetPanel(session, "main_tabs", selected = s$main_tabs)
  }, once = TRUE, ignoreNULL = TRUE)

  # ── last_update.txt を60秒ごとに再読み（外部スクリプト更新を検知）
  last_update_reader <- reactiveFileReader(
    intervalMillis = 60000,
    session        = session,
    filePath       = "data/last_update.txt",
    readFunc       = function(f) {
      if (file.exists(f)) readLines(f, warn=FALSE)[1] else "不明"
    }
  )

  # ── データソースバナー ─────────────────────────────────
  output$data_source_banner <- renderUI({
    fetch_time <- last_update_reader()
    if (IS_REAL_DATA) {
      tags$div(
        style = paste0(
          "background:#fff3cd;border:1px solid #ffc107;border-radius:6px;",
          "padding:8px 14px;font-size:0.82em;color:#856404;margin-bottom:12px;",
          "display:flex;justify-content:space-between;align-items:center;"
        ),
        tags$div(
          tags$strong("ℹ データについて: "),
          "過去分（概ね2023年以前）は感染症発生動向調査事業年報の確定値、",
          "直近分（当年の未確定期間）は感染症発生動向調査週報（IDWR）の速報値（暫定値）を使用しています。",
          "速報値は後日修正される場合があります。詳細・確定値は",
          tags$a(href="https://id-info.jihs.go.jp/surveillance/idwr/",
                 target="_blank", style="color:#856404;",
                 "JIHS公式サイト"),
          "でご確認ください。",
          tags$span(style="margin-left:12px;",
            paste0("最終データ取得: ", fetch_time)
          )
        ),
      )
    } else {
      tags$div(class="demo-banner",
        tags$strong("⚠ デモデータ表示中: "),
        "JIHS IDWRへの接続に失敗したため、参考用のシミュレーションデータを表示しています。",
        "ネットワーク接続を確認してください。"
      )
    }
  })

  # ── 自動更新タイマー（1日1回 正午） ────────────────────
  next_3am <- function() {
    now <- Sys.time()
    t3  <- as.POSIXct(format(now, "%Y-%m-%d 03:00:00"), tz = Sys.timezone())
    if (now >= t3) t3 <- t3 + 86400
    t3
  }
  # shinyapps.io はアイドル時にプロセスが停止するため、Rプロセスの起動タイミングが
  # 3時を過ぎていた場合、next_3am()は「翌日の3時」を返してしまい、その日の更新が
  # 丸ごとスキップされてしまう（起動時刻が3時台でなければ、当日分の自動更新が実行されない）。
  # そこで起動時に「当日分の更新がまだ行われていないか」を確認し、未実施なら
  # 次回3時を待たずに即座にキャッチアップする。
  initial_refresh_at <- local({
    lu <- tryCatch(readLines("data/last_update.txt", n = 1, warn = FALSE)[1],
                    error = function(e) NA_character_)
    last_date <- tryCatch(as.Date(substr(lu, 1, 10)), error = function(e) as.Date(NA))
    today3am  <- as.POSIXct(paste0(format(Sys.Date(), "%Y-%m-%d"), " 03:00:00"), tz = Sys.timezone())
    if ((is.na(last_date) || last_date < Sys.Date()) && Sys.time() >= today3am) {
      Sys.time()
    } else {
      next_3am()
    }
  })
  next_refresh_at  <- reactiveVal(initial_refresh_at)
  ebs_data         <- reactiveVal(EBS_CACHE)
  ebs_refresh_time <- reactiveVal(Sys.time())
  surv_updated     <- reactiveVal(NULL)
  auto_refreshing  <- reactiveVal(FALSE)

  # ── 自動更新ロジック（毎分チェック、正午到達時に実行） ────
  observe({
    next_t   <- next_refresh_at()
    ms_until <- as.numeric(difftime(next_t, Sys.time(), units="secs")) * 1000
    # 次回チェックは「残り時間」か「1分後」の短い方（最大1時間）
    invalidateLater(max(60000, min(ms_until, 3600000)))

    isolate({
      if (Sys.time() >= next_t && !auto_refreshing()) {
        auto_refreshing(TRUE)
        showNotification("自動更新中（定点データ）...", type="message", duration=NULL, id="auto_upd")

        # ① 定点把握データ
        tryCatch({
          new_surv <- get_surveillance_data(years=2001:2026)
          SURV_DATA <<- new_surv
          record_data_change("ibs", compute_recent_signature(new_surv, "date", 90))
        }, error=function(e) message("自動更新 IBS エラー: ", e$message))

        # ② 全数把握データ
        showNotification("自動更新中（全数データ）...", type="message", duration=NULL, id="auto_upd")
        tryCatch({
          new_zensu <- get_zensu_data(years=2001:2026)
          ZENSU_DATA <<- new_zensu
          record_data_change("zensu", compute_recent_signature(new_zensu, "date", 90))
        }, error=function(e) message("自動更新 Zensu エラー: ", e$message))

        # ③ EBSニュース
        showNotification("自動更新中（EBSニュース）...", type="message", duration=NULL, id="auto_upd")
        tryCatch({
          new_ebs <- fetch_all_ebs(use_gnews=TRUE, bearer_token=NULL)
          merged  <- merge_ebs_cache(new_ebs)
          ebs_data(merged)
          ebs_refresh_time(Sys.time())
          save_ebs_cache(merged)
          record_data_change("ebs", compute_recent_signature(merged, "pub_date", 60))
        }, error=function(e) message("自動更新 EBS エラー: ", e$message))

        surv_updated(Sys.time())
        tryCatch(writeLines(format(Sys.time(), "%Y-%m-%d %H:%M"), "data/last_update.txt"), error=function(e) NULL)
        next_refresh_at(next_3am())
        auto_refreshing(FALSE)
        removeNotification("auto_upd")
        showNotification(
          paste0("自動更新完了: ", format(Sys.time(), "%m/%d %H:%M")),
          type="message", duration=5
        )
      }
    })
  })

  # ── 手動更新ボタン ────────────────────────────────────
  observeEvent(input$ebs_refresh, {
    showNotification("EBSニュース取得中...", type="message", duration=NULL, id="ebs_upd")
    merged <- tryCatch({
      new_data <- fetch_all_ebs(use_gnews = TRUE)
      merge_ebs_cache(new_data)
    }, error = function(e) { showNotification(e$message, type="error"); ebs_data() })
    ebs_data(merged)
    ebs_refresh_time(Sys.time())
    save_ebs_cache(merged)
    tryCatch(record_data_change("ebs", compute_recent_signature(merged, "pub_date", 60)),
             error = function(e) NULL)
    removeNotification("ebs_upd")
    showNotification(
      paste0("EBSニュース更新完了（過去1年・", nrow(merged), "件）"),
      type="message", duration=3)
  })

  observeEvent(input$date_range_reset, {
    updateSliderInput(session, "date_range", value = c(Sys.Date() - 365*3, Sys.Date()))
  })

  observeEvent(input$surv_refresh, {
    showNotification("定点データ取得中...", type="message", duration=NULL, id="surv_upd")
    tryCatch({
      new_surv <- get_surveillance_data(years=2001:2026)
      SURV_DATA <<- new_surv
      surv_updated(Sys.time())
      record_data_change("ibs", compute_recent_signature(new_surv, "date", 90))
      removeNotification("surv_upd")
      showNotification("定点データ更新完了（キャッシュ保存済）", type="message", duration=3)
    }, error=function(e) showNotification(paste("エラー:", e$message), type="error"))
  })

  observeEvent(input$zensu_refresh, {
    showNotification("全数データ取得中...", type="message", duration=NULL, id="zensu_upd")
    tryCatch({
      new_zensu <- get_zensu_data(years=2001:2026)
      ZENSU_DATA <<- new_zensu
      surv_updated(Sys.time())
      record_data_change("zensu", compute_recent_signature(new_zensu, "date", 90))
      removeNotification("zensu_upd")
      showNotification("全数データ更新完了（キャッシュ保存済）", type="message", duration=3)
    }, error=function(e) showNotification(paste("エラー:", e$message), type="error"))
  })

  output$surv_last_updated <- renderUI({ NULL })

  output$data_fetch_times <- renderUI({
    # 各ソースについて「取得元データに実質的な変化があったと最後に判断した日時」を表示。
    # 単なる取得（チェック）日時ではなく、内容のハッシュ比較により変化がない限り
    # 前回の判定日時を維持する（change_tracker.R）。
    fmt_change <- function(source_id) {
      t <- tryCatch(get_last_change_time(source_id), error = function(e) NA)
      if (length(t) == 0 || is.na(t)) "不明" else format(t, "%Y-%m-%d %H:%M")
    }

    ibs_time   <- fmt_change("ibs")
    zensu_time <- fmt_change("zensu")
    ebs_time   <- fmt_change("ebs")
    iasr_time  <- fmt_change("iasr")

    style_row <- "font-size:0.78em;color:#888;padding:2px 0;display:flex;justify-content:space-between;"
    style_label <- "color:#aaa;"
    style_val <- "color:#555;font-weight:600;"

    tags$div(style="margin-top:4px;",
      tags$div(style="font-size:0.72em;color:#aaa;margin-bottom:2px;", "更新検知日時（内容に変化があった最終日時）"),
      tags$div(style=style_row,
        tags$span(style=style_label, "定点把握:"),
        tags$span(style=style_val, ibs_time)
      ),
      tags$div(style=style_row,
        tags$span(style=style_label, "全数把握:"),
        tags$span(style=style_val, zensu_time)
      ),
      tags$div(style=style_row,
        tags$span(style=style_label, "EBSニュース:"),
        tags$span(style=style_val, ebs_time)
      ),
      tags$div(style=style_row,
        tags$span(style=style_label, "病原体検出:"),
        tags$span(style=style_val, iasr_time)
      )
    )
  })

  output$auto_refresh_status <- renderUI({
    t <- next_refresh_at()
    tags$span(style="color:#aaa;font-size:0.78em;line-height:50px;padding-right:10px;",
      paste0("次回自動更新: ", format(t, "%m/%d %H:%M")))
  })
  output$next_refresh_ui <- renderUI({
    tags$span(style="color:#856404;",
      paste0("次回自動更新: ", format(next_refresh_at(), "%m/%d %H:%M")))
  })

  # ── データソース表示ヘルパー ────────────────────────────
  # is_provisional==FALSEは「確定値」「デモデータ」の両方であり得るため、
  # data_sourceの文字列（"デモデータ"を含むか）で区別する
  is_demo_source <- function(d) grepl("デモデータ", ifelse(is.na(d$data_source), "", d$data_source))

  provisional_label <- reactive({
    d <- filtered_data()
    if (nrow(d) == 0) return(NULL)
    if (any(d$is_provisional, na.rm = TRUE)) {
      tags$span(
        style = "color:#e67e22;font-weight:600;margin-left:8px;",
        "⚠ 速報値（暫定値）"
      )
    } else if (any(is_demo_source(d))) {
      tags$span(style = "color:#95a5a6;margin-left:8px;", "※デモデータ")
    } else {
      tags$span(style = "color:#27ae60;margin-left:8px;", "✓ 確定値")
    }
  })

  make_source_bar <- function(label_text) {
    d <- filtered_data()
    src <- if (nrow(d) > 0) unique(d$data_source)[1] else "―"
    tags$div(class = "data-source-bar",
      label_text,
      tags$span(style = "float:right;font-size:0.85em;",
        "出典: ", tags$strong(src),
        if (nrow(d) > 0 && any(d$is_provisional, na.rm = TRUE))
          tags$span(style = "color:#e67e22;font-weight:600;margin-left:6px;",
                    "⚠ 速報値（暫定値）")
        else if (nrow(d) > 0 && !any(is_demo_source(d)))
          tags$span(style = "color:#27ae60;font-weight:600;margin-left:6px;",
                    "✓ 確定値")
      )
    )
  }

  output$map_source_bar <- renderUI({
    sel <- map_selected_date()
    is_zensu <- !is.null(input$ts_mode) && input$ts_mode == "zensu"
    is_std   <- !is_zensu && input$disease %in% names(STD_DISEASE_CONFIG)
    week_label <- if (is_std) {
      if (!is.null(sel)) paste0(format(sel, "%Y年%m月"), " の都道府県別 月次報告数")
      else "都道府県別 月次報告数"
    } else if (!is.null(sel)) {
      paste0(format(sel, "%Y年第%W週（%m/%d〜）"), " の都道府県別 定点あたり報告数")
    } else "都道府県別 定点あたり報告数"
    make_source_bar(week_label)
  })
  output$ts_source_bar <- renderUI({
    make_source_bar("週別 定点あたり報告数（全国平均 + 選択都道府県）")
  })

  # ── フィルターバー（各タブ共通） ──────────────────────────────
  # 疾患ラベル逆引き用テーブル（EBSニュース）
  EBS_DISEASE_LABELS <- c(
    "flu"="インフルエンザ","covid"="COVID-19","rsv"="RSウイルス感染症",
    "ari"="急性呼吸器感染症（ARI）","hfmd"="手足口病","varicella"="水痘",
    "mumps"="流行性耳下腺炎","mycop"="マイコプラズマ肺炎","gi"="感染性胃腸炎",
    "strep"="A群溶血性レンサ球菌咽頭炎","measles"="麻疹（はしか）","rubella"="風疹",
    "pertussis"="百日咳","syphilis"="梅毒","ehec"="腸管出血性大腸菌感染症",
    "dengue"="デング熱","mpox"="エムポックス","tb"="結核",
    "igas"="劇症型溶血性レンサ球菌感染症","invasive_pneu"="侵襲性肺炎球菌感染症",
    "legionella"="レジオネラ症","hep_a"="Ａ型肝炎",
    "sfts"="重症熱性血小板減少症候群（SFTS）","ebola"="エボラ出血熱",
    "chlamydia_genital"="性器クラミジア感染症","herpes_genital"="性器ヘルペスウイルス感染症",
    "condyloma"="尖圭コンジローマ","gonorrhea"="淋菌感染症",
    "mrsa"="メチシリン耐性黄色ブドウ球菌感染症","prsp"="ペニシリン耐性肺炎球菌感染症",
    "general"="感染症全般"
  )

  make_filter_bar <- function(items) {
    chips <- Filter(Negate(is.null), lapply(names(items), function(label) {
      val <- items[[label]]
      if (is.null(val) || (length(val) == 1 &&
          (is.na(val) || nchar(trimws(as.character(val))) == 0))) return(NULL)
      display <- paste(val, collapse = "・")
      tags$span(
        style = paste0(
          "display:inline-flex;align-items:center;gap:4px;",
          "background:#eaf3fb;border:1px solid #aac8e8;border-radius:4px;",
          "padding:2px 8px;margin:2px 4px 2px 0;font-size:0.78em;color:#2c5f8a;"
        ),
        tags$span(style="color:#6c8fa8;font-weight:600;", paste0(label, ":")),
        tags$span(style="font-weight:500;", display)
      )
    }))
    if (length(chips) == 0) return(NULL)
    tags$div(
      style = paste0(
        "display:flex;flex-wrap:wrap;align-items:center;",
        "background:#f4f9fd;border:1px solid #d0e4f5;border-radius:5px;",
        "padding:5px 8px;margin-bottom:8px;"
      ),
      tags$span(
        style = "font-size:0.72em;color:#888;margin-right:6px;white-space:nowrap;",
        icon("filter", style="font-size:0.9em;margin-right:2px;"), "フィルター"
      ),
      chips
    )
  }

  # 定点/全数 疾患ラベル取得ヘルパー
  # 定点把握疾患の選択肢には週次報告（DISEASE_CONFIG）と月次報告
  # （STD_DISEASE_CONFIG）の両方が含まれるため、teiten_idはどちらか一致する方を見る
  get_disease_label <- function(mode, teiten_id, zensu_id, std_id = NULL) {
    if (is.null(mode) || mode == "teiten") {
      dc <- DISEASE_CONFIG[[teiten_id]]
      if (!is.null(dc)) return(dc$label)
      dc <- STD_DISEASE_CONFIG[[teiten_id]]
      if (!is.null(dc)) dc$label else teiten_id
    } else {
      dc <- ZENSU_DISEASE_CONFIG[[zensu_id]]
      if (!is.null(dc)) dc$label else zensu_id
    }
  }

  # 地図タブ
  output$filter_bar_map <- renderUI({
    disease_label <- get_disease_label(input$ts_mode, input$disease, input$zensu_disease_ts)
    pref_val <- if (!is.null(input$pref_filter)) input$pref_filter else "全国"
    region_val <- NULL
    sel <- map_selected_date()
    is_zensu <- !is.null(input$ts_mode) && input$ts_mode == "zensu"
    is_std   <- !is_zensu && input$disease %in% names(STD_DISEASE_CONFIG)
    week_val <- if (is.null(sel)) NULL
                else if (is_std) format(sel, "%Y年%m月")
                else format(sel, "%Y年第%W週（%m/%d〜）")
    make_filter_bar(list(
      "疾患"     = disease_label,
      "表示週"   = week_val,
      "都道府県" = pref_val,
      "地域"     = region_val
    ))
  })

  # 流行曲線タブ
  output$filter_bar_ts <- renderUI({
    disease_label <- get_disease_label(input$ts_mode, input$disease, input$zensu_disease_ts)
    pref_val <- if (!is.null(input$pref_filter)) input$pref_filter else "全国"
    dr <- input$date_range
    date_str <- if (!is.null(dr))
      paste0(format(dr[1], "%Y/%m/%d"), " 〜 ", format(dr[2], "%Y/%m/%d")) else NULL
    region_val <- NULL
    make_filter_bar(list(
      "疾患"     = disease_label,
      "都道府県" = pref_val,
      "期間"     = date_str,
      "地域"     = region_val
    ))
  })

  # 複数疾患比較タブ
  output$filter_bar_multi <- renderUI({
    dr <- input$date_range
    date_str <- if (!is.null(dr))
      paste0(format(dr[1], "%Y/%m/%d"), " 〜 ", format(dr[2], "%Y/%m/%d")) else NULL
    all_diseases <- names(DISEASE_CONFIG)
    sel <- input$multi_diseases
    disease_val <- if (is.null(sel) || setequal(sel, all_diseases)) {
      "全疾患"
    } else {
      labels <- sapply(sel, function(k) { dc <- DISEASE_CONFIG[[k]]; if (!is.null(dc)) dc$label else k })
      paste(labels, collapse = "・")
    }
    grp_choices <- c("すべて" = "all",
      setNames(names(DISEASE_GROUPS), sapply(DISEASE_GROUPS, `[[`, "label")))
    grp_label <- if (!is.null(input$multi_group) && input$multi_group != "all")
      names(grp_choices)[grp_choices == input$multi_group] else NULL
    view_label <- switch(if (is.null(input$multi_view)) "overlay_raw" else input$multi_view,
      overlay_raw = "重ね合わせ", facet = "2×2 グリッド", input$multi_view)
    make_filter_bar(list(
      "疾患"        = disease_val,
      "期間"        = date_str,
      "疾患グループ" = grp_label,
      "表示形式"    = view_label
    ))
  })

  # 実効再生産数タブ
  output$filter_bar_rt <- renderUI({
    is_teiten <- is.null(input$ts_mode) || input$ts_mode == "teiten"
    disease_label <- if (is_teiten) {
      dc <- DISEASE_CONFIG[[input$rt_disease]]; if (!is.null(dc)) dc$label else input$rt_disease
    } else {
      dc <- ZENSU_DISEASE_CONFIG[[input$rt_zensu_disease]]; if (!is.null(dc)) dc$label else input$rt_zensu_disease
    }
    pref_val <- if (!is.null(input$pref_filter)) input$pref_filter else "全国"
    dr <- input$date_range
    date_str <- if (!is.null(dr))
      paste0(format(dr[1], "%Y/%m/%d"), " 〜 ", format(dr[2], "%Y/%m/%d")) else NULL
    make_filter_bar(list(
      "疾患"     = disease_label,
      "都道府県" = pref_val,
      "期間"     = date_str
    ))
  })

  # EBSニュース（国内）フィルターバー
  output$filter_bar_ebs <- renderUI({
    sig_val <- if (!is.null(input$ebs_signal) && input$ebs_signal != "すべて")
      input$ebs_signal else NULL
    dis_id  <- input$ebs_disease_filter
    dis_val <- if (!is.null(dis_id) && dis_id != "すべて" && nchar(dis_id) > 0) {
      lbl <- EBS_DISEASE_LABELS[[dis_id]]; if (!is.null(lbl)) lbl else dis_id
    } else NULL
    recent_val <- if (isTRUE(input$ebs_recent_only))  "過去2週間のみ" else NULL
    pubmed_val <- if (isTRUE(input$ebs_show_pubmed))  "含む"          else NULL
    make_filter_bar(list(
      "シグナル" = sig_val,
      "疾患"     = dis_val,
      "期間"     = recent_val,
      "PubMed"   = pubmed_val
    ))
  })

  # EBSニュース（海外）フィルターバー
  output$filter_bar_ebs_ov <- renderUI({
    sig_val <- if (!is.null(input$ebs_ov_signal) && input$ebs_ov_signal != "すべて")
      input$ebs_ov_signal else NULL
    dis_id  <- input$ebs_ov_disease
    dis_val <- if (!is.null(dis_id) && dis_id != "すべて" && nchar(dis_id) > 0) {
      lbl <- EBS_DISEASE_LABELS[[dis_id]]; if (!is.null(lbl)) lbl else dis_id
    } else NULL
    recent_val <- if (isTRUE(input$ebs_ov_recent_only)) "過去2週間のみ" else NULL
    make_filter_bar(list(
      "シグナル" = sig_val,
      "疾患"     = dis_val,
      "期間"     = recent_val
    ))
  })

  # EBS Trends タブ フィルターバー
  output$filter_bar_trends <- renderUI({
    disease_label <- get_disease_label(input$ts_mode, input$disease, input$zensu_disease_ts)
    make_filter_bar(list(
      "疾患（Google Trends）" = disease_label
    ))
  })

  # ── IBS フィルタ ───────────────────────────────────────
  filtered_data <- reactive({
    dr <- input$date_range
    SURV_DATA %>%
      filter(disease==input$disease,
             date >= dr[1], date <= dr[2],
             region %in% unique(PREF_MASTER$region))
  })
  latest_week_data <- reactive({
    d <- filtered_data()
    if (nrow(d) == 0) return(d)
    latest <- max(d$date, na.rm=TRUE)
    if (is.infinite(latest)) return(d[0, ])
    d %>% filter(date == latest)
  })

  # ── 地図タブ：週スライダー ──────────────────────────────────
  # データ中の週日付一覧（昇順: 再生が古→新）
  map_available_dates <- reactive({
    is_zensu <- !is.null(input$ts_mode) && input$ts_mode == "zensu"
    is_std   <- !is_zensu && input$disease %in% names(STD_DISEASE_CONFIG)
    if (is_zensu) {
      d <- ZENSU_DATA
      if (is.null(d) || nrow(d) == 0) return(as.Date(character(0)))
      sort(unique(d$date[d$disease == input$zensu_disease_ts]))
    } else if (is_std) {
      d <- STD_DATA
      if (is.null(d) || nrow(d) == 0) return(as.Date(character(0)))
      sort(unique(d$date[d$disease == input$disease]))
    } else {
      sort(unique(filtered_data()$date))
    }
  })

  # スライダーUI（整数インデックス方式 — Dateスライダーはdate_rangeと干渉するため使用しない）
  # 目盛り・つまみの表示ラベルは素のインデックス番号のままだと分かりにくいため、
  # ionRangeSliderのprettifyコールバックで実際の日付文字列に置き換える
  output$map_week_selector_ui <- renderUI({
    dates <- map_available_dates()
    n <- length(dates)
    if (n == 0) return(NULL)
    date_labels <- format(dates, "%Y/%m/%d")
    tags$div(
      style = "padding:2px 4px 6px 4px;",
      # 選択週の日付テキスト表示
      uiOutput("map_week_label"),
      sliderInput(
        "map_week_idx",
        label = NULL,
        min   = 1L,
        max   = n,
        value = n,        # デフォルト = 最新週（末尾）
        step  = 1L,
        width = "100%",
        animate = animationOptions(
          interval    = 700,
          loop        = FALSE,
          playButton  = tags$span(icon("play"),  " 再生"),
          pauseButton = tags$span(icon("pause"), " 一時停止")
        )
      ),
      tags$script(HTML(sprintf(
        "(function(){
           var labels = %s;
           function applyPrettify(){
             var el = $('#map_week_idx');
             var inst = el.data('ionRangeSlider');
             if (inst) {
               inst.update({ prettify: function(num){ return labels[num-1] || num; } });
             } else {
               setTimeout(applyPrettify, 100);
             }
           }
           applyPrettify();
         })();",
        jsonlite::toJSON(date_labels)
      )))
    )
  })

  # 選択インデックス → 実際の日付
  map_selected_date <- reactive({
    dates <- map_available_dates()
    if (length(dates) == 0) return(NULL)
    idx <- input$map_week_idx
    if (is.null(idx) || idx < 1 || idx > length(dates)) return(max(dates))
    dates[as.integer(idx)]
  })

  # スライダー上部に選択週を日本語表示
  output$map_week_label <- renderUI({
    sel <- map_selected_date()
    if (is.null(sel)) return(NULL)
    is_zensu <- !is.null(input$ts_mode) && input$ts_mode == "zensu"
    is_std   <- !is_zensu && input$disease %in% names(STD_DISEASE_CONFIG)
    lbl <- if (is_std) format(sel, "%Y年%m月") else format(sel, "%Y年 第%W週（%m/%d〜）")
    tags$div(
      style = "font-size:0.85em;font-weight:600;color:#2c5f8a;margin-bottom:2px;",
      icon("calendar-week", style = "margin-right:4px;"),
      lbl
    )
  })

  # 地図用データ（定点把握・選択週）
  map_week_data <- reactive({
    sel <- map_selected_date()
    if (is.null(sel)) return(latest_week_data())
    d <- filtered_data()
    if (nrow(d) == 0) return(d)
    res <- d %>% filter(date == sel)
    if (nrow(res) == 0) res <- latest_week_data()
    res
  })

  # 地図用データ（全数把握・選択週）
  zensu_map_week_data <- reactive({
    d <- ZENSU_DATA
    if (is.null(d) || nrow(d) == 0) return(NULL)
    sel <- map_selected_date()
    if (is.null(sel)) return(zensu_latest_week())
    d %>%
      filter(disease == input$zensu_disease_ts, date == sel) %>%
      group_by(pref_name) %>%
      summarise(cases = sum(cases, na.rm = TRUE), .groups = "drop")
  })

  # 地図用データ（月報疾患・選択月）
  std_map_week_data <- reactive({
    d <- STD_DATA
    if (is.null(d) || nrow(d) == 0) return(NULL)
    sel <- map_selected_date()
    if (is.null(sel)) sel <- max(d$date[d$disease == input$disease], na.rm = TRUE)
    d %>%
      filter(disease == input$disease, date == sel) %>%
      group_by(pref_name) %>%
      summarise(reports = sum(reports, na.rm = TRUE), .groups = "drop")
  })
  national_avg <- reactive({
    dr <- input$date_range
    national_series_for(input$disease, dr)
  })

  # ±2SD の歴史参照用：date_range に依存しない全期間データ
  # （date_range をずらしても過去5年分が欠落しないようにするため）
  national_avg_hist <- reactive({
    national_series_for(input$disease, NULL)
  })

  # 都道府県選択時: date_range内の当該都道府県平均（流行曲線の主系列）
  ts_main_data <- reactive({
    pref <- input$pref_filter
    if (!is.null(pref) && pref != "全国") {
      filtered_data() %>%
        filter(pref_name == pref) %>%
        group_by(date, year, week) %>%
        summarise(reports_per_site = mean(reports_per_site, na.rm = TRUE), .groups = "drop") %>%
        arrange(date)
    } else {
      national_avg()
    }
  })

  # 都道府県選択時: date_rangeに依存しない当該都道府県の全期間データ（±2SD帯の算出用）
  ts_hist_data <- reactive({
    pref <- input$pref_filter
    if (!is.null(pref) && pref != "全国") {
      SURV_DATA %>%
        filter(disease == input$disease, pref_name == pref) %>%
        group_by(date, year, week) %>%
        summarise(reports_per_site = mean(reports_per_site, na.rm = TRUE), .groups = "drop") %>%
        arrange(date)
    } else {
      national_avg_hist()
    }
  })

  # 現在表示中の主系列（全国 or 選択都道府県）に IBS方式の±SD帯・逸脱スコアを付与した系列
  # timeseries_plot / kpi_alert / ranking_table / data_tab_df で共通利用
  ts_band_series <- reactive({
    compute_ibs_band(ts_main_data(), ts_hist_data())
  })

  # ── KPI ────────────────────────────────────────────────
  output$kpi_national <- renderUI({
    is_teiten <- is.null(input$ts_mode) || input$ts_mode == "teiten"
    is_std    <- is_teiten && input$disease %in% names(STD_DISEASE_CONFIG)
    if (is_std) {
      pref <- if (!is.null(input$pref_filter) && input$pref_filter != "全国") input$pref_filter else NULL
      d <- if (is.null(STD_DATA)) NULL else {
        x <- STD_DATA %>% filter(disease == input$disease)
        if (!is.null(pref)) x <- x %>% filter(pref_name == pref)
        x %>% group_by(year, month, date) %>%
          summarise(reports = sum(reports, na.rm = TRUE), .groups = "drop") %>% arrange(date)
      }
      if (is.null(d) || nrow(d) == 0) return(tags$div(class="kpi-box",
        tags$div(style="background:#2980b9;color:#fff;font-size:0.65em;font-weight:700;letter-spacing:0.08em;text-align:center;padding:2px 0;margin:-8px -12px 3px -12px;border-radius:4px 4px 0 0;", "IBS"),
        tags$div(class="kpi-value","―"),
        tags$div(class="kpi-label","月次報告数"),
        tags$div(class="kpi-delta flat","データなし")))
      recent2 <- slice_tail(d, n = 2)
      cur  <- slice_tail(recent2, n = 1)
      prev <- if (nrow(recent2) >= 2) slice_head(recent2, n = 1) else cur
      val  <- cur$reports[1]
      prevv <- prev$reports[1]
      delta_pct <- if (!is.na(prevv) && prevv > 0) (val - prevv) / prevv * 100 else NA
      dc <- if(is.na(delta_pct))"flat" else if(delta_pct>5)"up" else if(delta_pct < -5)"down" else "flat"
      dt <- if(is.na(delta_pct))"―" else sprintf("%+.1f%%", delta_pct)
      label <- paste0(if (is.null(pref)) "全国" else pref, " 月次報告数")
      col <- STD_DISEASE_CONFIG[[input$disease]]$color
      return(tags$div(class="kpi-box",
        tags$div(style="background:#2980b9;color:#fff;font-size:0.65em;font-weight:700;letter-spacing:0.08em;text-align:center;padding:2px 0;margin:-8px -12px 3px -12px;border-radius:4px 4px 0 0;", "IBS"),
        tags$div(class="kpi-value", style=paste0("color:",col), sprintf("%d件", as.integer(val))),
        tags$div(class="kpi-label", paste0(label, sprintf(" (%d年%d月)", cur$year[1], cur$month[1]))),
        tags$div(class=paste("kpi-delta",dc), paste0("前月比 ",dt))))
    }
    if (is_teiten) {
      pref <- if (!is.null(input$pref_filter) && input$pref_filter != "全国") input$pref_filter else NULL
      if (!is.null(pref)) {
        d <- latest_week_data()
        fd <- filtered_data()
        cur_d  <- d  %>% filter(pref_name == pref)
        prev_d <- fd %>% filter(date == max(fd$date, na.rm=TRUE) - 7, pref_name == pref)
        label  <- paste0(pref, " 定点あたり報告数")
        val  <- mean(cur_d$reports_per_site, na.rm=TRUE)
        prev <- mean(prev_d$reports_per_site, na.rm=TRUE)
        wk_txt <- if (nrow(cur_d) > 0) sprintf("(%d年第%d週)", cur_d$year[1], cur_d$week[1]) else ""
      } else {
        # 全国値はJIHS公式の「総数」行（都道府県の単純平均ではない）を用いる
        nat <- national_avg()
        label <- "全国 定点あたり報告数"
        if (nrow(nat) == 0) {
          val <- NaN; prev <- NA_real_; wk_txt <- ""
        } else {
          latest_dt <- max(nat$date, na.rm = TRUE)
          cur_row   <- nat %>% filter(date == latest_dt)
          prev_row  <- nat %>% filter(date == latest_dt - 7)
          val  <- cur_row$reports_per_site[1]
          prev <- if (nrow(prev_row) > 0) prev_row$reports_per_site[1] else NA_real_
          wk_txt <- sprintf("(%d年第%d週)", cur_row$year[1], cur_row$week[1])
        }
      }
      delta_pct <- if (!is.na(prev) && prev > 0) (val - prev) / prev * 100 else NA
      dc <- if(is.na(delta_pct))"flat" else if(delta_pct>5)"up" else if(delta_pct < -5)"down" else "flat"
      dt <- if(is.na(delta_pct))"―" else sprintf("%+.1f%%", delta_pct)
      tags$div(class="kpi-box",
        tags$div(style="background:#2980b9;color:#fff;font-size:0.65em;font-weight:700;letter-spacing:0.08em;text-align:center;padding:2px 0;margin:-8px -12px 3px -12px;border-radius:4px 4px 0 0;", "IBS"),
        tags$div(class="kpi-value", style=paste0("color:",DISEASE_CONFIG[[input$disease]]$color),
                 if(is.nan(val)) "―" else sprintf("%.2f", val)),
        tags$div(class="kpi-label", paste(label, wk_txt)),
        tags$div(class=paste("kpi-delta",dc), paste0("前週比 ",dt)))
    } else {
      # 全数把握モード：直近週報告数と前週差
      zd <- zensu_ts_filtered()
      if (is.null(zd) || nrow(zd) == 0) return(tags$div(class="kpi-box",
        tags$div(style="background:#2980b9;color:#fff;font-size:0.65em;font-weight:700;letter-spacing:0.08em;text-align:center;padding:2px 0;margin:-8px -12px 3px -12px;border-radius:4px 4px 0 0;", "IBS"),
        tags$div(class="kpi-value","―"),
        tags$div(class="kpi-label","直近週 報告数"),
        tags$div(class="kpi-delta flat","データなし")))
      latest_dt <- max(zd$date, na.rm=TRUE)
      prev_dt   <- latest_dt - 7
      cur_n  <- zd %>% filter(date == latest_dt) %>% summarise(n=sum(cases,na.rm=TRUE)) %>% pull(n)
      prev_n <- zd %>% filter(date == prev_dt)   %>% summarise(n=sum(cases,na.rm=TRUE)) %>% pull(n)
      diff_n <- if (length(prev_n)>0 && !is.na(prev_n)) cur_n - prev_n else NA
      dc <- if(is.na(diff_n))"flat" else if(diff_n>0)"up" else if(diff_n<0)"down" else "flat"
      dt <- if(is.na(diff_n))"―" else sprintf("%+d件", as.integer(diff_n))
      col <- ZENSU_DISEASE_CONFIG[[input$zensu_disease_ts]]$color
      if (is.null(col)) col <- "#2980b9"

      # 季節性の有無に応じた評価方式で現在の状況を判定（zensu_ibs_band参照）
      band <- tryCatch({
        hist_d <- zensu_hist()
        if (is.null(hist_d)) {
          NULL
        } else {
          recent2 <- zd %>%
            group_by(date, year, week) %>%
            summarise(val = sum(cases, na.rm=TRUE), .groups="drop") %>%
            arrange(date) %>% slice_tail(n = 2)
          if (nrow(recent2) == 0) {
            NULL
          } else {
            names(hist_d)[names(hist_d) == "cases"] <- "reports_per_site"
            cur  <- slice_tail(recent2, n=1)
            prev <- if (nrow(recent2) >= 2) slice_head(recent2, n=1) else cur
            zensu_ibs_band(
              cur_val=cur$val[1], cur_date=cur$date[1], cur_week=cur$week[1], cur_year=cur$year[1],
              prev_val=prev$val[1], prev_date=prev$date[1], prev_week=prev$week[1], prev_year=prev$year[1],
              hist_d=hist_d)
          }
        }
      }, error=function(e) NULL)
      status_col <- if (is.null(band)) "#95a5a6" else
        c("0"="#27ae60","1"="#d4ac0d","2"="#e67e22","3"="#c0392b")[as.character(band$score)]
      method_txt <- if (is.null(band)) "" else if (identical(band$method,"seasonal")) "季節性あり" else "散発疾患向け"

      tags$div(class="kpi-box",
        tags$div(style="background:#2980b9;color:#fff;font-size:0.65em;font-weight:700;letter-spacing:0.08em;text-align:center;padding:2px 0;margin:-8px -12px 3px -12px;border-radius:4px 4px 0 0;", "IBS"),
        tags$div(class="kpi-value", style=paste0("color:",col), cur_n),
        tags$div(class="kpi-label", "直近週 報告数"),
        tags$div(class=paste("kpi-delta",dc), paste0("前週差 ",dt)),
        if (!is.null(band)) tags$div(style=paste0("font-size:0.68em;color:",status_col,";font-weight:700;margin-top:2px;"),
          paste0(band$label, "（", method_txt, "）"),
          tags$a(href="javascript:void(0)", onclick="goToNotes('notes-zensu-ibs')",
                 style="margin-left:4px;font-weight:400;color:#999;", "詳細→")))
    }
  })
  output$kpi_pref <- renderUI({
    pref <- if (!is.null(input$pref_filter) && input$pref_filter != "全国") input$pref_filter else "全国"
    d <- latest_week_data()
    latest_dt <- if (nrow(d) > 0) max(d$date, na.rm=TRUE) else Sys.Date()
    if (is.infinite(latest_dt)) latest_dt <- Sys.Date()
    yr <- as.integer(format(latest_dt, "%Y"))
    wk <- if (nrow(d) > 0) { w <- max(d$week, na.rm=TRUE); if (is.infinite(w)) NA_integer_ else as.integer(w) } else NA_integer_
    date_txt <- if (!is.na(wk)) paste0(yr, "年 第", wk, "週（", format(latest_dt, "%m/%d"), "）") else format(latest_dt, "%Y/%m/%d")
    disease_label <- tryCatch(
      get_disease_label(input$ts_mode, input$disease, input$zensu_disease_ts),
      error = function(e) "")
    if (is.null(disease_label)) disease_label <- ""
    tags$div(class="kpi-box",
      if (nchar(disease_label) > 0)
        tags$div(style="font-size:1.1em;font-weight:700;color:#2980b9;text-align:center;margin-bottom:4px;letter-spacing:0.03em;", disease_label),
      tags$div(class="kpi-value", style="color:#2c3e50;font-size:1.5em;", pref),
      tags$div(class="kpi-label", "表示地域"),
      tags$div(class="kpi-delta flat", paste0("データ時点: ", date_txt)))
  })
  rt_series <- reactive({
    req(nrow(filtered_data())>14)
    dr <- input$date_range
    all_d <- SURV_DATA %>% filter(disease==input$disease,
                                   date >= dr[1], date <= dr[2])
    tryCatch(compute_rt_series(all_d,input$disease,input$pref_filter),
             error=function(e) tibble(date=as.Date(character()),rt=numeric(),
                                       rt_lower=numeric(),rt_upper=numeric()))
  })

  # ── 表示モードに応じてプルダウンを有効/無効化 ───────────────
  observe({
    is_teiten <- is.null(input$ts_mode) || input$ts_mode == "teiten"
    shinyjs::toggleState("disease",         condition = is_teiten)
    shinyjs::toggleState("zensu_disease_ts", condition = !is_teiten)
  })

  # ── サイドバー疾患 ↔ Rt タブ 双方向同期 ────────────────────
  # サイドバー → Rt タブ（Rt対応疾患の場合のみ）
  # 値が既に一致している場合は更新しない（双方向同期による無限ループ・
  # プルダウンの値が行ったり来たりする不具合を防ぐためのガード）
  observeEvent(input$disease, {
    if (input$disease %in% RT_DISEASE_IDS && !identical(input$rt_disease, input$disease)) {
      updateSelectInput(session, "rt_disease", selected = input$disease)
    }
  }, ignoreInit = TRUE)

  # Rt タブ → サイドバー
  observeEvent(input$rt_disease, {
    if (!identical(input$disease, input$rt_disease)) {
      updateSelectInput(session, "disease", selected = input$rt_disease)
    }
  }, ignoreInit = TRUE)

  # ── グループフィルターで複数疾患比較のチェックボックスを更新 ──
  observeEvent(input$multi_group, {
    grp <- input$multi_group
    if (grp == "all") {
      choices  <- setNames(names(DISEASE_CONFIG), sapply(DISEASE_CONFIG,`[[`,"label"))
      selected <- names(DISEASE_CONFIG)
    } else {
      ids      <- DISEASE_GROUPS[[grp]]$ids
      choices  <- setNames(ids, sapply(ids, function(x) DISEASE_CONFIG[[x]]$label))
      selected <- ids
    }
    updateCheckboxGroupInput(session, "multi_diseases", choices=choices, selected=selected)
  }, ignoreInit=TRUE)
  output$kpi_rt <- renderUI({
    is_zensu <- !is.null(input$ts_mode) && input$ts_mode == "zensu"
    is_std   <- !is_zensu && input$disease %in% names(STD_DISEASE_CONFIG)
    if (is_std) {
      # 性感染症・薬剤耐性菌はヒト→ヒトの短期伝搬連鎖モデルの前提となる
      # シリアルインターバル文献値が存在せず、月次データのため既存の
      # Rt推定ロジック（週次想定）を適用すると科学的に誤った値になるため対象外
      return(tags$div(class="kpi-box",
        tags$div(style="background:#2980b9;color:#fff;font-size:0.65em;font-weight:700;letter-spacing:0.08em;text-align:center;padding:2px 0;margin:-8px -12px 3px -12px;border-radius:4px 4px 0 0;", "IBS"),
        tags$div(class="kpi-value","―"),
        tags$div(class="kpi-label","実効再生産数 Rt"),
        tags$div(class="kpi-delta flat","推定対象外（月報疾患）")))
    }
    if (is_zensu) {
      did <- input$zensu_disease_ts
      rd <- tryCatch({
        if (is.null(ZENSU_DATA) || nrow(ZENSU_DATA) == 0) return(NULL)
        pref_f <- if (!is.null(input$pref_filter) && input$pref_filter != "全国") input$pref_filter else NULL
        compute_rt_series_zensu(ZENSU_DATA, did, pref_f) %>%
          filter(!is.na(rt)) %>% slice_tail(n=1)
      }, error=function(e) NULL)
      if (is.null(rd) || nrow(rd) == 0) return(tags$div(class="kpi-box",
        tags$div(style="background:#2980b9;color:#fff;font-size:0.65em;font-weight:700;letter-spacing:0.08em;text-align:center;padding:2px 0;margin:-8px -12px 3px -12px;border-radius:4px 4px 0 0;", "IBS"),
        tags$div(class="kpi-value","―"),
        tags$div(class="kpi-label","実効再生産数 Rt（直近7週）"),
        tags$div(class="kpi-delta flat","推定対象外")))
      rv <- rd$rt[1]
      rc <- if(rv>1.2)"#e74c3c" else if(rv>1.0)"#e67e22" else "#27ae60"
      tags$div(class="kpi-box",
        tags$div(style="background:#2980b9;color:#fff;font-size:0.65em;font-weight:700;letter-spacing:0.08em;text-align:center;padding:2px 0;margin:-8px -12px 3px -12px;border-radius:4px 4px 0 0;", "IBS"),
        tags$div(class="kpi-value",style=paste0("color:",rc),sprintf("%.2f",rv)),
        tags$div(class="kpi-label","実効再生産数 Rt（直近7週）"),
        tags$div(class=paste("kpi-delta",if(rv>1)"up" else "down"),
                 paste0(if(rv>1)"▲ 流行拡大傾向 " else "▼ 流行縮小傾向 ",
                        sprintf("(%d年第%d週)", rd$year[1], rd$week[1]))))
    } else {
      rd <- tryCatch(
        rt_series() %>% filter(!is.na(rt)) %>% slice_tail(n=1),
        error = function(e) tibble(date=as.Date(character()), rt=numeric(),
                                    year=integer(), week=integer())
      )
      if(nrow(rd)==0) return(tags$div(class="kpi-box",
        tags$div(style="background:#2980b9;color:#fff;font-size:0.65em;font-weight:700;letter-spacing:0.08em;text-align:center;padding:2px 0;margin:-8px -12px 3px -12px;border-radius:4px 4px 0 0;", "IBS"),
        tags$div(class="kpi-value","―"),tags$div(class="kpi-label","実効再生産数 Rt（直近7週）"),
        tags$div(class="kpi-delta flat","データ不足")))
      rv <- rd$rt[1]
      rc <- if(rv>1.2)"#e74c3c" else if(rv>1.0)"#e67e22" else "#27ae60"
      tags$div(class="kpi-box",
        tags$div(style="background:#2980b9;color:#fff;font-size:0.65em;font-weight:700;letter-spacing:0.08em;text-align:center;padding:2px 0;margin:-8px -12px 3px -12px;border-radius:4px 4px 0 0;", "IBS"),
        tags$div(class="kpi-value",style=paste0("color:",rc),sprintf("%.2f",rv)),
        tags$div(class="kpi-label","実効再生産数 Rt（直近7週）"),
        tags$div(class=paste("kpi-delta",if(rv>1)"up" else "down"),
                 paste0(if(rv>1)"▲ 流行拡大傾向 " else "▼ 流行縮小傾向 ",
                        sprintf("(%d年第%d週)", rd$year[1], rd$week[1]))))
    }
  })
  output$kpi_alert <- renderUI({
    is_std_alert <- (is.null(input$ts_mode) || input$ts_mode == "teiten") &&
                    input$disease %in% names(STD_DISEASE_CONFIG)
    if (is_std_alert) {
      did <- input$disease
      pref_f <- if (!is.null(input$pref_filter) && input$pref_filter != "全国") input$pref_filter else NULL
      hist_d <- if (is.null(STD_DATA)) NULL else {
        x <- STD_DATA %>% filter(disease == did)
        if (!is.null(pref_f)) x <- x %>% filter(pref_name == pref_f)
        x %>% group_by(year, month, date) %>%
          summarise(reports = sum(reports, na.rm = TRUE), .groups = "drop") %>% arrange(date)
      }
      band <- tryCatch({
        if (is.null(hist_d) || nrow(hist_d) == 0) {
          NULL
        } else {
          recent2 <- slice_tail(hist_d, n = 2)
          cur  <- slice_tail(recent2, n = 1)
          prev <- if (nrow(recent2) >= 2) slice_head(recent2, n = 1) else cur
          std_ibs_score(
            cur_val = cur$reports[1], cur_year = cur$year[1], cur_month = cur$month[1],
            prev_val = prev$reports[1], prev_year = prev$year[1], prev_month = prev$month[1],
            hist_d = hist_d)
        }
      }, error = function(e) NULL)
      wk_txt <- if (!is.null(hist_d) && nrow(hist_d) > 0) {
        d1 <- slice_tail(hist_d, n = 1)
        sprintf("(%d年%d月)", d1$year[1], d1$month[1])
      } else ""
      if (is.null(band)) return(tags$div(class="kpi-box",
        tags$div(style="background:#95a5a6;color:#fff;font-size:0.65em;font-weight:700;letter-spacing:0.08em;text-align:center;padding:2px 0;margin:-8px -12px 3px -12px;border-radius:4px 4px 0 0;", "IBS"),
        tags$div(class="kpi-value","―"),
        tags$div(class="kpi-label", paste0("現在の流行フェーズ", wk_txt)),
        tags$div(class="kpi-delta flat","データなし")))

      # Rt推定対象外・公式閾値なしのため、IBS（月次±2SD判定）のみで評価
      score <- compute_alert_score(value=NA_real_, thresh=NULL, rt_value=NA_real_, ibs_score=band$score)
      labels <- c("基準以下", "流行期（レベル1）", "注意（レベル2）", "警戒（レベル3）")
      level <- if (is.na(score)) "―" else labels[score + 1]
      col <- alert_color(level)

      return(tags$div(class="kpi-box",
        tags$div(style=paste0("background:",col,";color:#fff;font-size:0.65em;font-weight:700;letter-spacing:0.08em;text-align:center;padding:2px 0;margin:-8px -12px 3px -12px;border-radius:4px 4px 0 0;"),
          "IBS 総合判定"),
        tags$div(class="kpi-value", style=paste0("color:",col,";font-size:1.6em;"), level),
        tags$div(class="kpi-label", paste0("現在の流行フェーズ", wk_txt)),
        tags$div(class="kpi-delta flat",
          "月次報告数の過去5年比のみで判定（Rt・公式基準値は対象外）",
          tags$a(href="javascript:void(0)", onclick="goToNotes('notes-std')", style="margin-left:4px;", "詳細→"))))
    }
    if (!is.null(input$ts_mode) && input$ts_mode == "zensu") {
      did  <- input$zensu_disease_ts
      hist_d <- zensu_hist()
      zf     <- zensu_ts_filtered()
      band <- tryCatch({
        if (is.null(hist_d) || is.null(zf) || nrow(zf) == 0) {
          NULL
        } else {
          recent2 <- zf %>%
            group_by(date, year, week) %>%
            summarise(val = sum(cases, na.rm=TRUE), .groups="drop") %>%
            arrange(date) %>% slice_tail(n = 2)
          if (nrow(recent2) == 0) {
            NULL
          } else {
            names(hist_d)[names(hist_d) == "cases"] <- "reports_per_site"
            cur  <- slice_tail(recent2, n=1)
            prev <- if (nrow(recent2) >= 2) slice_head(recent2, n=1) else cur
            zensu_ibs_band(
              cur_val=cur$val[1], cur_date=cur$date[1], cur_week=cur$week[1], cur_year=cur$year[1],
              prev_val=prev$val[1], prev_date=prev$date[1], prev_week=prev$week[1], prev_year=prev$year[1],
              hist_d=hist_d)
          }
        }
      }, error=function(e) NULL)
      wk_txt <- if (!is.null(zf) && nrow(zf) > 0) {
        d1 <- zf %>% arrange(date) %>% slice_tail(n=1)
        sprintf("(%d年第%d週)", d1$year[1], d1$week[1])
      } else ""
      if (is.null(band)) return(tags$div(class="kpi-box",
        tags$div(style="background:#95a5a6;color:#fff;font-size:0.65em;font-weight:700;letter-spacing:0.08em;text-align:center;padding:2px 0;margin:-8px -12px 3px -12px;border-radius:4px 4px 0 0;", "IBS"),
        tags$div(class="kpi-value","―"),
        tags$div(class="kpi-label", paste0("現在の流行フェーズ", wk_txt)),
        tags$div(class="kpi-delta flat","データなし")))

      # 定点把握と同様、IBS（季節性自動判定によるバンドスコア）とRtを統合して総合フェーズを判定
      rt_latest <- tryCatch({
        pref_f <- if (!is.null(input$pref_filter) && input$pref_filter != "全国") input$pref_filter else NULL
        rd <- compute_rt_series_zensu(ZENSU_DATA, did, pref_f) %>% filter(!is.na(rt)) %>% slice_tail(n=1)
        if (nrow(rd)==0) NA_real_ else rd$rt[1]
      }, error=function(e) NA_real_)
      score <- compute_alert_score(value=NA_real_, thresh=NULL, rt_value=rt_latest, ibs_score=band$score)
      labels <- c("基準以下", "流行期（レベル1）", "注意（レベル2）", "警戒（レベル3）")
      level <- if (is.na(score)) "―" else labels[score + 1]
      col <- alert_color(level)
      n_inputs <- sum(!is.na(band$score), !is.na(rt_latest))

      return(tags$div(class="kpi-box",
        tags$div(style=paste0("background:",col,";color:#fff;font-size:0.65em;font-weight:700;letter-spacing:0.08em;text-align:center;padding:2px 0;margin:-8px -12px 3px -12px;border-radius:4px 4px 0 0;"),
          "IBS 総合判定"),
        tags$div(class="kpi-value", style=paste0("color:",col,";font-size:1.6em;"), level),
        tags$div(class="kpi-label", paste0("現在の流行フェーズ", wk_txt)),
        tags$div(class="kpi-delta flat",
          if (n_inputs > 0) paste0("報告数＋Rt(", n_inputs, "指標)を統合") else "指標設定なし",
          tags$a(href="javascript:void(0)", onclick="goToNotes('notes-zensu-ibs')", style="margin-left:4px;", "詳細→"))))
    }
    cur_row <- ts_main_data() %>% arrange(date) %>% slice_tail(n=1)
    val <- if (nrow(cur_row)==0) NA_real_ else cur_row$reports_per_site[1]
    rt_latest <- tryCatch({
      rd <- rt_series() %>% filter(!is.na(rt)) %>% slice_tail(n=1)
      if (nrow(rd)==0) NA_real_ else rd$rt[1]
    }, error=function(e) NA_real_)
    ibs_latest <- tryCatch({
      bd <- ts_band_series() %>% filter(!is.na(ibs_score)) %>% slice_tail(n=1)
      if (nrow(bd)==0) NA_real_ else bd$ibs_score[1]
    }, error=function(e) NA_real_)
    level <- classify_alert(val, input$disease, rt_latest, ibs_latest)
    col <- alert_color(level)
    thresh <- DISEASE_CONFIG[[input$disease]]$alert_threshold

    n_inputs <- sum(!is.null(thresh), !is.na(rt_latest), !is.na(ibs_latest))
    ibs_label <- if (!is.na(ibs_latest))
      c("過去5年平均以下","過去5年平均〜+1SD","過去5年+1〜+2SD","過去5年+2SD超過")[ibs_latest + 1] else NULL

    tags$div(class="kpi-box",
      tags$div(style=paste0("background:",col,";color:#fff;font-size:0.65em;font-weight:700;letter-spacing:0.08em;text-align:center;padding:2px 0;margin:-8px -12px 3px -12px;border-radius:4px 4px 0 0;"),
        "IBS 総合判定"),
      tags$div(class="kpi-value", style=paste0("color:",col,";font-size:1.6em;"), level),
      tags$div(class="kpi-label",
        paste0("現在の流行フェーズ",
               if (nrow(cur_row)>0) sprintf("(%d年第%d週)", cur_row$year[1], cur_row$week[1]) else "")),
      tags$div(class="kpi-delta flat",
        if (n_inputs > 0) paste0("報告数＋Rt＋過去5年比(", n_inputs, "指標)を統合") else "指標設定なし",
        tags$a(href="javascript:void(0)", onclick="goToNotes('notes-ibs')", style="margin-left:4px;", "詳細→")))
  })

  # ── 基準値による判定カード（研究班報告書ベースの注意報・警報基準値と現在値の比較）──
  output$kpi_threshold <- renderUI({
    if (!is.null(input$ts_mode) && input$ts_mode == "zensu") return(NULL)
    cur_row <- ts_main_data() %>% arrange(date) %>% slice_tail(n=1)
    val <- if (nrow(cur_row)==0) NA_real_ else cur_row$reports_per_site[1]
    thresh <- DISEASE_CONFIG[[input$disease]]$alert_threshold
    lbl <- thresh_level_label(val, thresh)
    col <- if (is.null(lbl)) "#95a5a6" else
      c("警報"="#c0392b","警戒"="#c0392b","注意報"="#e67e22","注意"="#e67e22",
        "流行期並み"="#f1c40f","平常"="#27ae60")[lbl]
    tags$div(class="kpi-box",
      tags$div(style="background:#2980b9;color:#fff;font-size:0.65em;font-weight:700;letter-spacing:0.08em;text-align:center;padding:2px 0;margin:-8px -12px 3px -12px;border-radius:4px 4px 0 0;", "IBS"),
      tags$div(class="kpi-value", style=paste0("color:",col,";font-size:1.6em;"),
        if (is.null(lbl)) "―" else lbl),
      tags$div(class="kpi-label",
        paste0("基準値による判定",
               if (nrow(cur_row)>0) sprintf("(%d年第%d週)", cur_row$year[1], cur_row$week[1]) else "")),
      tags$div(class="kpi-delta flat",
        if (!is.null(thresh)) {
          short <- if (is.list(thresh)) {
            paste(c(
              if (!is.null(thresh$chuiho_start)) paste0("注", thresh$chuiho_start) else NULL,
              paste0("警", thresh$keiho_start),
              if (!is.null(thresh$keiho_end)) paste0("終", thresh$keiho_end) else NULL
            ), collapse="/")
          } else paste0("参考", thresh)
          paste0("現在値", sprintf("%.2f", val), "（", short, "）")
        } else "基準値: 設定なし"))
  })

  # ── 統合活動レベルカード（IBS 95% + EBS 5%）────────────
  output$kpi_integrated <- renderUI({
    is_zensu <- !is.null(input$ts_mode) && input$ts_mode == "zensu"
    did      <- if (is_zensu) input$zensu_disease_ts else input$disease
    is_std   <- !is_zensu && did %in% names(STD_DISEASE_CONFIG)
    dlabel   <- if (is_zensu) ZENSU_DISEASE_CONFIG[[did]]$label
                else if (is_std) STD_DISEASE_CONFIG[[did]]$label
                else DISEASE_CONFIG[[did]]$label
    pref     <- if (!is.null(input$pref_filter) && input$pref_filter != "全国") input$pref_filter else NULL
    area_lbl <- if (is.null(pref)) "全国" else pref

    # ── IBS スコア: 直近2週を過去5年・5週MA±SDと比較 ──────
    # +2SD超過は2週連続で該当した場合のみ score=3
    # （月報疾患は月次データのため、直近2か月×過去5年・同月±1か月比較）
    ibs_info <- tryCatch({
      if (is_std) {
        if (is.null(STD_DATA) || nrow(STD_DATA) == 0) return(NULL)
        base_d <- STD_DATA %>% filter(disease == did)
        if (!is.null(pref)) base_d <- base_d %>% filter(pref_name == pref)
        hist_d <- base_d %>% group_by(year, month, date) %>%
          summarise(reports = sum(reports, na.rm = TRUE), .groups = "drop") %>% arrange(date)
        if (nrow(hist_d) == 0) return(NULL)
        recent2 <- slice_tail(hist_d, n = 2)
        cur  <- slice_tail(recent2, n = 1)
        prev <- if (nrow(recent2) >= 2) slice_head(recent2, n = 1) else cur
        cur_val <- cur$reports[1]
        band <- std_ibs_score(
          cur_val = cur_val, cur_year = cur$year[1], cur_month = cur$month[1],
          prev_val = prev$reports[1], prev_year = prev$year[1], prev_month = prev$month[1],
          hist_d = hist_d)
        band[c("score","label","detail")]
      } else if (!is_zensu) {
        # 定点把握疾患: 既存データから季節性を自動判定し、評価方式を切替
        #（季節性あり=同時期×過去5年比較、季節性なし=直近推移比較。zensu_ibs_band参照）
        base_d <- SURV_DATA %>% filter(disease == did)
        if (!is.null(pref)) {
          hist_d <- base_d %>%
            filter(pref_name == pref) %>%
            group_by(date, year, week) %>%
            summarise(reports_per_site = mean(reports_per_site, na.rm=TRUE), .groups="drop")
          recent2 <- latest_week_data() %>%
            filter(pref_name == pref) %>%
            group_by(date, year, week) %>%
            summarise(reports_per_site = mean(reports_per_site, na.rm=TRUE), .groups="drop") %>%
            arrange(date) %>% slice_tail(n = 2)
        } else {
          hist_d  <- national_avg_hist()
          recent2 <- national_avg() %>% slice_tail(n = 2)
        }
        if (nrow(recent2) == 0) return(NULL)
        cur  <- slice_tail(recent2, n=1)
        prev <- if (nrow(recent2) >= 2) slice_head(recent2, n=1) else cur
        cur_val  <- cur$reports_per_site[1]
        band <- zensu_ibs_band(
          cur_val=cur_val, cur_date=cur$date[1], cur_week=cur$week[1], cur_year=cur$year[1],
          prev_val=prev$reports_per_site[1], prev_date=prev$date[1], prev_week=prev$week[1], prev_year=prev$year[1],
          hist_d=hist_d, detail_fmt="rate")
        band[c("score","label","detail")]
      } else {
        # 全数把握疾患: 既存データから季節性を自動判定し、評価方式を切替
        #（季節性あり=同時期×過去5年比較、季節性なし=直近推移比較。zensu_ibs_band参照）
        hist_d <- zensu_hist()
        if (is.null(hist_d)) return(NULL)
        zf <- zensu_ts_filtered()
        if (is.null(zf) || nrow(zf) == 0) return(NULL)
        recent2 <- zf %>%
          group_by(date, year, week) %>%
          summarise(val = sum(cases, na.rm=TRUE), .groups="drop") %>%
          arrange(date) %>% slice_tail(n = 2)
        if (nrow(recent2) == 0) return(NULL)
        names(hist_d)[names(hist_d) == "cases"] <- "reports_per_site"
        cur  <- slice_tail(recent2, n=1)
        prev <- if (nrow(recent2) >= 2) slice_head(recent2, n=1) else cur
        cur_val  <- cur$val[1]
        band <- zensu_ibs_band(
          cur_val=cur_val, cur_date=cur$date[1], cur_week=cur$week[1], cur_year=cur$year[1],
          prev_val=prev$val[1], prev_date=prev$date[1], prev_week=prev$week[1], prev_year=prev$year[1],
          hist_d=hist_d)
        band[c("score","label","detail")]
      }
    }, error = function(e) list(score=0, label="計算エラー", detail=conditionMessage(e)))

    if (is.null(ibs_info)) return(NULL)

    # ── Rt（動態指標）: 参考基準値と共線性のある過去5年比較(IBS)とは独立に評価 ──
    # 月報疾患（性感染症・薬剤耐性菌）はヒト→ヒトの短期伝搬連鎖モデルが前提の
    # シリアルインターバル文献値が存在せず、月次データへの適用も科学的に不適切な
    # ため、Rtは推定対象外（NA）とする
    rt_latest <- if (is_std) NA_real_ else tryCatch({
      if (is_zensu) {
        if (is.null(ZENSU_DATA) || nrow(ZENSU_DATA) == 0) return(NA_real_)
        rd <- compute_rt_series_zensu(ZENSU_DATA, did, pref) %>% filter(!is.na(rt)) %>% slice_tail(n=1)
      } else {
        rd <- rt_series() %>% filter(!is.na(rt)) %>% slice_tail(n=1)
      }
      if (nrow(rd)==0) NA_real_ else rd$rt[1]
    }, error=function(e) NA_real_)
    thresh_val <- if (!is_zensu && !is_std) DISEASE_CONFIG[[did]]$alert_threshold else NULL

    # ── EBS スコア: 今週 vs 先週の重み付きシグナル変化率 ──
    # 都道府県選択時はタイトル・本文に都道府県名を含む記事に絞る
    ebs_info <- tryCatch({
      d <- ebs_data()
      weights <- c("Signal High"=3L, "Signal Low"=2L, "FYI"=0L)
      if (!is.null(d) && nrow(d) > 0) {
        d <- d %>% filter(!is.na(pub_date),
                          is.na(source_id) | source_id != "pubmed",
                          has_disease_tag(disease_tags, did))
        if (!is.null(pref)) {
          pref_short <- sub("(都|道|府|県)$", "", pref)
          d <- d %>% filter(
            (!is.na(ebs_pref) & ebs_pref == pref) |
            vapply(paste(coalesce(title,""), coalesce(summary,"")), function(txt)
              grepl(pref, txt, fixed=TRUE) | grepl(pref_short, txt, fixed=TRUE),
              FUN.VALUE = logical(1))
          )
        }
        ref_date <- if (exists("cur") && !is.null(cur) && nrow(cur) > 0 && !is.na(cur$date[1]))
          as.Date(cur$date[1]) else Sys.Date()
        if (as.numeric(Sys.Date() - ref_date) > EBS_SCORE_EVALUABLE_DAYS) {
          list(score=0L, label="評価不能", n=0L, high=0L, evaluable=FALSE)
        } else {
          d_this <- d %>% filter(pub_date >= ref_date - 7, pub_date <= ref_date)
          d_prev <- d %>% filter(pub_date >= ref_date - 14, pub_date < ref_date - 7)
          s_this <- if (nrow(d_this) > 0) sum(weights[as.character(d_this$signal_level)], na.rm=TRUE) else 0L
          s_prev <- if (nrow(d_prev) > 0) sum(weights[as.character(d_prev$signal_level)], na.rm=TRUE) else 0L
          chg <- if (s_prev > 0) (s_this - s_prev) / s_prev * 100
                 else if (s_this > 0) 100 else 0
          ebs_lv <- if (s_this == 0 && s_prev == 0) -1L
                    else if (chg < -20) -1L
                    else if (chg <  20)  0L
                    else if (chg <  50)  1L
                    else                 2L
          lbl <- switch(as.character(ebs_lv),
            "-1"="低調", "0"="横ばい", "1"="上昇", "2"="急上昇")
          list(score=max(0L, ebs_lv), label=lbl,
               n=nrow(d_this), high=sum(as.character(d_this$signal_level)=="Signal High"),
               evaluable=TRUE)
        }
      } else {
        list(score=0L, label="情報なし", n=0L, high=0L, evaluable=FALSE)
      }
    }, error = function(e) list(score=0L, label="エラー", n=0L, high=0L, evaluable=FALSE))

    # ── IBS統合スコア: 過去5年比較(ibs_info$score)・参考基準値・Rtを
    #    共線性を考慮した2段階加重平均で統合（classify_alertと同一ロジック）──
    ibs_blended <- tryCatch({
      s <- compute_alert_score(
        value = cur_val, thresh = thresh_val, rt_value = rt_latest, ibs_score = ibs_info$score
      )
      if (is.na(s)) ibs_info$score else s
    }, error = function(e) ibs_info$score)

    # ── 統合スコア: IBS（水準+Rt統合）95% + EBS 5% ───────
    ebs_scaled <- min(3, ebs_info$score * 1.5)
    combined   <- ibs_blended * 0.95 + ebs_scaled * 0.05
    act_level  <- min(4L, max(1L, as.integer(round(combined)) + 1L))

    lcfg <- list(
      `1` = list(label="レベル1", name="通常", color="#27ae60", bg="#eafaf1", border="#a9dfbf"),
      `2` = list(label="レベル2", name="注意", color="#d4ac0d", bg="#fefde7", border="#f9e79f"),
      `3` = list(label="レベル3", name="警戒", color="#e67e22", bg="#fef5e7", border="#f8c471"),
      `4` = list(label="レベル4", name="流行", color="#c0392b", bg="#fdedec", border="#f1948a")
    )
    cfg <- lcfg[[as.character(act_level)]]

    ibs_col <- c("0"="#27ae60","1"="#d4ac0d","2"="#e67e22","3"="#c0392b")[as.character(ibs_blended)]
    ebs_col <- c("0"="#27ae60","1"="#d4ac0d","2"="#e67e22")[as.character(min(2L, ebs_info$score))]
    rt_lbl  <- if (!is.na(rt_latest)) sprintf("Rt %.2f", rt_latest) else NULL

    make_pill <- function(label, val, col) {
      tags$span(style=paste0(
        "display:inline-block;padding:2px 10px;border-radius:12px;margin:0 6px;",
        "background:",col,"22;border:1px solid ",col,";color:",col,
        ";font-size:0.78em;font-weight:600;"), paste0(label, ": ", val))
    }

    tags$div(class="kpi-box",
      style=paste0("border-left:6px solid ",cfg$color,";background:",cfg$bg,
                   ";border:1px solid ",cfg$border,";padding:10px 14px;"),
      tags$div(style=paste0("background:#6c3483;color:#fff;font-size:0.63em;font-weight:700;",
        "letter-spacing:0.08em;text-align:center;padding:2px 0;",
        "margin:-10px -14px 8px -14px;border-radius:3px 3px 0 0;"),
        "IBS + EBS　統合活動レベル"),
      tags$div(style="font-size:0.68em;color:#999;text-align:center;margin-bottom:4px;",
        paste0(
          if (is_std) "評価期間: IBS2か月×過去5年比較／EBS7日変化率（Rt推定対象外）"
          else "評価期間: IBS2週×過去5年比較／Rt7週／EBS7日変化率",
          if (exists("cur") && !is.null(cur) && nrow(cur)>0) {
            if (is_std) sprintf("　(IBS/EBS: %d年%d月時点)", cur$year[1], cur$month[1])
            else sprintf("　(IBS/Rt/EBS: %d年第%d週時点)", cur$year[1], cur$week[1])
          } else "",
          if (!isTRUE(ebs_info$evaluable)) "　※EBSは直近1年分のみ評価可能" else "")),
      tags$div(style="display:flex;align-items:center;justify-content:center;gap:16px;flex-wrap:wrap;text-align:center;",
        tags$div(
          tags$span(style=paste0("font-size:2em;font-weight:900;color:",cfg$color,
                                 ";line-height:1.1;display:block;"), cfg$label),
          tags$span(style=paste0("font-size:1em;font-weight:700;color:",cfg$color,";"), cfg$name)
        ),
        tags$div(style="width:1px;height:48px;background:#ddd;"),
        tags$div(style="flex:1;min-width:160px;max-width:360px;text-align:center;",
          tags$div(style="display:flex;flex-wrap:wrap;gap:4px;align-items:center;justify-content:center;",
            make_pill("IBS", ibs_info$label, ibs_col),
            make_pill("EBS", ebs_info$label, ebs_col),
            if (!is.null(rt_lbl)) make_pill("Rt", sprintf("%.2f", rt_latest),
              if (rt_latest>=2.0) "#c0392b" else if (rt_latest>=1.0) "#d4ac0d" else "#27ae60")
          ),
          tags$div(style="font-size:0.73em;color:#777;margin-top:4px;text-align:center;",
            paste0(if (is_zensu || is_std) "報告数 " else "定点あたり ", ibs_info$detail,
                   if (!is.null(rt_lbl)) paste0("　", rt_lbl) else "",
                   "　EBS今週 ", ebs_info$n, "件",
                   if (ebs_info$high > 0) paste0("（高シグナル", ebs_info$high, "件）") else ""))
        ),
        tags$div(style="font-size:0.72em;text-align:center;",
          tags$a(href="javascript:void(0)", onclick="goToNotes('notes-integrated')", "詳細 →"))
      )
    )
  })

  # ── 活動レベル一覧（全疾患タイル）────────────────────────
  # 全疾患の統合活動レベルを一括計算するreactive
  all_disease_levels_data <- reactive({
    pref     <- if (!is.null(input$pref_filter) && input$pref_filter != "全国") input$pref_filter else NULL
    is_zensu <- !is.null(input$ts_mode) && input$ts_mode == "zensu"

    # ── EBS 疾患別スコアを一括計算 ─────────────────────
    ebs_d <- tryCatch(ebs_data(), error=function(e) NULL)
    weights <- c("Signal High"=3L, "Signal Low"=2L, "FYI"=0L)
    ebs_score_for <- function(did, ref_date = Sys.Date()) {
      tryCatch({
        if (is.null(ebs_d) || nrow(ebs_d) == 0) return(0L)
        if (is.na(ref_date) || as.numeric(Sys.Date() - as.Date(ref_date)) > EBS_SCORE_EVALUABLE_DAYS) return(0L)
        ref_date <- as.Date(ref_date)
        de <- ebs_d %>% filter(!is.na(pub_date),
                               is.na(source_id) | source_id != "pubmed",
                               has_disease_tag(disease_tags, did))
        if (!is.null(pref)) {
          ps <- sub("(都|道|府|県)$", "", pref)
          de <- de %>% filter(
            (!is.na(ebs_pref) & ebs_pref == pref) |
            vapply(paste(coalesce(title,""), coalesce(summary,"")),
                   function(t) grepl(pref, t, fixed=TRUE) | grepl(ps, t, fixed=TRUE),
                   FUN.VALUE = logical(1)))
        }
        s_this <- sum(weights[as.character(
          de %>% filter(pub_date >= ref_date - 7, pub_date <= ref_date) %>% pull(signal_level))], na.rm=TRUE)
        s_prev <- sum(weights[as.character(
          de %>% filter(pub_date >= ref_date - 14, pub_date < ref_date - 7) %>% pull(signal_level))], na.rm=TRUE)
        chg <- if (s_prev > 0) (s_this - s_prev)/s_prev*100 else if (s_this > 0) 100 else 0
        lv <- if (s_this==0 && s_prev==0) -1L
              else if (chg < -20) -1L else if (chg < 20) 0L
              else if (chg <  50) 1L  else 2L
        max(0L, lv)
      }, error=function(e) 0L)
    }

    # ── 統合スコア: 「現在の流行フェーズ」カード（kpi_alert）と同一ロジック ──
    # ①注意報・警報基準値・②Rt・③IBS過去5年比較(季節性自動判定)を2段階加重平均で
    # 統合したのち、EBS(5%)とブレンドする（compute_alert_score参照）
    combined_score <- function(value, thresh, rt_value, ibs_s, ebs_s) {
      ibs_blended <- tryCatch({
        s <- compute_alert_score(value = value, thresh = thresh, rt_value = rt_value, ibs_score = ibs_s)
        if (is.na(s)) ibs_s else s
      }, error = function(e) ibs_s)
      ebs_scaled <- min(3, ebs_s * 1.5)
      ibs_blended * 0.95 + ebs_scaled * 0.05
    }
    act_level <- function(sc) min(4L, max(1L, as.integer(round(sc)) + 1L))

    results <- list()

    # ── 定点把握疾患 ────────────────────────────────────
    # 全国（pref未指定）はJIHS公式の「総数」行を優先し、欠測週のみ都道府県平均で補完する
    if (!is_zensu) {
      if (is.null(pref)) {
        avg_all <- SURV_DATA %>%
          group_by(disease, date, year, week) %>%
          summarise(avg_val = mean(reports_per_site, na.rm=TRUE), .groups="drop")
        off_all <- if (is.null(SURV_DATA_NATIONAL) || nrow(SURV_DATA_NATIONAL) == 0) {
          tibble(disease=character(), date=as.Date(character()), official_val=numeric())
        } else {
          SURV_DATA_NATIONAL %>% select(disease, date, official_val = reports_per_site)
        }
        all_teiten <- avg_all %>%
          left_join(off_all, by=c("disease","date")) %>%
          mutate(reports_per_site = dplyr::coalesce(official_val, avg_val)) %>%
          select(disease, date, year, week, reports_per_site)
      } else {
        all_teiten <- SURV_DATA %>% filter(pref_name == pref) %>%
          group_by(disease, date, year, week) %>%
          summarise(reports_per_site = mean(reports_per_site, na.rm=TRUE), .groups="drop")
      }
    }

    dr <- input$date_range
    if (!is_zensu) for (did in names(DISEASE_CONFIG)) {
      tryCatch({
        dconf <- DISEASE_CONFIG[[did]]
        dd <- all_teiten %>% filter(disease == did) %>% arrange(date)
        if (nrow(dd) == 0) return(NULL)
        # スライダー(date_range)で選択した期間の末尾2週を評価対象とする
        # （過去5年比較baselineにはddの全期間データをそのまま使う）
        dd_sel <- dd %>% filter(date >= dr[1], date <= dr[2])
        if (nrow(dd_sel) == 0) return(NULL)
        recent2 <- slice_tail(dd_sel, n=2)
        cur  <- slice_tail(recent2, n=1)
        prev <- if (nrow(recent2) >= 2) slice_head(recent2, n=1) else cur
        # 定点把握疾患: 既存データから季節性を自動判定し評価方式を切替（zensu_ibs_band参照）
        band   <- zensu_ibs_band(
          cur_val=cur$reports_per_site[1], cur_date=cur$date[1], cur_week=cur$week[1], cur_year=cur$year[1],
          prev_val=prev$reports_per_site[1], prev_date=prev$date[1], prev_week=prev$week[1], prev_year=prev$year[1],
          hist_d=dd, detail_fmt="rate")
        ibs_s  <- band$score
        ebs_s  <- ebs_score_for(did, cur$date[1])
        rt_latest <- tryCatch({
          rd <- compute_rt_series(SURV_DATA %>% filter(disease == did, date >= dr[1], date <= dr[2]), did, pref) %>%
            filter(!is.na(rt)) %>% slice_tail(n=1)
          if (nrow(rd)==0) NA_real_ else rd$rt[1]
        }, error=function(e) NA_real_)
        sc     <- combined_score(cur$reports_per_site[1], dconf$alert_threshold, rt_latest, ibs_s, ebs_s)
        results[[did]] <- list(
          id=did, label=dconf$label, type="定点", color=dconf$color,
          ibs_score=ibs_s, ebs_score=ebs_s,
          act_level=act_level(sc), combined=sc,
          cur_val=cur$reports_per_site[1],
          ibs_label=band$label, ibs_method=band$method,
          year=cur$year[1], week=cur$week[1], is_std=FALSE
        )
      }, error=function(e) NULL)
    }

    # ── 定点把握疾患（月次報告：性感染症・薬剤耐性菌）──────────
    if (!is_zensu && !is.null(STD_DATA)) {
      all_std <- STD_DATA %>%
        { if (!is.null(pref)) filter(., pref_name == pref) else . } %>%
        group_by(disease, date, year, month) %>%
        summarise(reports = sum(reports, na.rm=TRUE), .groups="drop")

      for (did in names(STD_DISEASE_CONFIG)) {
        tryCatch({
          dconf <- STD_DISEASE_CONFIG[[did]]
          dd <- all_std %>% filter(disease == did) %>% arrange(date)
          if (nrow(dd) == 0) return(NULL)
          dd_sel <- dd %>% filter(date >= dr[1], date <= dr[2])
          if (nrow(dd_sel) == 0) return(NULL)
          recent2 <- slice_tail(dd_sel, n=2)
          cur  <- slice_tail(recent2, n=1)
          prev <- if (nrow(recent2) >= 2) slice_head(recent2, n=1) else cur
          band <- std_ibs_score(
            cur_val=cur$reports[1], cur_year=cur$year[1], cur_month=cur$month[1],
            prev_val=prev$reports[1], prev_year=prev$year[1], prev_month=prev$month[1],
            hist_d=dd)
          ibs_s <- band$score
          ebs_s <- ebs_score_for(did, cur$date[1])
          # Rt推定対象外・公式閾値なしのためIBS(月次)+EBSのみで統合
          sc    <- combined_score(cur$reports[1], NULL, NA_real_, ibs_s, ebs_s)
          results[[paste0("s_",did)]] <- list(
            id=did, label=dconf$label, type="定点（月次）", color=dconf$color,
            ibs_score=ibs_s, ebs_score=ebs_s,
            act_level=act_level(sc), combined=sc,
            cur_val=cur$reports[1],
            ibs_label=band$label, ibs_method=band$method,
            year=cur$year[1], week=NA_integer_, is_std=TRUE
          )
        }, error=function(e) NULL)
      }
    }

    # ── 全数把握疾患 ────────────────────────────────────
    if (is_zensu) all_zensu <- ZENSU_DATA %>%
      { if (!is.null(pref)) filter(., pref_name == pref) else . } %>%
      group_by(disease, date, year, week) %>%
      summarise(cases = sum(cases, na.rm=TRUE), .groups="drop") %>%
      rename(reports_per_site = cases)

    if (is_zensu) for (did in names(ZENSU_DISEASE_CONFIG)) {
      tryCatch({
        dconf <- ZENSU_DISEASE_CONFIG[[did]]
        dd <- all_zensu %>% filter(disease == did) %>% arrange(date)
        if (nrow(dd) == 0) return(NULL)
        # スライダー(date_range)で選択した期間の末尾2週を評価対象とする
        dd_sel <- dd %>% filter(date >= dr[1], date <= dr[2])
        if (nrow(dd_sel) == 0) return(NULL)
        recent2 <- slice_tail(dd_sel, n=2)
        cur  <- slice_tail(recent2, n=1)
        prev <- if (nrow(recent2) >= 2) slice_head(recent2, n=1) else cur
        # 全数把握疾患: 既存データから季節性を自動判定し評価方式を切替（zensu_ibs_band参照）
        band   <- zensu_ibs_band(
          cur_val=cur$reports_per_site[1], cur_date=cur$date[1], cur_week=cur$week[1], cur_year=cur$year[1],
          prev_val=prev$reports_per_site[1], prev_date=prev$date[1], prev_week=prev$week[1], prev_year=prev$year[1],
          hist_d=dd)
        ibs_s  <- band$score
        ebs_s  <- ebs_score_for(did, cur$date[1])
        rt_latest <- tryCatch({
          if (is.null(ZENSU_DATA) || nrow(ZENSU_DATA) == 0) {
            NA_real_
          } else {
            rd <- compute_rt_series_zensu(ZENSU_DATA, did, pref) %>% filter(!is.na(rt)) %>% slice_tail(n=1)
            if (nrow(rd)==0) NA_real_ else rd$rt[1]
          }
        }, error=function(e) NA_real_)
        sc     <- combined_score(cur$reports_per_site[1], NULL, rt_latest, ibs_s, ebs_s)
        results[[paste0("z_",did)]] <- list(
          id=did, label=dconf$label, type=dconf$class, color=dconf$color,
          ibs_score=ibs_s, ebs_score=ebs_s,
          act_level=act_level(sc), combined=sc,
          cur_val=cur$reports_per_site[1],
          ibs_label=band$label, ibs_method=band$method,
          year=cur$year[1], week=cur$week[1], is_std=FALSE
        )
      }, error=function(e) NULL)
    }

    # act_level 降順 → combined 降順 でソート
    results <- Filter(Negate(is.null), results)
    results[order(
      -sapply(results, `[[`, "act_level"),
      -sapply(results, `[[`, "combined")
    )]
  })

  output$all_levels_header <- renderUI({
    pref     <- if (!is.null(input$pref_filter) && input$pref_filter != "全国") input$pref_filter else "全国"
    is_zensu <- !is.null(input$ts_mode) && input$ts_mode == "zensu"
    mode_lbl <- if (is_zensu) "全数把握" else "定点把握"
    res  <- all_disease_levels_data()
    n_by_level <- if (length(res) > 0) table(sapply(res, `[[`, "act_level")) else table(integer(0))
    res_wk <- Filter(function(r) !is.na(r$week), res)
    wk_txt <- if (length(res_wk) > 0) {
      wk_key <- sapply(res_wk, function(r) paste0(r$year, "-", r$week))
      top_key <- names(sort(table(wk_key), decreasing = TRUE))[1]
      parts <- strsplit(top_key, "-")[[1]]
      sprintf("　%s年 第%s週時点", parts[1], parts[2])
    } else ""
    tags$div(style="margin-bottom:10px;display:flex;align-items:center;gap:16px;flex-wrap:wrap;",
      tags$div(style="font-size:0.9em;font-weight:700;color:#333;",
               paste0("疾患別 統合活動レベル一覧　（", pref, " / ", mode_lbl, "）　", length(res), "疾患", wk_txt)),
      tags$div(style="display:flex;gap:8px;",
        lapply(4:1, function(lv) {
          cfg <- list(`4`=list(col="#c0392b",name="流行"),`3`=list(col="#e67e22",name="警戒"),
                      `2`=list(col="#d4ac0d",name="注意"),`1`=list(col="#27ae60",name="通常"))[[as.character(lv)]]
          n <- as.integer(n_by_level[as.character(lv)]); if (is.na(n)) n <- 0L
          tags$span(style=paste0("background:",cfg$col,";color:#fff;border-radius:10px;",
            "padding:2px 10px;font-size:0.75em;font-weight:700;"),
            paste0("Lv",lv," ",cfg$name,": ",n,"疾患"))
        })
      )
    )
  })

  output$all_levels_ui <- renderUI({
    res <- all_disease_levels_data()
    if (length(res) == 0) return(tags$p("データなし"))

    lcfg <- list(
      `1`=list(color="#27ae60", bg="#eafaf1", border="#a9dfbf", name="通常"),
      `2`=list(color="#d4ac0d", bg="#fefde7", border="#f9e79f", name="注意"),
      `3`=list(color="#e67e22", bg="#fef5e7", border="#f8c471", name="警戒"),
      `4`=list(color="#c0392b", bg="#fdedec", border="#f1948a", name="流行")
    )

    cards <- lapply(res, function(r) {
      cfg <- lcfg[[as.character(r$act_level)]]
      type_col <- switch(r$type,
        "定点"="#2980b9", "定点（月次）"="#9c27b0", "1類"="#8e0000", "2類"="#c0392b",
        "3類"="#e67e22", "4類"="#f39c12", "5類全数"="#2980b9", "#7f8c8d")
      cur_fmt <- if (r$type == "定点") sprintf("%.2f", coalesce(r$cur_val, 0))
                 else sprintf("%d件", as.integer(coalesce(r$cur_val, 0)))

      tags$div(
        style=paste0(
          "border:1px solid ",cfg$border,";border-left:5px solid ",cfg$color,";",
          "background:",cfg$bg,";border-radius:6px;padding:8px 10px;cursor:pointer;",
          "display:flex;flex-direction:column;gap:3px;height:100%;box-sizing:border-box;"
        ),
        onclick=sprintf(
          "Shiny.setInputValue('disease_tile_click', {id:'%s', zensu:%s, t:Date.now()}, {priority:'event'})",
          r$id, if (identical(r$type, "定点") || isTRUE(r$is_std)) "false" else "true"
        ),
        title = paste0(r$label, "　Lv", r$act_level, "　", cur_fmt, "　", r$ibs_label, "　（クリックで流行曲線タブへ）"),
        # 疾患名 + 類型バッジ
        tags$div(style="display:flex;justify-content:space-between;align-items:flex-start;gap:4px;",
          tags$div(style="font-size:0.82em;font-weight:700;color:#222;line-height:1.3;flex:1;",
                   r$label),
          tags$span(style=paste0("font-size:0.62em;background:",type_col,
            ";color:#fff;border-radius:8px;padding:1px 5px;white-space:nowrap;flex-shrink:0;"),
            r$type)
        ),
        # 活動レベルバッジ
        tags$div(
          tags$span(style=paste0(
            "display:inline-block;padding:2px 8px;border-radius:10px;",
            "background:",cfg$color,";color:#fff;font-size:0.75em;font-weight:700;"),
            paste0("Lv",r$act_level," ",cfg$name))
        ),
        # 直近値 + IBS位置
        tags$div(style="font-size:0.7em;color:#666;",
          paste0(cur_fmt, "　", r$ibs_label)),
        # IBS評価方式（季節性の有無）を表示
        if (!is.null(r$ibs_method)) tags$div(
          style="font-size:0.62em;color:#999;",
          paste0("判定方式: ", if (identical(r$ibs_method,"seasonal")) "季節性あり" else "散発疾患向け", "　"),
          tags$a(href="javascript:void(0)", onclick="goToNotes('notes-zensu-ibs')", "詳細")
        )
      )
    })

    # 4カラムグリッド
    n <- length(cards)
    cols_per_row <- 4L
    rows <- split(seq_len(n), ceiling(seq_len(n) / cols_per_row))
    tagList(
      do.call(tagList, lapply(rows, function(idx) {
        fluidRow(style="margin-bottom:8px;",
          lapply(idx, function(i) column(3, style="padding:4px;", cards[[i]]))
        )
      })),
      tags$p(style="font-size:0.7em;color:#999;text-align:center;margin-top:4px;",
        "※ タイルをクリックするとその疾患・現在の都道府県フィルターの流行曲線タブに移動します。")
    )
  })

  # ── 活動レベル一覧（都道府県別）────────────────────────────
  # サイドバーで選択中の疾患について、全47都道府県の統合活動レベルを
  # 北から南の順（PREF_MASTER$pref_code順）にタイル表示する。
  # スコア計算ロジックはall_disease_levels_data（疾患別一覧）と同一で、
  # ループの軸を「疾患」から「都道府県」に入れ替えたもの。
  pref_levels_data <- reactive({
    is_zensu <- !is.null(input$ts_mode) && input$ts_mode == "zensu"
    did <- if (is_zensu) input$zensu_disease_ts else input$disease
    is_std <- !is_zensu && did %in% names(STD_DISEASE_CONFIG)

    ebs_d <- tryCatch(ebs_data(), error=function(e) NULL)
    weights <- c("Signal High"=3L, "Signal Low"=2L, "FYI"=0L)
    ebs_score_for_pref <- function(pref_name_i, ref_date = Sys.Date()) {
      tryCatch({
        if (is.null(ebs_d) || nrow(ebs_d) == 0) return(0L)
        if (is.na(ref_date) || as.numeric(Sys.Date() - as.Date(ref_date)) > EBS_SCORE_EVALUABLE_DAYS) return(0L)
        ref_date <- as.Date(ref_date)
        de <- ebs_d %>% filter(!is.na(pub_date),
                               is.na(source_id) | source_id != "pubmed",
                               has_disease_tag(disease_tags, did))
        ps <- sub("(都|道|府|県)$", "", pref_name_i)
        de <- de %>% filter(
          (!is.na(ebs_pref) & ebs_pref == pref_name_i) |
          vapply(paste(coalesce(title,""), coalesce(summary,"")),
                 function(t) grepl(pref_name_i, t, fixed=TRUE) | grepl(ps, t, fixed=TRUE),
                 FUN.VALUE = logical(1)))
        s_this <- sum(weights[as.character(
          de %>% filter(pub_date >= ref_date - 7, pub_date <= ref_date) %>% pull(signal_level))], na.rm=TRUE)
        s_prev <- sum(weights[as.character(
          de %>% filter(pub_date >= ref_date - 14, pub_date < ref_date - 7) %>% pull(signal_level))], na.rm=TRUE)
        chg <- if (s_prev > 0) (s_this - s_prev)/s_prev*100 else if (s_this > 0) 100 else 0
        lv <- if (s_this==0 && s_prev==0) -1L
              else if (chg < -20) -1L else if (chg < 20) 0L
              else if (chg <  50) 1L  else 2L
        max(0L, lv)
      }, error=function(e) 0L)
    }

    # ── 統合スコア: 「現在の流行フェーズ」カード（kpi_alert）と同一ロジック ──
    combined_score <- function(value, thresh, rt_value, ibs_s, ebs_s) {
      ibs_blended <- tryCatch({
        s <- compute_alert_score(value = value, thresh = thresh, rt_value = rt_value, ibs_score = ibs_s)
        if (is.na(s)) ibs_s else s
      }, error = function(e) ibs_s)
      ebs_scaled <- min(3, ebs_s * 1.5)
      ibs_blended * 0.95 + ebs_scaled * 0.05
    }
    act_level <- function(sc) min(4L, max(1L, as.integer(round(sc)) + 1L))

    thresh_val <- if (!is_zensu && !is_std) tryCatch(DISEASE_CONFIG[[did]]$alert_threshold, error=function(e) NULL) else NULL

    results <- list()
    pref_order <- PREF_MASTER$pref_name[order(PREF_MASTER$pref_code)]
    dr <- input$date_range

    if (is_std) {
      base_d <- if (is.null(STD_DATA)) STD_DATA[0,] else STD_DATA %>% filter(disease == did)
      for (pr in pref_order) {
        tryCatch({
          dd <- base_d %>% filter(pref_name == pr) %>%
            group_by(date, year, month) %>%
            summarise(reports = sum(reports, na.rm=TRUE), .groups="drop") %>%
            arrange(date)
          if (nrow(dd) == 0) return(NULL)
          dd_sel <- dd %>% filter(date >= dr[1], date <= dr[2])
          if (nrow(dd_sel) == 0) return(NULL)
          recent2 <- slice_tail(dd_sel, n=2)
          cur  <- slice_tail(recent2, n=1)
          prev <- if (nrow(recent2) >= 2) slice_head(recent2, n=1) else cur
          band <- std_ibs_score(
            cur_val=cur$reports[1], cur_year=cur$year[1], cur_month=cur$month[1],
            prev_val=prev$reports[1], prev_year=prev$year[1], prev_month=prev$month[1],
            hist_d=dd)
          ibs_s <- band$score
          ebs_s <- ebs_score_for_pref(pr, cur$date[1])
          sc    <- combined_score(cur$reports[1], NULL, NA_real_, ibs_s, ebs_s)
          results[[pr]] <- list(
            id=pr, label=pr, region=PREF_MASTER$region[PREF_MASTER$pref_name==pr][1],
            grid_row=PREF_MASTER$grid_row[PREF_MASTER$pref_name==pr][1],
            grid_col=PREF_MASTER$grid_col[PREF_MASTER$pref_name==pr][1],
            grid_colspan=PREF_MASTER$grid_colspan[PREF_MASTER$pref_name==pr][1],
            grid_rowspan=PREF_MASTER$grid_rowspan[PREF_MASTER$pref_name==pr][1],
            ibs_score=ibs_s, ebs_score=ebs_s,
            act_level=act_level(sc), combined=sc,
            cur_val=cur$reports[1],
            ibs_label=band$label, ibs_method=band$method,
            year=cur$year[1], week=NA_integer_
          )
        }, error=function(e) NULL)
      }
    } else if (!is_zensu) {
      base_d <- SURV_DATA %>% filter(disease == did)
      for (pr in pref_order) {
        tryCatch({
          dd <- base_d %>% filter(pref_name == pr) %>% arrange(date)
          if (nrow(dd) == 0) return(NULL)
          # スライダー(date_range)で選択した期間の末尾2週を評価対象とする
          # （過去5年比較baselineにはddの全期間データをそのまま使う）
          dd_sel <- dd %>% filter(date >= dr[1], date <= dr[2])
          if (nrow(dd_sel) == 0) return(NULL)
          recent2 <- slice_tail(dd_sel, n=2)
          cur  <- slice_tail(recent2, n=1)
          prev <- if (nrow(recent2) >= 2) slice_head(recent2, n=1) else cur
          # 定点把握疾患: 既存データから季節性を自動判定し評価方式を切替（zensu_ibs_band参照）
          band   <- zensu_ibs_band(
            cur_val=cur$reports_per_site[1], cur_date=cur$date[1], cur_week=cur$week[1], cur_year=cur$year[1],
            prev_val=prev$reports_per_site[1], prev_date=prev$date[1], prev_week=prev$week[1], prev_year=prev$year[1],
            hist_d=dd, detail_fmt="rate")
          ibs_s  <- band$score
          ebs_s  <- ebs_score_for_pref(pr, cur$date[1])
          rt_latest <- tryCatch({
            rd <- compute_rt_series(SURV_DATA %>% filter(disease == did, date >= dr[1], date <= dr[2]), did, pr) %>%
              filter(!is.na(rt)) %>% slice_tail(n=1)
            if (nrow(rd)==0) NA_real_ else rd$rt[1]
          }, error=function(e) NA_real_)
          sc     <- combined_score(cur$reports_per_site[1], thresh_val, rt_latest, ibs_s, ebs_s)
          results[[pr]] <- list(
            id=pr, label=pr, region=PREF_MASTER$region[PREF_MASTER$pref_name==pr][1],
            grid_row=PREF_MASTER$grid_row[PREF_MASTER$pref_name==pr][1],
            grid_col=PREF_MASTER$grid_col[PREF_MASTER$pref_name==pr][1],
            grid_colspan=PREF_MASTER$grid_colspan[PREF_MASTER$pref_name==pr][1],
            grid_rowspan=PREF_MASTER$grid_rowspan[PREF_MASTER$pref_name==pr][1],
            ibs_score=ibs_s, ebs_score=ebs_s,
            act_level=act_level(sc), combined=sc,
            cur_val=cur$reports_per_site[1],
            ibs_label=band$label, ibs_method=band$method,
            year=cur$year[1], week=cur$week[1]
          )
        }, error=function(e) NULL)
      }
    } else {
      base_d <- ZENSU_DATA %>% filter(disease == did)
      for (pr in pref_order) {
        tryCatch({
          dd <- base_d %>% filter(pref_name == pr) %>%
            group_by(date, year, week) %>%
            summarise(reports_per_site = sum(cases, na.rm=TRUE), .groups="drop") %>%
            arrange(date)
          if (nrow(dd) == 0) return(NULL)
          # スライダー(date_range)で選択した期間の末尾2週を評価対象とする
          dd_sel <- dd %>% filter(date >= dr[1], date <= dr[2])
          if (nrow(dd_sel) == 0) return(NULL)
          recent2 <- slice_tail(dd_sel, n=2)
          cur  <- slice_tail(recent2, n=1)
          prev <- if (nrow(recent2) >= 2) slice_head(recent2, n=1) else cur
          band <- zensu_ibs_band(
            cur_val=cur$reports_per_site[1], cur_date=cur$date[1], cur_week=cur$week[1], cur_year=cur$year[1],
            prev_val=prev$reports_per_site[1], prev_date=prev$date[1], prev_week=prev$week[1], prev_year=prev$year[1],
            hist_d=dd)
          ibs_s <- band$score
          ebs_s <- ebs_score_for_pref(pr, cur$date[1])
          rt_latest <- tryCatch({
            if (is.null(ZENSU_DATA) || nrow(ZENSU_DATA) == 0) {
              NA_real_
            } else {
              rd <- compute_rt_series_zensu(ZENSU_DATA, did, pr) %>% filter(!is.na(rt)) %>% slice_tail(n=1)
              if (nrow(rd)==0) NA_real_ else rd$rt[1]
            }
          }, error=function(e) NA_real_)
          sc    <- combined_score(cur$reports_per_site[1], NULL, rt_latest, ibs_s, ebs_s)
          results[[pr]] <- list(
            id=pr, label=pr, region=PREF_MASTER$region[PREF_MASTER$pref_name==pr][1],
            grid_row=PREF_MASTER$grid_row[PREF_MASTER$pref_name==pr][1],
            grid_col=PREF_MASTER$grid_col[PREF_MASTER$pref_name==pr][1],
            grid_colspan=PREF_MASTER$grid_colspan[PREF_MASTER$pref_name==pr][1],
            grid_rowspan=PREF_MASTER$grid_rowspan[PREF_MASTER$pref_name==pr][1],
            ibs_score=ibs_s, ebs_score=ebs_s,
            act_level=act_level(sc), combined=sc,
            cur_val=cur$reports_per_site[1],
            ibs_label=band$label, ibs_method=band$method,
            year=cur$year[1], week=cur$week[1]
          )
        }, error=function(e) NULL)
      }
    }

    Filter(Negate(is.null), results)
  })

  output$pref_levels_header <- renderUI({
    is_zensu <- !is.null(input$ts_mode) && input$ts_mode == "zensu"
    did   <- if (is_zensu) input$zensu_disease_ts else input$disease
    is_std <- !is_zensu && did %in% names(STD_DISEASE_CONFIG)
    label <- get_disease_label(if (is_zensu) "zensu" else "teiten", did, did)
    if (is.null(label)) label <- did
    res <- pref_levels_data()
    n_by_level <- if (length(res) > 0) table(sapply(res, `[[`, "act_level")) else table(integer(0))
    res_wk <- Filter(function(r) !is.na(r$week), res)
    wk_txt <- if (length(res_wk) > 0) {
      wk_key <- sapply(res_wk, function(r) paste0(r$year, "-", r$week))
      top_key <- names(sort(table(wk_key), decreasing = TRUE))[1]
      parts <- strsplit(top_key, "-")[[1]]
      sprintf("　%s年 第%s週時点", parts[1], parts[2])
    } else if (is_std && length(res) > 0) {
      ym_key <- sapply(res, function(r) r$year)
      sprintf("　%s年時点", names(sort(table(ym_key), decreasing = TRUE))[1])
    } else ""
    tags$div(style="margin-bottom:10px;display:flex;align-items:center;gap:16px;flex-wrap:wrap;",
      tags$div(style="font-size:0.9em;font-weight:700;color:#333;",
               paste0(label, " — 都道府県別 統合活動レベル一覧　", length(res), "都道府県", wk_txt)),
      tags$div(style="display:flex;gap:8px;",
        lapply(4:1, function(lv) {
          cfg <- list(`4`=list(col="#c0392b",name="流行"),`3`=list(col="#e67e22",name="警戒"),
                      `2`=list(col="#d4ac0d",name="注意"),`1`=list(col="#27ae60",name="通常"))[[as.character(lv)]]
          n <- as.integer(n_by_level[as.character(lv)]); if (is.na(n)) n <- 0L
          tags$span(style=paste0("background:",cfg$col,";color:#fff;border-radius:10px;",
            "padding:2px 10px;font-size:0.75em;font-weight:700;"),
            paste0("Lv",lv," ",cfg$name,": ",n,"都道府県"))
        })
      )
    )
  })

  output$pref_levels_ui <- renderUI({
    res <- pref_levels_data()
    if (length(res) == 0) return(tags$p("データなし"))

    lcfg <- list(
      `1`=list(color="#27ae60", bg="#eafaf1", border="#a9dfbf", name="通常"),
      `2`=list(color="#d4ac0d", bg="#fefde7", border="#f9e79f", name="注意"),
      `3`=list(color="#e67e22", bg="#fef5e7", border="#f8c471", name="警戒"),
      `4`=list(color="#c0392b", bg="#fdedec", border="#f1948a", name="流行")
    )

    is_zensu <- !is.null(input$ts_mode) && input$ts_mode == "zensu"
    did_cur  <- if (is_zensu) input$zensu_disease_ts else input$disease
    is_std   <- !is_zensu && did_cur %in% names(STD_DISEASE_CONFIG)
    cur_fmt_fn <- if (is_zensu || is_std)
      function(v) sprintf("%d件", as.integer(coalesce(v, 0)))
    else
      function(v) sprintf("%.2f", coalesce(v, 0))

    # デフォルメした日本地図のグリッド上に、都道府県ごとのタイルを配置する
    # （grid_row/grid_colはPREF_MASTERで定義した簡易的な相対配置）
    max_row <- max(sapply(res, function(r) r$grid_row + coalesce(r$grid_rowspan, 1L) - 1L))
    max_col <- max(sapply(res, function(r) r$grid_col + coalesce(r$grid_colspan, 1L) - 1L))

    tiles <- lapply(res, function(r) {
      cfg <- lcfg[[as.character(r$act_level)]]
      # 一部の都道府県（北海道・青森・福島・京都・和歌山は横2マス、熊本は縦2マス）は
      # 参考画像に合わせて複数マス分の大きさで表示する
      colspan <- coalesce(r$grid_colspan, 1L)
      rowspan <- coalesce(r$grid_rowspan, 1L)
      col_span <- if (colspan > 1) paste0(r$grid_col," / span ",colspan) else as.character(r$grid_col)
      row_span <- if (rowspan > 1) paste0(r$grid_row," / span ",rowspan) else as.character(r$grid_row)
      # 愛媛・香川・高知・徳島は、グリッド線を変えずに見た目だけ0.5マス右にずらす
      # （transformは要素自身のレイアウト幅に影響しないため、marginと違いカードが
      # 半分の幅に縮んでしまう問題が起きない）
      half_shift <- if (r$label %in% c("愛媛県","香川県","高知県","徳島県","神奈川県")) "transform:translateX(50%);"
                    else if (r$label == "沖縄県") "transform:translateX(-150%);"
                    else if (r$label %in% c("大阪府","奈良県","和歌山県","愛知県")) "transform:translateY(-50%);"
                    else if (r$label %in% c("群馬県","富山県")) "transform:translateY(50%);"
                    # 山梨は高さを150%に広げた後の座標系で0.5マス（=元の高さの50%）分
                    # 上にずらすため、拡大後の高さに対する比率(-50%/1.5)を使う
                    else if (r$label %in% c("山梨県","北海道")) "transform:translateY(-33.3333%);"
                    # 長野は縦2マス確保を高さ75%(=1.5マス分)に縮小した後の座標系で
                    # 0.5マス（=縮小前の1マス分/縮小後の高さ1.5マスに対する比率）分
                    # 下にずらすため、(0.5/1.5)の比率(33.3333%)を使う
                    else if (identical(r$label, "長野県")) "transform:translateY(33.3333%);"
                    else ""
      # 青森は、グリッド上は北海道と同じ2マス分を確保したまま、見た目の幅だけ
      # 1.5マス分（2マスの75%）に縮小する（右側の0.5マス分は空白として残る）
      # 兵庫・京都・岐阜・長野も同様に、縦2マス分を確保したまま高さだけ1.5マス分に縮小する
      # 静岡・新潟・山梨は逆に、グリッド上は1マス分のまま見た目だけ1.5マス分に拡大する
      # （隣接マス（神奈川・群馬）が0.5マスずれて空いた分に視覚的に重なる想定）
      size_override <- if (identical(r$label, "青森県")) "width:75%;"
                        else if (r$label %in% c("兵庫県","京都府","千葉県","岐阜県","長野県")) "height:75%;"
                        else if (identical(r$label, "静岡県")) "width:150%;"
                        else if (r$label %in% c("新潟県","山梨県","北海道")) "height:150%;"
                        else ""
      tags$div(
        style=paste0(
          "grid-row:",row_span,";grid-column:",col_span,";",
          "border:1px solid ",cfg$border,";border-left:4px solid ",cfg$color,";",
          "background:",cfg$bg,";border-radius:5px;padding:4px 6px;",
          "display:flex;flex-direction:column;gap:1px;box-sizing:border-box;",
          "font-size:0.68em;min-width:0;overflow:hidden;cursor:pointer;",
          half_shift, size_override
        ),
        onclick=sprintf(
          "Shiny.setInputValue('pref_tile_click', {pref:'%s', t:Date.now()}, {priority:'event'})",
          r$label
        ),
        title = paste0(r$label, "　Lv", r$act_level, " ", cfg$name,
                       "　", cur_fmt_fn(r$cur_val), "　", r$ibs_label, "　（クリックで流行曲線タブへ）"),
        tags$div(style="font-weight:700;color:#222;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;",
                 if (identical(r$label, "北海道")) r$label else sub("[都道府県]$", "", r$label)),
        tags$span(style=paste0(
          "display:inline-block;padding:0 5px;border-radius:8px;align-self:flex-start;",
          "background:",cfg$color,";color:#fff;font-size:0.95em;font-weight:700;"),
          paste0("Lv",r$act_level))
      )
    })

    tagList(
      tags$div(style=paste0(
        "display:grid;",
        "grid-template-columns:repeat(",max_col,", minmax(38px, 1fr));",
        "grid-template-rows:repeat(",max_row,", 46px);",
        "gap:3px;max-width:760px;margin:0 auto;"
      ), tiles),
      tags$p(style="font-size:0.7em;color:#999;text-align:center;margin-top:8px;",
        "※ タイルをクリックするとその都道府県・疾患の流行曲線タブに移動します。")
    )
  })

  # タイルクリック → サイドバーの都道府県フィルターを切り替えて流行曲線タブへ移動
  # （疾患・全数/定点の選択はサイドバーの現在の選択をそのまま引き継ぐ）
  observeEvent(input$pref_tile_click, {
    req(input$pref_tile_click$pref)
    updateSelectInput(session, "pref_filter", selected = input$pref_tile_click$pref)
    updateTabsetPanel(session, "main_tabs", selected = "患者サーベイランス")
    updateTabsetPanel(session, "sub_patient", selected = "流行曲線")
  })

  # 疾患別タイルクリック → 表示モード・疾患を切り替えて流行曲線タブへ移動
  # （都道府県フィルターの選択はそのまま引き継ぐ）
  observeEvent(input$disease_tile_click, {
    req(input$disease_tile_click$id)
    if (isTRUE(input$disease_tile_click$zensu)) {
      updateRadioButtons(session, "ts_mode", selected = "zensu")
      updateSelectInput(session, "zensu_disease_ts", selected = input$disease_tile_click$id)
    } else {
      updateRadioButtons(session, "ts_mode", selected = "teiten")
      updateSelectInput(session, "disease", selected = input$disease_tile_click$id)
    }
    updateTabsetPanel(session, "main_tabs", selected = "患者サーベイランス")
    updateTabsetPanel(session, "sub_patient", selected = "流行曲線")
  })

  # ── 入院サーベイランス（IDWR週報PDF）──────────────────────

  hosp_reactive <- reactiveVal(NULL)
  observe({
    hosp_reactive(HOSP_DATA)
  })

  observeEvent(input$hosp_update, {
    withProgress(message = "入院サーベイランスデータ更新中...", value = 0.3, {
      this_year <- as.integer(format(Sys.Date(), "%Y"))
      new_d <- tryCatch(
        update_hosp_data(years = (this_year - 1):this_year, force_latest_n = 3),
        error = function(e) { showNotification(paste("更新エラー:", e$message), type = "error"); NULL }
      )
      if (!is.null(new_d)) {
        hosp_reactive(new_d)
        tryCatch(
          record_data_change("hosp", compute_recent_signature(new_d, "date", 180)),
          error = function(e) NULL
        )
        showNotification("入院サーベイランスデータを更新しました", type = "message")
      }
    })
  })

  # date_range（表示範囲スライダー）に依存しない全期間データ。
  # ±2SD帯の過去5年比較の基準として使う（スライダーで表示範囲を絞っても
  # 過去の比較対象データが欠落しないようにするため）
  hosp_agg_all <- reactive({
    d <- hosp_reactive()
    if (is.null(d) || nrow(d) == 0) return(NULL)
    if (!is.null(input$pref_filter) && input$pref_filter != "全国") {
      d <- d %>% filter(pref_name == input$pref_filter)
    }
    d %>%
      group_by(year, week, date) %>%
      summarise(flu_hosp = sum(flu_hosp, na.rm = TRUE),
                covid_hosp = if (all(is.na(covid_hosp))) NA_real_ else sum(covid_hosp, na.rm = TRUE),
                .groups = "drop") %>%
      arrange(date)
  })

  # 表示範囲スライダー（date_range）で絞り込んだ主系列
  hosp_filtered <- reactive({
    d <- hosp_agg_all()
    if (is.null(d) || nrow(d) == 0) return(NULL)
    dr <- input$date_range
    if (!is.null(dr) && length(dr) == 2) {
      d <- d %>% filter(date >= dr[1], date <= dr[2])
    }
    d
  })

  output$hosp_last_updated <- renderUI({
    t <- tryCatch(get_last_change_time("hosp"), error = function(e) NA)
    tags$div(style="font-size:0.78em;color:#888;margin:2px 0 6px;",
      if (!is.na(t)) paste0("更新検知日時: ", format(t, "%Y-%m-%d %H:%M")) else "未取得")
  })

  output$hosp_plot <- renderPlotly({
    d <- hosp_filtered()
    hist_all <- hosp_agg_all()
    if (is.null(d) || nrow(d) == 0)
      return(plot_ly() %>% add_annotations(
        text = "データなし（「入院データ更新」を押して取得してください）", showarrow = FALSE))

    p <- plot_ly()
    if ("flu" %in% input$hosp_disease) {
      d_flu <- compute_hosp_band(d, hist_all, "flu_hosp")
      p <- p %>%
        add_ribbons(data = d_flu %>% filter(has_hist), x = ~date, ymin = ~ymin, ymax = ~ymax,
          fillcolor = "#3498db33", line = list(color = "transparent"),
          name = "インフルエンザ　過去5年平均±2SD", hoverinfo = "skip") %>%
        add_lines(data = d_flu, x = ~date, y = ~flu_hosp,
          line = list(color = "#3498db", width = 2.5), name = "インフルエンザ（入院患者）",
          hovertemplate = "%{x|%Y-%m-%d}　インフルエンザ入院 %{y}例<extra></extra>")
      exceed_flu <- d_flu %>% filter(has_hist, flu_hosp > ymax)
      if (nrow(exceed_flu) > 0) {
        p <- p %>% add_markers(data = exceed_flu, x = ~date, y = ~flu_hosp,
          marker = list(color = "#e74c3c", size = 6, symbol = "circle"),
          name = "インフルエンザ +2SD超過",
          hovertemplate = "%{x|%Y-%m-%d}: %{y}例<extra></extra>")
      }
    }
    if ("covid" %in% input$hosp_disease) {
      d_covid <- d %>% filter(!is.na(covid_hosp))
      if (nrow(d_covid) > 0) {
        d_covid <- compute_hosp_band(d_covid, hist_all, "covid_hosp")
        p <- p %>%
          add_ribbons(data = d_covid %>% filter(has_hist), x = ~date, ymin = ~ymin, ymax = ~ymax,
            fillcolor = "#e74c3c22", line = list(color = "transparent"),
            name = "新型コロナ　過去5年平均±2SD", hoverinfo = "skip") %>%
          add_lines(data = d_covid, x = ~date, y = ~covid_hosp,
            line = list(color = "#e74c3c", width = 2.5), name = "新型コロナウイルス感染症（入院患者）",
            hovertemplate = "%{x|%Y-%m-%d}　新型コロナ入院 %{y}例<extra></extra>")
        exceed_covid <- d_covid %>% filter(has_hist, covid_hosp > ymax)
        if (nrow(exceed_covid) > 0) {
          p <- p %>% add_markers(data = exceed_covid, x = ~date, y = ~covid_hosp,
            marker = list(color = "#c0392b", size = 6, symbol = "circle"),
            name = "新型コロナ +2SD超過",
            hovertemplate = "%{x|%Y-%m-%d}: %{y}例<extra></extra>")
        }
      }
    }
    p %>% layout(
      xaxis = list(title = "", showgrid = FALSE, type = "date"),
      yaxis = list(title = "入院患者報告数（例）", gridcolor = "#eee", rangemode = "nonnegative"),
      legend = list(orientation = "h", y = -0.15),
      hovermode = "x unified",
      plot_bgcolor = "#fff", paper_bgcolor = "#fff",
      margin = list(t = 20, b = 40, l = 60, r = 20)
    )
  })

  output$hosp_legend <- renderUI({
    tags$div(style="font-size:0.75em;color:#666;margin-top:2px;padding-left:4px;display:flex;gap:16px;align-items:center;flex-wrap:wrap;",
      tags$span(
        tags$span(style="display:inline-block;width:8px;height:8px;background:#e74c3c;border-radius:50%;margin-right:4px;vertical-align:middle;"),
        "+2SD超過"
      ),
      tags$span("帯: 過去5年間・同時期（±2週）平均±2SD（3年分以上のデータがある週のみ表示）"),
      tags$span(
        "出典: 感染症発生動向調査週報（IDWR）基幹定点 入院サーベイランス。",
        "新型コロナウイルス感染症（入院患者）は2023年5月の5類移行後のみ集計対象。",
        tags$a(href="javascript:void(0)", onclick="goToNotes('notes-hosp')", "詳細")
      )
    )
  })

  output$hosp_table <- renderDT({
    d <- hosp_filtered()
    if (is.null(d) || nrow(d) == 0) return(datatable(data.frame()))
    d %>%
      transmute(年 = year, 週 = week, 週開始日 = format(date, "%Y-%m-%d"),
                `インフルエンザ入院` = flu_hosp,
                `新型コロナ入院` = covid_hosp) %>%
      arrange(desc(年), desc(週)) %>%
      datatable(options = list(pageLength = 15, dom = "tip"), rownames = FALSE)
  })

  output$hosp_table_dl <- downloadHandler(
    filename = function() paste0("入院サーベイランス_", Sys.Date(), ".csv"),
    content = function(file) {
      d <- hosp_filtered()
      if (is.null(d) || nrow(d) == 0) { write.csv(data.frame(), file, row.names = FALSE); return() }
      out <- d %>%
        transmute(年 = year, 週 = week, 週開始日 = format(date, "%Y-%m-%d"),
                  インフルエンザ入院 = flu_hosp, 新型コロナ入院 = covid_hosp)
      write_csv_bom(out, file)
    }
  )

  # ── 病原体検出（IASR）────────────────────────────────────

  # カテゴリ変更時にウイルス絞り込み選択肢を更新
  observeEvent(list(input$iasr_category, input$iasr_time_type), {
    req(input$iasr_category, input$iasr_time_type)
    if (is.null(IASR_DATA) || nrow(IASR_DATA) == 0) return()
    d <- IASR_DATA %>%
      filter(category == input$iasr_category, time_type == input$iasr_time_type)
    viruses <- sort(unique(d$virus))
    updateSelectInput(session, "iasr_virus_filter", choices = viruses, selected = NULL)
  }, ignoreInit = FALSE)

  # フィルター後データ
  iasr_filtered <- reactive({
    req(input$iasr_category, input$iasr_time_type)
    if (is.null(IASR_DATA) || nrow(IASR_DATA) == 0) return(NULL)
    d <- IASR_DATA %>%
      filter(category == input$iasr_category,
             time_type == input$iasr_time_type)
    # コントロールパネルの期間スライダーと連動
    dr <- input$date_range
    if (!is.null(dr) && length(dr) == 2) {
      d <- d %>% filter(date >= dr[1], date <= dr[2])
    }
    if (!is.null(input$iasr_virus_filter) && length(input$iasr_virus_filter) > 0)
      d <- d %>% filter(virus %in% input$iasr_virus_filter)
    d %>% arrange(date, virus)
  })

  output$iasr_last_updated <- renderUI({
    cat_id <- input$iasr_category
    cache_f <- file.path(IASR_CACHE_DIR, paste0(cat_id, ".rds"))
    if (file.exists(cache_f))
      tags$span(paste0("更新: ", format(file.mtime(cache_f), "%m/%d %H:%M")))
    else tags$span("未取得")
  })

  output$iasr_plot <- renderPlotly({
    d <- iasr_filtered()
    if (is.null(d) || nrow(d) == 0)
      return(plot_ly() %>% add_annotations(text = "データなし", showarrow = FALSE))

    cat_label <- IASR_CATEGORIES[[input$iasr_category]]$label

    # 検出数合計0のウイルスは除外（折れ線・積み上げ共通）
    active_viruses <- d %>%
      group_by(virus) %>%
      summarise(total = sum(count), .groups = "drop") %>%
      filter(total > 0) %>%
      pull(virus)
    d <- d %>% filter(virus %in% active_viruses)

    if (nrow(d) == 0)
      return(plot_ly() %>% add_annotations(text = "検出数0（データなし）", showarrow = FALSE))

    # 日付ラベル
    if (input$iasr_time_type == "monthly") {
      d <- d %>% mutate(label = format(date, "%Y/%m"))
      xlab <- "年月"
    } else {
      d <- d %>% mutate(label = format(date, "%Y"))
      xlab <- "年"
    }

    # カラーパレット（ウイルス数に応じて）
    n_virus <- length(unique(d$virus))
    pal <- if (n_virus <= 8) RColorBrewer::brewer.pal(max(3, n_virus), "Set2")
           else colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(n_virus)
    virus_list <- sort(unique(d$virus))
    col_map <- setNames(pal[seq_along(virus_list)], virus_list)

    p <- plot_ly()
    if (input$iasr_chart_type == "bar") {
      for (v in virus_list) {
        dv <- d %>% filter(virus == v) %>% arrange(date)
        p <- p %>% add_bars(
          data = dv, x = ~label, y = ~count, name = v,
          marker = list(color = col_map[[v]]),
          hovertemplate = paste0(v, "<br>%{x}: %{y}件<extra></extra>"))
      }
      p <- p %>% layout(barmode = "stack")
    } else {
      for (v in virus_list) {
        dv <- d %>% filter(virus == v) %>% arrange(date)
        p <- p %>% add_lines(
          data = dv, x = ~label, y = ~count, name = v,
          line = list(color = col_map[[v]], width = 2),
          hovertemplate = paste0(v, "<br>%{x}: %{y}件<extra></extra>"))
      }
    }

    # 直近1か月は今後遅れて報告される可能性があるため、グレーの網掛けで示す
    # （月別表示のみ。年別表示は「1か月」の区切りに対応しないため対象外）
    recent_shapes <- list()
    recent_annotations <- list()
    if (input$iasr_time_type == "monthly") {
      n_labels <- length(unique(d$label))
      if (n_labels >= 1) {
        recent_shapes <- list(list(
          type = "rect", xref = "x domain", yref = "paper",
          x0 = 1 - 1 / n_labels, x1 = 1, y0 = 0, y1 = 1,
          fillcolor = "rgba(128,128,128,0.25)", line = list(width = 0),
          layer = "above"
        ))
        recent_annotations <- list(list(
          xref = "x domain", yref = "paper",
          x = 1 - 0.5 / n_labels, y = 1.03, xanchor = "center", yanchor = "bottom",
          text = "直近は参考値", showarrow = FALSE,
          font = list(size = 9, color = "#888")
        ))
      }
    }

    p %>% layout(
      title = list(text = paste0(cat_label, "　ウイルス検出状況"), x = 0, font = list(size = 14)),
      xaxis  = list(title = xlab, showgrid = FALSE, tickangle = -45),
      yaxis  = list(title = "検出数（件）", gridcolor = "#eee"),
      legend = list(orientation = "v", x = 1.01, y = 1, font = list(size = 10)),
      hovermode  = "x unified",
      plot_bgcolor  = "#fff",
      paper_bgcolor = "#fff",
      shapes = recent_shapes,
      annotations = recent_annotations,
      margin = list(t = 40, b = 80, l = 60, r = 160)
    )
  })

  output$iasr_table <- renderDT({
    d <- iasr_filtered()
    if (is.null(d) || nrow(d) == 0) return(datatable(data.frame()))

    d_wide <- d %>%
      filter(count > 0) %>%
      select(virus, label = date, count) %>%
      mutate(label = if (isolate(input$iasr_time_type) == "monthly")
                       format(label, "%Y/%m") else format(label, "%Y")) %>%
      pivot_wider(names_from = label, values_from = count, values_fill = 0L) %>%
      arrange(virus)

    datatable(d_wide,
      options = list(pageLength = 15, scrollX = TRUE, dom = "tip"),
      rownames = FALSE) %>%
      formatStyle(names(d_wide)[-1],
        background = styleColorBar(c(0, max(unlist(d_wide[,-1]), na.rm=TRUE)), "#bde0f5"),
        backgroundSize = "98% 60%", backgroundRepeat = "no-repeat",
        backgroundPosition = "center")
  })

  output$iasr_table_dl <- downloadHandler(
    filename = function() {
      cat_label <- IASR_CATEGORIES[[input$iasr_category]]$label
      paste0("IASR_", cat_label, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      d <- iasr_filtered()
      if (is.null(d) || nrow(d) == 0) { write.csv(data.frame(), file, row.names=FALSE); return() }
      d_wide <- d %>%
        filter(count > 0) %>%
        select(virus, label = date, count) %>%
        mutate(label = if (input$iasr_time_type == "monthly")
                         format(label, "%Y/%m") else format(label, "%Y")) %>%
        pivot_wider(names_from = label, values_from = count, values_fill = 0L) %>%
        arrange(virus)
      write_csv_bom(d_wide, file)
    }
  )

  # ── ARI病原体（急性呼吸器感染症サーベイランス週報）────────
  output$ari_last_updated <- renderUI({
    if (file.exists(ARI_DATA_CACHE))
      tags$span(style="font-size:0.78em;color:#888;",
        paste0("更新: ", format(file.mtime(ARI_DATA_CACHE), "%m/%d %H:%M")))
    else tags$span(style="font-size:0.78em;color:#888;", "未取得")
  })

  ari_count_filtered <- reactive({
    if (is.null(ARI_PATHOGEN_DATA) || is.null(ARI_PATHOGEN_DATA$counts)) return(NULL)
    d <- ARI_PATHOGEN_DATA$counts
    dr <- input$date_range
    if (!is.null(dr) && length(dr) == 2) d <- d %>% filter(date >= dr[1], date <= dr[2])
    d
  })

  output$ari_count_plot <- renderPlotly({
    d <- ari_count_filtered()
    if (is.null(d) || nrow(d) == 0)
      return(plot_ly() %>% add_annotations(text = "データなし", showarrow = FALSE))
    d <- d %>% mutate(category = factor(category, levels = rev(ARI_COUNT_ORDER)))
    p <- plot_ly()
    for (cat_id in ARI_COUNT_ORDER) {
      dd <- d %>% filter(category == cat_id)
      if (nrow(dd) == 0) next
      p <- p %>% add_bars(data = dd, x = ~date, y = ~reports,
        name = ARI_COUNT_LABELS[[cat_id]],
        marker = list(color = ARI_COUNT_LEGEND[[cat_id]]),
        hovertemplate = paste0(ARI_COUNT_LABELS[[cat_id]], "　%{x|%Y-%m-%d}: %{y}件<extra></extra>"))
    }
    p %>% layout(
      barmode = "stack",
      xaxis = list(title = "検体採取週", showgrid = FALSE, type = "date"),
      yaxis = list(title = "報告数（件）", gridcolor = "#eee"),
      legend = list(orientation = "h", y = -0.25, font = list(size = 9)),
      hovermode = "x unified",
      plot_bgcolor = "#fff", paper_bgcolor = "#fff",
      margin = list(t = 10, b = 40, l = 60, r = 20)
    )
  })

  output$ari_positivity_plot <- renderPlotly({
    if (is.null(ARI_PATHOGEN_DATA) || is.null(ARI_PATHOGEN_DATA$positivity))
      return(plot_ly() %>% add_annotations(text = "データなし", showarrow = FALSE))
    d <- ARI_PATHOGEN_DATA$positivity
    dr <- input$date_range
    if (!is.null(dr) && length(dr) == 2) d <- d %>% filter(date >= dr[1], date <= dr[2])
    if (nrow(d) == 0)
      return(plot_ly() %>% add_annotations(text = "データなし", showarrow = FALSE))
    p <- plot_ly()
    for (cat_id in names(ARI_POS_LEGEND)) {
      dd <- d %>% filter(category == cat_id) %>% arrange(date)
      if (nrow(dd) == 0) next
      p <- p %>% add_trace(data = dd, x = ~date, y = ~positivity, type = "scatter", mode = "lines+markers",
        name = ARI_POS_LABELS[[cat_id]],
        line = list(color = ARI_POS_LEGEND[[cat_id]], width = 2),
        marker = list(color = ARI_POS_LEGEND[[cat_id]], size = 5),
        hovertemplate = paste0(ARI_POS_LABELS[[cat_id]], "　%{x|%Y-%m-%d}: %{y:.1f}%<extra></extra>"))
    }
    p %>% layout(
      xaxis = list(title = "検体採取週", showgrid = FALSE, type = "date"),
      yaxis = list(title = "陽性率（%）", gridcolor = "#eee", rangemode = "nonnegative"),
      legend = list(orientation = "h", y = -0.25),
      hovermode = "x unified",
      plot_bgcolor = "#fff", paper_bgcolor = "#fff",
      margin = list(t = 10, b = 40, l = 60, r = 20)
    )
  })

  output$ari_regional_plot <- renderPlotly({
    if (is.null(ARI_PATHOGEN_DATA) || is.null(ARI_PATHOGEN_DATA$regional))
      return(plot_ly() %>% add_annotations(text = "データなし", showarrow = FALSE))
    reg_id <- if (is.null(input$ari_region)) "national" else input$ari_region
    d <- ARI_PATHOGEN_DATA$regional %>% filter(region == reg_id)
    dr <- input$date_range
    if (!is.null(dr) && length(dr) == 2) d <- d %>% filter(date >= dr[1], date <= dr[2])
    if (nrow(d) == 0)
      return(plot_ly() %>% add_annotations(text = "データなし", showarrow = FALSE))
    p <- plot_ly()
    for (cat_id in ARI_FIG8_ORDER) {
      dd <- d %>% filter(category == cat_id)
      if (nrow(dd) == 0) next
      p <- p %>% add_bars(data = dd, x = ~date, y = ~reports,
        name = ARI_FIG8_LABELS[[cat_id]],
        marker = list(color = ARI_FIG8_LEGEND[[cat_id]]),
        hovertemplate = paste0(ARI_FIG8_LABELS[[cat_id]], "　%{x|%Y-%m-%d}: %{y}件<extra></extra>"))
    }
    p %>% layout(
      barmode = "stack",
      title = list(text = ARI_REGIONS[[reg_id]], x = 0, font = list(size = 13)),
      xaxis = list(title = "検体採取週", showgrid = FALSE, type = "date"),
      yaxis = list(title = "報告数（件）", gridcolor = "#eee"),
      legend = list(orientation = "h", y = -0.25, font = list(size = 9)),
      hovermode = "x unified",
      plot_bgcolor = "#fff", paper_bgcolor = "#fff",
      margin = list(t = 30, b = 40, l = 60, r = 20)
    )
  })

  output$ari_table <- renderDT({
    d <- ari_count_filtered()
    if (is.null(d) || nrow(d) == 0) return(datatable(data.frame()))
    d %>%
      mutate(疾患名 = ARI_COUNT_LABELS[category]) %>%
      select(年 = year, 週 = week, 検体採取週 = date, 病原体 = 疾患名, 報告数 = reports) %>%
      arrange(desc(検体採取週), desc(報告数)) %>%
      datatable(options = list(pageLength = 20, dom = "tip"), rownames = FALSE)
  })

  output$ari_table_dl <- downloadHandler(
    filename = function() paste0("ARI病原体_", Sys.Date(), ".csv"),
    content = function(file) {
      d <- ari_count_filtered()
      if (is.null(d) || nrow(d) == 0) { write.csv(data.frame(), file, row.names = FALSE); return() }
      out <- d %>%
        mutate(疾患名 = ARI_COUNT_LABELS[category]) %>%
        select(年 = year, 週 = week, 検体採取週 = date, 病原体 = 疾患名, 報告数 = reports)
      write_csv_bom(out, file)
    }
  )

  # ── EBS + Google Trends 流行トレンド評価カード ────────────
  output$kpi_ebs_trend <- renderUI({
    is_zensu <- !is.null(input$ts_mode) && input$ts_mode == "zensu"
    did      <- if (is_zensu) input$zensu_disease_ts else input$disease
    pref     <- if (!is.null(input$pref_filter) && input$pref_filter != "全国") input$pref_filter else NULL

    # ── 評価基準日: IBS/Rtと同じくスライダー(date_range)に連動 ──
    ref_date <- tryCatch({
      dd <- if (is_zensu) zensu_ts_filtered() else ts_main_data()
      if (!is.null(dd) && nrow(dd) > 0) max(dd$date, na.rm=TRUE) else Sys.Date()
    }, error=function(e) Sys.Date())
    if (is.na(ref_date) || is.infinite(ref_date)) ref_date <- Sys.Date()
    ebs_evaluable <- as.numeric(Sys.Date() - ref_date) <= EBS_SCORE_EVALUABLE_DAYS

    # ── EBS シグナル評価（基準日の週 vs 前週の重みスコア比較）──
    ebs_calc <- if (!ebs_evaluable) {
      list(score=0, score_prev=0, n=0, n_event=0, n_signal=0)
    } else tryCatch({
      d <- ebs_data()
      weights <- c("Signal High"=3, "Signal Low"=2, "FYI"=0)
      if (!is.null(d) && nrow(d) > 0 && "pub_date" %in% names(d)) {
        d <- d %>% filter(!is.na(pub_date), source_id != "pubmed",
                          has_disease_tag(disease_tags, did))
        if (!is.null(pref)) {
          pref_short <- sub("(都|道|府|県)$", "", pref)
          if ("ebs_pref" %in% names(d)) {
            d <- d %>% filter(
              (!is.na(ebs_pref) & ebs_pref == pref) |
              vapply(paste(coalesce(title,""), coalesce(summary,"")), function(txt)
                grepl(pref, txt, fixed=TRUE) | grepl(pref_short, txt, fixed=TRUE),
                FUN.VALUE = logical(1))
            )
          } else {
            d <- d %>% filter(vapply(paste(coalesce(title,""), coalesce(summary,"")), function(txt)
              grepl(pref, txt, fixed=TRUE) | grepl(pref_short, txt, fixed=TRUE),
              FUN.VALUE = logical(1)))
          }
        }
        d_this   <- d %>% filter(pub_date >= ref_date - 7, pub_date <= ref_date)
        d_prev   <- d %>% filter(pub_date >= ref_date - 14, pub_date < ref_date - 7)
        score_this <- if (nrow(d_this) > 0) sum(weights[as.character(d_this$signal_level)], na.rm=TRUE) else 0
        score_prev <- if (nrow(d_prev) > 0) sum(weights[as.character(d_prev$signal_level)], na.rm=TRUE) else 0
        list(score=score_this, score_prev=score_prev, n=nrow(d_this),
             n_event=sum(as.character(d_this$signal_level)=="Signal High"),
             n_signal=sum(as.character(d_this$signal_level)=="Signal Low"))
      } else list(score=0, score_prev=0, n=0, n_event=0, n_signal=0)
    }, error=function(e) list(score=0, score_prev=0, n=0, n_event=0, n_signal=0))

    ebs_score      <- ebs_calc$score
    ebs_score_prev <- ebs_calc$score_prev
    ebs_n          <- ebs_calc$n
    ebs_change_pct <- if (ebs_score_prev > 0) (ebs_score - ebs_score_prev) / ebs_score_prev * 100
                      else if (ebs_score > 0) 100 else 0

    # ── Google Trends 評価（キャッシュのみ・APIは叩かない）──
    gt_score <- tryCatch({
      geo <- if (!is.null(pref)) {
        g <- PREF_GEO_MAP[pref]; if (!is.na(g)) g else "JP"
      } else "JP"
      cache_f <- gtrends_cache_path(geo)
      if (!file.exists(cache_f)) cache_f <- gtrends_cache_path("JP")
      gt <- if (file.exists(cache_f)) readRDS(cache_f) else NULL
      if (!is.null(gt) && nrow(gt) > 0 && "disease_id" %in% names(gt)) {
        # long形式: date, keyword, disease_id, hits
        gt2 <- gt %>% filter(disease_id == did) %>%
          group_by(date) %>% summarise(hits=mean(hits, na.rm=TRUE), .groups="drop") %>%
          arrange(date) %>% tail(28)
        if (nrow(gt2) >= 14) {
          recent <- mean(tail(gt2$hits, 14), na.rm=TRUE)
          prev   <- mean(head(gt2$hits, 14), na.rm=TRUE)
          if (!is.na(prev) && prev > 0) (recent - prev) / prev * 100 else 0
        } else 0
      } else 0
    }, error=function(e) 0)

    # ── 総合評価 ──
    # EBS前週比: -20%以下=低下, ±20%=横ばい, +20%=上昇, +50%=急上昇
    # 記事なし(score=0)は横ばい扱い
    # 発生動向調査（IDWR）は週1回の公表のため、その報道を伝える記事が公表日周辺に
    # 定例的にまとまって出やすい。記事数が少ない週は、1〜2件の増減だけで前週比が
    # 大きく振れやすく（例: 1件→2件で+100%）、この定例的な増減を実際の流行拡大の
    # シグナルと誤判定しやすいため、直近1週間の記事数(ebs_n)が一定件数未満の場合は
    # 変化率によらず「横ばい」扱いとする
    EBS_TREND_MIN_ARTICLES <- 3
    ebs_level <- if (ebs_score == 0 && ebs_score_prev == 0) 0
                 else if (ebs_n < EBS_TREND_MIN_ARTICLES) 0
                 else if (ebs_change_pct < -20) -1
                 else if (ebs_change_pct < 20)   0
                 else if (ebs_change_pct < 50)   1
                 else                             2
    gt_level  <- if (gt_score < -20) -1 else if (gt_score < 20) 0 else if (gt_score < 50) 1 else 2

    total <- ebs_level + gt_level
    trend_label <- if (total <= -1)     "低下傾向"
                   else if (total == 0) "横ばい"
                   else if (total == 1) "やや上昇"
                   else if (total == 2) "上昇傾向"
                   else                 "急上昇"

    trend_col <- if (total <= -1)     "#27ae60"
                 else if (total == 0) "#7f8c8d"
                 else if (total == 1) "#f39c12"
                 else if (total == 2) "#e67e22"
                 else                 "#e74c3c"

    trend_icon <- if (total <= -1)     "▼"
                  else if (total == 0) "―"
                  else if (total <= 2) "▲"
                  else                 "▲▲"

    ebs_txt <- if (!ebs_evaluable) "EBS: 評価不能（対象期間が直近1年より前）"
               else if (ebs_n == 0) "EBS今週: 記事なし"
               else paste0(
                 sprintf("EBS今週: %d件 (High:%d Low:%d) 前週比%+.0f%%",
                         ebs_n, ebs_calc$n_event, ebs_calc$n_signal, ebs_change_pct),
                 if (ebs_n < EBS_TREND_MIN_ARTICLES) "（少数のため横ばい扱い）" else ""
               )
    gt_txt <- if (gt_score == 0) "Trends: データなし"
              else sprintf("Trends: %+.0f%%", gt_score)

    wk <- tryCatch({
      wy <- as.integer(format(ref_date, "%G")); ww <- as.integer(format(ref_date, "%V"))
      sprintf("(%d年第%d週)", wy, ww)
    }, error=function(e) "")

    tags$div(class="kpi-box", style="text-align:center;",
      tags$div(style="background:#e67e22;color:#fff;font-size:0.65em;font-weight:700;letter-spacing:0.08em;text-align:center;padding:2px 0;margin:-8px -12px 3px -12px;border-radius:4px 4px 0 0;", "EBS"),
      tags$div(class="kpi-value", style=paste0("color:",trend_col,";"),
               if (!ebs_evaluable) "評価不能" else paste0(trend_icon, " ", trend_label)),
      tags$div(class="kpi-label", paste0("EBS/Trends 流行トレンド（直近14日）", wk)),
      tags$div(class="kpi-delta flat",
               paste0(ebs_txt, "　", gt_txt))
    )
  })

  # ── 地図 ───────────────────────────────────────────────

  # 全数把握: 直近週の都道府県別報告数
  zensu_latest_week <- reactive({
    d <- ZENSU_DATA
    if (is.null(d) || nrow(d) == 0) return(NULL)
    d <- d %>% filter(disease == input$zensu_disease_ts)
    if (nrow(d) == 0) return(NULL)
    ly <- max(d$year, na.rm = TRUE)
    lw <- max(d$week[d$year == ly], na.rm = TRUE)
    d %>%
      filter(year == ly, week == lw) %>%
      group_by(pref_name) %>%
      summarise(cases = sum(cases, na.rm = TRUE), .groups = "drop")
  })

  # 全数把握: 期間内の累積報告数（ランキング用）
  zensu_cumulative <- reactive({
    d <- ZENSU_DATA
    if (is.null(d) || nrow(d) == 0) return(NULL)
    dr <- input$date_range
    d %>%
      filter(disease == input$zensu_disease_ts,
             date >= dr[1], date <= dr[2]) %>%
      group_by(pref_name) %>%
      summarise(cumulative = sum(cases, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(cumulative))
  })

  output$ranking_title_ui <- renderUI({
    sel <- map_selected_date()
    is_zensu <- !is.null(input$ts_mode) && input$ts_mode == "zensu"
    is_std   <- !is_zensu && input$disease %in% names(STD_DISEASE_CONFIG)
    if (is_std) {
      month_str <- if (!is.null(sel)) format(sel, "%Y年%m月") else "直近月"
      dconf <- STD_DISEASE_CONFIG[[input$disease]]
      tags$h5(paste0(if (!is.null(dconf)) dconf$label else input$disease, " 報告数ランキング（", month_str, "）"),
              style = "font-weight:700")
    } else {
      week_str <- if (!is.null(sel)) format(sel, "%Y年第%W週") else "直近週"
      if (is_zensu) {
        dconf <- ZENSU_DISEASE_CONFIG[[input$zensu_disease_ts]]
        tags$h5(paste0(dconf$label, " 報告数ランキング（", week_str, "）"),
                style = "font-weight:700")
      } else {
        tags$h5(paste0("都道府県ランキング（", week_str, "）"), style = "font-weight:700")
      }
    }
  })

  # 凡例スケール固定用: 疾患全期間・全都道府県の最大値
  map_scale_max <- reactive({
    is_zensu <- !is.null(input$ts_mode) && input$ts_mode == "zensu"
    is_std   <- !is_zensu && input$disease %in% names(STD_DISEASE_CONFIG)
    if (is_zensu) {
      d <- ZENSU_DATA
      if (is.null(d) || nrow(d) == 0) return(1)
      # 都道府県×週 の症例数の最大値
      mx <- d %>%
        filter(disease == input$zensu_disease_ts) %>%
        group_by(pref_name, date) %>%
        summarise(cases = sum(cases, na.rm = TRUE), .groups = "drop") %>%
        pull(cases) %>% max(na.rm = TRUE)
      if (!is.finite(mx) || mx <= 0) 1 else mx
    } else if (is_std) {
      d <- STD_DATA
      if (is.null(d) || nrow(d) == 0) return(1)
      mx <- d %>%
        filter(disease == input$disease) %>%
        group_by(pref_name, date) %>%
        summarise(reports = sum(reports, na.rm = TRUE), .groups = "drop") %>%
        pull(reports) %>% max(na.rm = TRUE)
      if (!is.finite(mx) || mx <= 0) 1 else mx
    } else {
      mx <- max(filtered_data()$reports_per_site, na.rm = TRUE)
      if (!is.finite(mx) || mx <= 0) 1 else mx
    }
  })

  output$choropleth_map <- renderLeaflet({
    max_val <- map_scale_max()
    if (!is.null(input$ts_mode) && input$ts_mode == "zensu") {
      d <- zensu_map_week_data()
      if (is.null(d) || nrow(d) == 0)
        return(leaflet() %>% addTiles() %>% fitBounds(lng1 = 123, lat1 = 24, lng2 = 146, lat2 = 46))
      pal <- colorNumeric(c("#ffffcc","#fd8d3c","#800026"),
                          c(0, max_val * 1.1), na.color = "#cccccc")
      dconf <- ZENSU_DISEASE_CONFIG[[input$zensu_disease_ts]]
      if (!is.null(JAPAN_MAP)) {
        md <- JAPAN_MAP %>% left_join(d, by = "pref_name")
        leaflet(md) %>% addTiles(options = tileOptions(opacity = 0.4)) %>%
          addPolygons(fillColor = ~pal(cases), fillOpacity = 0.8,
            color = "#fff", weight = 1,
            highlight = highlightOptions(weight = 2, color = "#333", bringToFront = TRUE),
            label = ~paste0(pref_name, ": ",
              ifelse(is.na(cases), "NA", paste0(cases, "件"))),
            labelOptions = labelOptions(style = list("font-size" = "12px"))) %>%
          addLegend(pal = pal, values = c(0, max_val),
                    title = paste0(dconf$label, "<br>報告数（件）"),
                    position = "bottomright", labFormat = labelFormat(digits = 0)) %>%
          fitBounds(lng1 = 123, lat1 = 24, lng2 = 146, lat2 = 46)
      } else {
        leaflet() %>% addTiles() %>% fitBounds(lng1 = 123, lat1 = 24, lng2 = 146, lat2 = 46)
      }
    } else if (input$disease %in% names(STD_DISEASE_CONFIG)) {
      d <- std_map_week_data()
      if (is.null(d) || nrow(d) == 0)
        return(leaflet() %>% addTiles() %>% fitBounds(lng1 = 123, lat1 = 24, lng2 = 146, lat2 = 46))
      pal <- colorNumeric(c("#ffffcc","#fd8d3c","#800026"),
                          c(0, max_val * 1.1), na.color = "#cccccc")
      dconf <- STD_DISEASE_CONFIG[[input$disease]]
      if (!is.null(JAPAN_MAP)) {
        md <- JAPAN_MAP %>% left_join(d, by = "pref_name")
        leaflet(md) %>% addTiles(options = tileOptions(opacity = 0.4)) %>%
          addPolygons(fillColor = ~pal(reports), fillOpacity = 0.8,
            color = "#fff", weight = 1,
            highlight = highlightOptions(weight = 2, color = "#333", bringToFront = TRUE),
            label = ~paste0(pref_name, ": ",
              ifelse(is.na(reports), "NA", paste0(reports, "件"))),
            labelOptions = labelOptions(style = list("font-size" = "12px"))) %>%
          addLegend(pal = pal, values = c(0, max_val),
                    title = paste0(if (!is.null(dconf)) dconf$label else input$disease, "<br>報告数（件/月）"),
                    position = "bottomright", labFormat = labelFormat(digits = 0)) %>%
          fitBounds(lng1 = 123, lat1 = 24, lng2 = 146, lat2 = 46)
      } else {
        leaflet() %>% addTiles() %>% fitBounds(lng1 = 123, lat1 = 24, lng2 = 146, lat2 = 46)
      }
    } else {
      d <- map_week_data()
      pal <- colorNumeric(c("#ffffcc","#fd8d3c","#800026"),
                          c(0, max_val * 1.1), na.color="#cccccc")
      if(!is.null(JAPAN_MAP)){
        md <- JAPAN_MAP %>% left_join(d %>% select(pref_name,reports_per_site),by="pref_name")
        leaflet(md) %>% addTiles(options=tileOptions(opacity=0.4)) %>%
          addPolygons(fillColor=~pal(reports_per_site),fillOpacity=0.8,
            color="#fff",weight=1,
            highlight=highlightOptions(weight=2,color="#333",bringToFront=TRUE),
            label=~paste0(pref_name,": ",
              ifelse(is.na(reports_per_site),"NA",sprintf("%.2f",reports_per_site))," 報告/定点"),
            labelOptions=labelOptions(style=list("font-size"="12px"))) %>%
          addLegend(pal=pal, values=c(0, max_val), title="定点あたり<br>報告数",
                    position="bottomright", labFormat=labelFormat(digits=1)) %>%
          fitBounds(lng1=123,lat1=24,lng2=146,lat2=46)
      } else {
        leaflet() %>% addTiles() %>% fitBounds(lng1=123,lat1=24,lng2=146,lat2=46)
      }
    }
  })

  output$ranking_table <- renderDT({
    if (!is.null(input$ts_mode) && input$ts_mode == "zensu") {
      d <- zensu_map_week_data()
      if (is.null(d) || nrow(d) == 0)
        return(datatable(data.frame(都道府県=character(), 週次報告数=integer()),
                         options=list(dom="t"), rownames=FALSE))
      d %>%
        arrange(desc(cases)) %>%
        mutate(順位 = min_rank(desc(cases))) %>%
        select(順位, 都道府県=pref_name, 週次報告数=cases) %>%
        datatable(options=list(pageLength=10, dom="tp"), rownames=FALSE)
    } else if (input$disease %in% names(STD_DISEASE_CONFIG)) {
      d <- std_map_week_data()
      if (is.null(d) || nrow(d) == 0)
        return(datatable(data.frame(都道府県=character(), 月次報告数=integer()),
                         options=list(dom="t"), rownames=FALSE))
      d %>%
        arrange(desc(reports)) %>%
        mutate(順位 = min_rank(desc(reports))) %>%
        select(順位, 都道府県=pref_name, 月次報告数=reports) %>%
        datatable(options=list(pageLength=10, dom="tp"), rownames=FALSE)
    } else {
      map_week_data() %>%
        arrange(desc(reports_per_site)) %>%
        mutate(順位=min_rank(desc(reports_per_site)), `定点あたり`=round(reports_per_site,2)) %>%
        select(順位,都道府県=pref_name,`定点あたり`) %>%
        datatable(options=list(pageLength=10,dom="tp"),rownames=FALSE)
    }
  })

  # ── 流行曲線 ─────────────────────────────────────────────
  output$timeseries_plot <- renderPlotly({
    is_pref <- !is.null(input$pref_filter) && input$pref_filter != "全国"
    nat    <- ts_main_data()
    col    <- DISEASE_CONFIG[[input$disease]]$color
    lbl    <- DISEASE_CONFIG[[input$disease]]$label
    thresh <- DISEASE_CONFIG[[input$disease]]$alert_threshold
    main_label <- if (is_pref) input$pref_filter else "全国"
    # 各時点から過去5年間の同一週番号データで平均±2SD を計算（都道府県選択時はその都道府県のデータで算出）
    # ts_band_series() は date_range に依存しない全期間データ（ts_hist_data）を参照するため、
    # date_range をずらしても帯が短くならない
    nat2 <- ts_band_series() %>%
      mutate(ymin = ifelse(has_hist, pmax(0, mu - 2*s), NA_real_),
             ymax = ifelse(has_hist, mu + 2*s, NA_real_))
    p <- plot_ly() %>%
      add_ribbons(data=nat2 %>% filter(has_hist), x=~date,
        ymin=~ymin, ymax=~ymax,
        fillcolor=paste0(col,"33"), line=list(color="transparent"),
        name=paste0(main_label,"　過去5年平均±2SD"), hoverinfo="skip") %>%
      add_lines(data=nat, x=~date, y=~reports_per_site,
        line=list(color=col, width=2.5), name=main_label,
        hovertemplate=paste0(main_label, "　%{x|%Y-W%W}: %{y:.2f}<extra></extra>"))
    # +2SD 超過点を赤丸でプロット
    exceed <- nat2 %>% filter(has_hist, reports_per_site > ymax)
    if (nrow(exceed) > 0) {
      p <- p %>% add_markers(data=exceed, x=~date, y=~reports_per_site,
        marker=list(color="#e74c3c", size=6, symbol="circle"),
        name="+2SD超過", hovertemplate="%{x|%Y-W%W}: %{y:.2f}<extra></extra>")
    }
    # ── 短期予測（4週先まで）─────────────────────────────
    # 学習・起点にはスライダー(date_range)に依存しない全期間データ(ts_hist_data)を使う
    # （季節成分の学習に十分な長さのデータが必要、かつ直近の実測にスライダーの影響を受けない）
    if (isTRUE(input$show_forecast)) {
      hist_d <- ts_hist_data() %>% filter(!is.na(reports_per_site)) %>% arrange(date)
      if (nrow(hist_d) >= 4) {
      fc_method <- if (is.null(input$forecast_method)) "ensemble" else input$forecast_method
      si <- SERIAL_INTERVALS[[input$disease]]
      seasonal <- tryCatch(detect_seasonality(hist_d, value_col="reports_per_site"), error=function(e) FALSE)
      rt_val <- tryCatch({
        rd <- rt_series() %>% filter(!is.na(rt)) %>% slice_tail(n=1)
        if (nrow(rd)==0) NA_real_ else rd$rt[1]
      }, error=function(e) NA_real_)
      fc <- tryCatch(
        compute_forecast(hist_d$date, hist_d$reports_per_site, fc_method,
                          horizon=4, rt_value=rt_val,
                          si_mean=if (!is.null(si)) si$mean else NA_real_,
                          si_sd=if (!is.null(si)) si$sd else NA_real_,
                          seasonal=seasonal),
        error=function(e) NULL)
      if (!is.null(fc) && nrow(fc) > 0) {
        last_pt <- tail(hist_d, 1)
        fc_line <- rbind(
          data.frame(date=last_pt$date[1], value=last_pt$reports_per_site[1],
                     lower=last_pt$reports_per_site[1], upper=last_pt$reports_per_site[1]),
          fc[, c("date","value","lower","upper")]
        )
        p <- p %>%
          add_ribbons(data=fc_line, x=~date, ymin=~pmax(0,lower), ymax=~upper,
            fillcolor="rgba(155,89,182,0.18)", line=list(color="transparent"),
            name="予測区間（目安）", hoverinfo="skip") %>%
          add_lines(data=fc_line, x=~date, y=~value,
            line=list(color="#8e44ad", width=2, dash="dash"),
            name=paste0("予測（", FORECAST_METHOD_LABELS[[fc_method]],
                        if (seasonal) "・季節性反映" else "", "）"),
            hovertemplate="%{x|%Y-%m-%d}　予測 %{y:.2f}<extra></extra>")
      }
      }
    }
    keiho_start <- alert_threshold_keiho_start(thresh)
    if (!is.null(keiho_start)) {
      p <- p %>% add_lines(x=range(nat$date), y=c(keiho_start,keiho_start),
        line=list(color="#e74c3c", dash="dash", width=1.5),
        name=paste0("警報開始基準値(",keiho_start,")"), hoverinfo="skip")
    }
    chuiho_start <- if (is.list(thresh)) thresh$chuiho_start else NULL
    if (!is.null(chuiho_start)) {
      p <- p %>% add_lines(x=range(nat$date), y=c(chuiho_start,chuiho_start),
        line=list(color="#f39c12", dash="dot", width=1.5),
        name=paste0("注意報開始基準値(",chuiho_start,")"), hoverinfo="skip")
    }
    if (is_pref) {
      nd <- national_avg()
      p <- p %>% add_lines(data=nd,x=~date,y=~reports_per_site,
        line=list(color="#2c3e50",dash="dot",width=2),name="全国平均（参考）",
        hovertemplate="全国平均（参考）　%{x|%Y-W%W}: %{y:.2f}<extra></extra>")
    }
    p %>% layout(xaxis=list(title="",showgrid=FALSE),
      yaxis=list(title="定点あたり報告数",gridcolor="#eee"),
      legend=list(orientation="h",y=-0.15),hovermode="x unified",
      plot_bgcolor="#fff",paper_bgcolor="#fff",margin=list(t=20))
  })
  output$timeseries_legend <- renderUI({
    col <- DISEASE_CONFIG[[input$disease]]$color
    thresh <- DISEASE_CONFIG[[input$disease]]$alert_threshold
    is_pref <- !is.null(input$pref_filter) && input$pref_filter != "全国"
    band_scope <- if (is_pref) paste0(input$pref_filter, "の過去データ") else "全国の過去データ"
    tags$div(style="font-size:0.75em;color:#666;margin-top:2px;padding-left:4px;display:flex;gap:16px;align-items:center;flex-wrap:wrap;",
      tags$span(
        tags$span(style=paste0("display:inline-block;width:14px;height:10px;background:",
          col,"33",";border:1px solid ",col,";margin-right:4px;vertical-align:middle;")),
        paste0("帯: ", band_scope, "（直前5年間・前後2週移動平均±2SD）")
      ),
      tags$span(
        tags$span(style="display:inline-block;width:8px;height:8px;background:#e74c3c;border-radius:50%;margin-right:4px;vertical-align:middle;"),
        "+2SD超過"
      ),
      if (!is.null(thresh)) tags$span(
        tags$span(style="display:inline-block;width:20px;border-top:2px dashed #e74c3c;margin-right:4px;vertical-align:middle;"),
        if (is.list(thresh))
          paste0("赤破線: 警報開始基準値",
                 if (!is.null(thresh$chuiho_start)) "／オレンジ点線: 注意報開始基準値" else "",
                 "（", format_alert_threshold(thresh), " 報告/定点、研究班報告書ベース）")
        else
          paste0("赤破線: 参考基準値（", thresh, " 報告/定点、文献等で提唱された参考基準）")
      ),
      if (is_pref) tags$span(
        tags$span(style="display:inline-block;width:20px;border-top:2px dotted #2c3e50;margin-right:4px;vertical-align:middle;"),
        "点線: 全国平均（参考）"
      )
    )
  })
  output$heatmap_plot <- renderPlotly({
    cutoff1y <- Sys.Date() - 365
    d <- filtered_data() %>% filter(date >= cutoff1y) %>%
      group_by(pref_code, pref_name, date, week, year) %>%
      summarise(reports_per_site=mean(reports_per_site,na.rm=TRUE),.groups="drop") %>%
      mutate(week_label = paste0(year, "-W", sprintf("%02d", week)))
    pord <- d %>% distinct(pref_code,pref_name) %>% arrange(pref_code) %>% pull(pref_name)
    word <- d %>% distinct(week_label, date) %>% arrange(date) %>% pull(week_label)
    d <- d %>% mutate(pref_name=factor(pref_name,levels=rev(pord)),
                      week_label=factor(week_label,levels=word))
    plot_ly(data=d, x=~week_label, y=~pref_name, z=~reports_per_site, type="heatmap",
      colorscale=list(c(0,"#ffffcc"),c(0.5,"#fd8d3c"),c(1,"#800026")),
      hovertemplate="%{y} %{x}: %{z:.2f}<extra></extra>",
      colorbar=list(title="報告/定点")) %>%
      layout(xaxis=list(title="", tickangle=-45, dtick=4),
             yaxis=list(title="",tickfont=list(size=8)),margin=list(l=85,t=20,b=60))
  })
  output$region_plot <- renderPlotly({
    d <- filtered_data() %>%
      group_by(region,date) %>%
      summarise(reports_per_site=mean(reports_per_site,na.rm=TRUE),.groups="drop")
    regions <- unique(d$region)
    cols <- colorRampPalette(brewer.pal(min(8,length(regions)),"Set2"))(length(regions))
    p <- plot_ly()
    for(i in seq_along(regions)){
      rd <- d %>% filter(region==regions[i])
      p <- p %>% add_lines(data=rd,x=~date,y=~reports_per_site,
        name=regions[i],line=list(color=cols[i],width=2))
    }
    p %>% layout(xaxis=list(title="",showgrid=FALSE),
      yaxis=list(title="地域平均 報告/定点",gridcolor="#eee"),
      legend=list(orientation="h",y=-0.2,font=list(size=10)),
      hovermode="x unified",plot_bgcolor="#fff",paper_bgcolor="#fff",margin=list(t=20))
  })

  # ── 複数疾患比較 ────────────────────────────────────────
  # 都道府県選択時: 当該都道府県の値を使用。「全国」選択時は従来通り
  # 全国公式値（SURV_DATA_NATIONAL）を優先し、欠測週のみ都道府県平均で補完する
  multi_data <- reactive({
    req(length(input$multi_diseases)>0)
    dr <- input$date_range
    pref <- input$pref_filter

    avg_d <- SURV_DATA %>%
      filter(disease %in% input$multi_diseases,
             date >= dr[1], date <= dr[2])

    if (!is.null(pref) && pref != "全国") {
      avg_d <- avg_d %>%
        filter(pref_name == pref) %>%
        group_by(disease,date,year,week) %>%
        summarise(reports_per_site=mean(reports_per_site,na.rm=TRUE),.groups="drop")
    } else {
      avg_d <- avg_d %>%
        group_by(disease,date,year,week) %>%
        summarise(avg_val=mean(reports_per_site,na.rm=TRUE),.groups="drop")
      off_d <- if (is.null(SURV_DATA_NATIONAL) || nrow(SURV_DATA_NATIONAL) == 0) {
        tibble(disease=character(), date=as.Date(character()), official_val=numeric())
      } else {
        SURV_DATA_NATIONAL %>%
          filter(disease %in% input$multi_diseases, date >= dr[1], date <= dr[2]) %>%
          select(disease, date, official_val = reports_per_site)
      }
      avg_d <- avg_d %>%
        left_join(off_d, by=c("disease","date")) %>%
        mutate(reports_per_site = dplyr::coalesce(official_val, avg_val)) %>%
        select(disease, date, year, week, reports_per_site)
    }

    avg_d %>%
      mutate(disease_label=DISEASE_CONFIG[disease] %>%
               sapply(function(x) x$label))
  })

  output$multi_disease_plot <- renderPlotly({
    d <- multi_data()
    req(nrow(d)>0)
    view <- input$multi_view
    diseases <- unique(d$disease)
    cols <- setNames(sapply(diseases,function(x) DISEASE_CONFIG[[x]]$color), diseases)

    if(view=="overlay_raw"){
      p <- plot_ly()
      for(dis in diseases){
        dd <- d %>% filter(disease==dis)
        p <- p %>% add_lines(data=dd,x=~date,y=~reports_per_site,
          name=DISEASE_CONFIG[[dis]]$label,
          line=list(color=cols[dis],width=2))
      }
      p %>% layout(yaxis=list(title="定点あたり報告数"),
        xaxis=list(title="",showgrid=FALSE),
        hovermode="x unified",legend=list(orientation="h",y=-0.15),
        plot_bgcolor="#fff",paper_bgcolor="#fff",margin=list(t=20))

    } else {
      # 2×2 ファセット（subplot）
      plots <- lapply(diseases, function(dis){
        dd <- d %>% filter(disease==dis)
        thresh <- DISEASE_CONFIG[[dis]]$alert_threshold
        keiho_start <- alert_threshold_keiho_start(thresh)
        col <- cols[dis]
        pp <- plot_ly(data=dd,x=~date,y=~reports_per_site,type="scatter",mode="lines",
          name=DISEASE_CONFIG[[dis]]$label,
          line=list(color=col,width=2),showlegend=FALSE)
        if (!is.null(keiho_start)) {
          pp <- pp %>% add_lines(x=range(dd$date),y=c(keiho_start,keiho_start),
            line=list(color="#e74c3c",dash="dash",width=1),
            showlegend=FALSE,hoverinfo="skip")
        }
        pp %>%
          layout(annotations=list(list(
            text=DISEASE_CONFIG[[dis]]$label,
            x=0.5,y=1.05,xref="paper",yref="paper",
            showarrow=FALSE,font=list(size=12,color=col)
          )))
      })
      nrow_sub <- if(length(plots)<=2) 1 else 2
      do.call(subplot, c(plots, list(nrows=nrow_sub, shareX=TRUE,
        titleX=FALSE, titleY=FALSE, margin=0.06))) %>%
        layout(plot_bgcolor="#fff",paper_bgcolor="#fff")
    }
  })

  output$multi_summary_table <- renderDT({
    off_cur <- if (is.null(SURV_DATA_NATIONAL) || nrow(SURV_DATA_NATIONAL) == 0) {
      tibble(disease=character(), official_val=numeric())
    } else {
      SURV_DATA_NATIONAL %>%
        filter(disease %in% input$multi_diseases, year==CURRENT_YEAR, week==CURRENT_WEEK) %>%
        select(disease, official_val = reports_per_site)
    }
    SURV_DATA %>%
      filter(disease %in% input$multi_diseases) %>%
      filter(year==CURRENT_YEAR, week==CURRENT_WEEK) %>%
      group_by(disease) %>%
      summarise(
        国平均=mean(reports_per_site,na.rm=TRUE),
        最大値=round(max(reports_per_site,na.rm=TRUE),2),
        最多都道府県=pref_name[which.max(reports_per_site)],
        .groups="drop"
      ) %>%
      left_join(off_cur, by="disease") %>%
      mutate(国平均=round(dplyr::coalesce(official_val, 国平均), 2)) %>%
      select(-official_val) %>%
      mutate(
        疾患=sapply(disease, function(x) DISEASE_CONFIG[[x]]$label),
        Rt=sapply(disease, function(d) {
          if (!(d %in% names(SERIAL_INTERVALS))) return(NA_real_)
          dd <- SURV_DATA %>% filter(disease==d)
          rd <- tryCatch(compute_rt_series(dd, d, NULL), error=function(e) NULL)
          if (is.null(rd)) return(NA_real_)
          rd <- rd %>% filter(!is.na(rt)) %>% slice_tail(n=1)
          if (nrow(rd)==0) NA_real_ else round(rd$rt[1], 2)
        }),
        IBS=sapply(disease, function(d) {
          hist_d <- SURV_DATA %>% filter(disease==d, region %in% unique(PREF_MASTER$region)) %>%
            group_by(date, year, week) %>%
            summarise(reports_per_site=mean(reports_per_site, na.rm=TRUE), .groups="drop")
          cur <- hist_d %>% filter(year==CURRENT_YEAR, week==CURRENT_WEEK)
          if (nrow(cur)==0) return(NA_real_)
          bd <- tryCatch(compute_ibs_band(cur %>% slice_tail(n=1), hist_d), error=function(e) NULL)
          if (is.null(bd) || is.na(bd$ibs_score[1])) NA_real_ else bd$ibs_score[1]
        }),
        警戒レベル=mapply(classify_alert, 国平均, disease, Rt, IBS)
      ) %>%
      select(疾患,全国平均=国平均,最大値,最多都道府県,Rt,IBS,警戒レベル) %>%
      datatable(options=list(dom="t",pageLength=10),rownames=FALSE) %>%
      formatStyle("警戒レベル",backgroundColor=styleEqual(
        c("警戒（レベル3）","注意（レベル2）","流行期（レベル1）","基準以下"),
        c("#fadbd8","#fdebd0","#fef9e7","#d5f5e3")))
  })


  # ── Rt グラフ共通描画ヘルパー ────────────────────────────
  make_rt_plot <- function(rd, label, color) {
    rd <- rd %>% filter(!is.na(rt))
    if (nrow(rd) == 0) return(plot_ly() %>%
      add_annotations(text="データ不足", showarrow=FALSE))
    plot_ly(data=rd, x=~date) %>%
      add_ribbons(ymin=~rt_lower, ymax=~rt_upper,
        fillcolor=paste0(color,"33"), line=list(color="transparent"),
        name="95% CI", hoverinfo="skip") %>%
      add_lines(y=~rt, line=list(color=color, width=2.5), name=label,
        hovertemplate="%{x|%Y-%m-%d}　Rt = %{y:.2f}<extra></extra>") %>%
      add_lines(x=range(rd$date), y=c(1,1),
        line=list(color="#e74c3c", dash="dash", width=1.5),
        name="Rt=1", hoverinfo="skip") %>%
      layout(
        xaxis=list(title="", showgrid=FALSE),
        yaxis=list(title="実効再生産数 Rt", gridcolor="#eee",
          range=c(0, min(max(rd$rt_upper, na.rm=TRUE)*1.1, 10))),
        legend=list(orientation="h", y=-0.12),
        hovermode="x unified",
        plot_bgcolor="#fff", paper_bgcolor="#fff",
        margin=list(t=20, b=40, l=55, r=20))
  }

  # ── Rt: 定点 / 全数 切り替え ─────────────────────────────
  output$rt_plot_single <- renderPlotly({
    dr <- input$date_range
    if (input$ts_mode == "zensu") {
      did   <- req(input$rt_zensu_disease)
      si    <- SERIAL_INTERVALS[[did]]
      if (is.null(si)) return(plot_ly() %>%
        add_annotations(text="シリアルインターバル未定義", showarrow=FALSE))
      d <- ZENSU_DATA %>% filter(disease == did, date >= dr[1], date <= dr[2])
      if (is.null(d) || nrow(d) < 15) return(plot_ly() %>%
        add_annotations(text="データ不足（15週以上必要）", showarrow=FALSE))
      rd <- tryCatch(
        compute_rt_series_zensu(d, did, input$pref_filter),
        error=function(e) { message("Rt(zensu) ERROR: ", e$message); NULL }
      )
      if (is.null(rd) || nrow(rd)==0) return(plot_ly() %>%
        add_annotations(text="推定不可（データ不足）", showarrow=FALSE))
      make_rt_plot(rd, ZENSU_DISEASE_CONFIG[[did]]$label,
                   ZENSU_DISEASE_CONFIG[[did]]$color)
    } else {
      did <- req(input$rt_disease)
      si  <- SERIAL_INTERVALS[[did]]
      if (is.null(si)) return(plot_ly() %>%
        add_annotations(text="シリアルインターバル未定義", showarrow=FALSE))
      all_d <- SURV_DATA %>% filter(disease==did, date >= dr[1], date <= dr[2])
      if (nrow(all_d) < 15) return(plot_ly() %>%
        add_annotations(text="データ不足（15週以上必要）", showarrow=FALSE))
      rd <- tryCatch(
        compute_rt_series(all_d, did, input$pref_filter),
        error=function(e) { message("Rt ERROR: ", e$message); NULL }
      )
      if (is.null(rd) || nrow(rd)==0) return(plot_ly() %>%
        add_annotations(text="推定不可", showarrow=FALSE))
      make_rt_plot(rd, DISEASE_CONFIG[[did]]$label, DISEASE_CONFIG[[did]]$color)
    }
  })

  output$rt_si_info <- renderUI({
    did <- if (input$ts_mode == "zensu") input$rt_zensu_disease else input$rt_disease
    si  <- SERIAL_INTERVALS[[did]]
    if (is.null(si)) return(NULL)
    pub_label <- if (isTRUE(si$published)) "（文献値）" else if (isTRUE(si$derived_from == "incubation_period")) "（潜伏期間より推定）" else "（推定値）"
    tags$div(style="font-size:0.82em;color:#555;padding:6px 12px;border-top:1px solid #eee;margin-top:4px;",
      tags$b(paste0("シリアルインターバル仮定", pub_label, ": ")),
      sprintf("平均 %.1f 日、SD %.1f 日", si$mean, si$sd),
      tags$br(),
      tags$b("出典: "), si$source
    )
  })

  # ── 全数把握 類型フィルタ → disease選択肢を動的更新 ───────
  observeEvent(input$zensu_class, {
    cls <- input$zensu_class
    if (is.null(cls) || cls == "全て") {
      classes     <- c("1類","2類","3類","4類","5類全数")
      class_labels <- c("1類感染症","2類感染症","3類感染症","4類感染症","5類感染症（全数）")
      grp <- setNames(vector("list", length(classes)), class_labels)
      for (i in seq_along(classes)) {
        ids <- names(Filter(function(x) x$class == classes[i], ZENSU_DISEASE_CONFIG))
        grp[[class_labels[i]]] <- setNames(ids, sapply(ids, function(x) ZENSU_DISEASE_CONFIG[[x]]$label))
      }
      updateSelectInput(session, "zensu_disease_ts", choices = grp)
    } else {
      # "5類" → "5類全数" にマッチ
      pattern <- if (cls == "5類") "5類全数" else cls
      ids <- names(Filter(function(x) x$class == pattern, ZENSU_DISEASE_CONFIG))
      ch  <- setNames(ids, sapply(ids, function(x) ZENSU_DISEASE_CONFIG[[x]]$label))
      updateSelectInput(session, "zensu_disease_ts", choices = ch)
    }
  }, ignoreInit = TRUE)

  # ── サイドバー疾患ID取得ヘルパー ──────────────────────────
  sidebar_disease_id <- reactive({
    if (!is.null(input$ts_mode) && input$ts_mode == "zensu") input$zensu_disease_ts else input$disease
  })

  # すべて表示フラグ（ボタンで切替、疾患変更でリセット）
  ebs_show_all_flag    <- reactiveVal(FALSE)
  ebs_ov_show_all_flag <- reactiveVal(FALSE)
  pubmed_show_all_flag <- reactiveVal(FALSE)

  observeEvent(input$ebs_show_all,    { ebs_show_all_flag(TRUE) })
  observeEvent(input$ebs_ov_show_all, { ebs_ov_show_all_flag(TRUE) })
  observeEvent(input$pubmed_show_all, { pubmed_show_all_flag(TRUE) })
  observeEvent(sidebar_disease_id(),  {
    ebs_show_all_flag(FALSE)
    ebs_ov_show_all_flag(FALSE)
    pubmed_show_all_flag(FALSE)
  }, ignoreInit = TRUE)

  # ── EBS フィード（国内）────────────────────────────────────
  filtered_ebs <- reactive({
    d <- ebs_data()
    d <- d %>% filter(!mapply(is_overseas_article, title, summary,
                              if ("ebs_pref" %in% names(.)) ebs_pref else NA,
                              if ("source_id" %in% names(.)) source_id else "",
                              if ("source_name" %in% names(.)) source_name else ""))
    d <- d %>% filter(is.na(source_id) | source_id != "pubmed")
    if (isTRUE(input$ebs_official_only)) {
      d <- d %>% filter(is_official_ebs_source(source_id))
    }
    if (!ebs_show_all_flag()) {
      did <- sidebar_disease_id()
      if (!is.null(did) && did != "すべて")
        d <- d %>% filter(has_disease_tag(disease_tags, did))
    }
    pf <- input$pref_filter
    if (!is.null(pf) && pf != "全国") {
      pref_short <- sub("(都|道|府|県)$", "", pf)
      if ("ebs_pref" %in% names(d)) {
        d <- d %>% filter(
          (!is.na(ebs_pref) & ebs_pref == pf) |
          vapply(paste(coalesce(title,""), coalesce(summary,"")), function(txt)
            grepl(pf, txt, fixed = TRUE) | grepl(pref_short, txt, fixed = TRUE),
            FUN.VALUE = logical(1))
        )
      } else {
        d <- d %>% filter(
          vapply(paste(coalesce(title,""), coalesce(summary,"")), function(txt)
            grepl(pf, txt, fixed = TRUE) | grepl(pref_short, txt, fixed = TRUE),
            FUN.VALUE = logical(1))
        )
      }
    }
    if (!is.null(input$ebs_sort) && input$ebs_sort == "date") {
      d %>% arrange(desc(pub_date))
    } else {
      d %>% arrange(signal_level, desc(pub_date))
    }
  })
  output$ebs_last_updated <- renderUI({ NULL })

  # ── 文献（PubMed）─────────────────────────────────────────
  pubmed_filtered <- reactive({
    d <- ebs_data()
    if (is.null(d) || nrow(d) == 0) return(data.frame())
    d <- d %>% filter(!is.na(source_id) & source_id == "pubmed")
    if (!pubmed_show_all_flag()) {
      did <- sidebar_disease_id()
      if (!is.null(did) && did != "すべて")
        d <- d %>% filter(has_disease_tag(disease_tags, did))
    }
    d %>% arrange(signal_level, desc(pub_date))
  })

  output$pubmed_news_feed <- renderUI({
    d <- pubmed_filtered()
    if (nrow(d) == 0) {
      return(tags$div(class="demo-banner", "該当なし。サイドバーの疾患を変更するか「すべて表示」を押してください。"))
    }
    PAGE_SIZE <- as.integer(input$pubmed_page_size)
    total <- nrow(d)
    d <- head(d, PAGE_SIZE)
    dlabel <- EBS_DLABEL
    translate_mode_pm <- isTRUE(input$pubmed_translate == "on")

    cards <- lapply(seq_len(nrow(d)), function(i) .ebs_row_to_card(d[i, ], dlabel, translate_mode_pm))
    meta <- list(total = total, pageSize = PAGE_SIZE)

    tags$div(
      tags$div(id = "pubmed-cards-root"),
      tags$script(HTML(paste0(
        "renderEbsCards('pubmed-cards-root', ",
        jsonlite::toJSON(cards, auto_unbox = TRUE, null = "null"),
        ", ",
        jsonlite::toJSON(meta, auto_unbox = TRUE),
        ");"
      )))
    )
  })

  observeEvent(input$pubmed_translate, {
    if (isTRUE(input$pubmed_translate == "on"))
      shinyjs::runjs("setTimeout(function(){ ebsTranslateCards('pubmed_news_feed'); }, 500);")
    else
      shinyjs::runjs("ebsUntranslateCards('pubmed_news_feed');")
  }, ignoreInit = TRUE)

  # 翻訳トグル：カード内テキストをJS経由でGoogle翻訳
  observeEvent(input$ebs_translate, {
    if (isTRUE(input$ebs_translate == "on"))
      shinyjs::runjs("setTimeout(function(){ ebsTranslateCards('ebs_news_feed'); }, 500);")
    else
      shinyjs::runjs("ebsUntranslateCards('ebs_news_feed');")
  }, ignoreInit = TRUE)

  observeEvent(input$ebs_ov_translate, {
    if (isTRUE(input$ebs_ov_translate == "on"))
      shinyjs::runjs("setTimeout(function(){ ebsTranslateCards('ebs_ov_news_feed'); }, 500);")
    else
      shinyjs::runjs("ebsUntranslateCards('ebs_ov_news_feed');")
  }, ignoreInit = TRUE)
  output$ebs_signal_summary <- renderUI({
    d <- filtered_ebs()
    cnt <- d %>% count(signal_level) %>% mutate(l=as.character(signal_level))
    gn <- function(lv){ v <- cnt %>% filter(l==lv) %>% pull(n); if(length(v)==0) 0L else v }
    n_event  <- gn("Signal High")
    n_signal <- gn("Signal Low")
    n_fyi    <- gn("FYI")
    n_total  <- nrow(d)
    filter_label <- if (!is.null(input$ebs_disease_filter) && input$ebs_disease_filter != "すべて") {
      disease_names <- c(flu="インフルエンザ", covid="COVID-19", rsv="RSウイルス",
                         ari="ARI", mpox="エムポックス", measles="麻疹",
                         dengue="デング熱", ebola="エボラ", general="感染症全般")
      paste0("（", coalesce(disease_names[input$ebs_disease_filter], input$ebs_disease_filter), " フィルタ中）")
    } else ""
    period_txt <- {
      pd <- d$pub_date[!is.na(d$pub_date)]
      if (length(pd) > 0) sprintf("表示記事の期間: %s 〜 %s（%d件）", format(min(pd), "%Y/%m/%d"), format(max(pd), "%Y/%m/%d"), length(pd))
      else "表示記事の期間: -"
    }
    tags$div(
      tags$div(style="font-size:0.78em;color:#888;margin-bottom:6px;",
               paste0("EBS サマリー", filter_label)),
      tags$div(style="font-size:0.75em;color:#999;margin-bottom:6px;",
               period_txt, "　※古い記事はリンク切れの場合があります",
               "　※シグナル判定は開発中のアルゴリズムのため精度が変動する場合があります"),
      tags$div(style="display:flex;gap:12px;flex-wrap:wrap;margin-bottom:8px;",
        tags$div(class="kpi-box", style="flex:1;min-width:90px;border-left:4px solid #e74c3c;",
          tags$div(class="kpi-value", style="color:#e74c3c;font-size:1.8em;", n_event),
          tags$div(class="kpi-label", "Signal High")),
        tags$div(class="kpi-box", style="flex:1;min-width:90px;border-left:4px solid #e67e22;",
          tags$div(class="kpi-value", style="color:#e67e22;font-size:1.8em;", n_signal),
          tags$div(class="kpi-label", "Signal Low")),
        tags$div(class="kpi-box", style="flex:1;min-width:90px;border-left:4px solid #95a5a6;",
          tags$div(class="kpi-value", style="color:#95a5a6;font-size:1.8em;", n_fyi),
          tags$div(class="kpi-label", "FYI")),
        tags$div(class="kpi-box", style="flex:1;min-width:90px;border-left:4px solid #27ae60;",
          tags$div(class="kpi-value", style="color:#27ae60;font-size:1.8em;", n_total),
          tags$div(class="kpi-label", "総数"))
      ),
      NULL
    )
  })
  # EBSカード1行分をJSレンダラー(www/js/ebs_cards.js)向けのプレーンなリストに変換する。
  # 静的サイト化の第一段階として、DOM構築（tags$div/tags$aのツリー生成）をサーバー側の
  # R処理からクライアント側JSへ移す。ここではフィルタ済みデータをJSON化するだけに留める
  .ebs_row_to_card <- function(row, dlabel, translate_mode) {
    link <- row$link
    if (!is.na(link) && nchar(trimws(link)) > 0 && translate_mode) {
      link <- paste0("https://translate.google.com/translate?hl=ja&sl=auto&tl=ja&u=",
                      utils::URLencode(link, repeated = TRUE))
    }
    tgs <- strsplit(coalesce(row$disease_tags, ""), ",")[[1]]
    tgs <- tgs[nchar(trimws(tgs)) > 0]
    disease_labels <- vapply(tgs, function(tg) if (!is.na(dlabel[tg])) dlabel[tg] else tg,
                              character(1), USE.NAMES = FALSE)

    has_screen <- "ebs_unusual" %in% names(row)
    criteria_labels <- character(0)
    if (has_screen) {
      cols_map <- c(ebs_unusual="Unusual", ebs_serious_c="Serious (country)",
                    ebs_serious_j="Serious (Japan)", ebs_epidemic="Epidemic-prone",
                    ebs_mass="Mass exposure", ebs_high="High profile",
                    ebs_special="Special pathogen")
      for (col in names(cols_map)) {
        val <- if (col %in% names(row)) row[[col]] else ""
        if (!is.na(val) && val == "✓") criteria_labels <- c(criteria_labels, cols_map[[col]])
      }
    }

    location_text <- NULL
    if (has_screen && "ebs_location" %in% names(row) &&
        !is.na(row$ebs_location) && row$ebs_location != "Unknown") {
      location_text <- if ("ebs_region" %in% names(row) && !is.na(row$ebs_region) &&
                            row$ebs_region != "不明")
        paste0(row$ebs_location, " (", row$ebs_region, ")")
      else row$ebs_location
    }

    idsc_ref <- tryCatch({
      pref_val <- if ("ebs_pref" %in% names(row)) coalesce(row$ebs_pref, NA_character_) else NA_character_
      loc_val  <- if ("ebs_location" %in% names(row)) coalesce(row$ebs_location, NA_character_) else NA_character_
      r <- find_idsc_domestic_link(pref_val, loc_val)
      if (is.null(r) && !is.na(loc_val) && loc_val != "Global") r <- find_idsc_overseas_link(loc_val)
      r
    }, error = function(e) NULL)

    list(
      title        = row$title,
      link         = link,
      sourceName   = if (!is.na(row$source_id) && row$source_id == "who_eios") "Other Source" else row$source_name,
      isOfficial   = !is.na(row$source_id) && is_official_ebs_source(row$source_id),
      pubDate      = if (is.na(row$pub_date)) NULL else format(row$pub_date, "%Y-%m-%d"),
      signalLevel  = as.character(row$signal_level),
      signalColor  = signal_color(as.character(row$signal_level)),
      summary      = if (!is.na(row$summary) && nchar(row$summary) > 0) row$summary else NULL,
      diseaseTags  = as.list(disease_labels),
      criteriaLabels = as.list(criteria_labels),
      locationText = location_text,
      idscRef      = if (!is.null(idsc_ref)) list(label = idsc_ref$label, url = idsc_ref$url) else NULL,
      retweetCount = if (!is.na(row$retweet_count) && !is.null(row$retweet_count)) row$retweet_count else NULL,
      likeCount    = coalesce(row$like_count, 0L)
    )
  }

  output$ebs_news_feed <- renderUI({
    d <- filtered_ebs()
    if (nrow(d) == 0) {
      return(tags$div(class="demo-banner", "該当なし。サイドバーの疾患を変更するか「すべて表示」を押してください。"))
    }
    PAGE_SIZE <- as.integer(input$ebs_page_size)
    total <- nrow(d)
    d <- head(d, PAGE_SIZE)
    dlabel <- EBS_DLABEL
    translate_mode <- isTRUE(input$ebs_translate == "on")

    cards <- lapply(seq_len(nrow(d)), function(i) .ebs_row_to_card(d[i, ], dlabel, translate_mode))
    meta <- list(total = total, pageSize = PAGE_SIZE)

    tags$div(
      tags$div(id = "ebs-cards-root"),
      tags$script(HTML(paste0(
        "renderEbsCards('ebs-cards-root', ",
        jsonlite::toJSON(cards, auto_unbox = TRUE, null = "null"),
        ", ",
        jsonlite::toJSON(meta, auto_unbox = TRUE),
        ");"
      )))
    )
  })

  # ── EBSニュース（海外）─────────────────────────────────────
  ebs_overseas <- reactive({
    d <- ebs_data()
    if (is.null(d) || nrow(d) == 0) return(d)
    d <- d %>% filter(
      mapply(is_overseas_article, title, summary,
             if ("ebs_pref" %in% names(.)) ebs_pref else NA,
             if ("source_id" %in% names(.)) source_id else "",
             if ("source_name" %in% names(.)) source_name else "")
    )
    d <- d %>% filter(is.na(source_id) | source_id != "pubmed")
    if (isTRUE(input$ebs_ov_official_only)) {
      d <- d %>% filter(is_official_ebs_source(source_id))
    }
    if (!ebs_ov_show_all_flag()) {
      did <- sidebar_disease_id()
      if (!is.null(did) && did != "すべて")
        d <- d %>% filter(has_disease_tag(disease_tags, did))
    }
    if (!is.null(input$ebs_ov_sort) && input$ebs_ov_sort == "date") {
      d %>% arrange(desc(pub_date))
    } else {
      d %>% arrange(signal_level, desc(pub_date))
    }
  })

  output$ebs_ov_signal_summary <- renderUI({
    d <- ebs_overseas()
    cnt <- d %>% count(signal_level) %>% mutate(l = as.character(signal_level))
    gn <- function(lv) { v <- cnt %>% filter(l == lv) %>% pull(n); if (length(v) == 0) 0L else v }
    n_event  <- gn("Signal High")
    n_signal <- gn("Signal Low")
    n_fyi    <- gn("FYI")
    n_total  <- nrow(d)
    period_txt <- {
      pd <- d$pub_date[!is.na(d$pub_date)]
      if (length(pd) > 0) sprintf("表示記事の期間: %s 〜 %s（%d件）", format(min(pd), "%Y/%m/%d"), format(max(pd), "%Y/%m/%d"), length(pd))
      else "表示記事の期間: -"
    }
    tags$div(style="background:#f8f9fa;border-radius:8px;padding:12px 16px;margin-bottom:4px;",
      tags$div(style="font-size:0.78em;color:#888;margin-bottom:6px;",
               "EBS サマリー（海外）"),
      tags$div(style="font-size:0.75em;color:#999;margin-bottom:6px;",
               period_txt, "　※古い記事はリンク切れの場合があります",
               "　※シグナル判定は開発中のアルゴリズムのため精度が変動する場合があります"),
      tags$div(style="display:flex;gap:12px;flex-wrap:wrap;margin-bottom:8px;",
        tags$div(class="kpi-box", style="flex:1;min-width:90px;border-left:4px solid #e74c3c;",
          tags$div(class="kpi-value", style="color:#e74c3c;font-size:1.8em;", n_event),
          tags$div(class="kpi-label", "Signal High")),
        tags$div(class="kpi-box", style="flex:1;min-width:90px;border-left:4px solid #e67e22;",
          tags$div(class="kpi-value", style="color:#e67e22;font-size:1.8em;", n_signal),
          tags$div(class="kpi-label", "Signal Low")),
        tags$div(class="kpi-box", style="flex:1;min-width:90px;border-left:4px solid #95a5a6;",
          tags$div(class="kpi-value", style="color:#95a5a6;font-size:1.8em;", n_fyi),
          tags$div(class="kpi-label", "FYI")),
        tags$div(class="kpi-box", style="flex:1;min-width:90px;border-left:4px solid #27ae60;",
          tags$div(class="kpi-value", style="color:#27ae60;font-size:1.8em;", n_total),
          tags$div(class="kpi-label", "総数"))
      )
    )
  })

  output$ebs_ov_news_feed <- renderUI({
    d <- ebs_overseas()
    if (nrow(d) == 0) {
      return(tags$div(class="demo-banner", "該当なし。サイドバーの疾患を変更するか「すべて表示」を押してください。"))
    }
    PAGE_SIZE <- as.integer(input$ebs_ov_page_size)
    total <- nrow(d)
    d <- head(d, PAGE_SIZE)
    dlabel <- EBS_DLABEL
    translate_mode_ov <- isTRUE(input$ebs_ov_translate == "on")

    cards <- lapply(seq_len(nrow(d)), function(i) .ebs_row_to_card(d[i, ], dlabel, translate_mode_ov))
    meta <- list(total = total, pageSize = PAGE_SIZE)

    tags$div(
      tags$div(id = "ebs-ov-cards-root"),
      tags$script(HTML(paste0(
        "renderEbsCards('ebs-ov-cards-root', ",
        jsonlite::toJSON(cards, auto_unbox = TRUE, null = "null"),
        ", ",
        jsonlite::toJSON(meta, auto_unbox = TRUE),
        ");"
      )))
    )
  })

  # ── Google Trends ────────────────────────────────────────
  gtrends_force <- reactiveVal(0)  # 更新ボタン用カウンタ

  gtrends_data <- reactive({
    gtrends_force()  # 依存登録
    geo <- if (!is.null(input$pref_filter) && input$pref_filter != "全国") {
      PREF_GEO_MAP[input$pref_filter]
    } else "JP"
    if (is.na(geo)) geo <- "JP"
    force_val <- isolate(gtrends_force()) > 0
    tryCatch(fetch_google_trends(geo = geo, force = force_val), error = function(e) NULL)
  })

  observeEvent(input$gtrends_refresh, {
    withProgress(message = "Google Trends 取得中...", {
      geo <- if (!is.null(input$pref_filter) && input$pref_filter != "全国") {
        PREF_GEO_MAP[input$pref_filter]
      } else "JP"
      if (is.na(geo)) geo <- "JP"
      tryCatch(fetch_google_trends(geo = geo, force = TRUE), error = function(e) NULL)
      gtrends_force(gtrends_force() + 1)
    })
  })

  # 静的化: 疾患ID毎の週次Signal High/Low件数をまとめて計算し、クライアント側の
  # Plotly.reactで疾患切り替え（サーバー往復なし）に対応する
  # (www/js/ebs_trend_chart.js のupdateEbsTrendChart()が消費する)
  ebs_trend_data <- reactive({
    d <- ebs_data()
    if (is.null(d) || nrow(d) == 0) return(list())
    d <- d %>%
      filter(is.na(source_id) | source_id != "pubmed") %>%
      filter(!mapply(is_overseas_article,
                     coalesce(title, ""), coalesce(summary, ""),
                     if ("ebs_pref"   %in% names(.)) ebs_pref   else NA,
                     if ("source_id"  %in% names(.)) source_id  else "",
                     if ("source_name" %in% names(.)) source_name else "")) %>%
      filter(!is.na(pub_date), pub_date >= Sys.Date() - 60,
             as.character(signal_level) %in% c("Signal High", "Signal Low")) %>%
      mutate(week_start = as.Date(pub_date) - as.integer(format(as.Date(pub_date), "%u")) %% 7)

    all_disease_ids <- union(names(DISEASE_CONFIG), names(ZENSU_DISEASE_CONFIG))
    result <- lapply(all_disease_ids, function(did) {
      dd <- d %>% filter(has_disease_tag(disease_tags, did))
      if (nrow(dd) == 0) return(list())
      wk <- dd %>%
        count(week_start, signal_level, .drop = FALSE) %>%
        tidyr::pivot_wider(names_from = signal_level, values_from = n, values_fill = 0)
      if (!"Signal High" %in% names(wk)) wk$`Signal High` <- 0
      if (!"Signal Low"  %in% names(wk)) wk$`Signal Low`  <- 0
      wk <- wk %>% arrange(week_start)
      lapply(seq_len(nrow(wk)), function(i) list(
        week = format(wk$week_start[i], "%Y-%m-%d"),
        high = wk$`Signal High`[i],
        low  = wk$`Signal Low`[i]
      ))
    })
    names(result) <- all_disease_ids
    Filter(function(x) length(x) > 0, result)
  })

  output$ebs_trend_data_script <- renderUI({
    tags$script(HTML(paste0(
      "window.EBS_TREND_DATA = ",
      jsonlite::toJSON(ebs_trend_data(), auto_unbox = TRUE, null = "null"),
      "; if (window.updateEbsTrendChart) updateEbsTrendChart();"
    )))
  })

  # 静的化: gtrends_data()（都道府県切替・更新ボタン時のみ再取得される）が
  # 含む全疾患分をまとめてJSONに変換し、疾患切替はクライアント側の
  # Plotly.reactで完結させる（www/js/ebs_trend_chart.js のupdateGtrendsChart()）
  GTRENDS_LABEL_MAP <- c(
    flu="インフルエンザ", rsv="RSウイルス", covid="新型コロナ",
    hfmd="手足口病", mycop="マイコプラズマ肺炎",
    varicella="水痘", mumps="おたふくかぜ", gi="感染性胃腸炎",
    measles="麻疹", rubella="風疹", pertussis="百日咳",
    syphilis="梅毒", ehec="腸管出血性大腸菌", dengue="デング熱", mpox="エムポックス"
  )
  gtrends_json_data <- reactive({
    d <- gtrends_data()
    if (is.null(d) || nrow(d) == 0) return(list(series = list(), geoLabel = NULL))

    all_ids <- names(GTRENDS_LABEL_MAP)
    pal <- colorRampPalette(brewer.pal(8, "Set2"))(length(all_ids))
    colors <- setNames(pal, all_ids)

    series <- lapply(unique(d$disease_id), function(did) {
      dd <- d %>% filter(disease_id == did) %>% arrange(date)
      list(
        label = if (!is.na(GTRENDS_LABEL_MAP[did])) GTRENDS_LABEL_MAP[did] else did,
        color = unname(if (!is.na(colors[did])) colors[did] else "#888888"),
        points = lapply(seq_len(nrow(dd)), function(i) list(
          date = format(dd$date[i], "%Y-%m-%d"), hits = dd$hits[i]
        ))
      )
    })
    names(series) <- unique(d$disease_id)

    geo_label <- if (!is.null(input$pref_filter) && input$pref_filter != "全国")
      input$pref_filter else "日本全国"
    list(series = series, geoLabel = geo_label)
  })

  output$gtrends_data_script <- renderUI({
    tags$script(HTML(paste0(
      "window.GTRENDS_DATA = ",
      jsonlite::toJSON(gtrends_json_data(), auto_unbox = TRUE, null = "null"),
      "; if (window.updateGtrendsChart) updateGtrendsChart();"
    )))
  })



  # ── 全数把握疾患（流行曲線タブ統合） ──────────────────────────

  # date_range に依存しない全期間集計（±2SD 帯計算用）
  zensu_hist <- reactive({
    d <- ZENSU_DATA
    if (is.null(d) || nrow(d) == 0) return(NULL)
    d <- d %>% filter(disease == input$zensu_disease_ts)
    if (input$pref_filter != "全国") d <- d %>% filter(pref_name == input$pref_filter)
    d %>%
      group_by(date, year, week) %>%
      summarise(cases = sum(cases, na.rm = TRUE), .groups = "drop") %>%
      arrange(date)
  })

  zensu_ts_filtered <- reactive({
    d <- ZENSU_DATA
    if (is.null(d) || nrow(d) == 0) return(NULL)
    dr <- input$date_range
    d <- d %>%
      filter(disease == input$zensu_disease_ts,
             date >= dr[1], date <= dr[2])
    if (input$pref_filter != "全国") {
      d <- d %>% filter(pref_name == input$pref_filter)
    }
    d
  })

  output$zensu_ts_title_ui <- renderUI({
    did   <- input$zensu_disease_ts
    dconf <- ZENSU_DISEASE_CONFIG[[did]]
    pref_txt <- if (input$pref_filter == "全国") "全国合算" else input$pref_filter
    tags$h5(
      sprintf("全数把握疾患 — %s　流行曲線（週次報告数 / %s）| %s",
              dconf$label, pref_txt, dconf$class),
      style = "font-weight:700;margin-bottom:4px;"
    )
  })

  output$zensu_ts_plot <- renderPlotly({
    d <- zensu_ts_filtered()
    if (is.null(d) || nrow(d) == 0)
      return(plot_ly() %>% add_annotations(
        text = "データなし（疾患・期間を変更してください）", showarrow = FALSE))

    d_agg <- d %>%
      group_by(date, year, week) %>%
      summarise(cases = sum(cases, na.rm = TRUE), .groups = "drop") %>%
      arrange(date)

    dconf <- ZENSU_DISEASE_CONFIG[[input$zensu_disease_ts]]
    col   <- dconf$color

    # 過去5年・前後2週移動平均 ±2SD 帯
    zh <- zensu_hist()
    band_rows <- lapply(seq_len(nrow(d_agg)), function(i) {
      w  <- d_agg$week[i]; y <- d_agg$year[i]
      ws <- unique(pmax(1L, pmin(53L, (w-2L):(w+2L))))
      h  <- zh %>% filter(week %in% ws, year >= y - 5, year < y)
      n  <- sum(!is.na(h$cases))
      mu <- mean(h$cases, na.rm = TRUE)
      s  <- if (n >= 3) sd(h$cases, na.rm = TRUE) else 0
      if (is.na(s)) s <- 0
      data.frame(ymin = max(0, mu - 2*s), ymax = mu + 2*s, has_hist = n >= 3)
    })
    d_band <- cbind(d_agg, do.call(rbind, band_rows))

    p <- plot_ly() %>%
      add_ribbons(data = d_band %>% filter(has_hist), x = ~date,
        ymin = ~ymin, ymax = ~ymax,
        fillcolor = paste0(col, "33"), line = list(color = "transparent"),
        name = "過去5年平均±2SD", hoverinfo = "skip") %>%
      add_bars(data = d_agg, x = ~date, y = ~cases,
        marker = list(color = col, opacity = 0.82),
        hovertemplate = "%{x|%Y-%m-%d}　%{y}件<extra></extra>",
        name = dconf$label)

    # +2SD 超過点を赤丸でプロット
    exceed <- d_band %>% filter(has_hist, cases > ymax)
    if (nrow(exceed) > 0) {
      p <- p %>% add_markers(data = exceed, x = ~date, y = ~cases,
        marker = list(color = "#e74c3c", size = 6, symbol = "circle"),
        name = "+2SD超過",
        hovertemplate = "%{x|%Y-%m-%d}: %{y}件<extra></extra>")
    }

    # ── 短期予測（4週先まで）─────────────────────────────
    # 学習・起点にはスライダー(date_range)に依存しない全期間データ(zensu_hist)を使う
    if (isTRUE(input$show_forecast)) {
      hist_d <- zh %>% filter(!is.na(cases)) %>% arrange(date)
      if (nrow(hist_d) >= 4) {
      fc_method <- if (is.null(input$forecast_method)) "ensemble" else input$forecast_method
      did <- input$zensu_disease_ts
      si  <- SERIAL_INTERVALS[[did]]
      pref_f <- if (!is.null(input$pref_filter) && input$pref_filter != "全国") input$pref_filter else NULL
      seasonal <- tryCatch(detect_seasonality(hist_d, value_col="cases"), error=function(e) FALSE)
      rt_val <- tryCatch({
        if (is.null(ZENSU_DATA) || nrow(ZENSU_DATA) == 0) NA_real_
        else {
          rd <- compute_rt_series_zensu(ZENSU_DATA, did, pref_f) %>% filter(!is.na(rt)) %>% slice_tail(n=1)
          if (nrow(rd)==0) NA_real_ else rd$rt[1]
        }
      }, error=function(e) NA_real_)
      fc <- tryCatch(
        compute_forecast(hist_d$date, hist_d$cases, fc_method,
                          horizon=4, rt_value=rt_val,
                          si_mean=if (!is.null(si)) si$mean else NA_real_,
                          si_sd=if (!is.null(si)) si$sd else NA_real_,
                          seasonal=seasonal),
        error=function(e) NULL)
      if (!is.null(fc) && nrow(fc) > 0) {
        last_pt <- tail(hist_d, 1)
        fc_line <- rbind(
          data.frame(date=last_pt$date[1], value=last_pt$cases[1],
                     lower=last_pt$cases[1], upper=last_pt$cases[1]),
          fc[, c("date","value","lower","upper")]
        )
        p <- p %>%
          add_ribbons(data=fc_line, x=~date, ymin=~pmax(0,lower), ymax=~upper,
            fillcolor="rgba(155,89,182,0.18)", line=list(color="transparent"),
            name="予測区間（目安）", hoverinfo="skip") %>%
          add_lines(data=fc_line, x=~date, y=~value,
            line=list(color="#8e44ad", width=2, dash="dash"),
            name=paste0("予測（", FORECAST_METHOD_LABELS[[fc_method]],
                        if (seasonal) "・季節性反映" else "", "）"),
            hovertemplate="%{x|%Y-%m-%d}　予測 %{y:.1f}件<extra></extra>")
      }
      }
    }

    p %>% layout(
        xaxis  = list(title = "", showgrid = FALSE, type = "date"),
        yaxis  = list(title = "報告数（件）", gridcolor = "#eee", tickformat = "d"),
        bargap = 0.08,
        barmode = "overlay",
        legend = list(orientation = "h", y = -0.15),
        hovermode = "x unified",
        plot_bgcolor  = "#fff",
        paper_bgcolor = "#fff",
        margin = list(t = 10, b = 40, l = 60, r = 20)
      )
  })

  output$zensu_ts_legend <- renderUI({
    col <- ZENSU_DISEASE_CONFIG[[input$zensu_disease_ts]]$color
    tags$div(style="font-size:0.75em;color:#666;margin-top:2px;padding-left:4px;display:flex;gap:16px;align-items:center;flex-wrap:wrap;",
      tags$span(
        tags$span(style=paste0("display:inline-block;width:14px;height:10px;background:",
          col,"33",";border:1px solid ",col,";margin-right:4px;vertical-align:middle;")),
        "帯: 直前5年間・前後2週移動平均±2SD"
      ),
      tags$span(
        tags$span(style="display:inline-block;width:8px;height:8px;background:#e74c3c;border-radius:50%;margin-right:4px;vertical-align:middle;"),
        "+2SD超過"
      )
    )
  })

  # ── 年別重ね合わせ（定点把握）────────────────────────────
  output$yearly_overlay_plot <- renderPlotly({
    dr  <- input$date_range
    pref <- if (!is.null(input$pref_filter) && input$pref_filter != "全国") input$pref_filter else NULL
    d <- SURV_DATA %>% filter(disease == input$disease)
    if (!is.null(pref)) d <- d %>% filter(pref_name == pref)
    d <- d %>%
      group_by(year, week) %>%
      summarise(val = mean(reports_per_site, na.rm=TRUE), .groups="drop") %>%
      filter(week <= 53)

    years <- sort(unique(d$year))
    pal   <- colorRampPalette(c("#bdc3c7","#2980b9","#e74c3c"))(length(years))
    col   <- DISEASE_CONFIG[[input$disease]]$color
    cur_yr <- max(years)

    p <- plot_ly()
    for (i in seq_along(years)) {
      yr  <- years[i]
      dy  <- d %>% filter(year == yr) %>% arrange(week)
      lw  <- if (yr == cur_yr) 2.5 else 1
      op  <- if (yr == cur_yr) 1 else 0.5
      clr <- if (yr == cur_yr) col else pal[i]
      p <- p %>% add_trace(
        data=dy, x=~week, y=~val, type="scatter", mode="lines",
        name=as.character(yr),
        line=list(color=clr, width=lw),
        opacity=op,
        hovertemplate=paste0(yr, " 第%{x}週: %{y:.2f}<extra></extra>"))
    }
    p %>% layout(
      xaxis = list(title="週", dtick=4, range=c(1,53), showgrid=TRUE, gridcolor="#eee"),
      yaxis = list(title="定点あたり報告数", gridcolor="#eee"),
      legend= list(orientation="h", y=-0.2),
      plot_bgcolor="#fff", paper_bgcolor="#fff",
      margin=list(t=10, b=60, l=60, r=20)
    )
  })

  # ── 年別重ね合わせ（全数把握）────────────────────────────
  output$zensu_yearly_overlay_plot <- renderPlotly({
    zd <- ZENSU_DATA
    if (is.null(zd) || nrow(zd) == 0)
      return(plot_ly() %>% add_annotations(text="データなし", showarrow=FALSE))
    pref <- if (!is.null(input$pref_filter) && input$pref_filter != "全国") input$pref_filter else NULL
    d <- zd %>% filter(disease == input$zensu_disease_ts)
    if (!is.null(pref)) d <- d %>% filter(pref_name == pref)
    d <- d %>%
      group_by(year, week) %>%
      summarise(cases = sum(cases, na.rm=TRUE), .groups="drop") %>%
      filter(week <= 53)

    years <- sort(unique(d$year))
    pal   <- colorRampPalette(c("#bdc3c7","#2980b9","#e74c3c"))(length(years))
    col   <- ZENSU_DISEASE_CONFIG[[input$zensu_disease_ts]]$color
    if (is.null(col)) col <- "#2980b9"
    cur_yr <- max(years)

    p <- plot_ly()
    for (i in seq_along(years)) {
      yr  <- years[i]
      dy  <- d %>% filter(year == yr) %>% arrange(week)
      lw  <- if (yr == cur_yr) 2.5 else 1
      op  <- if (yr == cur_yr) 1 else 0.5
      clr <- if (yr == cur_yr) col else pal[i]
      p <- p %>% add_trace(
        data=dy, x=~week, y=~cases, type="scatter", mode="lines",
        name=as.character(yr),
        line=list(color=clr, width=lw),
        opacity=op,
        hovertemplate=paste0(yr, " 第%{x}週: %{y}件<extra></extra>"))
    }
    p %>% layout(
      xaxis = list(title="週", dtick=4, range=c(1,53), showgrid=TRUE, gridcolor="#eee"),
      yaxis = list(title="報告数（件）", gridcolor="#eee", tickformat="d"),
      legend= list(orientation="h", y=-0.2),
      plot_bgcolor="#fff", paper_bgcolor="#fff",
      margin=list(t=10, b=60, l=60, r=20)
    )
  })

  # 都道府県別ヒートマップ（全数把握・直近1年）
  output$zensu_heatmap_plot <- renderPlotly({
    zd <- ZENSU_DATA
    if (is.null(zd) || nrow(zd) == 0)
      return(plot_ly() %>% add_annotations(text="データなし", showarrow=FALSE))
    cutoff1y <- Sys.Date() - 365
    d <- zd %>% filter(disease == input$zensu_disease_ts, date >= cutoff1y) %>%
      group_by(pref_code, pref_name, date, week, year) %>%
      summarise(cases=sum(cases, na.rm=TRUE), .groups="drop") %>%
      mutate(week_label = paste0(year, "-W", sprintf("%02d", week)))
    if (nrow(d) == 0)
      return(plot_ly() %>% add_annotations(text="データなし", showarrow=FALSE))
    pord <- d %>% distinct(pref_code,pref_name) %>% arrange(pref_code) %>% pull(pref_name)
    word <- d %>% distinct(week_label, date) %>% arrange(date) %>% pull(week_label)
    d <- d %>% mutate(pref_name=factor(pref_name,levels=rev(pord)),
                      week_label=factor(week_label,levels=word))
    plot_ly(data=d, x=~week_label, y=~pref_name, z=~cases, type="heatmap",
      colorscale=list(c(0,"#ffffcc"),c(0.5,"#fd8d3c"),c(1,"#800026")),
      hovertemplate="%{y} %{x}: %{z}件<extra></extra>",
      colorbar=list(title="件")) %>%
      layout(xaxis=list(title="", tickangle=-45, dtick=4),
             yaxis=list(title="",tickfont=list(size=8)),margin=list(l=85,t=20,b=60))
  })

  # 地域別比較（全数把握）
  output$zensu_region_plot <- renderPlotly({
    zd <- ZENSU_DATA
    if (is.null(zd) || nrow(zd) == 0)
      return(plot_ly() %>% add_annotations(text="データなし", showarrow=FALSE))
    d <- zd %>% filter(disease == input$zensu_disease_ts) %>%
      left_join(PREF_MASTER %>% select(pref_code, region), by = "pref_code") %>%
      group_by(region,date) %>%
      summarise(cases=sum(cases, na.rm=TRUE), .groups="drop")
    if (nrow(d) == 0)
      return(plot_ly() %>% add_annotations(text="データなし", showarrow=FALSE))
    regions <- unique(d$region)
    cols <- colorRampPalette(brewer.pal(min(8,length(regions)),"Set2"))(length(regions))
    p <- plot_ly()
    for(i in seq_along(regions)){
      rd <- d %>% filter(region==regions[i])
      p <- p %>% add_lines(data=rd,x=~date,y=~cases,
        name=regions[i],line=list(color=cols[i],width=2))
    }
    p %>% layout(xaxis=list(title="",showgrid=FALSE),
      yaxis=list(title="地域合計 報告数（件）",gridcolor="#eee"),
      legend=list(orientation="h",y=-0.2,font=list(size=10)),
      hovermode="x unified",plot_bgcolor="#fff",paper_bgcolor="#fff",margin=list(t=20))
  })

  # ── 月報疾患（性感染症・薬剤耐性菌）────────────────────────
  std_agg_all <- reactive({
    d <- STD_DATA
    if (is.null(d) || nrow(d) == 0) return(NULL)
    did <- input$disease
    if (is.null(did)) return(NULL)
    d <- d %>% filter(disease == did)
    if (!is.null(input$pref_filter) && input$pref_filter != "全国") {
      d <- d %>% filter(pref_name == input$pref_filter)
    }
    d %>%
      group_by(year, month, date) %>%
      summarise(reports = sum(reports, na.rm = TRUE), .groups = "drop") %>%
      arrange(date)
  })

  std_filtered <- reactive({
    d <- std_agg_all()
    if (is.null(d) || nrow(d) == 0) return(NULL)
    dr <- input$date_range
    if (!is.null(dr) && length(dr) == 2) {
      d <- d %>% filter(date >= dr[1], date <= dr[2])
    }
    d
  })

  output$std_ts_title_ui <- renderUI({
    did <- input$disease
    dconf <- STD_DISEASE_CONFIG[[did]]
    pref_txt <- if (is.null(input$pref_filter) || input$pref_filter == "全国") "全国合算" else input$pref_filter
    tags$h5(
      sprintf("定点把握疾患（月次報告） — %s　流行曲線（月次報告数 / %s）", if (!is.null(dconf)) dconf$label else did, pref_txt),
      style = "font-weight:700;margin-bottom:4px;"
    )
  })

  output$std_ts_plot <- renderPlotly({
    d <- std_filtered()
    hist_all <- std_agg_all()
    if (is.null(d) || nrow(d) == 0)
      return(plot_ly() %>% add_annotations(
        text = "データなし（月報未掲載の月・期間が含まれます）", showarrow = FALSE))

    did <- input$disease
    dconf <- STD_DISEASE_CONFIG[[did]]
    col <- if (!is.null(dconf)) dconf$color else "#2980b9"

    d_band <- compute_std_band(d, hist_all, "reports")

    p <- plot_ly() %>%
      add_ribbons(data = d_band %>% filter(has_hist), x = ~date, ymin = ~ymin, ymax = ~ymax,
        fillcolor = paste0(col, "33"), line = list(color = "transparent"),
        name = "過去5年平均±2SD", hoverinfo = "skip") %>%
      add_lines(data = d_band, x = ~date, y = ~reports,
        line = list(color = col, width = 2.5),
        name = if (!is.null(dconf)) dconf$label else did,
        hovertemplate = "%{x|%Y-%m}　%{y}件<extra></extra>")

    exceed <- d_band %>% filter(has_hist, reports > ymax)
    if (nrow(exceed) > 0) {
      p <- p %>% add_markers(data = exceed, x = ~date, y = ~reports,
        marker = list(color = "#e74c3c", size = 6, symbol = "circle"),
        name = "+2SD超過", hovertemplate = "%{x|%Y-%m}: %{y}件<extra></extra>")
    }

    # ── 短期予測（月報疾患は月次データのため1か月先のみ）────────
    # Rtは推定対象外のため、予測手法にRtベースが選ばれていてもアンサンブル
    # （ポアソン回帰＋Holt法の平均）にフォールバックする
    if (isTRUE(input$show_forecast)) {
      hist_d <- hist_all %>% filter(!is.na(reports)) %>% arrange(date)
      if (nrow(hist_d) >= 4) {
        fc_method <- if (is.null(input$forecast_method) || input$forecast_method == "rt") "ensemble" else input$forecast_method
        fc <- tryCatch(
          compute_forecast(hist_d$date, hist_d$reports, fc_method,
                            horizon = 1, seasonal = FALSE, unit = "month"),
          error = function(e) NULL)
        if (!is.null(fc) && nrow(fc) > 0) {
          last_pt <- tail(hist_d, 1)
          fc_line <- rbind(
            data.frame(date = last_pt$date[1], value = last_pt$reports[1],
                       lower = last_pt$reports[1], upper = last_pt$reports[1]),
            fc[, c("date", "value", "lower", "upper")]
          )
          p <- p %>%
            add_ribbons(data = fc_line, x = ~date, ymin = ~pmax(0, lower), ymax = ~upper,
              fillcolor = "rgba(155,89,182,0.18)", line = list(color = "transparent"),
              name = "予測区間（目安）", hoverinfo = "skip") %>%
            add_lines(data = fc_line, x = ~date, y = ~value,
              line = list(color = "#8e44ad", width = 2, dash = "dash"),
              name = paste0("予測（", FORECAST_METHOD_LABELS[[fc_method]], "・1か月後）"),
              hovertemplate = "%{x|%Y-%m}　予測 %{y:.0f}件<extra></extra>")
        }
      }
    }

    p %>% layout(
      xaxis = list(title = "", showgrid = FALSE, type = "date"),
      yaxis = list(title = "報告数（件/月）", gridcolor = "#eee", rangemode = "nonnegative"),
      legend = list(orientation = "h", y = -0.15),
      hovermode = "x unified",
      plot_bgcolor = "#fff", paper_bgcolor = "#fff",
      margin = list(t = 20, b = 40, l = 60, r = 20)
    )
  })

  output$std_ts_legend <- renderUI({
    tags$div(style="font-size:0.75em;color:#666;margin-top:2px;padding-left:4px;display:flex;gap:16px;align-items:center;flex-wrap:wrap;",
      tags$span(
        tags$span(style="display:inline-block;width:8px;height:8px;background:#e74c3c;border-radius:50%;margin-right:4px;vertical-align:middle;"),
        "+2SD超過"
      ),
      tags$span("帯: 過去5年間・同月（前後1か月）平均±2SD（3年分以上のデータがある月のみ表示）"),
      tags$span(
        "出典: 感染症発生動向調査週報（IDWR）月報（定点把握・基幹定点）。月1回程度の掲載のため、月報未掲載の月は欠測。",
        tags$a(href="javascript:void(0)", onclick="goToNotes('notes-std')", "詳細")
      )
    )
  })

  # 年別重ね合わせ（月報疾患）
  output$std_yearly_overlay_plot <- renderPlotly({
    did  <- input$disease
    pref <- if (!is.null(input$pref_filter) && input$pref_filter != "全国") input$pref_filter else NULL
    d <- STD_DATA
    if (is.null(d) || nrow(d) == 0)
      return(plot_ly() %>% add_annotations(text = "データなし", showarrow = FALSE))
    d <- d %>% filter(disease == did)
    if (!is.null(pref)) d <- d %>% filter(pref_name == pref)
    d <- d %>%
      group_by(year, month) %>%
      summarise(val = sum(reports, na.rm = TRUE), .groups = "drop")
    if (nrow(d) == 0)
      return(plot_ly() %>% add_annotations(text = "データなし", showarrow = FALSE))

    years  <- sort(unique(d$year))
    pal    <- colorRampPalette(c("#bdc3c7","#2980b9","#e74c3c"))(length(years))
    dconf  <- STD_DISEASE_CONFIG[[did]]
    col    <- if (!is.null(dconf)) dconf$color else "#2980b9"
    cur_yr <- max(years)

    p <- plot_ly()
    for (i in seq_along(years)) {
      yr  <- years[i]
      dy  <- d %>% filter(year == yr) %>% arrange(month)
      lw  <- if (yr == cur_yr) 2.5 else 1
      op  <- if (yr == cur_yr) 1 else 0.5
      clr <- if (yr == cur_yr) col else pal[i]
      p <- p %>% add_trace(
        data = dy, x = ~month, y = ~val, type = "scatter", mode = "lines",
        name = as.character(yr),
        line = list(color = clr, width = lw),
        opacity = op,
        hovertemplate = paste0(yr, " %{x}月: %{y}件<extra></extra>"))
    }
    p %>% layout(
      xaxis = list(title = "月", dtick = 1, range = c(1,12), showgrid = TRUE, gridcolor = "#eee"),
      yaxis = list(title = "報告数（件/月）", gridcolor = "#eee"),
      legend= list(orientation = "h", y = -0.2),
      plot_bgcolor = "#fff", paper_bgcolor = "#fff",
      margin = list(t = 10, b = 60, l = 60, r = 20)
    )
  })

  # 都道府県別ヒートマップ（月報疾患・直近2年）
  output$std_heatmap_plot <- renderPlotly({
    did <- input$disease
    d <- STD_DATA
    if (is.null(d) || nrow(d) == 0)
      return(plot_ly() %>% add_annotations(text = "データなし", showarrow = FALSE))
    cutoff <- Sys.Date() - 730
    d <- d %>% filter(disease == did, date >= cutoff) %>%
      left_join(PREF_MASTER %>% select(pref_name, pref_code), by = "pref_name") %>%
      group_by(pref_code, pref_name, date, year, month) %>%
      summarise(reports = sum(reports, na.rm = TRUE), .groups = "drop") %>%
      mutate(month_label = sprintf("%d-%02d", year, month))
    if (nrow(d) == 0)
      return(plot_ly() %>% add_annotations(text = "データなし", showarrow = FALSE))
    pord <- d %>% distinct(pref_code, pref_name) %>% arrange(pref_code) %>% pull(pref_name)
    mord <- d %>% distinct(month_label, date) %>% arrange(date) %>% pull(month_label)
    d <- d %>% mutate(pref_name = factor(pref_name, levels = rev(pord)),
                       month_label = factor(month_label, levels = mord))
    plot_ly(data = d, x = ~month_label, y = ~pref_name, z = ~reports, type = "heatmap",
      colorscale = list(c(0,"#ffffcc"), c(0.5,"#fd8d3c"), c(1,"#800026")),
      hovertemplate = "%{y} %{x}: %{z}件<extra></extra>",
      colorbar = list(title = "件/月")) %>%
      layout(xaxis = list(title = "", tickangle = -45),
             yaxis = list(title = "", tickfont = list(size = 8)), margin = list(l = 85, t = 20, b = 60))
  })

  # 地域別比較（月報疾患）
  output$std_region_plot <- renderPlotly({
    did <- input$disease
    d <- STD_DATA
    if (is.null(d) || nrow(d) == 0)
      return(plot_ly() %>% add_annotations(text = "データなし", showarrow = FALSE))
    d <- d %>% filter(disease == did) %>%
      left_join(PREF_MASTER %>% select(pref_name, region), by = "pref_name") %>%
      group_by(region, date) %>%
      summarise(reports = sum(reports, na.rm = TRUE), .groups = "drop")
    if (nrow(d) == 0)
      return(plot_ly() %>% add_annotations(text = "データなし", showarrow = FALSE))
    regions <- unique(d$region)
    cols <- colorRampPalette(brewer.pal(min(8, length(regions)), "Set2"))(length(regions))
    p <- plot_ly()
    for (i in seq_along(regions)) {
      rd <- d %>% filter(region == regions[i])
      p <- p %>% add_lines(data = rd, x = ~date, y = ~reports,
        name = regions[i], line = list(color = cols[i], width = 2))
    }
    p %>% layout(xaxis = list(title = "", showgrid = FALSE),
      yaxis = list(title = "地域合計 報告数（件/月）", gridcolor = "#eee"),
      legend = list(orientation = "h", y = -0.2, font = list(size = 10)),
      hovermode = "x unified", plot_bgcolor = "#fff", paper_bgcolor = "#fff", margin = list(t = 20))
  })

  # ── データ出力 ─────────────────────────────────────────
  # サイドバーの ts_mode に連動（定点/全数）
  data_tab_df <- reactive({
    is_zensu <- !is.null(input$ts_mode) && input$ts_mode == "zensu"
    dr <- input$date_range
    if (is_zensu) {
      d <- ZENSU_DATA
      if (is.null(d) || nrow(d) == 0) return(data.frame())
      d <- d %>% filter(disease == input$zensu_disease_ts,
                        date >= dr[1], date <= dr[2])
      if (!is.null(input$pref_filter) && input$pref_filter != "全国")
        d <- d %>% filter(pref_name == input$pref_filter)
      did <- input$zensu_disease_ts
      dlabel <- if (!is.null(ZENSU_DISEASE_CONFIG[[did]])) ZENSU_DISEASE_CONFIG[[did]]$label else did
      d %>%
        mutate(疾患名 = dlabel) %>%
        select(年=year, 週=week, 日付=date,
               都道府県=pref_name, 都道府県コード=pref_code,
               疾患名, 報告数=cases) %>%
        arrange(desc(年), desc(週), desc(報告数))
    } else {
      dlabel <- if (!is.null(DISEASE_CONFIG[[input$disease]])) DISEASE_CONFIG[[input$disease]]$label else input$disease
      rt_tbl  <- tryCatch(rt_series() %>% select(date, rt), error=function(e) tibble(date=as.Date(character()), rt=numeric()))
      ibs_tbl <- tryCatch(ts_band_series() %>% select(date, ibs_score), error=function(e) tibble(date=as.Date(character()), ibs_score=numeric()))
      filtered_data() %>%
        left_join(rt_tbl, by="date") %>%
        left_join(ibs_tbl, by="date") %>%
        mutate(`定点あたり報告数` = round(reports_per_site, 2),
               警戒レベル = classify_alert(reports_per_site, input$disease, rt, ibs_score),
               疾患名 = dlabel) %>%
        select(年=year, 週=week, 日付=date, 都道府県=pref_name, 地域=region,
               疾患名, `定点あたり報告数`, 警戒レベル) %>%
        arrange(desc(年), desc(週), desc(`定点あたり報告数`))
    }
  })

  output$download_csv <- downloadHandler(
    filename = function() {
      is_zensu <- !is.null(input$ts_mode) && input$ts_mode == "zensu"
      if (is_zensu)
        paste0("nesid_", input$zensu_disease_ts, "_", Sys.Date(), ".csv")
      else
        paste0("idwr_", input$disease, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv_bom(data_tab_df(), file)
    }
  )
}

shinyApp(ui, server)
