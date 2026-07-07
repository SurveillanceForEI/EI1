# EBS ルールベーススクリーニング - Shiny組み込みモジュール
# APIキー不要・完全オフライン
#
# 使い方:
#   source("ebs_screening_module.R")
#   UI:     screenerUI("id")
#   Server: result_rv <- screenerServer("id", reactive(input$title), reactive(input$summary))

library(shiny)
# ebs_rule_screening.R は app.R から先にsource済み

# ============================================================
# UI モジュール
# ============================================================

screenerUI <- function(id) {
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(icon("robot"), " AI自動スクリーニング（ルールベース）"),
      actionButton(ns("run"), "この記事を評価", class = "btn-success btn-block",
                   icon = icon("search")),
      br(),
      uiOutput(ns("result_panel"))
    )
  )
}

# ============================================================
# Server モジュール
# ============================================================

screenerServer <- function(id, reactive_title, reactive_summary = reactive("")) {
  moduleServer(id, function(input, output, session) {

    result <- eventReactive(input$run, {
      req(reactive_title())
      screen_entry(
        title   = reactive_title(),
        summary = if (is.null(reactive_summary())) "" else reactive_summary()
      )
    })

    output$result_panel <- renderUI({
      req(result())
      r <- result()

      chk <- function(val) {
        if (val == "✓") tags$span(style = "color:#27ae60; font-size:1.3em;", "✓")
        else            tags$span(style = "color:#bdc3c7;", "−")
      }

      weight_color <- switch(r$signal_weight,
        "Signal High" = "#e74c3c",
        "Signal Low"  = "#e67e22",
        "FYI"         = "#3498db"
      )

      tagList(
        tags$hr(),
        # Signal重みづけバッジ
        div(style = "text-align:center; margin-bottom:10px;",
          tags$span(
            style = paste0("background:", weight_color,
                           "; color:white; padding:6px 18px; border-radius:4px;",
                           " font-size:1.1em; font-weight:bold;"),
            r$signal_weight
          )
        ),
        # 基準チェック表
        tags$table(
          class = "table table-condensed table-bordered",
          style = "font-size:0.9em;",
          tags$tbody(
            tags$tr(tags$td("Unusual/unexpected"),            tags$td(chk(r$unusual_unexpected))),
            tags$tr(tags$td("Serious PH impact (country)"),   tags$td(chk(r$serious_ph_country))),
            tags$tr(tags$td("Serious PH impact (Japan)"),     tags$td(chk(r$serious_ph_japan))),
            tags$tr(tags$td("Epidemic-prone"),                tags$td(chk(r$epidemic_prone))),
            tags$tr(tags$td("Mass exposure"),                 tags$td(chk(r$mass_exposure))),
            tags$tr(tags$td("High profile"),                  tags$td(chk(r$high_profile))),
            tags$tr(tags$td("Special pathogen/bioterrorism"), tags$td(chk(r$special_pathogen)))
          )
        ),
        # 分類情報
        div(class = "well well-sm", style = "font-size:0.9em;",
          tags$dl(class = "dl-horizontal",
            tags$dt("疾患分類"), tags$dd(r$disease_category),
            tags$dt("疾患名"),   tags$dd(paste0(r$disease_name_en, " / ", r$disease_name_ja)),
            tags$dt("場所"),     tags$dd(paste0(r$location, "（", r$region, "）"))
          )
        ),
        div(class = "text-muted", style = "font-size:0.8em;",
          icon("info-circle"), " ルールベース判定（自動）。必要に応じて手動修正してください。"
        )
      )
    })

    return(result)
  })
}

# ============================================================
# デモアプリ（単体動作確認用）
# ============================================================

run_demo <- function() {
  ui <- fluidPage(
    titlePanel("EBS スクリーニング デモ"),
    sidebarLayout(
      sidebarPanel(
        textInput("title", "Signalタイトル",
                  value = "Ebola outbreak – DRC"),
        textAreaInput("summary", "概要", rows = 4,
                      value = "コンゴ民主共和国でエボラ出血熱のアウトブレイク。10例報告、うち4例死亡。WHOが調査チームを派遣。"),
        screenerUI("demo")
      ),
      mainPanel(
        h4("使い方"),
        tags$ol(
          tags$li("タイトルと概要を入力"),
          tags$li("「この記事を評価」をクリック"),
          tags$li("結果を確認・手動修正")
        )
      )
    )
  )

  server <- function(input, output, session) {
    screenerServer("demo",
                   reactive_title   = reactive(input$title),
                   reactive_summary = reactive(input$summary))
  }

  shiny::shinyApp(ui, server)
}
