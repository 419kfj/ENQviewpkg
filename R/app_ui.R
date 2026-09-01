app_ui <- function(app_version = "1.0.0") {
  shiny::fluidPage(
    shiny::titlePanel(paste(" ENQview_pkg Ver:", app_version)),
    shiny::tabPanel(
      "基本集計",
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          # 直接 df が渡されていない場合のみ RDA 選択 UI を表示
          shiny::uiOutput("rda_input_container"),

          shiny::uiOutput("variables_ui"),
          tags$hr(),
          shiny::uiOutput("cross_var_ui"),
          shiny::uiOutput("layer_var_ui"),
          shiny::uiOutput("dynamic_stratify_filter"),
          shiny::uiOutput("variables2_ui"),
          shiny::uiOutput("hist_var_ui")
        ),

        shiny::mainPanel(
          shiny::tabsetPanel(
            type = "tabs",
            shiny::tabPanel("単変数集計",
                            shiny::h2("棒グラフと度数分布"),
                            shiny::plotOutput("barchart"),
                            DT::dataTableOutput("simple_table")
            ),
            shiny::tabPanel("2変数分析",
                            shiny::h2("クロス集計（gtsummary::tbl_cross )"),
                            gt::gt_output(outputId = "my_gt_table2"),
                            shiny::plotOutput("crosschart", width = 600, height = 600),
                            shiny::h3("χ2乗検定"),
                            shiny::verbatimTextOutput("chisq_test2"),
                            shiny::h3("クロス表の対応分析"),
                            shiny::plotOutput("Crosstable_CA")
            ),
            shiny::tabPanel("pairs",
                            shiny::h2("GGally::pairs"),
                            shiny::plotOutput("pairs", width = 600, height = 600)
            ),
            shiny::tabPanel("pairs_multi",
                            shiny::h2("GGally::pairs 多変数"),
                            shiny::plotOutput("pairs_multi", width = 900, height = 900)
            ),
            shiny::tabPanel("2変数分析（層化）",
                            shiny::h2("クロス集計（gtsummary::tbl_cross )"),
                            gt::gt_output(outputId = "my_gt_table"),
                            shiny::plotOutput("crosschart2", width = 900, height = 600),
                            shiny::h3("χ2乗検定"),
                            shiny::verbatimTextOutput("chisq_test")
            ),
            shiny::tabPanel("MA plot(Bar)",
                            shiny::h2("MA変数集計"),
                            shiny::plotOutput("MAplot", width = 600, height = 600)
            ),
            shiny::tabPanel("MA plot(Dot)",
                            shiny::h2("MA変数集計"),
                            shiny::plotOutput("MAplot_Dot", width = 600, height = 600)
            ),
            shiny::tabPanel("層化 MA plot",
                            shiny::h2("層化MA変数集計"),
                            shiny::plotOutput("MAplot_lineDot", width = 600, height = 400),
                            shiny::plotOutput("MAplot_lineDotwarp", width = 600, height = 600)
            ),
            shiny::tabPanel("層化 MA plot2",
                            shiny::h2("層化MA変数集計（Legendなし）"),
                            shiny::plotOutput("MAplot_lineDot2", width = 600, height = 400),
                            shiny::plotOutput("MAplot_lineDotwarp", width = 600, height = 600)
            ),
            shiny::tabPanel("Grid回答 General mosaic表示",
                            shiny::h2("Grid回答mosaic表示"),
                            shiny::plotOutput("GridAnswerG_mosaic", width = 600, height = 600),
                            shiny::plotOutput("GridAnswerG_CA", width = 700, height = 700)
            ),
            shiny::tabPanel("単変数check",
                            shiny::h2("棒グラフと度数分布"),
                            shiny::plotOutput("barchart2"),
                            DT::dataTableOutput("simple_table2")
            ),
            shiny::tabPanel("選択変数のデータ一覧",
                            shiny::h2("データ一覧"),
                            DT::dataTableOutput("table_for_plot")
            ),
            shiny::tabPanel("選択変数の構造表示（str）",
                            shiny::h2("データ一覧"),
                            shiny::verbatimTextOutput("str_data")
            ),
            shiny::tabPanel("使い方",
                            shiny::p("アプリの詳細な使い方は、以下のリンク先をご確認ください。"),
                            shiny::a("Shinyアプリの使い方ガイド（外部サイト）",
                                     href = "https://www.fujimotolabo.uk/Shiny_app_how2/",
                                     target = "_blank")
            )
          )
        )
      )
    )
  )
}
