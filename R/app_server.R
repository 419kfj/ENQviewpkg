#' @import shiny
#' @importFrom stats chisq.test reorder setNames
#' @importFrom utils str
NULL

app_server <- function(input, output, session, initial_df = NULL) {

  # --- RDAファイル読込枠の動的UI生成 ---
  output$rda_input_container <- renderUI({
    # 引数で直接 df が渡されている場合は RDA 選択 UI を出さない
    if (!is.null(initial_df)) {
      return(tags$div(
        tags$p(tags$b("【データ読み込み完了】"), style = "color: green;"),
        tags$p(paste("作業中のデータフレームを使用中 (行数:", nrow(initial_df), ", 列数:", ncol(initial_df), ")")),
        tags$hr()
      ))
    }
    # 引数 df がない場合のみ従来の RDA アップロード UI を表示
    tagList(
      fileInput("rda_file", "RDAファイルを選択", accept = ".rda"),
      uiOutput("rda_objects_ui")
    )
  })

  # --- データ保持用 reactiveVal ---
  data.df <- reactiveVal(NULL)

  # 初期状態のセットアップ
  observe({
    if (!is.null(initial_df)) {
      data.df(initial_df)
    }
  })

  # RDAファイルがアップロードされた場合のロード処理
  rda_env <- reactive({
    req(input$rda_file)
    env <- new.env()
    load(input$rda_file$datapath, envir = env)
    env
  })

  output$rda_objects_ui <- renderUI({
    req(rda_env())
    selectInput("rda_object", "RDA 内のオブジェクトを選択",
                choices = ls(rda_env()), selected = ls(rda_env())[1])
  })

  observeEvent(input$rda_object, {
    req(input$rda_object)
    obj <- get(input$rda_object, envir = rda_env())
    if (is.data.frame(obj)) {
      data.df(obj)
    } else {
      showNotification("選択したオブジェクトはデータフレームではありません。", type = "error")
      data.df(NULL)
    }
  })

  # --- 以下、元の解析・グラフ描画ロジックはそのまま維持 ---

  observeEvent(data.df(), {
    req(data.df())
    data <- data.df()

    updateSelectInput(session, "select_input_data_for_hist",
                      choices = colnames(data))
    updateSelectInput(session, "select_input_data_for_cross",
                      choices = c(" ", colnames(data)))
    updateSelectInput(session, "select_input_data_for_layer",
                      choices = c(" ", colnames(data)))
    updateSelectInput(session, "variables",
                      choices = colnames(data),
                      selected = colnames(data)[3:4])
  })

  # # Console へのデバッグ出力
  # observe({
  #   cat("\n=== [DEBUG] Current data.df status ===\n")
  #   cat("Is NULL?: ", is.null(data.df()), "\n")
  #   if (!is.null(data.df())) {
  #     cat("Rows: ", nrow(data.df()), " Cols: ", ncol(data.df()), "\n")
  #     cat("Colnames: ", paste(colnames(data.df())[1:min(5, ncol(data.df()))], collapse = ", "), "\n")
  #   }
  #   cat("input$variables: ", paste(input$variables, collapse = ", "), "\n")
  #   cat("=====================================\n\n")
  # })

  data_for_plot <- reactive({
    req(data.df())
    data.df()
  })

  output$hist_var_ui <- renderUI({
    req(data.df())
    selectInput("select_input_data_for_hist", "確認したい単変数", choices = names(data.df()))
  })

  output$cross_var_ui <- renderUI({
    req(data.df())
    selectInput("select_input_data_for_cross", "クロス集計変数を選択", choices = c(" ", names(data.df())))
  })

  output$layer_var_ui <- renderUI({
    req(data.df())
    selectInput("select_input_data_for_layer", "層変数を選択", choices = c(" ", names(data.df())))
  })

  output$variables_ui <- renderUI({
    req(data.df())
    selectInput("variables", "変数を選択",
                choices = names(data.df()),
                selected = names(data.df())[1],
                multiple = TRUE,
                selectize = FALSE,
                size = 7)
  })

  # (※その他の各描画アウトプット: barchart, pairs, crosschart, MAplot, GridAnswerG_mosaic 等は元のコードのままで動作します)

  # ... [元のグラフ/計算ロジック群] ...
  ####---- functions for each output

  # 選択列のプレビュー
  output$table_preview <- renderTable({
    req(data.df(), input$variables)
    data.df()[, input$variables, drop = FALSE]
  })

  # barplot by ggplot2 動作確認用function
  output$barchart <- renderPlot({
    req(data_for_plot(), input$variables, length(input$variables) > 0)
#    print("--- barchart rendering started ---")
    data_for_plot() |> dplyr::count(!!!rlang::syms(input$variables[1])) |>
      dplyr::rename(V1=1) |>  dplyr::filter(V1 != "非該当") |>
      dplyr::mutate(rate=100 * .data[["n"]]/sum(.data[["n"]])) |>
      ggplot2::ggplot(ggplot2::aes(x=V1,y=rate)) + ggplot2::geom_col(ggplot2::aes(fill=V1)) +
      ggplot2::ggtitle(input$variables[1])
  })

  output$barchart2 <- renderPlot({
    req(data_for_plot(), input$variables, length(input$variables) > 0)
    data_for_plot() |> dplyr::count(!!!rlang::syms(input$select_input_data_for_hist)) |> dplyr::rename(V1=1) |> filter(V1 != "非該当") |>
      dplyr::mutate(rate=100* .data[["n"]]/ sum(.data[["n"]])) |>
      ggplot2::ggplot(ggplot2::aes(x=V1,y=rate)) + ggplot2::geom_col(ggplot2::aes(fill=V1)) + ggplot2::ggtitle(input$select_input_data_for_hist)
  })



  # GGally::ggpairs
  output$pairs <- renderPlot({
    req(data_for_plot(), input$variables, length(input$variables) > 0,input$select_input_data_for_cross)
    data_for_plot()[,c(input$variables[1],input$select_input_data_for_cross)] |>
      GGally::ggpairs(mapping = ggplot2::aes(color = !!as.name(input$variables[1]))) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle=45,hjust = 1)) +
      ggplot2::ggtitle(input$variables[1]) -> p
    p
  })

  output$pairs_multi<- renderPlot({
    req(data_for_plot(), input$variables, length(input$variables) > 0)
    data_for_plot()[,input$variables] |>
      GGally::ggpairs(mapping = ggplot2::aes(color = !!as.name(input$variables))) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle=45,hjust = 1)) +
      ggplot2::ggtitle(input$variables) -> p
    p
  })

  # CA 2026/05/19,05/22追記/UIにも
  output$Crosstable_CA <- shiny::renderPlot({
    req(data_for_plot(), input$variables, length(input$variables) > 0,input$select_input_data_for_cross)
    res.CA <- FactoMineR::CA(table(
      data_for_plot()[[input$variables[1]]],
      data_for_plot()[[input$select_input_data_for_cross]]
    )
    )
  })

  # mosaic plot
  output$crosschart <- renderPlot({
    req(
      data_for_plot(),
      input$variables,
      length(input$variables) > 0,
      input$select_input_data_for_cross,
      input$select_input_data_for_cross != " "
    )
    .tbl <- table(data_for_plot()[[input$select_input_data_for_cross]],
                  data_for_plot()[[input$variables[1]]])  #select_input_data_for_hist
    .tbl.p <- round(100 * prop.table(.tbl ,margin = 1),1)
    tab <- ifelse(.tbl.p < 1, NA, .tbl.p)

    data_for_plot()[,c(input$select_input_data_for_cross,input$variables[1])] |> #,     #select_input_data_for_hist,
      vcd::structable() |>
      vcd::mosaic(shade=TRUE,las=2,
                  labeling=vcd::labeling_values
      )
  })
  # 層化mosaic plot
  output$crosschart2 <- renderPlot({
    validate(
      need(input$select_input_data_for_layer != " " && !is.null(input$select_input_data_for_layer),
           "【未選択エラー】『層化変数』を一つ選択してください。")
    )

    req(input$selected_categories, input$variables)

    # 【修正点1】選択されたカテゴリだけでデータを絞り込む
    plot_df <- data_for_plot() |>
      dplyr::filter(.data[[input$select_input_data_for_layer]] %in% input$selected_categories)

    .tbl <- table(plot_df[[input$select_input_data_for_cross]],
                  plot_df[[input$variables[1]]])  # select_input_data_for_hist
    .tbl.p <- round(100 * prop.table(.tbl ,margin = 1),1)
    tab <- ifelse(.tbl.p < 1, NA, .tbl.p)

    plot_df[,c(input$variables[1], #select_input_data_for_hist,
               input$select_input_data_for_cross,
               input$select_input_data_for_layer)] |>
      vcd::structable() |>
      vcd::mosaic(condvars = 3, # input$select_input_data_for_layer を指定
                  split_vertical = TRUE,# 分割は垂直
                  shade=TRUE,las=2,
                  labeling=vcd::labeling_values
      )
  })

#   # gtsummary でクロス表表示
#   output$my_gt_table <- render_gt(
# #    plot_df |> tbl_cross(col = input$select_input_data_for_cross,
#     data_for_plot() |> tbl_cross(col = input$select_input_data_for_cross,
#                           row = input$variables[1], #select_input_data_for_hist,
#                           percent = "row") |>
#       add_p(test="chisq.test") |>
#       bold_labels() |>
#       as_gt()
#   )
#
#   output$my_gt_table2 <- render_gt(
# #   plot_df |> tbl_cross(row = input$select_input_data_for_cross,
#     data_for_plot() |> tbl_cross(row = input$select_input_data_for_cross,
#                           col = input$variables[1], #select_input_data_for_hist,
#                           percent = "row") |>
#       add_p(test="chisq.test") |>
#       bold_labels() |>
#       as_gt()
#   )

  # 修正後（ gt::render_gt に変更 ）
  output$my_gt_table <- gt::render_gt(
    data_for_plot() |>
      gtsummary::tbl_cross(col = input$select_input_data_for_cross,
                           row = input$variables[1],
                           percent = "row") |>
      gtsummary::add_p(test = "chisq.test") |>
      gtsummary::bold_labels() |>
      gtsummary::as_gt()
  )

  output$my_gt_table2 <- gt::render_gt(
    data_for_plot() |>
      gtsummary::tbl_cross(row = input$select_input_data_for_cross,
                           col = input$variables[1],
                           percent = "row") |>
      gtsummary::add_p(test = "chisq.test") |>
      gtsummary::bold_labels() |>
      gtsummary::as_gt()
  )

  # chisq.test
  output$chisq_test <- renderPrint({
    res.chisq <- chisq.test(table(data_for_plot()[[input$select_input_data_for_cross]],
                                  data_for_plot()[[input$variables[1]]]),correct = FALSE)  #select_input_data_for_hist]))
    print(res.chisq)
  })

  output$chisq_test2 <- renderPrint({
    res.chisq <- chisq.test(table(data_for_plot()[[input$select_input_data_for_cross]],
                                  data_for_plot()[[input$variables[1]]]),correct = FALSE)  #select_input_data_for_hist]))
    print(res.chisq)
  })


  output$str_data <- renderPrint({
    str(data_for_plot())
  })

  # # gtsummay でMA表
  # output$MA_gt_table <- render_gt(
  #   data_for_plot() |> tbl_cross(row = input$select_input_data_for_cross,
  #                                 col = input$variables[1],#select_input_data_for_hist,
  #                                 percent = "row") |>
  #     add_p(test="chisq.test") |>
  #     bold_labels() |>
  #     as_gt()
  # )

  output$MA_gt_table <- gt::render_gt(
    data_for_plot() |>
      gtsummary::tbl_cross(row = input$select_input_data_for_cross,
                           col = input$variables[1],
                           percent = "row") |>
      gtsummary::add_p(test = "chisq.test") |>
      gtsummary::bold_labels() |>
      gtsummary::as_gt()
  )

  #    MA plot
  output$MAplot <- renderPlot({
    selected_vars <- input$variables  # 選択された変数を取得
    if (length(selected_vars) > 0) {
      # 選択された変数を用いてプロット
      selected_data <- data_for_plot()[, selected_vars, drop = FALSE]
      selected_data |> dplyr::reframe(across(everything(), ~ mean(. == 1,na.rm =TRUE))) |>
        tidyr::pivot_longer(cols = everything(), names_to = "Question", values_to = "Ratio") -> ratio_df

      ggplot2::ggplot(ratio_df, ggplot2::aes(x = Question, y = Ratio)) +
        ggplot2::geom_bar(stat = "identity", fill = "skyblue") +
        ggplot2::scale_y_continuous(labels = scales::percent_format()) +
        ggplot2::labs(title = selected_vars,
                      x = "質問項目",
                      y = "割合（% )") +
        ggplot2::theme_minimal()
    }
  })

  # MAplot Cleverland Dot Plot

  output$MAplot_Dot <- renderPlot({
    selected_vars <- input$variables  # 選択された変数を取得
    if (length(selected_vars) > 0) {
      # 選択された変数を用いてプロット
      selected_data <- data_for_plot()[, selected_vars, drop = FALSE]
      selected_data |> dplyr::reframe(across(everything(), ~ mean(. == 1,na.rm =TRUE))) |>
        tidyr::pivot_longer(cols = everything(), names_to = "Question", values_to = "Ratio") -> ratio_df

      ratio_df |>
        ggplot2::ggplot(ggplot2::aes(x=Ratio, y=reorder(Question,Ratio))) + # 並べ替え
        ggplot2::geom_point(size=3) +
        ggplot2::theme_bw() +
        ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                       panel.grid.minor.x = ggplot2::element_blank(),
                       panel.grid.major.y = ggplot2::element_line(colour="grey60",linetype="dashed")) +
        ggplot2::labs(title = selected_vars,
                      x = "割合（% )",y = "質問項目")
    }
  })

  # 層化MA折れ線グラフ
  # 1. 動的UIの生成（ボタンとチェックボックス）
  output$dynamic_stratify_filter <- renderUI({
    req(input$select_input_data_for_layer)
    if (trimws(input$select_input_data_for_layer) == "") return(NULL)

    # データの該当カラムからユニークな値を取得
    # raw_data() はお手元のデータフレーム用リアクティブ値に置き換えてください
    levs <- unique(data_for_plot()[[input$select_input_data_for_layer]])
    levs <- sort(levs) # 見やすくソート

    tagList(
      tags$div(style = "margin-top: 10px; margin-bottom: 5px;",
               actionButton("all_select", "全選択", class = "btn-xs"),
               actionButton("all_clear", "全解除", class = "btn-xs")
      ),
      checkboxGroupInput("selected_categories",
                         label = "表示するカテゴリ:",
                         choices = levs,
                         selected = levs) # 初期値は全選択
    )
  })

  # 2. 「全選択」ボタンが押された時の処理
  observeEvent(input$all_select, {
    levs <- unique(data_for_plot()[[input$select_input_data_for_layer]])
    updateCheckboxGroupInput(session, "selected_categories", selected = levs)
  })

  # 3. 「全解除」ボタンが押された時の処理
  observeEvent(input$all_clear, {
    updateCheckboxGroupInput(session, "selected_categories", selected = character(0))
  })

  #-------
  # 4. グラフ描画（フィルタリングを適用）
  output$MAplot_lineDot <- renderPlot({
    # バリデーション
    validate(
      need(trimws(input$select_input_data_for_layer) != "", "層化変数を選択してください。"),
      need(!is.null(input$selected_categories) && length(input$selected_categories) > 0, "カテゴリを1つ以上選択してください。")
    )

    req(input$selected_categories, input$variables)

    # 【修正点1】選択されたカテゴリだけでデータを絞り込む
    plot_df <- data_for_plot() |>
      dplyr::filter(.data[[input$select_input_data_for_layer]] %in% input$selected_categories)

    selected_vars <- input$variables
    gp_vari <- input$select_input_data_for_layer # 層化変数名

    if (length(selected_vars) > 0) {

      # 【修正点2】data_for_plot() ではなく、絞り込んだ「plot_df」を使って集計する！
      plot_df |>
        dplyr::group_by(!!!rlang::syms(gp_vari)) |>
        dplyr::reframe(
          度数 = dplyr::n(),
          across(all_of(selected_vars), ~ sum(. == 1, na.rm = TRUE) / dplyr::n(), .names = "ratio_{col}")
        ) -> MA_group_tbl

      # 集計結果が空でないか確認
      req(nrow(MA_group_tbl) > 0)

      MA_group_tbl |>
        dplyr::select(-度数) |>
        tidyr::pivot_longer(
          cols = starts_with("ratio_"),
          names_to = "variable",
          values_to = "value"
        ) -> df_long

      # グラフ描画
      ggplot2::ggplot(df_long, ggplot2::aes(x = !!as.name(gp_vari), y = value,
                                   shape = variable, group = variable)) +
        ggplot2::geom_line(ggplot2::aes(color = variable)) +
        ggplot2::geom_point(ggplot2::aes(color = variable), size = 4) +
        ggplot2::labs(x = gp_vari, y = "割合", shape = "変数", color = "変数") +
        ggplot2::theme_minimal() +
        ggplot2::scale_color_discrete() +
        ggplot2::scale_shape_manual(values = 1:length(selected_vars))
    }
  })



  # 層化MA折れ線グラフ(legend なし)
  output$MAplot_lineDot2 <- renderPlot({
    validate(
      need(input$select_input_data_for_layer != " " && !is.null(input$select_input_data_for_layer),
           "【未選択エラー】『層化変数』を一つ選択してください。")
    )
    req(input$selected_categories, input$variables)
    # 【修正点1】選択されたカテゴリだけでデータを絞り込む
    plot_df <- data_for_plot() |>
      dplyr::filter(.data[[input$select_input_data_for_layer]] %in% input$selected_categories)

    selected_vars <- input$variables

    if (length(selected_vars) > 0) {
      selected_data <- data_for_plot()[, selected_vars, drop = FALSE]
      gp_vari <- input$select_input_data_for_layer # 層化変数
      ## data_for plot ではなく、plot_dfを使う
      plot_df |> dplyr::group_by(!!!rlang::syms(gp_vari)) |>
        dplyr::reframe(度数=dplyr::n(),across(selected_vars, ~ sum(. == 1,na.rm = TRUE)/dplyr::n(),.names="ratio_{col}")) -> MA_group_tbl # ここで、行を選択すればいよい
      MA_group_tbl |> dplyr::select(-度数) |>
        tidyr::pivot_longer(cols = starts_with("ratio_"),  # ratio_で始まる列 (変数1〜8) をlong形式に変換
                            names_to = "variable",         # 変数名の列を"variable"として格納
                            values_to = "value") -> df_long

      ggplot2::ggplot(df_long, ggplot2::aes(x = !!as.name(gp_vari), y = value, #color = variable,
                                   shape =variable, group = variable)) +
        ggplot2::geom_line(ggplot2::aes(color = variable)) +  # 折れ線グラフ
        ggplot2::geom_point(ggplot2::aes(color = variable),size=4) + # ポイントを追加（必要なら )
        ggplot2::labs(x = gp_vari, y = "割合", shape = "変数",color = "変数") +  # 軸ラベルと凡例の設定
        ggplot2::theme_minimal() +  # 見た目をシンプルに
        ggplot2::scale_color_discrete() +
        #  scale_color_discrete(labels = names(df)[74:89]) + # 変数のラベルを設定
        ggplot2::scale_shape_manual(values = 1:length(selected_vars)) +
        ggplot2::theme(legend.position = 'none')
    }
  })


  # 層化MA warp Faset

  output$MAplot_lineDotwarp <- renderPlot({
    validate(
      need(input$select_input_data_for_layer != " " && !is.null(input$select_input_data_for_layer),
           "【未選択エラー】『層化変数』を一つ選択してください。")
    )
    req(input$selected_categories, input$variables)
    # 【修正点1】選択されたカテゴリだけでデータを絞り込む
    plot_df <- data_for_plot() |>
      dplyr::filter(.data[[input$select_input_data_for_layer]] %in% input$selected_categories)

    selected_vars <- input$variables # 選択された変数群
    if (length(selected_vars) > 0) {
      selected_data <- data_for_plot()[, selected_vars, drop = FALSE]

      gp_vari <- input$select_input_data_for_layer # 層化する変数
      plot_df |> dplyr::group_by(!!!rlang::syms(gp_vari)) |>
        dplyr::reframe(度数=dplyr::n(),across(selected_vars, ~ sum(. == 1,na.rm = TRUE)/dplyr::n(),.names="ratio_{col}")) -> MA_group_tbl

      MA_group_tbl |> dplyr::select(-度数) |>
        tidyr::pivot_longer(cols = starts_with("ratio_"),  # ratio_で始まる列 (変数1〜8) をlong形式に変換
                            names_to = "variable",         # 変数名の列を"variable"として格納
                            values_to = "value") -> df_long


      ggplot2::ggplot(df_long, ggplot2::aes(x = !!as.name(gp_vari), y = value, color = variable, group = variable)) +
        ggplot2::geom_line() + ggplot2::geom_point() +
        ggplot2::facet_wrap(~ variable,ncol=3) +# scales = "free_y") + # 各変数ごとにfacetで分割
        ggplot2::labs(x = "Group", y = "Value") +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "none",axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))  # ラベルを90度回転

    }
  })


  # Grid Mosaic
  #

  output$GridAnswer_mosaic <- renderPlot({
    req(input$variables)
    selected_vars <- input$variables
    count_categories <- function(x) {
      table(factor(x, levels = c("++", "+", "-", "--", "DK", "無回答"), exclude = NULL))
    }

    # df の 1 列目から 20 列目の各列ごとにカテゴリを集計
    category_count_tbl <- data_for_plot() |>
      #dplyr::summarise(across(selected_vars, ~ count_categories(.)))
      dplyr::reframe(across(selected_vars, ~ count_categories(.)))

    category_count_tbl |> as.matrix() -> cat_tbl
    rownames(cat_tbl) <- c("++","+","-","--","DK","無回答")
    cat_tbl

    rownames(t(cat_tbl)) -> rnames
    t(cat_tbl) |> tibble::as.tibble() |> dplyr::mutate(ID=rnames,IDn=1:length(rnames)) |>
      dplyr::mutate(Like=`++`+`+`) |>
      dplyr::arrange(desc(Like)) |>
      dplyr::select(IDn) |> unlist() |>
      setNames(NULL) -> order_vec

    t(cat_tbl)[order_vec,] |>
      vcd::mosaic(shade = TRUE,rot_labels = c(0, 0),
                  margins=c(left=9,top=5),just_labels=c(left="right",top="left"))

  })

  ## GridAnswer CA

  output$GridAnswer_CA <- renderPlot({
    selected_vars <- input$variables
    count_categories <- function(x) {
      table(factor(x, levels = c("++", "+", "-", "--", "DK", "無回答"), exclude = NULL))
    }

    # df の 1 列目から 20 列目の各列ごとにカテゴリを集計
    category_count_tbl <- data_for_plot() |>
      #dplyr::summarise(across(selected_vars, ~ count_categories(.)))
      dplyr::reframe(across(selected_vars, ~ count_categories(.)))

    category_count_tbl |> as.matrix() -> cat_tbl
    rownames(cat_tbl) <- c("++","+","-","--","DK","無回答")
    cat_tbl

    res.CA <- FactoMineR::CA(t(cat_tbl))

  })

  # Grid Mosaic 2 LK/DLK

  output$GridAnswer2_mosaic <- renderPlot({
    grid_ptn <- c("A","B","C","D","E","NA") # 仮のパターン
    selected_vars <- input$variables
    count_categories <- function(x) {
      table(factor(x, levels = grid_ptn,
                   exclude = NULL))
    }

    # df の 1 列目から 20 列目の各列ごとにカテゴリを集計
    category_count_tbl <- data_for_plot() |>
      dplyr::reframe(across(selected_vars, ~ count_categories(.)))

    category_count_tbl |> as.matrix() -> cat_tbl
    rownames(cat_tbl) <- grid_ptn
    cat_tbl

    rownames(t(cat_tbl)) -> rnames
    t(cat_tbl) |> as.tibble() |> dplyr::mutate(ID=rnames,IDn=1:length(rnames)) |>
      #        mutate(Like=`++`+`+`) |>
      dplyr::arrange(desc(A)) |>
      dplyr::select(IDn) |> unlist() |>
      setNames(NULL) -> order_vec

    #      t(cat_tbl)[order_vec,]
    t(cat_tbl) |> vcd::mosaic(shade = TRUE,rot_labels = c(0, 0),
                               margins=c(left=9,top=5),just_labels=c(left="right",top="left"))

  })


  # GridAnswer2 CA
  output$GridAnswer2_CA <- renderPlot({
    grid_ptn <- c("A","B","C","D","E","NA") # 仮のパターン
    selected_vars <- input$variables
    count_categories <- function(x) {
      table(factor(x, levels = grid_ptn,#c("++", "+", "-", "--", "DK", "無回答"),
                   exclude = NULL))
    }

    # df の 1 列目から 20 列目の各列ごとにカテゴリを集計
    category_count_tbl <- data_for_plot() |>
      dplyr::reframe(across(selected_vars, ~ count_categories(.)))

    category_count_tbl |> as.matrix() -> cat_tbl
    rownames(cat_tbl) <- grid_ptn #c("++","+","-","--","DK","無回答")
    cat_tbl

    res.CA <- FactoMineR::CA(t(cat_tbl))

  })


  # Grid Mosaic Genenral

  output$GridAnswerG_mosaic <- renderPlot({
    req(data_for_plot(), input$variables, length(input$variables) > 0)
#    req(input$variables)
    selected_vars <- input$variables

    # 【★超重要・一撃必殺の防弾処理】
    # 選択された列のデータを、Factor型から「ただのプレーンな文字列型(character)」に強制変換する
    # これにより、3838環境下での "1", "2" への化けを根絶します
    cleaned_data <- data_for_plot() |>
      dplyr::mutate(across(dplyr::all_of(selected_vars), as.character))

    # 1. 以降の集計は、すべてこの文字列化した「cleaned_data」をベースに行う！
    vectors <- purrr::map(selected_vars, ~ {
      cleaned_data |>
        dplyr::pull(!!sym(.x)) |>
        unique()
    })

    union_all <- reduce(vectors, union)
    union_all <- ifelse(is.na(union_all), "NA", union_all)

    # カテゴリを集計するための関数（ここも cleaned_data を見るように徹底）
    count_categories <- function(x) {
      table(factor(x, levels = union_all, exclude = NULL))
    }

    # 2. selected_vars の各列ごとにカテゴリを集計
    category_count_tbl <- cleaned_data |>
      dplyr::reframe(across(dplyr::all_of(selected_vars), ~ count_categories(.)))

    # （※これ以降の3、4、5、6の行列変換・描画コードは、今のままで全く触らなくて大丈夫です！）

    # 3. マトリックス（行列）形式に変換し、行名（回答カテゴリ）を付与
    cat_tbl <- as.matrix(category_count_tbl)
    rownames(cat_tbl) <- union_all

    # 4. 行列を転置して、Rの標準機能（order）で頑丈に並び替え
    mat_t <- t(cat_tbl)
    order_vec <- order(mat_t[, 1], decreasing = TRUE)
    sorted_matrix <- mat_t[order_vec, , drop = FALSE]

    # 5. 2次元のクロス表オブジェクト（table）に完全固定
    final_table <- as.table(sorted_matrix)
    names(dimnames(final_table)) <- c("Question", "Response")

    # 【★この1行だけを仕込む！】
    print("--- 3838環境での final_table のナマの構造 ---")
    str(final_table)

    # 6. 完全に構造が保証された table を mosaic に投入 2026/07/10　shade=TRUEを削除。col_matrixをつくり、使う

    final_table |>
      vcd::mosaic(#shade = TRUE,
                  rot_labels = c(0, 0),
                  margins = c(left = 12, top = 5),
                  just_labels = c(left = "right", top = "left"))
  })


  #### Degug用

  # 3838ポートの画面に、データの生構造を送り出すデバッグプローブ
  output$debug_str <- renderPrint({
    req(input$variables)

    # グラフを描く直前と「100%完全に同じデータ」をここでもう一度再現する
    selected_vars <- input$variables
    vectors <- purrr::map(selected_vars, ~ {
      data_for_plot() |> dplyr::pull(!!sym(.x)) |> unique()
    })
    union_all <- reduce(vectors, union)
    union_all <- ifelse(is.na(union_all), "NA", union_all)
    count_categories <- function(x) table(factor(x, levels = union_all, exclude = NULL))
    category_count_tbl <- data_for_plot() |> dplyr::reframe(across(dplyr::all_of(selected_vars), ~ count_categories(.)))
    cat_tbl <- as.matrix(category_count_tbl)
    rownames(cat_tbl) <- union_all
    mat_t <- t(cat_tbl)
    order_vec <- order(mat_t[, 1], decreasing = TRUE)
    sorted_matrix <- mat_t[order_vec, , drop = FALSE]
    final_table <- as.table(sorted_matrix)

    # 【最重要】画面に向かって、型と構造を吐き出す
    str(final_table)
  })




  # GridAnswerG CA
  output$GridAnswerG_CA <- renderPlot({
    req(data_for_plot(), input$variables, length(input$variables) > 0)
#    req(input$variables)
    selected_vars <- input$variables
    #browser()
    vectors <- purrr::map(selected_vars, ~ {
      data_for_plot() |> dplyr::select(selected_vars) |>
        dplyr::count(!!sym(.x)) |>  # 選択した列ごとにカウント
        dplyr::pull(1)               # 最初の列のユニークな値を取得
    })

    union_all <- reduce(vectors, union)

    count_categories <- function(x) {
      table(factor(x, levels = union_all, exclude = NULL))
    }

    # df のselected_varsの各列ごとにカテゴリを集計
    category_count_tbl <- data_for_plot() |>
      #dplyr::summarise(across(selected_vars, ~ count_categories(.)))
      dplyr::reframe(across(selected_vars, ~ count_categories(.)))

    category_count_tbl |> as.matrix() -> cat_tbl
    rownames(cat_tbl) <- union_all #grid_ptn
    cat_tbl

    res.CA <- FactoMineR::CA(t(cat_tbl))

  })


  # タイムスタンプを取得
  output$timestamp <- renderText({
    file_info <- file.info("app.R")  # ファイル情報を取得
    timestamp <- file_info$mtime     # 最終更新日時
    paste("app.Rの最終更新:", format(timestamp, "%Y-%m-%d %H:%M:%S"))
  })

  # 度数分布表
  output$simple_table <- DT::renderDataTable({ #renderTable({#
    table(data_for_plot()[[input$variables[1]]]) -> tmp #select_input_data_for_hist]) -> tmp
    round(100*prop.table(tmp),1) -> tmp2
    data.frame(tmp,rate=tmp2)[,c(1,2,4)]
  })

  output$simple_table2 <- DT::renderDataTable({ #renderTable({#
    table(data_for_plot()[[input$select_input_data_for_hist]]) -> tmp
    round(100*prop.table(tmp),1) -> tmp2
    data.frame(tmp,rate=tmp2)[,c(1,2,4)]
  })


  output$table_for_plot <- DT::renderDataTable({
    data_for_plot() |> dplyr::select(input$variables)
  })

}
