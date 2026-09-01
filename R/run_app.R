#' ENQview_lite アプリを起動する関数
#'
#' @param df 作業中のデータフレーム（オプショナル）。渡された場合はそのデータを優先して使用します。
#' @export
run_ENQview <- function(df = NULL) {


  # # ★ここに追加：関数が呼ばれたか＆dfが入っているか確認
  # cat("\n========================================\n")
  # cat("[DEBUG 1] run_ENQview 起動成功\n")
  # cat("[DEBUG 1] df is NULL?:", is.null(df), "\n")
  # if (!is.null(df)) cat("[DEBUG 1] df rows:", nrow(df), "\n")
  # cat("========================================\n\n")

  # バージョン情報の取得（パッケージインストール時とローカル実行の両対応）
  app_version <- tryCatch({
    as.character(utils::packageVersion("ENQviewpkg")) # ※ご自身のパッケージ名に合致させてください
  }, error = function(e) {
    if (file.exists("DESCRIPTION")) {
      desc_info <- read.dcf("DESCRIPTION")
      desc_info[1, "Version"]
    } else {
      "1.0.0"
    }
  })

  shiny::shinyApp(
    ui = app_ui(app_version = app_version),
#    ui = app_ui(app_version = "1.0.0"),
    server = function(input, output, session) {
      app_server(input, output, session, initial_df = df)
    }
  )
}
