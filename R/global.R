# R/globals.R

library(showtext)

# 1. フォントの取得と登録（"noto" という名前で登録）
sysfonts::font_add_google("Noto Sans JP", "noto")

# 2. showtext の有効化（TRUE を明示して OK です！）
showtext_auto(TRUE)

# 3. ggplot2 全体のデフォルトフォントを "noto" に切り替える（★これが必須です！）
ggplot2::theme_set(ggplot2::theme_minimal(base_family = "noto"))

utils::globalVariables(c(
  "++", ".data", "A", "IDn", "Like", "Question", "Ratio", "V1",
  "across", "actionButton", "all_of", "as.tibble", "checkboxGroupInput",
  "chisq.test", "desc", "everything", "fileInput", "filter", "need",
  "observe", "observeEvent", "rate", "reactive", "reactiveVal", "reduce",
  "renderPlot", "renderPrint", "renderTable", "renderText", "renderUI",
  "reorder", "req", "selectInput", "setNames", "showNotification",
  "starts_with", "str", "tagList", "tags", "uiOutput",
  "updateCheckboxGroupInput", "updateSelectInput", "validate",
  "value", "variable", "度数"
))
