# R/globals.R

# 1. Google Fonts から Noto Sans JP を追加
sysfonts::font_add_google("Noto Sans JP", "noto")

# 2. showtext を有効化
showtext::showtext_auto()

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
