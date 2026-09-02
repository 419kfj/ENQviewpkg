# R/globals.R

library(showtext)

# Linux OS に入った Noto Sans CJK JP を指定
sysfonts::font_add("noto", "/usr/share/fonts/truetype/noto/NotoSansCJK-Regular.ttc")
showtext::showtext_auto(TRUE)

# ggplot2 のデフォルトに設定
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
