require(shiny)

shiny::shinyUI(shiny::navbarPage(
  title = "Mobile Sites",
  source("./src/ui/sites_ui.R", local = TRUE)$value,
  source("./src/ui/classification_report_ui.R", local = TRUE)$value,
  source("./src/ui/category_report_ui.R", local = TRUE)$value,
  tags$head(tags$link(rel="shortcut icon", href="favicon.ico"))
))
