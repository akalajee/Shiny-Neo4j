require(shiny)
require(visNetwork)

shiny::shinyUI(shiny::navbarPage(
  title = "Mobile Sites",
  source("./src/ui/expanded_ui.R", local = TRUE)$value,
  source("./src/ui/direct_ui.R", local = TRUE)$value
))
