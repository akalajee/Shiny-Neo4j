library(shiny)
library(visNetwork)
source("common.R")

ui <- shiny::tabPanel(
  
    title = "Sites Classification",

    # Define UI for dataset viewer app ----
   fluidPage(
  
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
    
      # Sidebar panel for inputs ----
      sidebarPanel(
        uiOutput("siteNameUI"),
        uiOutput("displaySiteCountSliderUI"),
        checkboxInput(inputId = "showSource", label = "Show source?", value = FALSE),
        textOutput(outputId = "totalSiteCountUI"),
        textOutput(outputId = "siteClassificationUI"),
        width = 3
      ),
    
    # Main panel for displaying outputs ----
      mainPanel(
        img(src='dulogo.png', align = "right", width="20%", style="margin-right: -65px; margin-top: -30px;"),
        visNetworkOutput("network", height = "600px", width = "100%"),
        width = 9
      )
    ),
    downloadButton('download',"Download the data"),
    tabPanel('Display Sites', DT::dataTableOutput('sitesList'))
  )
)