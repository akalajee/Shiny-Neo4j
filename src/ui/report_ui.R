library(shiny)
library(visNetwork)
source("common.R")

ui <- shiny::tabPanel(
  
  title = "Classification Report",
  
  # Define UI for dataset viewer app ----
  fluidPage(
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
      
      # Sidebar panel for inputs ----
      sidebarPanel(
        uiOutput("siteClassificationSelectUI"),
        uiOutput("siteClassificationOutputUI"),
        textOutput(outputId = "totalSiteCountReportUI"),
        width = 3
      ),
      
      # Main panel for displaying outputs ----
      mainPanel(
        img(src='dulogo.png', align = "right", width="20%", style="margin-right: -65px; margin-top: -30px;"),
        width = 9
      )
    ),
    downloadButton('downloadReport',"Download the data"),
    tabPanel('Display Sites', DT::dataTableOutput('sitesListReport'))
  )
)