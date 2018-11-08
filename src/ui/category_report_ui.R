library(shiny)
library(visNetwork)
source("common.R")

ui <- shiny::tabPanel(
  
  title = "Category Report",
  
  # Define UI for dataset viewer app ----
  fluidPage(
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
      
      # Sidebar panel for inputs ----
      sidebarPanel(
        uiOutput("siteCategorySelectUI"),
        uiOutput("siteCategoryOutputUI"),
        textOutput(outputId = "totalSiteCountCategoryReportUI"),
        width = 3
      ),
      
      # Main panel for displaying outputs ----
      mainPanel(
        img(src='dulogo.png', align = "right", width="15%"),
        width = 9
      )
    ),
    downloadButton('downloadCategoryReport',"Download the data"),
    tabPanel('Display Sites', DT::dataTableOutput('categorySitesListReport'))
  )
)