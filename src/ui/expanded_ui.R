library(shiny)
library(visNetwork)
source("common.R")

relation_type_query = "MATCH (n)-[r]-(m) RETURN distinct type(r) AS `Connection Type`"
relation_type = cypher(graph, relation_type_query)

ui <- shiny::tabPanel(
  
    title = "Expanded network",

    # Define UI for dataset viewer app ----
   fluidPage(
  
    # App title ----
    titlePanel("Sites"),
  
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
    
      # Sidebar panel for inputs ----
      sidebarPanel(
        uiOutput("secondSelectionExpanded"),
        uiOutput("sliderExpanded"),
        uiOutput("countInputExpanded"),
        textOutput(outputId = "totalSiteCountExpanded")
      ),
    
    # Main panel for displaying outputs ----
      mainPanel(
        visNetworkOutput("networkExpanded", height = "600px", width = "100%")
      )
    ),
    tabPanel('Display Sites', DT::dataTableOutput('sitesListExpanded'))
  )
)