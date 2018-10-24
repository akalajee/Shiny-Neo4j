library(shiny)
library(visNetwork)
source("common.R")

relation_type_query = "MATCH (n)-[r]-(m) RETURN distinct type(r) AS `Connection Type`"
relation_type = cypher(graph, relation_type_query)

ui <- shiny::tabPanel(
  
    title = "Expanded network",

    # Define UI for dataset viewer app ----
   fluidPage(
  
    #img(src='dulogo.png'), #, align = "right", width="100", height="100"),
    # App title ----
    titlePanel("Sites classification"),
  
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
    
      # Sidebar panel for inputs ----
      sidebarPanel(
        uiOutput("secondSelectionExpanded"),
        uiOutput("sliderExpanded"),
        #uiOutput("countInputExpanded"),
        checkboxInput(inputId = "showSourceExpanded", label = "Show source?", value = FALSE),
        textOutput(outputId = "totalSiteCountExpanded"),
        textOutput(outputId = "siteClassification"),
        width = 3
      ),
    
    # Main panel for displaying outputs ----
      mainPanel(
        img(src='dulogo.png', align = "right", width="20%"),
        visNetworkOutput("networkExpanded", height = "600px", width = "100%")
      )
    ),
    downloadButton('downloadExpanded',"Download the data"),
    tabPanel('Display Sites', DT::dataTableOutput('sitesListExpanded'))
  )
)