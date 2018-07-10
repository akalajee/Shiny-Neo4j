library(shiny)
library(visNetwork)
source("common.R")

relation_type_query = "MATCH (n)-[r]-(m) RETURN distinct type(r) AS `Connection Type`"
relation_type = cypher(graph, relation_type_query)

ui <- shiny::tabPanel(
  
    title = "Directly connected",
    

    # Define UI for dataset viewer app ----
   fluidPage(
  
    # App title ----
    titlePanel("Sites"),
  
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
    
      # Sidebar panel for inputs ----
      sidebarPanel(
        
        # Input: Text for providing a caption ----
        selectizeInput(inputId = "connectiontype",
                       label = "Choose a connection type:",
                       choices = relation_type
        ),
        uiOutput("secondSelection"),
        uiOutput("slider"),
        #uiOutput("countInput"),
        textOutput(outputId = "totalSiteCount")
      ),
    
    # Main panel for displaying outputs ----
      mainPanel(
        visNetworkOutput("network", height = "600px", width = "100%")
      )
    ),
    downloadButton('download',"Download the data"),
    tabPanel('Display Sites', DT::dataTableOutput('sitesList'))
  )
)