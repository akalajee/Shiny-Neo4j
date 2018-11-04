library(shiny)
library(visNetwork)
library(R.cache)
source("common.R")

setCacheRootPath("~/.Rcache")

output$siteClassificationSelectUI <- renderUI({
  
  # Input: Text for providing a caption ----
  selectizeInput(inputId = "classification",
                 label = "Choose site classification:",
                 choices = list(Classification = c("BSC", "A", "B", "C", "D"))
  )

})


output$siteClassificationOutputUI <- renderUI({
  
  classification = input$classification
  funcName = "getReportNodeData"

  getReportNodeData <- function(classification)
  {
    if(!is.null(classification) && classification != "")
    {
      total_classified_node_count_query = paste("MATCH (n{classification:'",classification,"'})
                                         RETURN count(n)
                                         ", sep="")
      
      total_node_count = cypher(graph, total_classified_node_count_query)[1,1]
      total_node_count = ifelse(total_node_count >= 1, total_node_count, 1)
      
      total_classified_node_query = paste("MATCH (m{classification:'",classification,"'})
                                           RETURN m.name as `Site name`, replace(LABELS(m)[0] + ' - ' +  coalesce(m.type2,'$') + ' - ' + coalesce(m.type3,'$'), ' - $', '') AS group, apoc.text.join((m.cat),\", \") as category, m.olt_customers as olt_customers
                                          ", sep="")
      
      total_nodes = cypher(graph, total_classified_node_query)
      
      return_var_list = list(
        total_node_count = total_node_count,
        total_nodes = total_nodes
      )
    }
  }
  
  key = list(funcName, classification)
  node_count_var_list = loadCache(key)
  if (is.null(node_count_var_list)) {
    node_count_var_list = getReportNodeData(classification)
    saveCache(node_count_var_list, key=key)
  }
  
  total_node_count = node_count_var_list[["total_node_count"]]
  total_nodes = node_count_var_list[["total_nodes"]]
  
  #output-display-1
  
  if (!is.null(total_nodes)) {
    reactiveNodeListReport(total_nodes)
  }
  
  output$totalSiteCountReportUI = renderText({paste("Total Sites Count: ", total_node_count)})
  
  return()
  
})

output$sitesListReport <- DT::renderDataTable({
  datalist = reactiveNodeListReport()
  if(length(datalist) > 0)
  {
    DT::datatable(datalist, options = list(pageLength = 25))
  }
})

output$downloadReport <- downloadHandler(
  filename = function() {
    paste("sitesClassificationReport-",input$classification, Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(reactiveNodeListReport(), file)
  }
)

