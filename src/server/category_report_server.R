library(shiny)
library(visNetwork)
library(R.cache)
source("common.R")

output$siteCategorySelectUI <- renderUI({
  
  all_categories_cql = paste("match (n)
                              with distinct(n.cat) as c
                              unwind c as d
                              return distinct d as Category", sep="")
  
  all_categories = cypher(graph, all_categories_cql)
  
  # Input: Text for providing a caption ----
  selectizeInput(inputId = "category",
                 label = "Choose site category:",
                 choices = list(Category = all_categories)
  )

})


output$siteCategoryOutputUI <- renderUI({
  
  category = input$category
  funcName = "getCategoryReportNodeData"
  
  getCategoryReportNodeData <- function(category)
  {
    if(!is.null(category) && category != "")
    {
      total_classified_node_count_query = paste("MATCH (n:Site)
                                         where any(x IN n.cat WHERE x = \"",category,"\")
                                         RETURN count(n)
                                         ", sep="")
      
      total_node_count = cypher(graph, total_classified_node_count_query)[1,1]
      total_node_count = ifelse(total_node_count >= 1, total_node_count, 1)
      
      total_classified_node_query = paste("MATCH (m:Site)
                                           where any(x IN m.cat WHERE x = \"",category,"\")
                                           RETURN m.name as `Site name`, apoc.text.join((m.cat),\", \") as group, m.olt_customers as olt_customers
                                          ", sep="")
      
      total_nodes = cypher(graph, total_classified_node_query)
      
      return_var_list = list(
        total_node_count = total_node_count,
        total_nodes = total_nodes
      )
    }
  }
  
  key = list(funcName, category)
  node_count_var_list = loadCache(key)
  if (is.null(node_count_var_list)) {
    node_count_var_list = getCategoryReportNodeData(category)
    saveCache(node_count_var_list, key=key)
  }
  
  total_node_count = node_count_var_list[["total_node_count"]]
  total_nodes = node_count_var_list[["total_nodes"]]
  
  #output-display-1
  
  if (!is.null(total_nodes)) {
    reactiveNodeListReport(total_nodes)
  }
  
  output$totalSiteCountCategoryReportUI = renderText({paste("Total Sites Count: ", total_node_count)})
  
  return()
  
})

output$categorySitesListReport <- DT::renderDataTable({
  datalist = reactiveNodeListReport()
  if(length(datalist) > 0)
  {
    DT::datatable(datalist, options = list(pageLength = 25))
  }
})

output$downloadCategoryReport <- downloadHandler(
  filename = function() {
    paste("SitesCategoryReport-",input$category, "-" , Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(reactiveNodeListReport(), file)
  }
)

