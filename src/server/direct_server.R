library(shiny)
library(visNetwork)
source("common.R")


  output$secondSelection <- renderUI({
    
    
    all_node_query = paste("MATCH (n)-[r:",input$connectiontype,"]->() 
                           WITH DISTINCT (n) AS m
                           order by m.name asc
                           RETURN m.name AS sitename")
    
    all_node_names_list = cypherToList(graph, all_node_query)
    node_matrix = matrix(unlist(all_node_names_list))
    
    all_node_names = list(node_matrix)
    if(length(node_matrix) > 1)
    {
      all_node_names = list("Site name" = node_matrix)  
    }
    
    # Input: Text for providing a caption ----
    selectizeInput(inputId = "sitename",
                   label = "Choose source site name:",
                   choices = all_node_names
    )
    
  })
  
  #output$countInput <- renderUI({
    
      #textInput(inputId = "totalSiteCount",
      #            label = "Total Nodes count",
      #            value = reactiveTotalNodeCount()
      #          )
    
      #textOutput(outputId = "totalSiteCount")
    
  #})
  
  output$slider <- renderUI({
    
    # Thought to reactively show max number of nodes to display, then based on experiment
    # Its decided to staticly maximum display 100 nodes
    # sliderInput(inputId = "maxnodes", label = "Maximum displayed nodes", min = 1, max = reactiveTotalNodeCount(), value = 5, step = 1)
    totalNodeCount = reactiveTotalNodeCount()
    #sliderMaximum = min(c(100, reactiveTotalNodeCount()))
    sliderMaximum = totalNodeCount
    sliderInput(inputId = "maxnodes", label = "Maximum displayed nodes", min = 1, max = sliderMaximum, value = 5, step = 1)
    
  })
  
  
  output$network <- renderVisNetwork({
    
    connectiontype = isolate(input$connectiontype)
    nodeName = input$sitename
    maxnodes = input$maxnodes
    
    if(!is.null(nodeName) && nodeName != "")
    {
      
      total_node_query = paste("MATCH ({name:'",nodeName,"'})-[r:",connectiontype,"]->(n) 
                               WITH DISTINCT (n) AS m
                               RETURN count(m)+1", sep="")
      
      total_node_count = cypher(graph, total_node_query)[1,1]
      
      reactiveTotalNodeCount(total_node_count)
      output$totalSiteCount = renderText({paste("Total Nodes Count: ", total_node_count)})
      
      node_query = paste("MATCH ({name:'",nodeName,"'})-[r:",connectiontype,"]->(n) 
                       WITH DISTINCT (n) AS m
                       RETURN m.name AS id,
                       m.name AS label,
                       LABELS(m)[0] AS group
                       LIMIT ",maxnodes," - 1
                       UNION
                       MATCH (n{name:'",nodeName,"'})-[r:",connectiontype,"]->() 
                       WITH DISTINCT (n) AS m
                       RETURN m.name AS id,
                       m.name AS label,
                       LABELS(m)[0] AS group
                       LIMIT 1
                       ", sep="")
      
      edge_query = paste("MATCH (source{name:'",nodeName,"'})-[r:",connectiontype,"]->(dist) RETURN source.name as from, 	
                       dist.name AS to,
                       TYPE(r) AS label LIMIT ",maxnodes," + 10
                       ", sep="")
      
      nodes = cypher(graph, node_query)
      edges = cypher(graph, edge_query)
      
      visNetwork(nodes, edges) %>% 
        visEdges(shadow = FALSE,
                 arrows =list(to = list(enabled = TRUE, scaleFactor = 1)),
                 color = list(color = "lightblue", highlight = "pink")) %>%
        visLayout(randomSeed = 12) # to have always the same network
    }
    
  })
  

