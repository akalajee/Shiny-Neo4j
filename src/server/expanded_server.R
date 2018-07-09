library(shiny)
library(visNetwork)
source("common.R")


  output$secondSelectionExpanded <- renderUI({
    
    
    all_node_query = paste("MATCH (n)-[]->() 
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
    selectizeInput(inputId = "sitenameExpanded",
                   label = "Choose source site name:",
                   choices = all_node_names
    )
    
  })
  
  
  output$sliderExpanded <- renderUI({
    
    # Thought to reactively show max number of nodes to display, then based on experiment
    # Its decided to staticly maximum display 100 nodes
    # sliderInput(inputId = "maxnodes", label = "Maximum displayed nodes", min = 1, max = reactiveTotalNodeCount(), value = 5, step = 1)
    totalNodeCountExpanded = reactiveTotalNodeCountExpanded()
    #sliderMaximum = min(c(100, reactiveTotalNodeCount()))
    sliderMaximumExpanded = totalNodeCountExpanded
    sliderInput(inputId = "maxnodesExpanded", label = "Maximum displayed nodes", min = 1, max = sliderMaximumExpanded, value = 5, step = 1)
    
  })
  
  
  output$networkExpanded <- renderVisNetwork({
    
    nodeName = input$sitenameExpanded
    maxnodes = input$maxnodesExpanded
    
    if(!is.null(nodeName) && nodeName != "")
    {
      
      total_node_query = paste("MATCH p=shortestPath(
                                  (src{name:'",nodeName,"'})-[*]->(dst)
                               ) where src.name <> dst.name
                               WITH DISTINCT (dst.name) AS m
                               RETURN count(m)+1", sep="")
      
      total_node_count = cypher(graph, total_node_query)[1,1]
      
      
      reactiveTotalNodeCountExpanded(total_node_count)
      output$totalSiteCountExpanded = renderText({paste("Total Nodes Count: ", total_node_count)})
      
      node_query = paste("
                       MATCH p=shortestPath((src{name:'",nodeName,"'})-[r*]->(dst)) 
                       where src.name <> dst.name 
                       with distinct nodes(p) as t
                       unwind t as f
                       with distinct f as m
                       RETURN m.name AS id,
                       m.name AS label,
                       LABELS(m)[0] AS group
                       LIMIT ",maxnodes,"
                       ", sep="")
      
      edge_query = paste("
                         MATCH p=shortestPath((src{name:'",nodeName,"'})-[r*]->(dst))
                         where src.name <> dst.name
                         with extract(x IN r | {link_id: id(x), start: startNode(x).name, end: endNode(x).name, type: type(x)}) AS record_list, LABELS(dst)[0] AS group
                         unwind record_list as record
                         with distinct record.link_id as link_id, record.start as from, record.end AS to, record.type AS label, group
                         return from,  to,label, group
                         ", sep = "")
      
      nodes = cypher(graph, node_query)
      edges = cypher(graph, edge_query)
      
      visNetwork(nodes, edges) %>% 
        visEdges(shadow = FALSE,
                 arrows =list(to = list(enabled = TRUE, scaleFactor = 1)),
                 color = list(color = "lightblue", highlight = "pink")) %>%
        visLayout(randomSeed = 12) # to have always the same network
    }
    
  })
  

