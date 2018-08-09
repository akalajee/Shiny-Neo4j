library(shiny)
library(visNetwork)
source("common.R")


  output$secondSelectionExpanded <- renderUI({
    
    
    all_node_query = paste("MATCH (n)
                           RETURN n.name AS sitename
                           order by n.name asc
                           ")
    
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
    
    totalNodeCountExpanded = reactiveTotalNodeCountExpanded()
    sliderMaximumExpanded = totalNodeCountExpanded
    sliderInput(inputId = "maxnodesExpanded", label = "Maximum displayed nodes", min = 1, max = sliderMaximumExpanded, value = sliderMaximumExpanded, step = 1)
    
  })
  
  
  output$networkExpanded <- renderVisNetwork({
    
    nodeName = input$sitenameExpanded
    maxnodes = input$maxnodesExpanded
    showSource = input$showSourceExpanded
    
    if(!is.null(nodeName) && nodeName != "")
    {
      
      total_node_query = paste("MATCH p=shortestPath(
                                  (src{name:'",nodeName,"'})-[:Link*..30]->(dst)
                                ) where id(src) <> id(dst)
                               UNWIND (nodes(p)) as my_nodes
                               WITH DISTINCT(id(my_nodes)) as n
                               RETURN count(n)
                               ", sep="")
      if(showSource)
      {
        total_node_query = paste("MATCH p=shortestPath(
                                  (dst{name:'",nodeName,"'})<-[:Link*..30]-(src)
                                  ) where id(src) <> id(dst) and src.bsc = true
                                 UNWIND (nodes(p)) as my_nodes
                                 WITH DISTINCT(id(my_nodes)) as n
                                 RETURN count(n)
                                 ", sep="")
      }
      
      total_node_count = cypher(graph, total_node_query)[1,1]
      total_node_count = ifelse(total_node_count >= 1, total_node_count, 1)
      
      reactiveTotalNodeCountExpanded(total_node_count)
      output$totalSiteCountExpanded = renderText({paste("Total Nodes Count: ", total_node_count)})
      
      siteClassification = ifelse(total_node_count >= 21, 'A',
                              ifelse(total_node_count >= 11 && total_node_count <= 20, 'B',
                                        ifelse(total_node_count >= 2 && total_node_count <= 19, 'C',
                                                  'D'
                                               )
                                     )
                              )
      output$siteClassification = renderText({paste("Site Classification: ", siteClassification)})
      if(showSource)
      {
        output$siteClassification = renderText({""})
      }
      
      node_limited_query = paste("
                                 MATCH p=shortestPath(
                            	   (src{name:'",nodeName,"'})-[:Link*..30]->(dst)
                                  ) where id(src) <> id(dst)
                                 unwind nodes(p) AS k
                                 with distinct(k) as m
                                 RETURN m.name AS id,
                                 m.name AS label,
                                 replace(LABELS(m)[0] + ' - ' +  coalesce(m.type2,'$') + ' - ' + coalesce(m.type3,'$'), ' - $', '') AS group
                                 LIMIT ",maxnodes,"
                                 UNION MATCH (m{name:'",nodeName,"'})
                                 RETURN m.name AS id,
                                 m.name AS label,
                                 replace(LABELS(m)[0] + ' - ' +  coalesce(m.type2,'$') + ' - ' + coalesce(m.type3,'$'), ' - $', '') AS group
                       ", sep="")
      
      node_query = paste("
                                 MATCH p=shortestPath(
                            	   (src{name:'",nodeName,"'})-[:Link*..30]->(dst)
                                  ) where id(src) <> id(dst)
                                 unwind nodes(p) AS k
                                 with distinct(k) as m
                                 RETURN m.name as `Site name`, replace(LABELS(m)[0] + ' - ' +  coalesce(m.type2,'$') + ' - ' + coalesce(m.type3,'$'), ' - $', '') AS Group
                                 UNION MATCH (m{name:'",nodeName,"'})
                                 RETURN m.name as `Site name`, replace(LABELS(m)[0] + ' - ' +  coalesce(m.type2,'$') + ' - ' + coalesce(m.type3,'$'), ' - $', '') AS Group
                                ", sep="")
      
      edge_query = paste("
                         MATCH p=shortestPath((src{name:'",nodeName,"'})-[:Link*..30]->(dst))
                         where id(src) <> id(dst)
                         with extract(x IN relationships(p) | {link_id: id(x), start: startNode(x).name, end: endNode(x).name, type: type(x), group: replace(LABELS(dst)[0] + ' - ' +  coalesce(dst.type2,'$') + ' - ' + coalesce(dst.type3,'$'), ' - $', '') }) AS rl
                         unwind rl as record
                         with distinct record.link_id as link_id, record.start as from, record.end AS to, record.type AS label, record.group as group
                         return from,  to,label, group
                         ", sep = "")
      
      if(showSource)
      {
        node_limited_query = paste("
                                 MATCH p=shortestPath(
                                   (dst{name:'",nodeName,"'})<-[:Link*..30]-(src)
                                    ) where id(src) <> id(dst) and src.bsc = true
                                   unwind nodes(p) AS k
                                   with distinct(k) as m
                                   RETURN m.name AS id,
                                   m.name AS label,
                                   replace(LABELS(m)[0] + ' - ' +  coalesce(m.type2,'$') + ' - ' + coalesce(m.type3,'$'), ' - $', '') AS group
                                   LIMIT ",maxnodes,"
                                   UNION MATCH (m{name:'",nodeName,"'})
                                   RETURN m.name AS id,
                                   m.name AS label,
                                   replace(LABELS(m)[0] + ' - ' +  coalesce(m.type2,'$') + ' - ' + coalesce(m.type3,'$'), ' - $', '') AS group
                                   ", sep="")
        
        node_query = paste("
                           MATCH p=shortestPath(
                           (dst{name:'",nodeName,"'})<-[:Link*..30]-(src)
                           ) where id(src) <> id(dst) and src.bsc = true
                           unwind nodes(p) AS k
                           with distinct(k) as m
                           RETURN m.name as `Site name`, replace(LABELS(m)[0] + ' - ' +  coalesce(m.type2,'$') + ' - ' + coalesce(m.type3,'$'), ' - $', '') as group
                           UNION MATCH (m{name:'",nodeName,"'})
                           RETURN m.name as `Site name`, replace(LABELS(m)[0] + ' - ' +  coalesce(m.type2,'$') + ' - ' + coalesce(m.type3,'$'), ' - $', '') as group
                           ", sep="")
        
        edge_query = paste("
                           MATCH p=shortestPath((dst{name:'",nodeName,"'})<-[:Link*..30]-(src))
                           where id(src) <> id(dst) and src.bsc = true
                           with extract(x IN relationships(p) | {link_id: id(x), start: startNode(x).name, end: endNode(x).name, type: type(x), group: replace(LABELS(dst)[0] + ' - ' +  coalesce(dst.type2,'$') + ' - ' + coalesce(dst.type3,'$'), ' - $', '') }) AS rl
                           unwind rl as record
                           with distinct record.link_id as link_id, record.start as from, record.end AS to, record.type AS label, record.group as group
                           return from,  to,label, group
                           ", sep = "")
      }
      
      nodes_limited = cypher(graph, node_limited_query)
      nodes = cypher(graph, node_query)
      edges = cypher(graph, edge_query)
  
      edges = filterEdges(nodes_limited, edges)
      reactiveNodeListExpanded(nodes)
      
      doubleClickJs = "function(event) {
      clicked_node = event.nodes[0]
      if(!!clicked_node){
          var selectElement = $('#sitenameExpanded').eq(0);
          var selectize = selectElement.data('selectize');
          selectize.setValue(clicked_node)
      }
      ;}"
      
      no_edges = FALSE
      if(length(edges) < 1)
      {
        no_edges = TRUE 
        edge_query = paste("
                         MATCH (src{name:'",nodeName,"'})
                         return
                         src.name as from, NULL as to, src.type as label, replace(LABELS(src)[0] + ' - ' +  coalesce(src.type2,'$') + ' - ' + coalesce(src.type3,'$'), ' - $', '') as group", sep = "")
        edges = cypher(graph, edge_query)
      }
      
      var_visNetwork = visNetwork(nodes_limited, edges, height = "100%", width = "100%") %>% 
        visPhysics(stabilization = FALSE) %>%
        visEdges(smooth = FALSE,
                 shadow = FALSE,
                 arrows =list(to = list(enabled = TRUE, scaleFactor = 1)),
                 color = list(color = "lightblue", highlight = "pink")) %>%
        visLayout(randomSeed = 12) %>%
        visOptions(nodesIdSelection = list(enabled = TRUE,
                                           selected = nodeName,
                                           style = 'width: 200px;'
        )) %>%
        visEvents(
          doubleClick = doubleClickJs
        ) 
      
      var_visNetwork = addVisGroups(var_visNetwork, nodes_limited)
        
      if(!no_edges)
      {
        var_visNetwork = var_visNetwork %>%visIgraphLayout()
      }
      
      return (var_visNetwork)
      
    }
    
  })
  
  output$sitesListExpanded <- DT::renderDataTable({
    datalist = reactiveNodeListExpanded()
    if(length(datalist) > 0)
    {
       DT::datatable(datalist, options = list(pageLength = 25))
    }
  })
  
  output$downloadExpanded <- downloadHandler(
    filename = function() {
      showSourceString = ""
      if(input$showSourceExpanded)
      {
        showSourceString = "-source"
      }
      paste("expanded-sites-",input$sitenameExpanded, showSourceString,"-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(reactiveNodeListExpanded(), file)
    }
  )
  