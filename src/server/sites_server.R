library(shiny)
library(visNetwork)
library(R.cache)
source("common.R")

#cat(file=stderr(), "R.cache directory: ", print(getCacheRootPath()), "\n")

  output$siteNameUI <- renderUI({
    
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
    selectizeInput(inputId = "sitename",
                   label = "Choose source site name:",
                   choices = all_node_names
    )
    
    
  })
  
  
  output$displaySiteCountClassificationUI <- renderUI({
    
    nodeName = input$sitename
    showSource = input$showSource
    funcName = "getVisNetworkNodeCountData"
    
    getVisNetworkNodeCountData <- function(nodeName, showSource)
    {
      if(!is.null(nodeName) && nodeName != "")
      {
        total_node_query = paste("MATCH p1=shortestPath(
                                 (src{name:'",nodeName,"'})-[:Link*..30]->(dst)
                                  ) where id(src) <> id(dst)
                                  with (nodes(p1)) as p1_nodes
                                  optional MATCH p2=shortestPath(
                                 (src{name:'",nodeName,"'})-[:OSN_Link*..10]-(dst)
                                  ) where id(src) <> id(dst)
                                 with p1_nodes + coalesce((nodes(p2)),[]) as all_nodes
                                 UNWIND all_nodes as my_nodes
                                 WITH DISTINCT(id(my_nodes)) as n
                                 RETURN count(n)
                                 ", sep="")
        
        q1_node_query = paste("MATCH p1=shortestPath(
                         (src{name:'",nodeName,"'})-[:Link*..30]->(dst)
                          ) where id(src) <> id(dst)
                         with (nodes(p1)) as p1_nodes 
                         UNWIND p1_nodes as my_nodes 
                         return DISTINCT(id(my_nodes))", sep="")
        
        q2_node_query = paste("MATCH p2=shortestPath(
                         (src{name:'",nodeName,"'})-[:OSN_Link*..10]-(dst)
                          ) where id(src) <> id(dst)
                         with (nodes(p2)) as p2_nodes 
                         UNWIND p2_nodes as my_nodes 
                         return DISTINCT(id(my_nodes))", sep="")
      
        if(showSource)
        {
          q1_node_query = paste("MATCH p1=shortestPath(
                                   (dst{name:'",nodeName,"'})<-[:Link*..30]-(src)
          ) where id(src) <> id(dst) and src.bsc = true
                                   with (nodes(p1)) as p1_nodes
                                   UNWIND p1_nodes as my_nodes
                                   return DISTINCT(id(my_nodes))", sep="")
          
          q2_node_query = paste("MATCH p2=shortestPath(
                                (src{name:'",nodeName,"'})-[:OSN_Link*..10]-(dst)
                                ) where id(src) <> id(dst)
                                with (nodes(p2)) as p2_nodes
                                UNWIND p2_nodes as my_nodes
                                return DISTINCT(id(my_nodes))", sep="")
        }
        
        q1_node = cypher(graph, q1_node_query)
        q2_node = cypher(graph, q2_node_query)
        combined_node = rbind(q1_node,q2_node)
        unique_node = unique(combined_node[[1]])
        total_node_count = length(unique_node)
        
        total_node_count = ifelse(total_node_count >= 1, total_node_count, 1)
        
        detail_node_info_query = paste("MATCH (n{name:'",nodeName,"'})
                                       RETURN n
                                       ", sep="")
        detail_node_info = cypherToList(graph, detail_node_info_query)
        
        siteClassification = getNodeClassification(detail_node_info, total_node_count)
        
        return_var_list = list(
          total_node_count = total_node_count,
          siteClassification = siteClassification
        )
      }
    }
    
    key = list(funcName, nodeName, showSource)
    node_count_var_list = loadCache(key)
    if (is.null(node_count_var_list)) {
      node_count_var_list = getVisNetworkNodeCountData(nodeName, showSource)
      saveCache(node_count_var_list, key=key)
    }
    
    total_node_count = node_count_var_list[["total_node_count"]]
    siteClassification = node_count_var_list[["siteClassification"]]
    
    totalNodeCount = total_node_count
    
    #output-display-1
    output$totalSiteCountUI = renderText({paste("Total Sites Count: ", total_node_count)})
    
    #output-display-2
    output$siteClassificationUI = renderText({paste("Site Classification: ", siteClassification)})
    if(showSource)
    {
      output$siteClassificationUI = renderText({""})
    }
    
    return()
  })
  
  
  output$network <- renderVisNetwork({
    
    nodeName = input$sitename
    #maxnodes = input$maxnodes
    showSource = input$showSource
    funcName = "createVisNetworkData"
    
    createVisNetworkData <- function(nodeName, showSource)
    {
      if(!is.null(nodeName) && nodeName != "")
      {
        node_query = paste("
                           MATCH p1=shortestPath(
                           (src{name:'",nodeName,"'})-[:Link*..30]->(dst)
                           ) where id(src) <> id(dst)
                           with (nodes(p1)) as p1_nodes
                           unwind p1_nodes AS k
                           with distinct(k) as m
                           RETURN m.name AS id, m.name AS label, m.name as `Site name`, apoc.text.join((m.cat),\", \") AS group, m.olt_customers as olt_customers
                           UNION MATCH p2=shortestPath(
                           (src{name:'",nodeName,"'})-[:OSN_Link*..10]-(dst)
                            ) where id(src) <> id(dst)
                           with (nodes(p2)) as p2_nodes
                           unwind p2_nodes AS k
                           with distinct(k) as m
                           RETURN m.name AS id, m.name AS label, m.name as `Site name`, apoc.text.join((m.cat),\", \") AS group, m.olt_customers as olt_customers
                           UNION MATCH (m{name:'",nodeName,"'})
                           RETURN m.name AS id, m.name AS label, m.name as `Site name`, apoc.text.join((m.cat),\", \") AS group, m.olt_customers as olt_customers
                           ", sep="")
        
        edge_query = paste("
                           MATCH p1=shortestPath((src{name:'",nodeName,"'})-[:Link*..30]->(dst))
                           where id(src) <> id(dst)
                           with extract(x IN relationships(p1) | {link_id: id(x), start: startNode(x).name, end: endNode(x).name, type: type(x), group: apoc.text.join((dst.cat),\", \") }) AS rl1
                           unwind rl1 as record
                           with distinct record.link_id as link_id, record.start as from, record.end AS to, record.type AS label, record.group as group
                           return from,  to,label, group
                           UNION MATCH p2=shortestPath( (src{name:'",nodeName,"'})-[:OSN_Link*..10]-(dst) ) 
                           where id(src) <> id(dst)
                           with extract(x IN relationships(p2) | {link_id: id(x), start: startNode(x).name, end: endNode(x).name, type: type(x), group: apoc.text.join((dst.cat),\", \") }) AS rl2
                           unwind rl2 as record
                           with distinct record.link_id as link_id, record.start as from, record.end AS to, record.type AS label, record.group as group
                           return from,  to,label, group
                           ", sep = "")
        
        if(showSource)
        {
          
          node_query = paste("
                             MATCH p1=shortestPath(
                             (dst{name:'",nodeName,"'})<-[:Link*..30]-(src)
                             ) where id(src) <> id(dst) and src.bsc = true
                             with (nodes(p1)) as p1_nodes
                             unwind p1_nodes AS k
                             with distinct(k) as m
                             RETURN m.name AS id, m.name AS label, m.name as `Site name`, apoc.text.join((m.cat),\", \") AS group, m.olt_customers as olt_customers
                             UNION MATCH p2=shortestPath(
                             (src{name:'",nodeName,"'})-[:OSN_Link*..10]-(dst)
                              ) where id(src) <> id(dst)
                             with (nodes(p2)) as p2_nodes
                             unwind p2_nodes AS k
                             with distinct(k) as m
                             RETURN m.name AS id, m.name AS label, m.name as `Site name`, apoc.text.join((m.cat),\", \") as group, m.olt_customers as olt_customers
                             UNION MATCH (m{name:'",nodeName,"'})
                             RETURN m.name AS id, m.name AS label, m.name as `Site name`, apoc.text.join((m.cat),\", \") as group, m.olt_customers as olt_customers
                             ", sep="")
          
          edge_query = paste("
                             MATCH p1=shortestPath((dst{name:'",nodeName,"'})<-[:Link*..30]-(src))
                             where id(src) <> id(dst) and src.bsc = true
                             with extract(x IN relationships(p1) | {link_id: id(x), start: startNode(x).name, end: endNode(x).name, type: type(x), group: apoc.text.join((dst.cat),\", \") }) AS rl1
                             unwind rl1 as record
                             with distinct record.link_id as link_id, record.start as from, record.end AS to, record.type AS label, record.group as group
                             return from,  to,label, group
                             UNION MATCH p2=shortestPath( (src{name:'",nodeName,"'})-[:OSN_Link*..10]-(dst) ) 
                             where id(src) <> id(dst)
                             with extract(x IN relationships(p2) | {link_id: id(x), start: startNode(x).name, end: endNode(x).name, type: type(x), group: apoc.text.join((dst.cat),\", \") }) AS rl2
                             unwind rl2 as record
                             with distinct record.link_id as link_id, record.start as from, record.end AS to, record.type AS label, record.group as group
                             return from,  to,label, group
                             ", sep = "")
        }
        
        nodes = cypher(graph, node_query)
        edges = cypher(graph, edge_query)
        
        edges = filterEdges(nodes, edges)
        
        doubleClickJs = "function(event) {
        clicked_node = event.nodes[0]
        if(!!clicked_node){
        var selectElement = $('#sitename').eq(0);
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
                             src.name as from, NULL as to, src.type as label, apoc.text.join((src.cat),\", \") as group", sep = "")
          edges = cypher(graph, edge_query)
        }
        
        var_visNetwork = visNetwork(nodes, edges, height = "100%", width = "100%") %>% 
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
        
        var_visNetwork = addVisGroups(var_visNetwork, nodes)
        
        if(!no_edges)
        {
          var_visNetwork = var_visNetwork %>%visIgraphLayout()
        }
        
        return_var_list = list(
          nodes = nodes,
          var_visNetwork = var_visNetwork
        )
      }
    }
    
    key = list(funcName, nodeName, showSource)
    my_network_var_list = loadCache(key)
    if (is.null(my_network_var_list)) {
      my_network_var_list = createVisNetworkData(nodeName, showSource)
      saveCache(my_network_var_list, key=key)
    }
    
    nodes = my_network_var_list[["nodes"]]
    var_visNetwork = my_network_var_list[["var_visNetwork"]]
    
    reactiveNodeList(nodes)
    
    var_visNetwork
  })
  
  output$sitesList <- DT::renderDataTable({
    datalist = reactiveNodeList()
    if(length(datalist) > 0)
    {
      datalist = datalist[,!names(datalist) %in% c("id", "label")]
      DT::datatable(datalist, options = list(pageLength = 25))
    }
  })
  
  output$download <- downloadHandler(
    filename = function() {
      showSourceString = ""
      if(input$showSource)
      {
        showSourceString = "-source"
      }
      paste("Sites-",input$sitename, showSourceString,"-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      datalist = reactiveNodeList()
      datalist = datalist[,!names(datalist) %in% c("id", "label")]
      write.csv(datalist, file)
    }
  )
  