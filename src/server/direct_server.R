library(shiny)
library(visNetwork)
source("common.R")


  output$secondSelection <- renderUI({
    
    connectiontype = ifelse(input$connectiontype == 'All', '', input$connectiontype)
    connectiontypeString = ifelse(connectiontype == '', '', paste("r:",connectiontype) )
    
    all_node_query = paste("MATCH (n)<-[",connectiontypeString,"]-() 
                           WITH collect(n) AS nn
                           MATCH ()<-[",connectiontypeString,"]-(n) 
                           WITH collect(n) + nn AS j
                           unwind j as f
                           with distinct f as m
                           order by m.name asc
                           RETURN m.name AS sitename")
    
    
    all_node_names_list = cypherToList(graph, all_node_query)
    
    site_name_label = "Site name";
    all_node_names = selectizeListInput(all_node_names_list, site_name_label)
    
    # Input: Text for providing a caption ----
    selectizeInput(inputId = "sitename",
                   label = "Choose source site name:",
                   choices = all_node_names
    )
    
  })
  
  output$slider <- renderUI({
    
    totalNodeCount = reactiveTotalNodeCount()
    sliderMaximum = totalNodeCount
    sliderInput(inputId = "maxnodes", label = "Maximum displayed nodes", min = 1, max = sliderMaximum, value = sliderMaximum, step = 1)
    
  })
  
  
  output$network <- renderVisNetwork({
    
    connectiontype = isolate(input$connectiontype)
    connectiontype = ifelse(connectiontype == 'All', '', connectiontype)
    connectiontypeString = ifelse(connectiontype == '', 'r', paste("r:",connectiontype) )
    nodeName = input$sitename
    maxnodes = input$maxnodes
    
    if(!is.null(nodeName) && nodeName != "")
    {
      
      total_node_query = paste("MATCH ({name:'",nodeName,"'})<-[",connectiontypeString,"]-(dst)
                               WITH collect(DISTINCT id(dst)) AS n
                               OPTIONAL MATCH (src)<-[",connectiontypeString,"]-({name:'",nodeName,"'})
                               WITH collect(DISTINCT id(src)) + n as f
                               unwind f as j
                               with distinct(j) as m
                               RETURN count(m)+1
                               ", sep="")
      
      total_node_count = cypher(graph, total_node_query)[1,1]
      
      reactiveTotalNodeCount(total_node_count)
      output$totalSiteCount = renderText({paste("Total Nodes Count: ", total_node_count)})
      
      
      node_limited_query = paste("MATCH p=({name:'",nodeName,"'})<-[",connectiontypeString,"]-(n) 
                       with COALESCE(collect(nodes(p)), []) as tt
                       OPTIONAL MATCH p=()<-[",connectiontypeString,"]-(n{name:'",nodeName,"'})
                       with COALESCE(collect(nodes(p)), []) + tt as t
                       unwind t as ff
					             unwind ff as f
                       with distinct f as m
                       RETURN m.name AS id,
                       m.name AS label,
                       LABELS(m)[0] AS group
                       LIMIT ",maxnodes,"
                       ", sep="")
      
      node_query = paste("MATCH p=({name:'",nodeName,"'})<-[",connectiontypeString,"]-(n) 
                         with COALESCE(collect(nodes(p)), []) as tt
                         OPTIONAL MATCH p=()<-[",connectiontypeString,"]-(n{name:'",nodeName,"'})
                         with COALESCE(collect(nodes(p)), []) + tt as t
                         unwind t as ff
                         unwind ff as f
                         with distinct f as m
                         RETURN m.name AS id,
                         m.name AS label,
                         LABELS(m)[0] AS group
                                 ", sep="")
      

      edge_query = paste("MATCH (src{name:'",nodeName,"'})<-[",connectiontypeString,"]-(dst)
                         with COALESCE(collect({start: startNode(r).name, end: endNode(r).name, type: type(r), group: LABELS(dst)[0]}), []) AS collected_record
                         OPTIONAL MATCH (src)<-[",connectiontypeString,"]-(dst{name:'",nodeName,"'})
                         with COALESCE(collect({start: startNode(r).name, end: endNode(r).name, type: type(r), group: LABELS(dst)[0]}), []) + collected_record AS d_collected_record
                         unwind d_collected_record as non_distinct_record
                         with distinct non_distinct_record as record
                         return record.start as from, record.end AS to, record.type AS label, record.group  AS group
                         ", sep = "")
      
      nodes_limited = cypher(graph, node_limited_query)
      nodes = cypher(graph, node_query)
      edges = cypher(graph, edge_query)
      
      edges = filterEdges(nodes_limited, edges)
      
      reactiveNodeList(nodes)
      
      visNetwork(nodes_limited, edges) %>%
        visIgraphLayout() %>%
        visPhysics(stabilization = FALSE) %>%
        visEdges(smooth = FALSE,
                 shadow = FALSE,
                 arrows =list(to = list(enabled = TRUE, scaleFactor = 1)),
                 color = list(color = "lightblue", highlight = "pink")) %>%
        visLayout(randomSeed = 12) %>%
        visOptions(nodesIdSelection = list(enabled = TRUE,
                                           selected = nodeName,
                                           style = 'width: 200px;'
        ))
    }
    
  })
  
  output$sitesList <- DT::renderDataTable({
    datalist = reactiveNodeList()
    if(length(datalist) > 0)
    {
      DT::datatable(datalist, options = list(pageLength = 25))
    }
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste("direct-sites-",input$connectiontype,"-",input$sitename,"-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(reactiveNodeList(), file)
    }
  )

