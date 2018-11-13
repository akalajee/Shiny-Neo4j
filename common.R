library(RNeo4j)
source("config.R")
graph = startGraph(url = neo4j.url,
                   username = neo4j.username,
                   password = neo4j.password
                   )

reactiveTotalNodeCount = reactiveVal(0, "totalNodeCount")
reactiveNodeList = reactiveVal(list(), "nodeList")
reactiveNodeListClassificationReport = reactiveVal(list(), "reactiveNodeListClassificationReport")
reactiveNodeListCategoryReport = reactiveVal(list(), "reactiveNodeListCategoryReport")

randomTemp = reactiveVal(0, "randomTemp")

filterEdges = function(nodes, edges) {
  if(length(edges) < 1)
  {
    return (edges)
  }
  to_be_deleted = c()
  i = 1
  while (i<=nrow(edges)){
    from_node = edges[i, ]["from"][,1]
    to_node = edges[i, ]["to"][,1]
    if(!(from_node %in% nodes[,1]) || !(to_node %in% nodes[,1]))
    {
      to_be_deleted = append(to_be_deleted, i)
    }
    i = i+1
  }
  
  if(length(to_be_deleted) > 0)
  {
    edges = edges[-to_be_deleted,] 
  }
  return (edges)
}

selectizeListInput = function(items_list, items_label) {
  items_matrix = matrix(unlist(items_list))
  
  if(length(items_matrix) > 1)
  {
    all_items_names = list()
    all_items_names[[items_label]] = items_matrix
  }
  else
  {
    all_items_names = list(items_matrix)
  }
  return (all_items_names)
}

addVisGroups = function(var_visNetwork, nodes) {
  
  
  if(length(nodes) < 1)
  {
    return (visNetwork)
  }
  i = 1
  to_be_added = c()
  
  while (i<=nrow(nodes)){
    group_node = nodes[i, ]["group"][,1]
    if(!(group_node %in% to_be_added))
    {
      to_be_added = append(to_be_added, group_node)
    }
    i = i+1
  }
  
  var_visNetwork = var_visNetwork %>%  visGroups(useDefaultGroups = FALSE, groupname = "Site", color =list(
      background = "#9EC2F7",
      border = "#3E7EE2",
      highlight = list( background = "#D5E5FD", border = "#3E7EE2" )
    ))
  
  var_visNetwork = var_visNetwork %>%  visGroups(useDefaultGroups = FALSE, groupname = "Site - osn - iib", color =list(
      background = "#EC8584",
      border = "#E74237",
      highlight = list(background = "#F4B2B3", border = "#E74237")
    ))
  var_visNetwork = var_visNetwork %>%  visGroups(useDefaultGroups = FALSE, groupname = "Site - iib", color =list(
      background = "#FFFD54",
      border = "#F4AE3D",
      highlight = list( background =  "#FFFEAE", border = "#F4AE3D" )
    ))
  var_visNetwork = var_visNetwork %>%  visGroups(useDefaultGroups = FALSE, groupname = "Site - osn", color =list(
      background = "#58AB3A",
      border = "#357533",
      highlight = list(background = "#9CF08F", border = "#357533" )
    ))
  
  if(length(to_be_added) > 1)
  {
    var_visNetwork = var_visNetwork %>% visLegend()
  }

  return (var_visNetwork)
  
}

getNodeClassification = function(detail_node_info, total_node_count, update_db_classification = TRUE) {
  
  node_name = detail_node_info[[1]][[1]][["name"]]
  node_category = detail_node_info[[1]][[1]][["cat"]]
  olt_customers = as.numeric(detail_node_info[[1]][[1]][["olt_customers"]])
  node_classification = detail_node_info[[1]][[1]][["classification"]]
  if(node_classification != '' && !is.null(node_classification) && !is.na(node_classification))
  {
    return(node_classification)
  }
  
  
  isBSC = ("BSC" %in% node_category)
  isOSN = ("OSN" %in% node_category)
  isOTN = ("OTN" %in% node_category)
  isIIB_AGGR = ("IIB_AGGR" %in% node_category)
  isIIB_PREAGG = ("IIB_PREAGG" %in% node_category)
  isIIB_SRM = ("IIB_SRM" %in% node_category)
  isOLT = ("OLT" %in% node_category)
  isVIP = ("VIP" %in% node_category)
  isMW = ("MW" %in% node_category)
  
  #classificationLookup = data.frame(
  #  code = c("D", "C", "B", "A", "BSC"),
  #  rank = c(1:5)
  #)
  
  MWsiteClassificationCode = ifelse(total_node_count >= 21, 'A',
                                    ifelse(total_node_count >= 11 && total_node_count <= 20, 'B',
                                           ifelse(total_node_count >= 2 && total_node_count <= 10, 'C',
                                                  'D'
                                           )
                                    )
  )
  
  siteClassification = ifelse(isBSC, 'BSC', 
                              ifelse(isOTN || isIIB_AGGR || isVIP || (isOLT && olt_customers >= 2000) || (MWsiteClassificationCode == 'A') , 'A', 
                                     ifelse(isOSN || isIIB_PREAGG || (isOLT && olt_customers < 2000 && olt_customers > 500) || (MWsiteClassificationCode == 'B') , 'B',
                                            ifelse((isOLT && olt_customers <= 500) || (MWsiteClassificationCode == 'C'), 'C', 
                                                   ifelse(isIIB_SRM || (MWsiteClassificationCode == 'D'), 'D', 'D')
                                                   ))))
  
  
  #nonMWsiteClassificationRank = classificationLookup[match(nonMWsiteClassificationCode, classificationLookup$code), "rank"]
  
  #MWsiteClassificationRank = classificationLookup[match(MWsiteClassificationCode, classificationLookup$code), "rank"]
  
  #highestClassificationRank = max(MWsiteClassificationRank, nonMWsiteClassificationRank)
  #highestClassificationCode = classificationLookup[match(highestClassificationRank, classificationLookup$rank), "code"]
  
  #siteClassification = highestClassificationCode
  if(update_db_classification)
  {
    updateNodeClassification(nodeName = node_name, classification = siteClassification) 
  }
  
  return (siteClassification)
  
}

updateNodeClassification = function(nodeName, classification)
{
  query = "MATCH (n{name: {nodeName} })
            RETURN n
            LIMIT 1"
  
  node = getSingleNode(graph, query, nodeName=nodeName)
  node = updateProp(node, classification = classification)
}

classifyAllDBNodes = function()
{
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message = "Classifying sites", value = 0)
  all_node_query = paste("MATCH (n)
                           RETURN n.name AS sitename
                         order by n.name asc
                         ")
  
  all_node_names_list = cypherToList(graph, all_node_query)
  all_nodes_count = length(all_node_names_list)
  
  tx = newTransaction(graph)
  update_node_classification_query = paste("
                                                MATCH (n{name: {nodeName} })
                                           set n.classification = {siteClassification}
                                           ", sep="")
  
  i = 1
  while (i<=length(all_node_names_list)){

    # If we were passed a progress update function, call it
    if (is.function(updateProgress)) {
      updateProgress(progress, i, all_nodes_count)
    }
    
    nodeName = all_node_names_list[[i]][[1]]
    if(!is.null(nodeName) && nodeName != "")
    {
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
      
      siteClassification = getNodeClassification(detail_node_info, total_node_count, update_db_classification = FALSE)
      
      #cat(file=stdout(), "Nodename: ", nodeName, " ||| classification: ", siteClassification ,"\n")
      #update_node_classification_query = paste("
      #                                          MATCH (n{name:\"",nodeName,"\"})
      #                                          set n.classification = \"",siteClassification,"\"
      #                                         ", sep="")
      
      appendCypher(tx, update_node_classification_query,
                   nodeName=nodeName,
                   siteClassification=siteClassification
                   )
    }
        

    i = i+1
  }
  
  commit(tx)
  
}

checkIfDBNodesClassified = function()
{
  random_node_classification_query = paste("MATCH (n)
                           RETURN n.classification
                         limit 1
                         ")
  
  random_node_classification = cypher(graph, random_node_classification_query)
  return (!is.na(random_node_classification[[1]]))
}

updateProgress = function(progress, i, total) {
  progress$inc(1/total, detail = paste(i, " out of ", total))
}
