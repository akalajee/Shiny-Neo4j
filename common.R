library(RNeo4j)
source("config.R")
graph = startGraph("http://192.168.1.1:7474/db/data/",
                   username = neo4j.username,
                   password = neo4j.password)

reactiveTotalNodeCount = reactiveVal(0, "totalNodeCount")
reactiveTotalNodeCountExpanded = reactiveVal(0, "totalNodeCountExpanded")

reactiveNodeList = reactiveVal(list(), "nodeList")
reactiveNodeListExpanded = reactiveVal(list(), "nodeListExpanded")

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
  
  groups = list(
    "Site" = list (groupname = "Site", color =list(
      background = "#9EC2F7",
      border = "#3E7EE2",
      highlight = list( background = "#D5E5FD", border = "#3E7EE2" )
    )),
    "Site - osn - iib" = list(groupname = "Site - osn - iib", color =list(
      background = "#EC8584",
      border = "#E74237",
      highlight = list(background = "#F4B2B3", border = "#E74237")
    )),
    "Site - iib" = list(groupname = "Site - iib", color =list(
      background = "#FFFD54",
      border = "#F4AE3D",
      highlight = list( background =  "#FFFEAE", border = "#F4AE3D" )
    )),
    "Site - osn" = list(groupname = "Site - osn", color =list(
      background = "#FFFD54",
      border = "#F4AE3D",
      highlight = list(background = "#FFFEAE", border = "#F4AE3D" )
    ))
  )

  k = 1
  selected_groups = list()
  
  while (k<=length(to_be_added)){
    group_label = to_be_added[k]
    group_prop = groups[group_label]
    selected_groups = append(selected_groups, group_prop)
    k = k + 1
  }
  
  #browser()
  if(length(selected_groups) > 1)
  {
    print("addVisGroup")
    var_visNetwork = var_visNetwork %>% visGroups(selected_groups) %>% visLegend()
  }

  return (var_visNetwork)
  
}