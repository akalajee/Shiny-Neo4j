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