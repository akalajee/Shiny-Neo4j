# API.R
require(shiny)
library(RNeo4j)
source("common.R")

#* Classify nodes
#* @get /classify
function(){
  if(!checkIfDBNodesClassified())
  {
    count = classifyAllDBNodes(FALSE) 
    return (paste("Classified " , count , " nodes",sep = ""));
  }
  return ("Already classified");
}
