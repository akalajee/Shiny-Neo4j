library(RNeo4j)
source("config.R")
graph = startGraph("http://localhost:7474/db/data/",
                   username = neo4j.username,
                   password = neo4j.password)

reactiveTotalNodeCount = reactiveVal(0, "totalNodeCount")