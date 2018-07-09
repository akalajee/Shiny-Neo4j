require(shiny)
require(visNetwork)

shinyServer(function(input, output) {
  
  source("./src/server/direct_server.R", local = TRUE, encoding = "UTF-8")
  source("./src/server/expanded_server.R", local = TRUE, encoding = "UTF-8")
  
})
