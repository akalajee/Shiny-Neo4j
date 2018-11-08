require(shiny)
library(R.cache)

setCacheRootPath("~/.Rcache")
shinyServer(function(input, output) {
  source("./src/server/sites_server.R", local = TRUE, encoding = "UTF-8")
  source("./src/server/classification_report_server.R", local = TRUE, encoding = "UTF-8")
  source("./src/server/category_report_server.R", local = TRUE, encoding = "UTF-8")
})
