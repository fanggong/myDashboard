server <- function(input, output, session) {
  secure_server(check_credentials = check_creds())

  source("server/data_management.R", encoding = "UTF-8", local = TRUE)
  
  source("server/stock_summary.R", encoding = "UTF-8", local = TRUE)
  
  source("server/stock_positions.R", encoding = "UTF-8", local = TRUE)
  
  
  source("server/pub_summary.R", encoding = "UTF-8", local = TRUE)
  
  source("server/pub_products.R", encoding = "UTF-8", local = TRUE)
  
}