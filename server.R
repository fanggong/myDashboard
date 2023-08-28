server <- function(input, output, session) {
  secure_server(check_credentials = check_creds())

  source("server/data_management.R", encoding = "UTF-8", local = TRUE)
  
  observeEvent(input$stock_type, {
    if (input$stock_type == "summary") {
      output$stock_result <- renderUI({
        box(
          title = "Summary", status = "lightblue", solidHeader = TRUE, width = NULL,
          id = "stock_summary",
          fluidRow(
            column(
              width = 5,
              shinyWidgets::radioGroupButtons(
                inputId = "stock_summary_type", selected = "daily", justified = TRUE,
                choices = c("Daily" = "daily", "Monthly" = "monthly", "Quarterly" = "quarterly", "Yearly" = "yearly")
              )
            )
          ),
          fluidRow(
            column(width = 6, plotlyOutput(outputId = "stock_summary_periodly")),
            column(width = 6, plotlyOutput(outputId = "stock_summary_cumsum"))
          )
        )
      })
    } 
    else if (input$stock_type == "positions") {
      output$stock_result <- renderUI({
        box(
          title = "Positions", status = "lightblue", solidHeader = TRUE, width = NULL,
          id = "stock_positions",
          fluidRow(
            column(
              width = 5,
              shinyWidgets::pickerInput(
                inputId = "stock_positions_code", label = NULL, 
                choices = stock_subjects,
                choicesOpt = list(subtext = stock_subjects),
                options = list(size = 5)
              )
            )
          ),
          fluidRow(
            column(width = 6, plotlyOutput(outputId = "stock_positions_periodly")),
            column(width = 6, plotlyOutput(outputId = "stock_positions_cumsum"))
          )
        )
      })
    }
  })
  
  source("server/stock_.R", encoding = "UTF-8", local = TRUE)
  source("server/stock_summary.R", encoding = "UTF-8", local = TRUE)
  source("server/stock_positions.R", encoding = "UTF-8", local = TRUE)
  
  observeEvent(input$pub_type, {
    if (input$pub_type == "summary") {
      output$pub_result <- renderUI({
        box(
          title = "Summary", status = "lightblue", solidHeader = TRUE, width = NULL,
          id = "pub_summary",
          fluidRow(
            column(
              width = 5,
              shinyWidgets::radioGroupButtons(
                inputId = "pub_summary_type", selected = "daily", justified = TRUE,
                choices = c("Daily" = "daily", "Monthly" = "monthly", "Quarterly" = "quarterly", "Yearly" = "yearly")
              )
            )
          ),
          fluidRow(
            column(width = 6, plotlyOutput(outputId = "pub_summary_periodly")),
            column(width = 6, plotlyOutput(outputId = "pub_summary_cumsum"))
          )
        )
      })
    }
    else if (input$pub_type == "products") {
      output$pub_result <- renderUI({
        box(
          title = "Products", status = "lightblue", solidHeader = TRUE, width = NULL,
          id = "pub_products",
          fluidRow(
            column(
              width = 5,
              shinyWidgets::pickerInput(
                inputId = "pub_products_code", label = NULL, 
                choices = pub_product_subjects,
                choicesOpt = list(subtext = pub_product_subjects),
                options = pickerOptions(size = 5, liveSearch = TRUE)
              )
            )
          ),
          fluidRow(
            column(width = 6, plotlyOutput(outputId = "pub_products_periodly")),
            column(width = 6, plotlyOutput(outputId = "pub_products_cumsum"))
          ),
          fluidRow(
            column(width = 12, plotlyOutput(outputId = "pub_products_sales"))
          )
        )
      })
    }
    else if (input$pub_type == "customer") {
      output$pub_result <- renderUI({
        box(
          title = "Customer", status = "lightblue", solidHeader = TRUE, width = NULL,
          id = "pub_customer",
          fluidRow(
            column(
              width = 5,
              shinyWidgets::pickerInput(
                inputId = "pub_customer_name", label = NULL,
                choices = pub_customer_subjects,
                options = pickerOptions(size = 5, liveSearch = TRUE)
              )
            )
          ),
          fluidRow(
            column(width = 12, plotlyOutput(outputId = "pub_customer_summary"))
          )
        )
      })
    }
    else if (input$pub_type == "stock") {
      output$pub_result <- renderUI({
        box(
          title = "Stock", status = "lightblue", solidHeader = TRUE, width = NULL,
          id = "pub_stock",
          DTOutput("pub_stock_dat")
        )
      })
    }
  })
  
  source("server/pub_.R", encoding = "UTF-8", local = TRUE)
  source("server/pub_infobox.R", encoding = "UTF-8", local = TRUE)
  source("server/pub_summary.R", encoding = "UTF-8", local = TRUE)
  source("server/pub_products.R", encoding = "UTF-8", local = TRUE)
  source("server/pub_customer.R", encoding = "UTF-8", local = TRUE)
  source("server/pub_stock.R", encoding = "UTF-8", local = TRUE)
  
}