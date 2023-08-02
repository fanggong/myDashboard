
ui <- dashboardPage(
  scrollToTop = TRUE,
  header = dashboardHeader(),
  # sidebar ----
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("Data Management", tabName = "data_management", icon = icon("database")),
      menuItem("A-share Market", tabName = "stock", icon = icon("money-bill-trend-up")),
      menuItem("Crypto Currencies", tabName = "crypto", icon = icon("bitcoin")),
      menuItem("Craft Pub", tabName = "pub", icon = icon("wine-glass"))
    )
  ),
  # body ----
  body = dashboardBody(
    shinyjs::useShinyjs(),
    tabItems(
      ## data management tab ----
      tabItem(
        tabName = "data_management",
        ### buttons ----
        fluidRow(
          column(
            width = 3,
            daterangepicker::daterangepicker(
              inputId = "dm_period", label = "Period", start = Sys.Date(), end = Sys.Date(),
              min = "2023-01-01", max = Sys.Date(), ranges = list(
                "Yesterday" = c(Sys.Date() - 1, Sys.Date() - 1),
                "Last 3 Days" = c(Sys.Date() - 2, Sys.Date()),
                "Last 7 Days" = c(Sys.Date() - 6, Sys.Date())
              )
            ),
            selectizeInput(
              inputId = "dm_database", label = "Database", choices = names(TBL_LST), multiple = FALSE,
              selected = names(TBL_LST)[1], width = "100%"
            ),
            selectizeInput(
              inputId = "dm_tbl", label = "Table", choices = TBL_LST[[1]]$tbl_name, multiple = TRUE,
              selected = TBL_LST[[1]]$tbl_name[1]
            ),
            fluidRow(
              column(
                width = 6,
                actionButton(
                  inputId = "dm_setting", label = "Setting", width = "100%"
                )
              ),
              column(
                width = 6, 
                actionButton(
                  inputId = "dm_update", label = "Update", width = "100%"
                )
              )
            )
          ),
          column(
            width = 9,
            div(
              style = "height: 290px; overflow-y: auto; background-color: #FFF;",
              verbatimTextOutput(outputId = "dm_message", placeholder = TRUE)
            ),
            tags$style("#dm_message {background-color: #FFF; border: 0px; pading: 0;")
          )
        ),
        ### dt ----
        box(
          title = "Data Update Time", status = "lightblue", solidHeader = TRUE, width = NULL,
          id = "dm_status",
          column(
            width = 12,
            DTOutput(
              outputId = "dm_status"
            )
          )
        ),
        tags$style("#dm_status {margin-top: 10px}")
      ),
      ## stock tab ----
      tabItem(
        tabName = "stock",
        ### summary ----
        box(
          title = "Summary", status = "lightblue", solidHeader = TRUE, width = NULL,
          id = "stock_summary",
          fluidRow(
            column(
              width = 4,
              daterangepicker::daterangepicker(
                inputId = "stock_summary_period", start = Sys.Date() - 29, end = Sys.Date(),
                min = "2023-01-01", max = Sys.Date(), label = NULL, ranges = list(
                  "Current Month" = c(lubridate::floor_date(Sys.Date(), unit = "month"), Sys.Date()),
                  "Current Quarter" = c(lubridate::floor_date(Sys.Date(), unit = "quarter"), Sys.Date()),
                  "Current Year" = c(lubridate::floor_date(Sys.Date(), unit = "year"), Sys.Date())
                )
              )
            ),
            column(
              width = 5, offset = 3,
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
        ),
        ### positions ----
        box(
          title = "Positions", status = "lightblue", solidHeader = TRUE, width = NULL,
          id = "stock_positions",
          fluidRow(
            column(
              width = 4,
              daterangepicker::daterangepicker(
                inputId = "stock_positions_period", start = Sys.Date() - 29, end = Sys.Date(),
                min = "2023-01-01", max = Sys.Date(), label = NULL, ranges = list(
                  "Current Month" = c(lubridate::floor_date(Sys.Date(), unit = "month"), Sys.Date()),
                  "Current Quarter" = c(lubridate::floor_date(Sys.Date(), unit = "quarter"), Sys.Date()),
                  "Current Year" = c(lubridate::floor_date(Sys.Date(), unit = "year"), Sys.Date())
                )
              )
            ),
            column(
              width = 4, offset = 4,
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
      ),
      ## crypto tab ----
      tabItem(
        tabName = "crypto",
        div("crypto")
      ),
      ## pub tab ----
      tabItem(
        tabName = "pub",
        ### sales ----
        box(
          title = "Summary", status = "lightblue", solidHeader = TRUE, width = NULL,
          id = "pub_summary",
          fluidRow(
            column(
              width = 4,
              daterangepicker::daterangepicker(
                inputId = "pub_summary_period", start = lubridate::floor_date(Sys.Date(), unit = "month"), end = Sys.Date(),
                min = "2023-06-15", max = Sys.Date(), label = NULL, ranges = list(
                  "Current Month" = c(lubridate::floor_date(Sys.Date(), unit = "month"), Sys.Date()),
                  "Current Quarter" = c(lubridate::floor_date(Sys.Date(), unit = "quarter"), Sys.Date()),
                  "Current Year" = c(lubridate::floor_date(Sys.Date(), unit = "year"), Sys.Date())
                )
              )
            ),
            column(
              width = 5, offset = 3,
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
        ),
        ### products ----
        box(
          title = "Products", status = "lightblue", solidHeader = TRUE, width = NULL,
          id = "pub_products",
          fluidRow(
            column(
              width = 4,
              daterangepicker::daterangepicker(
                inputId = "pub_products_period", start = lubridate::floor_date(Sys.Date(), unit = "month"), end = Sys.Date(),
                min = "2023-06-15", max = Sys.Date(), label = NULL, ranges = list(
                  "Current Month" = c(lubridate::floor_date(Sys.Date(), unit = "month"), Sys.Date()),
                  "Current Quarter" = c(lubridate::floor_date(Sys.Date(), unit = "quarter"), Sys.Date()),
                  "Current Year" = c(lubridate::floor_date(Sys.Date(), unit = "year"), Sys.Date())
                )
              )
            ),
            column(
              width = 4, offset = 4,
              shinyWidgets::pickerInput(
                inputId = "pub_products_code", label = NULL, 
                choices = pub_subjects,
                choicesOpt = list(subtext = pub_subjects),
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
      )
    )
  ),
  # control bar ----
  controlbar = dashboardControlbar(
    collapsed = TRUE,
    div(class = "p-3", skinSelector()),
    pinned = TRUE
  )
)

# ui <- secure_app(ui, theme = shinytheme("yeti"))
