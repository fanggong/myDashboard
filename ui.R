
ui <- dashboardPage(
  scrollToTop = TRUE,
  header = dashboardHeader(),
  # sidebar ----
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("Data Management", tabName = "data_management", icon = icon("database")),
      menuItem("A-share Market", tabName = "stock", icon = icon("money-bill-trend-up")),
      menuItem("Craft Pub", tabName = "pub", icon = icon("wine-glass")),
      menuItem("Crypto Currencies", tabName = "crypto", icon = icon("bitcoin"))
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
        fluidRow(
          column(
            width = 4,
            daterangepicker::daterangepicker(
              inputId = "stock_period", start = Sys.Date() - 29, end = Sys.Date(),
              min = "2023-01-01", max = Sys.Date(), label = NULL, ranges = list(
                "Current Month" = c(lubridate::floor_date(Sys.Date(), unit = "month"), Sys.Date()),
                "Current Quarter" = c(lubridate::floor_date(Sys.Date(), unit = "quarter"), Sys.Date()),
                "Current Year" = c(lubridate::floor_date(Sys.Date(), unit = "year"), Sys.Date())
              )
            )
          ),
          column(
            width = 8,
            shinyWidgets::radioGroupButtons(
              inputId = "stock_type", selected = "summary", justified = TRUE, size = "sm",
              choices = c("Summary" = "summary", "Positions" = "positions")
            )
          )
        ),
        uiOutput(outputId = "stock_result")
      ),
      ## pub tab ----
      tabItem(
        tabName = "pub",
        fluidRow(
          column(
            width = 4,
            daterangepicker::daterangepicker(
              inputId = "pub_period", start = lubridate::floor_date(Sys.Date(), unit = "month"), end = Sys.Date(),
              min = "2023-06-15", max = Sys.Date(), label = NULL, ranges = list(
                "Current Month" = c(lubridate::floor_date(Sys.Date(), unit = "month"), Sys.Date()),
                "Current Quarter" = c(lubridate::floor_date(Sys.Date(), unit = "quarter"), Sys.Date()),
                "Current Year" = c(lubridate::floor_date(Sys.Date(), unit = "year"), Sys.Date())
              )
            )
          ),
          column(
            width = 8,
            shinyWidgets::radioGroupButtons(
              inputId = "pub_type", selected = "summary", justified = TRUE, size = "sm",
              choices = c("Summary" = "summary", "Products" = "products", "Customer" = "customer",
                          "Stock" = "stock")
            )
          )
        ),
        fluidRow(
          infoBoxOutput(outputId = "pub_v_income", width = 3),
          infoBoxOutput(outputId = "pub_v_cost", width = 3),
          infoBoxOutput(outputId = "pub_v_sales", width = 3),
          infoBoxOutput(outputId = "pub_v_customer", width = 3)
        ),
        uiOutput(outputId = "pub_result")
      ),
      ## crypto tab ----
      tabItem(
        tabName = "crypto",
        div("crypto")
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

ui <- secure_app(ui, theme = shinytheme("yeti"))
