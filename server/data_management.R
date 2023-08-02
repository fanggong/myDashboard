output$dm_status <- renderDT({
  dat_status[database == input$dm_database]
},
options = list(
  scrollX = TRUE
))

observeEvent(input$dm_database, {
  updateSelectizeInput(
    session = session, inputId = "dm_tbl", 
    choices = TBL_LST[[input$dm_database]]$tbl_name,
    selected = TBL_LST[[input$dm_database]]$tbl_name[1]
  )
})


observeEvent(input$dm_setting, {
  
  showModal(modalDialog(
    title = "Setting",
    fileInput(
      inputId = "quant_r_stock_position_file", label = "stock_position", accept = ".xlsx",
      width = "100%"
    ),
    easyClose = TRUE, footer = NULL, size = "l"
  ))
  
})


observeEvent(input$dm_update, {
  
  withCallingHandlers({
    shinyjs::html("dm_message", "")
    conn <- dbConnect(
      RPostgres::Postgres() , dbname = input$dm_database, user = pg_username, 
      password = pg_pwd, host = host, port = 5432
    )
    if (input$dm_database == "quant_r") {
      update_stock(
        input$dm_tbl, start_date = input$dm_period[1], end_date = input$dm_period[2],
        api = api, conn = conn, 
        quant_r_stock_position_path = input$quant_r_stock_position_file$datapath
      )
    } else if (input$dm_database == "haiyue") {
      update_pub(
        input$dm_tbl, start_date = input$dm_period[1], end_date = input$dm_period[2],
        app_id = app_id, app_key = app_key, conn
      )
    }
  },
  message = function(m) {
    shinyjs::html(id = "dm_message", html = m$message, add = TRUE)
  })
  
  dat_status <- get_data_status()
  output$dm_status <- renderDT({
    dat_status[database == input$dm_database]
  },
  options = list(
    scrollX = TRUE
  ))
})