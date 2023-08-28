stock_pnl_daily_dat <- reactive({
  stock_pnl_daily[cal_date >= input$stock_period[1] & cal_date <= input$stock_period[2]]
})

stock_pnl_detail_dat <- reactive({
  stock_pnl_detail[cal_date >= input$stock_period[1] & cal_date <= input$stock_period[2]]
})
