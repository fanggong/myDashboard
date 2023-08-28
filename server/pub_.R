tkt_detail_dat <- reactive({
  tkt_detail[cal_date >= input$pub_period[1] & cal_date <= input$pub_period[2]]
})

customer_detail_dat <- reactive({
  customer_detail[cal_date >= input$pub_period[1] & cal_date <= input$pub_period[2]]
})

stock_flow_dat <- reactive({
  stock_flow[confirmed_date >= input$pub_period[1] & confirmed_date <= input$pub_period[2]]
})