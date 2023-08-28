
pub_customer_dat <- reactive({
  customer_detail_dat()[name == input$pub_customer_name]
})


output$pub_customer_summary <- renderPlotly({
  cols <- colorRampPalette(c(NORMAL_COLOR, BASE_COLOR))(3)
  fig <- plot_ly(opacity = OPACITY) %>%
    add_trace(
      x = pub_customer_dat()$cal_date, y = 0, type = "bar",
      marker = list(color = WHITE), hoverinfo = "text",
      hovertext = paste0("</br> <b>", pub_customer_dat()$cal_date, "</b>")
    ) %>% 
    add_trace(
      x = pub_customer_dat()$cal_date, y = pub_customer_dat()$balance_recharge, type = "bar",
      marker = list(color = cols[1]), hoverinfo = "text",
      hovertext = paste0("</br> 本金余额：", pub_customer_dat()$balance_recharge)
    ) %>%
    add_trace(
      x = pub_customer_dat()$cal_date, y = pub_customer_dat()$balance_gift, type = "bar",
      marker = list(color = cols[2]), hoverinfo = "text",
      hovertext = paste0("</br> 赠金余额：", pub_customer_dat()$balance_gift)
    ) %>%
    add_trace(
      x = pub_customer_dat()$cal_date, y = pub_customer_dat()$ticket_recharge_amount_cumsum, type = "bar",
      marker = list(color = cols[3]), hoverinfo = "text",
      hovertext = paste0("</br> 消费金额：", pub_customer_dat()$ticket_recharge_amount_cumsum)
    ) %>%
    layout(
      xaxis = list(zeroline = FALSE, title = "Date"),
      yaxis = list(zeroline = FALSE, title = "Balance"),
      barmode = "stack", hovermode = "x unified", showlegend = FALSE
    )
  fig
})
