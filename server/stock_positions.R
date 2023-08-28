
stock_positions_dat <- reactive({
  stock_pnl_detail_dat()[code == input$stock_positions_code]
})

output$stock_positions_periodly <- renderPlotly({
  up <- stock_positions_dat()[daily > 0]
  down <- stock_positions_dat()[daily <= 0]
  
  plot_ly(opacity = OPACITY) %>% 
    add_trace(
      x = up$cal_date, y = up$daily, type = "bar", 
      marker = list(color = UP_COLOR), hoverinfo = "text",
      hovertext = paste0(
        "</br> Date: ", up$cal_date,
        "</br> P&L: ", .format_number(up$daily)
      )
    ) %>% 
    add_trace(
      x = down$cal_date, y = down$daily, type = "bar", 
      marker = list(color = DOWN_COLOR), hoverinfo = "text",
      hovertext = paste0(
        "</br> Date: ", down$cal_date,
        "</br> P&L: ", .format_number(down$daily)
      )
    ) %>%
    layout(
      showlegend = FALSE, hovermode = "x unified",
      xaxis = list(
        zeroline = FALSE, title = "Date",
        rangebreaks = list(list(values = as.list(NO_TRADE_DATE)))
      ),
      yaxis = list(zeroline = FALSE, title = "P&L")
    )
})


output$stock_positions_cumsum <- renderPlotly({
  fig <- plot_ly(opacity = OPACITY) %>% 
    add_trace(
      x = stock_positions_dat()$cal_date, y = stock_positions_dat()$daily_cumsum, 
      type = "scatter", mode = "lines", fill = "tozeroy", 
      line = list(color = NORMAL_COLOR),
      hoverinfo = "text", hovertext = paste0(
        "</br> Date: ", stock_positions_dat()$cal_date,
        "</br> Cumulative P&L: ", .format_number(stock_positions_dat()$daily_cumsum)
      )
    ) %>% 
    add_trace(
      x = stock_positions_dat()$cal_date, y = stock_positions_dat()$all, 
      type = "scatter", mode = "lines",
      line = list(color = NORMAL_COLOR, dash = "dot"),
      hoverinfo = "text", hovertext = paste0(
        "</br> Overall P&L: ", .format_number(stock_positions_dat()$all)
      ) 
    ) %>% 
    layout(
      showlegend = FALSE, hovermode = "x unified",
      xaxis = list(
        zeroline = FALSE, title = "Date",
        rangebreaks = list(list(values = as.list(NO_TRADE_DATE)))
      ),
      yaxis = list(zeroline = FALSE, title = "Cumulative P&L") 
    )
  
  period <- c(first(stock_positions_dat()$cal_date), last(stock_positions_dat()$cal_date))
  period_y <- c(first(stock_positions_dat()$daily_cumsum), last(stock_positions_dat()$daily_cumsum))
  overall_y <- c(first(stock_positions_dat()$all), last(stock_positions_dat()$all))
  
  fig <- fig %>% add_trace(
    x = period, y = period_y, type = "scatter", mode = "markers", 
    marker = list(color = NORMAL_COLOR, size = 6), hoverinfo = "none"
  ) %>% 
    add_trace(
      x = period, y = overall_y, type = "scatter", mode = "markers",
      marker = list(color = NORMAL_COLOR, size = 6), hoverinfo = "none"
    ) %>% 
    layout(
      annotations = list(
        xref = "paper", yref = "y", x = 0.02, y = c(overall_y[1], period_y[1]),
        xanchor = "left", yanchor = "top",
        text = .format_number(c(overall_y[1], period_y[1])),
        font = list(size = 12, color = NORMAL_COLOR),
        showarrow = FALSE, name = NULL
      )
    ) %>% 
    layout(
      annotations = list(
        xref = "paper", yref = "y", x = 0.97, y = c(overall_y[2], period_y[2]),
        xanchor = "right", yanchor = "bottom",
        text = .format_number(c(overall_y[2], period_y[2])),
        font = list(size = 12, color = NORMAL_COLOR),
        showarrow = FALSE, name = NULL
      )
    ) 
  fig
})
