stock_summary_dat <- reactive({
  tmp <- stock_pnl_daily
  tmp <- tmp[cal_date >= input$stock_summary_period[1] & cal_date <= input$stock_summary_period[2]]
   if (input$stock_summary_type == "monthly") {
     tmp <- tmp[
       , .(
         all = last(all),
         periodly = sum(periodly),
         periodly_cumsum = last(periodly_cumsum)
       ), .(
         start_date = lubridate::floor_date(cal_date, unit = "month"),
         end_date = lubridate::ceiling_date(cal_date, unit = "month") - 1
       )
     ][
       , `:=`(
         cal_date = .mid_date(start_date, end_date),
         start_date = .ifelse(start_date > input$stock_summary_period[1], start_date, input$stock_summary_period[1]),
         end_date = .ifelse(end_date < input$stock_summary_period[2], end_date, input$stock_summary_period[2])
       )
     ]
  } else if (input$stock_summary_type == "quarterly") {
    tmp <- tmp[
      , .(
        all = last(all),
        periodly = sum(periodly),
        periodly_cumsum = last(periodly_cumsum)
      ), .(
        start_date = lubridate::floor_date(cal_date, unit = "quarter"),
        end_date = lubridate::ceiling_date(cal_date, unit = "quarter") - 1
      )
    ][
      , `:=`(
        cal_date = .mid_date(start_date, end_date),
        start_date = .ifelse(start_date > input$stock_summary_period[1], start_date, input$stock_summary_period[1]),
        end_date = .ifelse(end_date < input$stock_summary_period[2], end_date, input$stock_summary_period[2])
      )
    ]
  } else if (input$stock_summary_type == "yearly") {
    tmp <- tmp[
      , .(
        all = last(all),
        periodly = sum(periodly),
        periodly_cumsum = last(periodly_cumsum)
      ), .(
        start_date = lubridate::floor_date(cal_date, unit = "year"),
        end_date = lubridate::ceiling_date(cal_date, unit = "year") - 1
      )
    ][
      , `:=`(
        cal_date = .mid_date(start_date, end_date),
        start_date = .ifelse(start_date > input$stock_summary_period[1], start_date, input$stock_summary_period[1]),
        end_date = .ifelse(end_date < input$stock_summary_period[2], end_date, input$stock_summary_period[2])
      )
    ]
  }
  tmp
})

output$stock_summary_periodly <- renderPlotly({
  
  up <- stock_summary_dat()[periodly > 0]
  down <- stock_summary_dat()[periodly <= 0]
  
  fig <- plot_ly(opacity = OPACITY) %>%
    add_trace(
      x = up$cal_date, y = up$periodly, type = "bar", 
      marker = list(color = UP_COLOR), hoverinfo = "text",
      hovertext = paste0(
        ifelse(input$stock_summary_type == "daily", "</br> Date: ", "</br> Period: "),
        .ifelse(input$stock_summary_type == "daily", up$cal_date, paste0(up$start_date, " ~ ", up$end_date)),
        "</br> P&L: ", .format_number(up$periodly)
      )
    ) %>%
    add_trace(
      x = down$cal_date, y = down$periodly, type = "bar", 
      marker = list(color = DOWN_COLOR), hoverinfo = "text",
      hovertext = paste0(
        .ifelse(input$stock_summary_type == "daily", "</br> Date: ", "</br> Period: "),
        .ifelse(input$stock_summary_type == "daily", down$cal_date, paste0(down$start_date, " ~ ", down$end_date)),
        "</br> P&L: ", .format_number(down$periodly)
      )
    ) %>% 
    layout(
      showlegend = FALSE, hovermode = "x unified",
      xaxis = list(zeroline = FALSE, title = "Date"),
      yaxis = list(zeroline = FALSE, title = "P&L")
    )
  if (input$stock_summary_type == "daily") {
    fig <- fig %>% 
      layout(
        xaxis = list(rangebreaks = list(list(values = as.list(NO_TRADE_DATE))))
      )
  }
  fig
  
})

output$stock_summary_cumsum <- renderPlotly({
  
  # stock_summary_dat <- function() {
  #   stock_pnl_daily
  # }
  # 
  # input <- list(stock_summary_type = "daily")
  
  fig <- plot_ly(opacity = OPACITY) %>%
    add_trace(
      x = stock_summary_dat()$cal_date, y = stock_summary_dat()$periodly_cumsum,
      type = "scatter", mode = "lines", fill = "tozeroy",
      line = list(color = NORMAL_COLOR),
      hoverinfo = "text", hovertext = paste0(
        "</br> Date: ", stock_summary_dat()$end_date,
        "</br> Cumulative P&L: ", .format_number(stock_summary_dat()$periodly_cumsum)
      )
    ) %>%
    add_trace(
      x = stock_summary_dat()$cal_date, y = stock_summary_dat()$all, 
      type = "scatter", mode = "lines",
      line = list(color = NORMAL_COLOR, dash = "dot"),
      hoverinfo = "text", hovertext = paste0(
        "</br> Overall P&L: ", .format_number(stock_summary_dat()$all)
      ) 
    ) %>% 
    layout(
      showlegend = FALSE, hovermode = "x unified",
      xaxis = list(zeroline = FALSE, title = "Date"),
      yaxis = list(zeroline = FALSE, title = "Cumulative P&L")
    )
  if (input$stock_summary_type == "daily") {
    fig <- fig %>% 
      layout(
        xaxis = list(rangebreaks = list(list(values = as.list(NO_TRADE_DATE))))
      )
  }
  
  period <- c(first(stock_summary_dat()$cal_date), last(stock_summary_dat()$cal_date))
  period_y <- c(first(stock_summary_dat()$periodly_cumsum), last(stock_summary_dat()$periodly_cumsum))
  overall_y <- c(first(stock_summary_dat()$all), last(stock_summary_dat()$all))
  
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