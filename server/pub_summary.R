pub_summary_dat <- reactive({
  tmp <- tkt_detail_dat()[
    , .(
      periodly = sum(total_amount)
    ), .(cal_date)
  ]
  
  tmp <- merge(
    data.table(cal_date = seq.Date(as.Date(input$pub_period[1]), as.Date(input$pub_period[2]), by = 1)),
    tmp, all.x = TRUE
  )

  tmp <- tmp[
    , `:=`(
      periodly = nafill(periodly, fill = 0)
    )
  ][
    order(cal_date), `:=`(
      periodly_cumsum = cumsum(periodly),
      start_date = cal_date,
      end_date = cal_date
    )
  ][
    order(cal_date)
  ]
  
  if (input$pub_summary_type == "monthly") {
    tmp <- tmp[
      , .(
        periodly = sum(periodly),
        periodly_cumsum = last(periodly_cumsum)
      ), .(
        start_date = lubridate::floor_date(cal_date, unit = "month"),
        end_date = lubridate::ceiling_date(cal_date, unit = "month") - 1
      )
    ][
      , `:=`(
        cal_date = .mid_date(start_date, end_date),
        start_date = .ifelse(start_date > input$pub_period[1], start_date, input$pub_period[1]),
        end_date = .ifelse(end_date < input$pub_period[2], end_date, input$pub_period[2])
      )
    ]
  } else if (input$pub_summary_type == "quarterly") {
    tmp <- tmp[
      , .(
        periodly = sum(periodly),
        periodly_cumsum = last(periodly_cumsum)
      ), .(
        start_date = lubridate::floor_date(cal_date, unit = "quarter"),
        end_date = lubridate::ceiling_date(cal_date, unit = "quarter") - 1
      )
    ][
      , `:=`(
        cal_date = .mid_date(start_date, end_date),
        start_date = .ifelse(start_date > input$pub_period[1], start_date, input$pub_period[1]),
        end_date = .ifelse(end_date < input$pub_period[2], end_date, input$pub_period[2])
      )
    ]
  } else if (input$pub_summary_type == "yearly") {
    tmp <- tmp[
      , .(
        periodly = sum(periodly),
        periodly_cumsum = last(periodly_cumsum)
      ), .(
        start_date = lubridate::floor_date(cal_date, unit = "year"),
        end_date = lubridate::ceiling_date(cal_date, unit = "year") - 1
      )
    ][
      , `:=`(
        cal_date = .mid_date(start_date, end_date),
        start_date = .ifelse(start_date > input$pub_period[1], start_date, input$pub_period[1]),
        end_date = .ifelse(end_date < input$pub_period[2], end_date, input$pub_period[2])
      )
    ]
  }
  tmp
})

output$pub_summary_periodly <- renderPlotly({
  
  fig <- plot_ly(opacity = OPACITY) %>%
    add_trace(
      x = pub_summary_dat()$cal_date, y = pub_summary_dat()$periodly, type = "bar", 
      marker = list(color = NORMAL_COLOR), hoverinfo = "text",
      hovertext = paste0(
        ifelse(input$pub_summary_type == "daily", "</br> Date: ", "</br> Period: "),
        .ifelse(
          input$pub_summary_type == "daily", 
          pub_summary_dat()$cal_date, 
          paste0(pub_summary_dat()$start_date, " ~ ", pub_summary_dat()$end_date)
        ),
        "</br> Sales: ", .format_number(pub_summary_dat()$periodly)
      )
    ) %>% 
    layout(
      showlegend = FALSE, hovermode = "x unified",
      xaxis = list(zeroline = FALSE, title = "Date"),
      yaxis = list(zeroline = FALSE, title = "Sales")
    )
  fig
  
})

output$pub_summary_cumsum <- renderPlotly({
  
  fig <- plot_ly(opacity = OPACITY) %>%
    add_trace(
      x = pub_summary_dat()$cal_date, y = pub_summary_dat()$periodly_cumsum,
      type = "scatter", mode = "lines", fill = "tozeroy",
      line = list(color = NORMAL_COLOR),
      hoverinfo = "text", hovertext = paste0(
        "</br> Date: ", pub_summary_dat()$end_date,
        "</br> Cumulative Sales: ", .format_number(pub_summary_dat()$periodly_cumsum)
      )
    ) %>% 
    layout(
      showlegend = FALSE, hovermode = "x unified",
      xaxis = list(zeroline = FALSE, title = "Date"),
      yaxis = list(zeroline = FALSE, title = "Cumulative Sales")
    )
  
  period <- c(first(pub_summary_dat()$cal_date), last(pub_summary_dat()$cal_date))
  period_y <- c(first(pub_summary_dat()$periodly_cumsum), last(pub_summary_dat()$periodly_cumsum))

  fig <- fig %>% add_trace(
    x = period, y = period_y, type = "scatter", mode = "markers", 
    marker = list(color = NORMAL_COLOR, size = 6), hoverinfo = "none"
  ) %>% 
    layout(
      annotations = list(
        xref = "paper", yref = "y", x = 0.97, y = period_y[2],
        xanchor = "right", yanchor = "bottom",
        text = .format_number(period_y[2]),
        font = list(size = 12, color = NORMAL_COLOR),
        showarrow = FALSE, name = NULL
      )
    ) 
  fig
})