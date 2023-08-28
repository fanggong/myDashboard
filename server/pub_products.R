pub_products_tmp <- reactive({
  data.frame(
    cal_date = seq.Date(
      as.Date(input$pub_period[1]), 
      as.Date(input$pub_period[2]), 
      by = 1
    )
  )
})


pub_products_dat <- reactive({
  tmp <- tkt_detail_dat()[
    , .(
      product_quantity = sum(quantity),
      product_size = first(product_size),
      material_quantity = sum(material_quantity),
      product_unit = first(product_unit),
      amount = sum(total_amount)
    ), .(cal_date, product_name, product_barcode)
  ]
  
  products <- unique(tkt_detail_dat()[
    material_barcode == input$pub_products_code, 
    .(product_barcode, product_name, product_size, product_unit)
  ])
  products <- merge.data.frame(pub_products_tmp(), products)
  tmp <- merge(products, tmp, all.x = TRUE)
  setDT(tmp)
  tmp <- tmp[
    is.na(product_quantity), `:=`(
      product_quantity = 0,
      material_quantity = 0,
      amount = 0
    )
  ]
  tmp[
    order(cal_date), `:=`(
      quantity_cumsum = cumsum(product_quantity),
      material_cumsum = cumsum(material_quantity),
      amount = cumsum(amount)
    ), .(product_name, product_barcode)
  ]
  tmp[order(cal_date)]
})


pub_material_dat <- reactive({
  
  tmp <- tkt_detail_dat()[
    , .(
      quantity = sum(material_quantity),
      amount = sum(total_amount),
      product_unit = first(product_unit)
    ), .(cal_date, material_name, material_barcode)
  ]
  
  products <- unique(tkt_detail[
    material_barcode == input$pub_products_code, 
    .(material_name, material_barcode, product_unit)
  ])
  products <- merge.data.frame(pub_products_tmp(), products)
  
  tmp <- merge(products, tmp, all.x = TRUE)
  setDT(tmp)
  tmp <- tmp[
    is.na(quantity), `:=`(
      quantity = 0,
      amount = 0
    )
  ]
  tmp[
    order(cal_date), `:=`(
      quantity_cumsum = cumsum(quantity),
      amount_cumsum = cumsum(amount)
    )
  ]
  tmp[order(cal_date)]
  
})


output$pub_products_sales <- renderPlotly({
  fig <- plot_ly(opacity = OPACITY) %>% 
    add_trace(
      x = pub_material_dat()$cal_date, y = pub_material_dat()$amount_cumsum,
      type = "scatter", mode = "lines", name = paste0(pub_material_dat()$material_name, " Sales"),
      line = list(color = NORMAL_COLOR),
      hoverinfo = "text", hovertext = paste0(
        "</br> Date: ", pub_material_dat()$cal_date,
        "</br> Cumulative Sales: ", .format_number(pub_material_dat()$amount_cumsum)
      )
    ) %>%
    add_trace(
      x = last(pub_material_dat()$cal_date), y = last(pub_material_dat()$amount_cumsum), 
      type = "scatter", mode = "markers", 
      marker = list(color = NORMAL_COLOR, size = 6), hoverinfo = "none"
    ) %>% 
    layout(
      showlegend = FALSE, hovermode = "x unified",
      xaxis = list(title = "Date", zeroline = FALSE),
      yaxis = list(title = "Sales", zeroline = FALSE),
      annotations = list(
        xref = "paper", yref = "y", x = 0.97, y = last(pub_material_dat()$amount_cumsum),
        xanchor = "right", yanchor = "bottom",
        text = .format_number(last(pub_material_dat()$amount_cumsum)),
        font = list(size = 12, color = NORMAL_COLOR),
        showarrow = FALSE, name = NULL
      )
    )
  fig
})


output$pub_products_cumsum <- renderPlotly({
  fig <- subplot(
    plot_ly(opacity = OPACITY) %>% 
      add_trace(
        x = pub_material_dat()$cal_date, y = pub_material_dat()$quantity_cumsum, 
        type = "scatter", mode = "lines", fill = "tozeroy", name = pub_material_dat()$material_name,
        line = list(color = NORMAL_COLOR),
        hoverinfo = "text", hovertext = paste0(
          "</br> Date: ", pub_material_dat()$cal_date,
          "</br> CMC: ", paste0(.format_number(pub_material_dat()$quantity_cumsum), pub_material_dat()$product_unit)
        )
      ) %>%
      add_trace(
        x = last(pub_material_dat()$cal_date), y = last(pub_material_dat()$quantity_cumsum), 
        type = "scatter", mode = "markers+text", 
        marker = list(color = NORMAL_COLOR, size = 6), hoverinfo = "none",
        text = paste0(.format_number(last(pub_material_dat()$quantity_cumsum)), pub_material_dat()$product_unit),
        textfont = list(size = 12, color = NORMAL_COLOR),
        textposition = "bottom center",
        showlegend = FALSE
      ) %>% 
      layout(
        xaxis = list(zeroline = FALSE),
        yaxis = list(title = "Material Cumsum", zeroline = FALSE)
      ),
    plot_ly(opacity = OPACITY) %>% 
      add_trace(
        x = pub_products_dat()$cal_date, y = pub_products_dat()$quantity_cumsum,
        type = "scatter", mode = "lines",
        color = pub_products_dat()$product_name,
        colors = colorRampPalette(c(NORMAL_COLOR, BASE_COLOR))(uniqueN(pub_products_dat()$product_name)),
        hoverinfo = "text",
        hovertext = paste0(
          "</br> ", pub_products_dat()$product_name, ": ", 
          as.integer(pub_products_dat()$quantity_cumsum)
        )
      ) %>% 
      add_trace(
        x = pub_products_dat()[cal_date == input$pub_period[2]]$cal_date, 
        y = pub_products_dat()[cal_date == input$pub_period[2]]$quantity_cumsum, 
        marker = list(
          color = colorRampPalette(c(NORMAL_COLOR, BASE_COLOR))(uniqueN(pub_products_dat()$product_name)),
          size = 6
        ),
        type = "scatter", mode = "markers+text",
        text = as.integer(pub_products_dat()[cal_date == input$pub_period[2]]$quantity_cumsum),
        textfont = list(
          color = colorRampPalette(c(NORMAL_COLOR, BASE_COLOR))(uniqueN(pub_products_dat()$product_name)),
          size = 12
        ),
        textposition = "bottom center",
        hoverinfo = "none", showlegend = FALSE
      ) %>% 
      layout(
        xaxis = list(zeroline = FALSE),
        yaxis = list(title = "Quantity Cumsum", zeroline = FALSE)
      ),
    nrows = 2, shareX = TRUE, titleY = TRUE
  ) %>% 
    layout(
      hovermode = "x unified", 
      xaxis = list(title = "Date"),
      legend = list(orientation = "h", x=-0.1, y = 1.15)
    )
  fig
})


output$pub_products_periodly <- renderPlotly({
  
  fig <- subplot(
    plot_ly(opacity = OPACITY) %>% 
      add_trace(
        x = pub_material_dat()$cal_date, y = pub_material_dat()$quantity,
        type = "scatter", mode = "markers+lines", name = pub_material_dat()$material_name,
        marker = list(size = 4, color = NORMAL_COLOR),
        line = list(color = NORMAL_COLOR),
        hoverinfo = "text", 
        hovertext = paste0(
          "</br> Date: ", pub_material_dat()$cal_date,
          "</br> Material Consumption: ", paste0(.format_number(pub_material_dat()$quantity), pub_material_dat()$product_unit)
        )
      ) %>% 
      layout(
        xaxis = list(zeroline = FALSE),
        yaxis = list(zeroline = FALSE, title = "Material Consumption")
      ),
    plot_ly(opacity = OPACITY) %>% 
      add_trace(
        x = pub_products_dat()$cal_date, y = pub_products_dat()$product_quantity, type = "bar", 
        color = pub_products_dat()$product_name,
        colors = colorRampPalette(c(NORMAL_COLOR, BASE_COLOR))(uniqueN(pub_products_dat()$product_name)),
        hoverinfo = "text",
        hovertext = paste0(
          "</br> ", pub_products_dat()$product_name, ": ", 
          as.integer(pub_products_dat()$product_quantity)
        )
      ) %>% 
      layout(
        xaxis = list(zeroline = FALSE),
        yaxis = list(zeroline = FALSE, title = "Quantity"),
        barmode = "stack"
      ),
    nrows = 2, shareX = TRUE, titleY = TRUE
  ) %>% 
    layout(
      hovermode = "x unified", 
      xaxis = list(title = "Date"),
      legend = list(orientation = "h", x = -0.1, y = 1.15)
    )
  fig
    
})