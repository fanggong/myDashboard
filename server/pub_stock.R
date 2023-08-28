output$pub_stock_dat <- renderDT(
  {
    stock_flow[
      order(confirmed_time), .(
        datetime = confirmed_time,
        productName = product_name,
        updateStock = update_stock,
        buyPrice = round(unit_buy_price, 2),
        totalAmount = round(total_amount, 2)
      )
    ]
  },
  extensions = 'Buttons', 
  options = list(
    scrollX = TRUE,
    dom = 'Bfrtip', 
    buttons = list(
      list(extend = "copy"), 
      list(
        extend = "excel", filename = paste0(
          "stock_flow_", str_remove_all(input$pub_period[1], "-"), "_",
          str_remove_all(input$pub_period[2], "-")
        )
      )
    )
  )
)