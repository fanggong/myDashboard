output$pub_v_cost <- renderInfoBox({
  infoBox(
    title = "Cost",
    value = paste0("¥ ", sum(stock_flow_dat()$total_amount)),
    color = "primary"
  )
})

output$pub_v_customer <- renderInfoBox({
  tmp <- customer_detail_dat()[
    , .(
      balance_recharge = sum(balance_recharge),
      balance_gift = sum(balance_gift)
    ), .(cal_date)
  ]
  infoBox(
    title = "Recharge / Gift Sales",
    value = paste0("¥ ", first(tmp$balance_recharge) - last(tmp$balance_recharge), " / ",
                   "¥ ", first(tmp$balance_gift) - last(tmp$balance_gift)),
    color = "primary"
  )
})

output$pub_v_sales <- renderInfoBox({
  infoBox(
    title = "Sales",
    value = paste0("¥ ", sum(tkt_detail_dat()$total_amount)),
    color = "primary"
  )
})

output$pub_v_income <- renderInfoBox({
  tmp <- customer_detail_dat()[
    , .(
      balance = sum(balance)
    ), .(cal_date)
  ]
  infoBox(
    title = "Income",
    value = paste0("¥ ", sum(tkt_detail_dat()$total_amount) - (first(tmp$balance) - last(tmp$balance))),
    color = "primary"
  )
})
