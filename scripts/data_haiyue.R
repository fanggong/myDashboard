
conn <- dbConnect(
  RPostgres::Postgres() , dbname = "haiyue", user = pg_username, 
  password = pg_pwd, host = host, port = 5432
)


# data ----
## tkt_detail ----
sql <- "
select
  datetime,
  detail.product_barcode product_barcode,
  detail.name product_name,
  product_size,
  product_unit,
  material_name,
  material_barcode,
  case when ticket_type = 'SELL_RETURN' then -quantity else quantity end quantity,
  case when ticket_type = 'SELL_RETURN' then -total_amount else total_amount end total_amount
from (
  select datetime, uid, customer_uid, ticket_type
  from ticket
  where invalid = 0
) src
left join (
  select uid, product_barcode, name, quantity, total_amount
  from ticket_detail
) detail on src.uid = detail.uid
left join (
  select product_name, product_size, product_barcode, material_name, material_barcode, product_unit
  from material
) material on detail.product_barcode = material.product_barcode
"

tkt_detail <- dbGetQuery(conn, sql)
setDT(tkt_detail)

tkt_detail[
  , `:=`(
    datetime_adj = as.POSIXct(datetime, tz = "Asia/Shanghai") - 12*60*60,
    material_quantity = quantity * product_size
  )
][
  , `:=`(
    cal_date = as.Date(datetime_adj, tz = "Asia/Shanghai")
  )
]
tkt_detail <- tkt_detail[order(cal_date)]
tkt_detail <- tkt_detail[!is.na(material_name)]

tmp <- tkt_detail[
  , .(
    total_amount = sum(total_amount)
  ), .(material_name, material_barcode)
][
  order(total_amount, decreasing = TRUE)
]

pub_product_subjects <- tmp$material_barcode
names(pub_product_subjects) <- tmp$material_name


## customer_detail ----
sql <- "
select
  datetime,
  name,
  ticket_recharge_amount
from (
  select customer_uid, ticket_recharge_amount, datetime
  from ticket
  where invalid = 0
) src
inner join (
  select customer_uid, name
  from customer
) customer on src.customer_uid = customer.customer_uid
"
customer_detail <- dbGetQuery(conn, sql)
setDT(customer_detail)
customer_detail <- customer_detail[
  , `:=`(
    datetime_adj = as.POSIXct(datetime, tz = "Asia/Shanghai") - 12*60*60
  )
][
  , `:=`(
    cal_date = as.Date(datetime_adj, tz = "Asia/Shanghai")
  )
][
  , .(
    ticket_recharge_amount = sum(ticket_recharge_amount)
  ), .(cal_date, name)
]

pub_customer_subjects <- unique(customer_detail$name)


sql <- "
select 
  name,
  recharge_money, 
  gift_money,
  datetime
from (
  select customer_uid, recharge_money, gift_money, datetime
  from customer_recharge_log
) src
left join (
  select customer_uid, name
  from customer
) customer on src.customer_uid = customer.customer_uid
"
recharge <- dbGetQuery(conn, sql)
setDT(recharge)
recharge <- recharge[
  , `:=`(
    datetime_adj = as.POSIXct(datetime, tz = "Asia/Shanghai") - 12*60*60
  )
][
  , `:=`(
    cal_date = ifelse(
      as.Date(datetime_adj, tz = "Asia/Shanghai") == "2023-06-18", 
      as.Date("2023-06-15"), as.Date(datetime_adj, tz = "Asia/Shanghai")
    )
  )
][
  , .(
    recharge_money = sum(recharge_money),
    gift_money = sum(gift_money)
  ), .(cal_date, name)
]

tmp <- expand.grid(
  cal_date = seq.Date(as.Date("2023-06-15"), Sys.Date(), by = 1),
  name = pub_customer_subjects
)
setDT(tmp)

customer_detail <- merge(tmp, customer_detail, all.x = TRUE)
customer_detail <- merge(customer_detail, recharge, all.x = TRUE)
customer_detail[
  , `:=`(
    ticket_recharge_amount = nafill(ticket_recharge_amount, fill = 0),
    recharge_money = nafill(recharge_money, fill = 0),
    gift_money = nafill(gift_money, fill = 0)
  )
][
  , `:=`(
    recharge_money_cumsum = cumsum(recharge_money),
    gift_money_cumsum = cumsum(gift_money),
    ticket_recharge_amount_cumsum = cumsum(ticket_recharge_amount)
  ), .(name)
][
  , `:=`(
    balance = recharge_money_cumsum + gift_money_cumsum - ticket_recharge_amount_cumsum
  ), .(name)
][
  , `:=`(
    balance_recharge = ifelse(balance > gift_money_cumsum, balance - gift_money_cumsum, 0),
    balance_gift = ifelse(balance > gift_money_cumsum, gift_money_cumsum, balance)
  ), .(name)
]

## stock_flow ----
sql <- "
select
  product_uid, 
  product_name, 
  update_stock, 
  unit_quantity, 
  unit_buy_price,
  unit_quantity * unit_buy_price total_amount,
  confirmed_time
from stock_flow
where stock_flow_type_number = 12
order by confirmed_time
"
stock_flow <- dbGetQuery(conn, sql)
setDT(stock_flow)
stock_flow[
  , `:=`(
    confirmed_date = as.Date(as.POSIXct(confirmed_time, tz = "Asia/Shanghai") - 12*60*60, tz = "Asia/Shanghai")
  )
]
stock_flow

dbDisconnect(conn)

