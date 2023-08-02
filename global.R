library(shiny)
library(fresh)
library(shinythemes)
library(shinymanager)
library(bs4Dash)
library(shinyWidgets)
library(shinyjs)
library(glue)

library(data.table)
library(stringr)
library(lubridate)
library(plotly)
library(daterangepicker)
library(DT)

library(RPostgres)
library(Tushare)
library(openxlsx)
library(R6)

source("config.R")
source("utils.R")
source("scripts/updater.R")

options(shiny.reactlog = TRUE)

## plot setting ----
UP_COLOR <- "#E83015"
DOWN_COLOR <- "#227D51" 
NORMAL_COLOR <- "#3182BD"
BASE_COLOR <- "#A96360"
OPACITY <- 0.7


## credential ----
check_creds <- function() {
  conn <- dbConnect(
    RPostgres::Postgres() , dbname = "quant_r", user = pg_username, 
    password = pg_pwd, host = host, port = 5432
  )
  function(user, password) {
    sql <- "select * from qt_user where user_name = {user} and password = {password}"
    sql <- glue_sql(sql, user = user, password = password, .con = conn)
    res <- dbGetQuery(conn, sql)
    if (nrow(res) > 0) {
      list(result = TRUE)
    } else {
      list(result = FALSE)
    }
  }
}


## data ----
#### data management ----
###### constant ----
TBL_LST <- list(
  quant_r = data.frame(
    tbl_name = c("daily", "stock_position", "trade_cal"),
    partition = c("trade_date", "trade_date", "cal_date")
  ),
  haiyue = data.frame(
    tbl_name = c("ticket", "product", "stock_flow", "customer"),
    partition = c("datetime", "updated_at", "confirmed_time", "updated_at")
  )
)


###### funcs ----
get_data_status <- function(tar = list("quant_r", "haiyue")) {
  result <- list()
  for (i in 1:length(tar)) {
    conn <- dbConnect(
      RPostgres::Postgres() , dbname = tar[[i]], user = pg_username, 
      password = pg_pwd, host = host, port = 5432
    )
    tmp <- dbGetQuery(conn, "select * from data_manage")
    tmp <- list(tmp)
    names(tmp) <- tar[[i]]
    result <- append(result, tmp)
    dbDisconnect(conn)
  }
  result <- data.table::rbindlist(result, idcol = "database")
  result$updated_at <- .as_character(result$updated_at)
  setDT(result)
  result
}

dat_status <- get_data_status()

#### stock ----
###### setting ----
conn <- dbConnect(
  RPostgres::Postgres() , dbname = "quant_r", user = pg_username, 
  password = pg_pwd, host = host, port = 5432
)
api <- Tushare::pro_api(token = ts_token)

###### constant ----
sql <- "select cal_date from trade_cal where is_open = 1"
TRADE_DATE <- as.Date(dbGetQuery(conn, sql)$cal_date, format = "%Y%m%d")

sql <- "select cal_date from trade_cal where is_open = 0"
NO_TRADE_DATE <- as.Date(dbGetQuery(conn, sql)$cal_date, format = "%Y%m%d")

sql <- "select max(trade_date) cal_date from daily limit 10"
UPDATE_DATE <- as.Date(dbGetQuery(conn, sql)$cal_date, format = "%Y%m%d")

###### data ----
sql <- "
select
  src.cal_date cal_date,
  pos.ts_code code,
  stk_basic.name name,
  sum(size) size,
  cast(sum(price * size) as decimal(18, 2)) cost,
  cast(sum(close * size) as decimal(18, 2)) value
from (
  select cal_date
  from trade_cal
  where is_open = 1 
    and cal_date between '20221230' and {nowdate}
) src
left join (
  select ts_code, trade_date, price, size
  from stock_position
) pos on src.cal_date >= pos.trade_date
left join (
  select trade_date, ts_code, close
  from daily
) daily on src.cal_date = daily.trade_date and pos.ts_code = daily.ts_code
left join (
  select ts_code, name
  from stock_basic
) stk_basic on pos.ts_code = stk_basic.ts_code
group by src.cal_date, pos.ts_code, stk_basic.name
order by cost desc
"
sql <- glue_sql(sql, nowdate = .format_date(UPDATE_DATE), .con = conn)
stock_pnl_detail <- dbGetQuery(conn, sql)
setDT(stock_pnl_detail)

stock_pnl_detail <- stock_pnl_detail[
  , `:=`(cal_date = as.Date(cal_date, tryFormats = "%Y%m%d"))
]

stock_pnl_daily <- stock_pnl_detail[
  order(cal_date), .(
    start_date = min(cal_date),
    end_date = max(cal_date),
    cost = sum(cost),
    value = sum(value),
    all = sum(value) - sum(cost)
  ), .(cal_date)  
][
  , `:=`(
    periodly = all - shift(all, n = 1, type = "lag")
  )
][
  !is.na(periodly)
][
  , `:=`(
    periodly_cumsum = cumsum(periodly),
    cal_date = .mid_date(start_date, end_date)
  )
][
  , .(cal_date, start_date, end_date, all, periodly, periodly_cumsum)
]


tmp <- unique(stock_pnl_detail[order(cost, decreasing = TRUE), .(code, name)])
stock_subjects <- tmp$code
names(stock_subjects) <- tmp$name

dbDisconnect(conn = conn)


#### pub ----
###### setting ----
conn <- dbConnect(
  RPostgres::Postgres() , dbname = "haiyue", user = pg_username, 
  password = pg_pwd, host = host, port = 5432
)

###### data ----
sql <- "
select
  datetime,
  detail.product_barcode product_barcode,
  name product_name,
  product_size,
  product_unit,
  material_name,
  material_barcode,
  quantity,
  total_amount
from (
  select datetime, uid
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

tmp <- dbGetQuery(conn, "select material_name, material_barcode from material")
setDT(tmp)
tmp <- unique(tmp)

pub_subjects <- tmp$material_barcode
names(pub_subjects) <- tmp$material_name


dbDisconnect(conn = conn)


