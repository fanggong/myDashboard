library(RPostgres)
library(stringr)

source("utils.R")
source("scripts/updater_.R")
source("config.R")


nowdate <- Sys.Date() - 1

# constant ----
TBL_LST <- list(
  quant_r = data.frame(
    tbl_name = c("daily", "stock_position", "trade_cal"),
    partition = c("trade_date", "trade_date", "cal_date")
  ),
  haiyue = data.frame(
    tbl_name = c("ticket", "product", "stock_flow", "customer",
                 "customer_recharge_log"),
    partition = c("datetime", "updated_at", "confirmed_time", "updated_at",
                  "updated_at")
  )
)


# quant_r ----
conn <- RPostgres::dbConnect(
  RPostgres::Postgres() , dbname = "quant_r", user = pg_username,
  password = pg_pwd, host = host, port = 5432
)
api <- Tushare::pro_api(token = ts_token)

sql <- "select cal_date from trade_cal where is_open = 1"
TRADE_DATE <- as.Date(dbGetQuery(conn, sql)$cal_date, format = "%Y%m%d")

update_stock(
  tbl_lst = c("daily"), 
  start_date = nowdate, end_date = nowdate,
  api = api, conn = conn
)

# haiyue ----
conn <- dbConnect(
  RPostgres::Postgres() , dbname = "haiyue", user = pg_username,
  password = pg_pwd, host = host, port = 5432
)
update_pub(
  tbl_lst = c("ticket", "product", "customer", "stock_flow", "customer_recharge_log"), 
  start_date = nowdate - 1, end_date = nowdate,
  app_id = app_id, app_key = app_key, conn
)
