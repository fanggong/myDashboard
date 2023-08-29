library(RPostgres)
library(stringr)

source("utils.R")
source("scripts/updater_.R")
source("config.R")


nowdate <- Sys.Date()

# quant_r ----
conn <- RPostgres::dbConnect(
  RPostgres::Postgres() , dbname = "quant_r", user = pg_username,
  password = pg_pwd, host = host, port = 5432
)
api <- Tushare::pro_api(token = ts_token)
updater <- stockUpdater$new(api = api, nowdate = nowdate, conn = conn)
updater$update_daily()

# haiyue ----
conn <- dbConnect(
  RPostgres::Postgres() , dbname = "haiyue", user = pg_username,
  password = pg_pwd, host = host, port = 5432
)
updater <- yinbaoUpdater$new(app_id = app_id, app_key = app_key, conn = conn, nowdate = nowdate - 1)
updater$update_ticket()
updater$update_product()
updater$update_customer()
updater$update_stock_flow()
updater$update_customer_recharge_log()

updater <- yinbaoUpdater$new(app_id = app_id, app_key = app_key, conn = conn, nowdate = nowdate)
updater$update_ticket()
updater$update_stock_flow()
updater$update_customer_recharge_log()
