
conn <- dbConnect(
  RPostgres::Postgres() , dbname = "quant_r", user = pg_username, 
  password = pg_pwd, host = host, port = 5432
)
api <- Tushare::pro_api(token = ts_token)

# constant ----
sql <- "select cal_date from trade_cal where is_open = 1"
TRADE_DATE <- as.Date(dbGetQuery(conn, sql)$cal_date, format = "%Y%m%d")

sql <- "select cal_date from trade_cal where is_open = 0"
NO_TRADE_DATE <- as.Date(dbGetQuery(conn, sql)$cal_date, format = "%Y%m%d")

sql <- "select max(trade_date) cal_date from daily limit 10"
UPDATE_DATE <- as.Date(dbGetQuery(conn, sql)$cal_date, format = "%Y%m%d")

# stock_pnl_detail ----
sql <- "
select
  src.cal_date cal_date,
  pos.ts_code code,
  stk_basic.name name_t,
  sum(size) size,
  cast(sum(price * size) as decimal(18, 2)) cost_t,
  cast(sum(close * size) as decimal(18, 2)) value_t
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
order by cost_t desc
"
sql <- glue_sql(sql, nowdate = .format_date(UPDATE_DATE), .con = conn)
stock_pnl_detail <- dbGetQuery(conn, sql)
names(stock_pnl_detail) <- c("cal_date", "code", "name", "size", "cost", "value")
setDT(stock_pnl_detail)
stock_pnl_detail <- stock_pnl_detail[order(cal_date)]


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

stock_pnl_detail <- stock_pnl_detail[order(cal_date)][
  , `:=`(
    start_date = cal_date,
    end_date = cal_date,
    all = value - cost
  )
][
  , `:=`(
    daily = all - shift(all, n = 1, type = "lag")
  ), .(code)
][
  , `:=`(
    daily = fifelse(is.na(daily), first(all), daily)
  ), .(code)
][
  cal_date >= "2023-01-01"
][
  , `:=`(
    daily_cumsum = cumsum(daily),
    cal_date = .mid_date(start_date, end_date)
  ), .(code)
][
  , .(
    cal_date, start_date, end_date, code, name, size, value, all, daily, daily_cumsum
  )
]

dbDisconnect(conn = conn)



