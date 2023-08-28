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
library(jsonlite)

library(RPostgres)
library(Tushare)
library(openxlsx)
library(R6)

source("config.R")
source("utils.R")
source("scripts/updater_.R")
source("scripts/data_haiyue.R")
source("scripts/data_quant_r.R")

options(shiny.reactlog = TRUE)

## plot setting ----
UP_COLOR <- "#E83015"
DOWN_COLOR <- "#227D51" 
NORMAL_COLOR <- "#3182BD"
FILL_COLOR <- "rgba(49,130,189,0.5)"
BASE_COLOR <- "#A96360"
WHITE <- "#FFFFFF"
BLANK <- "rgba(0,0,0,0)"
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

## constant ----
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

## funcs ----
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



