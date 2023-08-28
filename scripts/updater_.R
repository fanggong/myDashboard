# stock updater ----
stockUpdater <- R6::R6Class(
  classname = "stockUpdater",
  public = list(
    conn = NA,
    api = NA,
    nowdate = NA,
    initialize = function(nowdate = Sys.Date(), api = NA, conn = NA) {
      if (!lubridate::is.Date(nowdate)) {
        nowdate <- as.Date(nowdate, origin = "1970-01-01")
      }
      self$nowdate <- format(nowdate, "%Y%m%d")
      self$api <- api
      self$conn <- conn
    },
    get_query = function(sql) {
      dbGetQuery(self$conn, sql)
    },
    retry_api = function(api_name, ...) {
      result <- NULL
      retry <- 0
      while (is.null(result) && retry < 5) {
        tryCatch({
          result <- self$api(api_name = api_name, ...)
        }, error = function(e) {
          retry <<- retry + 1
          message("Error occurred, retrying (", retry, ")...")
        })
      }
      result
    },
    full_update = function(tbl_name, dat) {
      if (is.null(dat)) {
        message("No Data Available")
        return(NULL)
      }
      dat["updated_at"] <- Sys.time()
      if (!tbl_name %in% dbListTables(self$conn)) {
        dbCreateTable(self$conn, tbl_name, dat)
      } else {
        sql <- "delete from {tbl_name} where 1 = 1"
        sql <- .str_replace(sql, "tbl_name", tbl_name)
        dbSendQuery(self$conn, sql)
      }
      tmp <- dbAppendTable(self$conn, tbl_name, dat)
      message(sprintf("%s %s updated", tbl_name, tmp))
    },
    partition_update = function(tbl_name, dat, index, partition) {
      if (is.null(dat)) {
        message("No Data Available")
        return(NULL)
      }
      dat["updated_at"] <- Sys.time()
      if (!tbl_name %in% dbListTables(self$conn)) {
        dbCreateTable(self$conn, tbl_name, dat)
        if (!is.null(index)) {
          for (each in index) {
            sql <- "create index {index_name} on {tbl_name} ({index})"
            sql <- .str_replace(sql, "index_name", paste(c("ix", tbl_name, each), collapse = "_"))
            sql <- .str_replace(sql, "tbl_name", tbl_name)
            sql <- .str_replace(sql, "index", each)
            dbSendQuery(self$conn, sql)
          }
        }
      } else {
        sql <- "delete from {tbl_name} where {partition}"
        sql <- .str_replace(sql, "tbl_name", tbl_name)
        partition <- .format_partition(partition)
        sql <- .str_replace(sql, "partition", partition)
        dbSendQuery(self$conn, sql)
      }
      tmp <- dbAppendTable(self$conn, tbl_name, dat)
      message(sprintf("%s %s %s updated", tbl_name, partition, tmp))
    },
    update_trade_cal = function(...) {
      tbl_name <- "trade_cal"
      dat <- self$retry_api(
        api_name = tbl_name, exchange = "SSE", 
        start_date = "20220101", end_date = paste0(format(Sys.Date(), "%Y"), "1231")
      )
      self$full_update(tbl_name = tbl_name, dat = dat)
    },
    update_stock_basic = function(...) {
      tbl_name <- "stock_basic"
      dat <- self$retry_api(
        api_name = tbl_name, exchange = "", list_status = "L", 
        fields = "ts_code,name,area,industry,market"
      )
      self$full_update(tbl_name = tbl_name, dat = dat)
    },
    update_stock_company = function(...) {
      tbl_name <- "stock_company"
      dat <- self$retry_api(
        api_name = tbl_name, 
        fields = "ts_code,exchange,chairman,manager,secretary,reg_capital,setup_date,province,city,introduction,website,email,office,employees,main_business,business_scope"
      )
      self$full_update(tbl_name = tbl_name, dat = dat)
    },
    update_daily_basic = function(...) {
      tbl_name <- "daily_basic"
      dat <- self$retry_api(
        api_name = tbl_name, trade_date = self$nowdate,
        fields = "ts_code,trade_date,close,turnover_rate,turnover_rate_f,volume_ratio,pe,pe_ttm,pb,ps,ps_ttm,dv_ratio,dv_ttm,total_share,float_share,free_share,total_mv,circ_mv"
      )
      self$partition_update(tbl_name, dat, c("ts_code", "trade_date"), list(trade_date = self$nowdate))
    },
    update_daily = function(...) {
      tbl_name <- "daily"
      dat <- self$retry_api(api_name = tbl_name, trade_date = self$nowdate)
      self$partition_update(tbl_name, dat, c("ts_code", "trade_date"), list(trade_date = self$nowdate))
    },
    update_moneyflow = function(...) {
      tbl_name <- "moneyflow"
      dat <- self$retry_api(api_name = tbl_name, trade_date = self$nowdate)
      self$partition_update(tbl_name, dat, c("ts_code", "trade_date"), list(trade_date = self$nowdate))
    },
    update_moneyflow_hsgt = function(...) {
      tbl_name <- "moneyflow_hsgt"
      dat <- self$retry_api(api_name = tbl_name, trade_date = self$nowdate)
      self$partition_update(tbl_name, dat, NULL, list(trade_date = self$nowdate))
    },
    update_stock_position = function(quant_r_stock_position_path, ...) {
      tbl_name <- "stock_position"
      dat <- read.xlsx(quant_r_stock_position_path)
      self$full_update(tbl_name, dat)
    }
  )
)



# yinbao updater ----
yinbaoUpdater <- R6::R6Class(
  classname = "yinbaoUpdater",
  public = list(
    url = "https://area71-win.pospal.cn:443/pospal-api2/openapi/v1",
    app_id = NA,
    app_key = NA,
    conn = NA,
    nowdate = NA,
    initialize = function(app_id, app_key, conn, nowdate = Sys.Date()) {
      if (!lubridate::is.Date(nowdate)) {
        nowdate <- as.Date(nowdate, origin = "1970-01-01")
      }
      self$app_id <- app_id
      self$app_key <- app_key
      self$conn <- conn
      self$nowdate <- format(nowdate, "%Y-%m-%d")
    },
    get_timestamp = function() {
      as.integer(Sys.time()) * 1000
    },
    get_body = function(...) {
      tmp <- list(appId = self$app_id, ...)
      jsonlite::toJSON(tmp, auto_unbox = TRUE)
    },
    get_signature = function(body) {
      toupper(digest::digest(paste0(self$app_key, body), algo = "md5", serialize = FALSE))
    },
    get_headers = function(timestamp, signature) {
      headers <- c(
        "User-Agent" = "openApi",
        "Content-Type" = "application/json; charset=utf-8",
        "accept-encoding" = "gzip,deflate",
        "time-stamp" = timestamp,
        "data-signature" = signature
      )
      headers
    },
    get_result = function(api, ...) {
      timestamp <- self$get_timestamp()
      body <- self$get_body(...)
      signature <- self$get_signature(body)
      headers <- self$get_headers(timestamp, signature)
      # print(body)
      # print(signature)
      # print(headers)
      # print(paste0(self$url, api))
      response <- httr::POST(
        url = paste0(self$url, api), httr::add_headers(.headers = headers), body = body
      )
      result <- httr::content(response)
      if (result$status == "success") {
        return(result$data)
      } else {
        stop(result$messages[[1]])
      }
    },
    full_update = function(tbl_name, dat) {
      if (is.null(dat)) {
        message("No Data Available")
        return(NULL)
      }
      data.table::setDF(dat)
      dat["updated_at"] <- Sys.time()
      if (!tbl_name %in% dbListTables(self$conn)) {
        dbCreateTable(self$conn, tbl_name, dat)
      } else {
        sql <- "delete from {tbl_name} where 1 = 1"
        sql <- .str_replace(sql, "tbl_name", tbl_name)
        dbSendQuery(self$conn, sql)
      }
      tmp <- dbAppendTable(self$conn, tbl_name, dat)
      message(sprintf("%s %s updated", tbl_name, tmp))
    },
    partition_update = function(tbl_name, dat, index, partition) {
      if (is.null(dat)) {
        message("No Data Available")
        return(NULL)
      }
      data.table::setDF(dat)
      dat["updated_at"] <- Sys.time()
      if (!tbl_name %in% dbListTables(self$conn)) {
        dbCreateTable(self$conn, tbl_name, dat)
        if (!is.null(index)) {
          for (each in index) {
            sql <- "create index {index_name} on {tbl_name} ({index})"
            sql <- .str_replace(sql, "index_name", paste(c("ix", tbl_name, each), collapse = "_"))
            sql <- .str_replace(sql, "tbl_name", tbl_name)
            sql <- .str_replace(sql, "index", each)
            dbSendQuery(self$conn, sql)
          }
        }
      } else {
        sql <- "delete from {tbl_name} where {partition}"
        sql <- .str_replace(sql, "tbl_name", tbl_name)
        partition <- .format_partition(partition)
        sql <- .str_replace(sql, "partition", partition)
        dbSendQuery(self$conn, sql)
      }
      tmp <- dbAppendTable(self$conn, tbl_name, dat)
      message(sprintf("%s %s %s updated", tbl_name, partition, tmp))
    },
    update_ticket = function(...) {
      startTime <- paste0(self$nowdate, " 00:00:00")
      endTime <- paste0(self$nowdate, " 23:59:59")
      api <- "/ticketOpenApi/queryTicketPages"
      result <- self$get_result(api = api, startTime = startTime, endTime = endTime)$result
      if (length(result) == 0) {
        message("No data available between ", startTime, " and ", endTime)
        return(NULL)
      }
      for (i in 1:length(result)) {
        dat <- result[[i]]
        payments <- data.table::rbindlist(lapply(result[[i]]$payments, data.table::as.data.table))
        ticket_recharge_amount <- payments[code == "payCode_2"]$amount
        ticket_other_amount <- payments[code != "payCode_2"]$amount

        ticket <- data.table::data.table(
          casher_uid = dat$cashierUid,
          customer_uid = dat$customerUid,
          uid = dat$uid,
          sn = dat$sn,
          datetime = dat$datetime,
          total_amount = dat$totalAmount,
          ticket_recharge_amount = ifelse(length(ticket_recharge_amount) == 0, 0, ticket_recharge_amount),
          ticket_other_amount = ticket_other_amount,
          total_profit = dat$totalProfit,
          discount = dat$discount,
          rounding = dat$rounding,
          ticket_type = dat$ticketType,
          invalid = dat$invalid,
          sys_update_time = dat$sysUpdateTime,
          remark = dat$remark,
          people_num = dat$ticketSpendDetail$peopleNum,
          spend_out_store = dat$ticketSpendDetail$spendOutStore
        )
        self$partition_update(
          tbl_name = "ticket", dat = ticket, index = NULL,
          partition = list(uid = dat$uid, datetime = c(startTime, endTime))
        )

        ticket_detail <- data.table::rbindlist(lapply(dat$items, data.table::as.data.table))
        ticket_detail <- ticket_detail[
          , .(
            uid = dat$uid,
            id = id,
            name = name,
            buy_price = buyPrice,
            sell_price = sellPrice,
            customer_price = customerPrice,
            quantity = quantity,
            discount = discount,
            customer_discount = customerDiscount,
            total_amount = totalAmount,
            total_profit = totalProfit,
            is_customer_discount = isCustomerDiscount,
            product_uid = productUid,
            product_barcode = productBarcode
          )
        ]
        self$partition_update(
          tbl_name = "ticket_detail", dat = ticket_detail, index = NULL,
          partition = list(uid = dat$uid)
        )
      }
    },
    update_product = function(...) {

      api <- "/productOpenApi/queryProductCategoryPages"
      result <- self$get_result(api = api)$result
      product_category <- data.table::rbindlist(lapply(result, data.table::as.data.table))

      api <- "/productOpenApi/queryProductPages"
      product <- list()
      result <- self$get_result(api = api)
      while (length(result$result) == result$pageSize) {
        product <- c(product, result$result)
        result <- self$get_result(api = api, postBackParameter = result$postBackParameter)
      }
      product <- c(product, result$result)
      
      product <- data.table::rbindlist(lapply(product, data.table::as.data.table), fill = TRUE)
      
      product <- data.table::merge.data.table(
        product, product_category, by.x = "categoryUid", by.y = "uid",
        suffixes = c("_product", "_product_category")
      )
      
      product <- product[
        , .(
          uid = uid,
          name = name_product,
          category_uid = categoryUid,
          category_name = name_product_category,
          barcode = barcode,
          buy_price = buyPrice,
          sell_price = sellPrice,
          sell_price2 = sellPrice2,
          stock = stock,
          max_stock = maxStock,
          min_stock = minStock,
          no_stock = noStock,
          pinyin = pinyin,
          customer_price = customerPrice,
          description = description,
          is_customer_discount = isCustomerDiscount,
          supplier_uid = supplierUid,
          enable = enable,
          created_datetime = createdDatetime,
          updated_datetime = updatedDatetime
        )
      ]
      self$full_update(tbl_name = "product", dat = product)
      source("scripts/maintain_haiyue.R")
    },
    update_customer = function(...) {
      api <- "/customerOpenApi/queryCustomerPages"
      result <- self$get_result(api = api)$result
      customer <- list()
      for (i in 1:length(result)) {
        if (result[[i]]$enable == 1) {
          dat <- result[[i]]
          tmp <- data.table::data.table(
            customer_uid = dat$customerUid,
            category_name = dat$categoryName,
            number = dat$number,
            name = dat$name,
            point = dat$point,
            discount = dat$discount,
            balance = dat$balance,
            phone = dat$phone,
            birthday = dat$birthday,
            qq = dat$qq,
            email = dat$email,
            remarks = ifelse(is.null(dat$remarks), "", dat$remarks),
            create_date = dat$createdDate,
            on_account = dat$onAccount,
            create_store_appid_or_account = dat$createStoreAppIdOrAccount,
            total_point = dat$extInfo$totalPoint,
            total_ticket_amount = dat$extInfo$totalTicketAmount,
            total_recharge_amount = ifelse(is.null(dat$extInfo$totalRechargeAmount), 0, dat$extInfo$totalRechargeAmount),
            total_ticket_num = dat$extInfo$totalTicketNum
          )
          customer <- c(customer, list(tmp))
        }
      }
      customer <- data.table::rbindlist(customer, fill = TRUE)
      self$full_update("customer", customer)
    },
    update_stock_flow = function(...) {
      startTime <- paste0(self$nowdate, " 00:00:00")
      endTime <- paste0(self$nowdate, " 23:59:59")
      excludeEndTime <- paste0(as.Date(self$nowdate) + 1, " 00:00:00")
      api <- "/stockFlowOpenApi/queryStockFlowPages"
      result <- self$get_result(api = api, startTime = startTime, excludeEndTime = excludeEndTime)$result
      if (length(result) == 0) {
        message("No data available between ", startTime, " and ", endTime)
        return(NULL)
      }
      stk_flow <- list()
      for (i in 1:length(result)) {
        dat <- result[[i]]
        items <- data.table::rbindlist(lapply(dat$items, data.table::as.data.table), fill = TRUE)
        tmp <- data.table::data.table(
          to_user_app_id = dat$toUserAppId,
          to_user_account = dat$toUserAccount,
          to_user_company = dat$toUserCompany,
          operator_user_app_id = dat$operatorUserAppId,
          operator_user_account = dat$operatorUserAccount,
          operator_user_company = dat$operatorUserCompany,
          id = dat$id,
          product_uid = items$productUid,
          category_uid = items$categoryUid,
          supplier_uid = items$supplierUid,
          product_name = items$productName,
          sell_price = items$sellPrice,
          update_stock = items$updateStock,
          buy_price = items$buyPrice,
          barcode = items$barcode,
          product_unit_uid = items$productUnitUid,
          unit_quantity = items$unitQuantity,
          unit_buy_price = items$unitBuyPrice,
          confirmation_required = dat$confirmationRequired,
          created_datetime = dat$createdDateTime,
          stock_flow_type_number = dat$stockflowTypeNumber,
          confirmed = dat$confirmed,
          variance_confirmation = dat$varianceConfirmation,
          confirmed_time = dat$confirmedTime,
          remarks = dat$remarks
        )
        stk_flow <- c(stk_flow, list(tmp))
      }
      stk_flow <- data.table::rbindlist(stk_flow, fill = TRUE)
      self$partition_update(
        tbl_name = "stock_flow", dat = stk_flow, index = NULL,
        partition = list(confirmed_time = c(startTime, endTime))
      )
    },
    update_customer_recharge_log = function(...) {
      stateDate <- paste0(self$nowdate, " 00:00:00")
      endDate <- paste0(self$nowdate, " 23:59:59")
      api <- "/customerOpenApi/queryAllRechargeLogs"
      result <- self$get_result(api = api, stateDate = stateDate, endDate = endDate)$result
      if (length(result) == 0) {
        message("No data available between ", stateDate, " and ", endDate)
        return(NULL)
      }
      result <- data.table::rbindlist(lapply(result, data.table::as.data.table))
      result <- data.table::data.table(
        recharge_store_app_id = result$rechargeStoreAppId,
        recharge_store_account = result$rechargeStoreAccount,
        customer_uid = result$customerUid,
        casher_uid = result$cashierUid,
        recharge_money = result$rechargeMoney,
        gift_money = result$giftMoney,
        datetime = result$datetime,
        uid = result$uid,
        pay_method = result$payMethod,
        pay_method_code = result$payMethodCode,
        remark = result$remark
      )
      self$full_update("customer_recharge_log", result)
    }
  )
)

# test ----
# conn <- dbConnect(
#   RPostgres::Postgres() , dbname = "haiyue", user = pg_username,
#   password = pg_pwd, host = host, port = 5432
# )
# # tmp <- stockUpdater$new(api = api, nowdate = "2022-12-31", conn = conn)
# updater <- yinbaoUpdater$new(app_id = app_id, app_key = app_key, conn = conn, nowdate = "2023-08-25")
# result <- updater$update_product()

# funcs ----

update_data_management <- function(tbl_name, conn, partition, database) {
  sql <- "select partition from data_manage where tbl_name = {tbl_name}"
  sql <- glue::glue_sql(sql, tbl_name = tbl_name, .con = conn)
  
  is_exist <- length(dbGetQuery(conn, sql)$partition) != 0
  
  if (!is_exist) {
    partition <- TBL_LST[[database]]$partition[TBL_LST[[database]]$tbl_name == tbl_name]
    tmp <- data.frame(
      tbl_name = tbl_name,
      partition = partition
    )
  } else {
    partition <- dbGetQuery(conn, sql)$partition
  }
  
  sql <- "select max({partition}) from {tbl_name}"
  sql <- .str_replace(sql, "partition", partition)
  sql <- .str_replace(sql, "tbl_name", tbl_name)
  newest_partition <- dbGetQuery(conn, sql)[1, 1]
  if (is.timepoint(newest_partition)) {
    newest_partition <- .as_character(newest_partition)
  }
  
  if (!is_exist) {
    tmp["newest_partition"] <- newest_partition
    tmp["updated_at"] <- Sys.time()
    dbAppendTable(conn, "data_manage", tmp)
  } else {
    sql <- "
    update data_manage
    set 
      newest_partition = {newest_partition},
      updated_at = {nowtime}
    where tbl_name = {tbl_name}
    "
    sql <- glue::glue_sql(
      sql, tbl_name = tbl_name, newest_partition = newest_partition,
      nowtime = as.character(Sys.time(), tz = "UTC"), .con = conn
    )
    dbSendQuery(conn, sql)
  }

  message("data manage updated")
}


update_pub <- function(tbl_lst, start_date, end_date, app_id, app_key, conn) {
  message("----------- Yinbao Updater Start -----------", sep = "")
  message("Table Name: ", paste(tbl_lst, collapse = ", "), sep = "")
  message("Period: From ", start_date, " to ", end_date, sep = "")
  suppressWarnings({
    for (nowdate in seq.Date(as.Date(start_date), as.Date(end_date), by = 1)) {
      tmp <- yinbaoUpdater$new(app_id = app_id, app_key = app_key, conn = conn, nowdate = nowdate)
      for (tbl_name in tbl_lst) {
        tmp[[paste0("update_", tbl_name)]]()
        update_data_management(
          tbl_name, conn, TBL_LST$haiyue$partition[TBL_LST$haiyue$tbl_name == tbl_name], "haiyue"
        )
      }
    }
  })
  message("----------- Yinbao Updater End -----------", sep = "")
}


update_stock <- function(tbl_lst, start_date, end_date, api, conn, ...) {
  message("----------- Stock Updater Start -----------", sep = "")
  message("Table Name: ", paste(tbl_lst, collapse = ", "), sep = "")
  message("Period: From ", start_date, " to ", end_date, sep = "")
  suppressWarnings({
    for (nowdate in seq.Date(as.Date(start_date), as.Date(end_date), by = 1)) {
      if (nowdate %in% TRADE_DATE) {
        tmp <- stockUpdater$new(api = api, nowdate = nowdate, conn = conn)
        for (tbl_name in tbl_lst) {
          tmp[[paste0("update_", tbl_name)]](...)
          update_data_management(
            tbl_name, conn, TBL_LST$quant_r$partition[TBL_LST$quant_r$tbl_name == tbl_name], "quant_r"
          )
        }
      }
    }
  })
  message("----------- Stock Updater End -----------", sep = "")
}


