suppressWarnings({
  # setting ----
  conn <- dbConnect(
    RPostgres::Postgres() , dbname = "haiyue", user = pg_username, 
    password = pg_pwd, host = host, port = 5432
  )
  
  ## delete test product ----
  sql <- "
  delete from stock_flow 
  where barcode in ('2306191242211-500', '2306191242211-350')
  "
  dbSendQuery(conn, sql)
  
  ## putao buy price error maintain ----
  sql <- "
  delete from stock_flow
  where id in ('1747235', '1747236')
  "
  dbSendQuery(conn, sql)
  
  sql <- "
  update stock_flow
  set 
    buy_price = 32,
    unit_buy_price = 32
  where id = '1740427' and barcode = '2306302357247'
  "
  dbSendQuery(conn, sql)
  
  ## update product name and barcode in ticket detail ----
  sql <- "
  update ticket_detail
  set 
    name = '麻蛇博士三倍西海岸-350ml'
  where product_barcode = '2197220525357'
  "
  dbSendQuery(conn, sql)
  
  sql <- "
  update ticket_detail
  set
    name = '(美)场外深渊双倍IPA-350ml'
  where product_barcode = '2412590145265'
  "
  dbSendQuery(conn, sql)
  
  sql <- "
  update ticket_detail
  set
    name = '伏魔IPA-350ml',
    product_uid = 584817481406207744,
    product_barcode = '2306181513208'
  where name = '伏魔IPA-380ml'
  "
  dbSendQuery(conn, sql)
  
  sql <- "
  update ticket_detail
  set
    name = '美国黑烟囱响亮多汁-350ml',
    product_uid = 1086870015501579392,
    product_barcode = '2353323736674'
  where name in ('黑烟囱', '美国黑烟囱响亮多汁-350ml')
  "
  dbSendQuery(conn, sql)
  
  sql <- "
  update ticket_detail
  set
    name = '熊猫小麦白啤-1瓶',
    product_uid = 872362974775156992,
    product_barcode = '2418992018411'
  where name = '熊猫小麦白啤'
  "
  dbSendQuery(conn, sql)
  
  sql <- "
  update ticket_detail
  set 
    name = '熊猫小麦精酿白啤-6瓶',
    product_uid = 238045898780207744,
    product_barcode = '2959232931768'
  where name = '熊猫小麦白啤-6瓶'
  "
  dbSendQuery(conn, sql)
  
  sql <- "
  update ticket_detail
  set 
    name = '熊猫小麦精酿白啤-6瓶',
    product_uid = 238045898780207744,
    product_barcode = '2959232931768'
  where name = '熊猫小麦白啤-6瓶'
  "
  dbSendQuery(conn, sql)
  
  sql <- "
  update ticket_detail
  set 
    name = '熊猫小麦精酿白啤-12瓶',
    product_uid = 149583064416372704,
    product_barcode = '2476943124741'
  where name = '熊猫小麦白啤-12瓶'
  "
  dbSendQuery(conn, sql)
  
  
  sql <- "
  update ticket_detail
  set
    name = '德式贰发小麦-350ml',
    product_uid = 849259858408043136,
    product_barcode = '2306181503308'
  where name = '德式小麦-350ml'
  "
  dbSendQuery(conn, sql)
  
  sql <- "
  update ticket_detail
  set
    name = '德式贰发小麦-500ml',
    product_uid = 824340137085414528,
    product_barcode = '2306181506149'
  where name = '德式小麦-500ml'
  "
  dbSendQuery(conn, sql)
  
  ## update price error in stock flow ----
  sql <- "
  update stock_flow
  set 
    buy_price = 14.5,
    unit_buy_price = 14.5
  where product_name = '德式贰发小麦'
    and confirmed_time between '2023-07-05 00:00:00' and '2023-08-05 23:59:59'
  "
  dbSendQuery(conn, sql)
  
  ## raw material setting ----
  sql <- "
  select
    pro.name product_name,
    pro.size product_size,
    pro.barcode product_barcode,
    src.name material_name,
    src.barcode material_barcode
  from (
    select
      name,
      (string_to_array(name, '-'))[1] material_name,
      (string_to_array(name, '-'))[2] size,
      barcode
    from product
    where category_name != '原材料'
  ) pro
  left join (
    select name, barcode
    from product
    where category_name = '原材料'
  ) src on src.name = pro.material_name
  "
  tmp <- dbGetQuery(conn, sql)
  data.table::setDT(tmp)
  
  erfa <- unique(tmp[material_name == '德式贰发小麦', .(material_name, material_barcode)])
  tmp[
    product_name %in% c('德式小麦-350ml', '德式小麦-500ml'),
    `:=`(material_name = erfa$material_name, material_barcode = erfa$material_barcode)
  ]
  
  xiongmao <- unique(tmp[material_name == '熊猫小麦白啤', .(material_name, material_barcode)])
  tmp[
    product_name %in% c('熊猫小麦精酿白啤-6瓶', '熊猫小麦精酿白啤-12瓶'),
    `:=`(material_name = xiongmao$material_name, material_barcode = xiongmao$material_barcode)
  ]
  
  tmp[
    product_size == '1shot', 
    `:=`(product_size = '30ml')
  ]
  
  tmp[
    , `:=`(
      product_size = str_extract(product_size, "[0-9]+"),
      product_unit = str_extract(product_size, "[ml|瓶]+")
    )
  ][
    , `:=`(
      product_size = data.table::fifelse(product_unit == 'ml', as.numeric(product_size) / 1000, as.numeric(product_size)),
      product_unit = data.table::fifelse(product_unit == 'ml', 'L', product_unit),
      updated_at = Sys.time()
    )
  ]
  
  tmp[
    is.na(material_name), `:=`(
      product_size = 1,
      material_name = product_name,
      material_barcode = product_barcode,
      product_unit = "份"
    )
  ]
  
  if (!"material" %in% dbListTables(conn)) {
    dbCreateTable(conn, "material", tmp)
  } else {
    dbSendQuery(conn, "delete from material where 1 = 1")
  }
  
  res <- dbAppendTable(conn, "material", tmp)
  
  dbDisconnect(conn = conn)
})
