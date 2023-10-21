sql <- "
select
	src.ts_code ts_code
	,name
	,current_size
	,avg_buy_price
	,pre_close
	,current_size * (pre_close - avg_buy_price) pnl
from (
	select 
		ts_code
		,sum(size) current_size
		,sum(size * price) / sum(size) avg_buy_price
	from stock_position
	group by ts_code
	having sum(size) > 0
) src
left join (
	select ts_code, name
	from stock_basic
) stk_basic on src.ts_code = stk_basic.ts_code
left join (
	select 
		ts_code
		,close pre_close
	from (
		select 
			ts_code
			,close
			,row_number() over(partition by ts_code order by trade_date desc) rn 
		from daily
	) tmp
	where rn = 1
) daily on src.ts_code = daily.ts_code
order by current_size * pre_close desc
"