%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%  Finance service shared by Nova controllers and future Cowboy handlers.
%%% @end
%%%-------------------------------------------------------------------
-module(eadm_finance_service).
-author("wangcw").

-export([search/4]).

%%====================================================================
%% API functions
%%====================================================================

search(SourceType, InOrOut, StartTime, EndTime) ->
    {Sql, Params} = finance_query(SourceType, InOrOut, StartTime, EndTime),
    {ok, ResCol, ResData} = eadm_pgpool:equery(pool_pg, Sql, Params),
    Items = eadm_utils:pg_as_map(ResCol, ResData),
    #{
        <<"items">> => Items,
        <<"total">> => length(Items)
    }.

%%====================================================================
%% Internal functions
%%====================================================================

finance_query(0, 0, StartTime, EndTime) ->
    {base_sql(""), [StartTime, EndTime]};
finance_query(0, InOrOut, StartTime, EndTime) ->
    {base_sql(" and inorout = $3"), [StartTime, EndTime, InOrOut]};
finance_query(SourceType, 0, StartTime, EndTime) ->
    {base_sql(" and sourcetype = $3"), [StartTime, EndTime, SourceType]};
finance_query(SourceType, InOrOut, StartTime, EndTime) ->
    {base_sql(" and sourcetype = $3 and inorout = $4"), [StartTime, EndTime, SourceType, InOrOut]}.

base_sql(Filter) ->
    "select id,
            sourcetype as \"sourceType\",
            inorout as \"inOrOut\",
            tradetype as \"tradeType\",
            amount,
            to_char(tradetime, 'yyyy-mm-dd hh24:mi:ss') as \"tradeTime\"
    from fn_paybilldetail
    where tradetime >= $1
      and tradetime < $2
      and deleted is false" ++ Filter ++ "
    order by tradetime desc;".
