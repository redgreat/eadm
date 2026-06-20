%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%  Health data service shared by Nova controllers and future Cowboy handlers.
%%% @end
%%%-------------------------------------------------------------------
-module(eadm_health_service).
-author("wangcw").

-export([search/3]).

%%====================================================================
%% API functions
%%====================================================================

search(DataType, StartTime, EndTime) ->
    ParameterStartTime = eadm_utils:parse_date_time(StartTime),
    ParameterEndTime = eadm_utils:parse_date_time(EndTime),
    case health_sql(DataType) of
        {ok, Sql} ->
            {ok, ResCol, ResData} = eadm_pgpool:equery(pool_pg, Sql, [ParameterStartTime, ParameterEndTime]),
            Items = eadm_utils:pg_as_map(ResCol, ResData),
            {ok, #{
                <<"items">> => Items,
                <<"total">> => length(Items)
            }};
        error ->
            {error, validation_error, <<"不支持的健康数据类型">>}
    end.

%%====================================================================
%% Internal functions
%%====================================================================

health_sql(<<"1">>) ->
    {ok, "select to_char(ptime, 'yyyy-mm-dd hh24:mi:ss') as \"utcTime\", steps
          from lc_watchstep
          where ptime >= $1
            and ptime < $2
            and steps is not null
          order by ptime desc;"};
health_sql(<<"2">>) ->
    {ok, "select to_char(ptime, 'yyyy-mm-dd hh24:mi:ss') as \"utcTime\", heartbeat
          from lc_watchhb
          where ptime >= $1
            and ptime < $2
            and heartbeat is not null
          order by ptime desc;"};
health_sql(<<"3">>) ->
    {ok, "select to_char(ptime, 'yyyy-mm-dd hh24:mi:ss') as \"utcTime\",
                 bodytemperature as \"bodyTemperature\",
                 wristtemperature as \"wristTemperature\"
          from lc_watchbt
          where ptime >= $1
            and ptime < $2
            and bodytemperature is not null
          order by ptime desc;"};
health_sql(<<"4">>) ->
    {ok, "select to_char(ptime, 'yyyy-mm-dd hh24:mi:ss') as \"utcTime\",
                 diastolic,
                 shrink
          from lc_watchbp
          where ptime >= $1
            and ptime < $2
            and diastolic is not null
          order by ptime desc;"};
health_sql(<<"5">>) ->
    {ok, "select to_char(ptime, 'yyyy-mm-dd hh24:mi:ss') as \"utcTime\",
                 sleeptype as \"sleepType\",
                 starttime as \"startTime\",
                 endtime as \"endTime\",
                 minute
          from lc_watchsleep
          where ptime >= $1
            and ptime < $2
            and sleeptype is not null
          order by ptime desc;"};
health_sql(<<"6">>) ->
    {ok, "select to_char(ptime, 'yyyy-mm-dd hh24:mi:ss') as \"utcTime\",
                 battery,
                 signal
          from lc_watchsb
          where ptime >= $1
            and ptime < $2
            and battery is not null
          order by ptime desc;"};
health_sql(_) ->
    error.
