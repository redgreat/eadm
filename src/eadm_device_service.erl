%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%  Device service shared by Nova controllers and future Cowboy handlers.
%%% @end
%%%-------------------------------------------------------------------
-module(eadm_device_service).
-author("wangcw").

-export([list/1]).

%%====================================================================
%% API functions
%%====================================================================

list(DeviceNo) ->
    {Sql, Params} = device_query(DeviceNo),
    {ok, ResCol, ResData} = eadm_pgpool:equery(pool_pg, Sql, Params),
    Items = eadm_utils:pg_as_map(ResCol, ResData),
    #{
        <<"items">> => Items,
        <<"total">> => length(Items)
    }.

%%====================================================================
%% Internal functions
%%====================================================================

device_query(<<>>) ->
    {"select deviceno as \"deviceNo\",
             imei,
             simno as \"simNo\",
             remark,
             enable,
             createdat as \"createdAt\"
      from eadm_device
      where deleted is false
      order by createdat desc;", []};
device_query(DeviceNo) ->
    {"select deviceno as \"deviceNo\",
             imei,
             simno as \"simNo\",
             remark,
             enable,
             createdat as \"createdAt\"
      from eadm_device
      where deviceno like $1
        and deleted is false
      order by createdat desc;", [<<"%", DeviceNo/binary, "%">>]}.
