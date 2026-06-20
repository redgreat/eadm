%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%  Location service shared by Nova controllers and future Cowboy handlers.
%%% @end
%%%-------------------------------------------------------------------
-module(eadm_location_service).
-author("wangcw").

-export([search/4]).

%%====================================================================
%% API functions
%%====================================================================

search(LoginName, DeviceNo, ParameterStartTime, ParameterEndTime) ->
    case DeviceNo of
        <<>> ->
            search_all_allowed(LoginName, ParameterStartTime, ParameterEndTime);
        _ ->
            search_device(LoginName, DeviceNo, ParameterStartTime, ParameterEndTime)
    end.

%%====================================================================
%% Internal functions
%%====================================================================

search_all_allowed(LoginName, ParameterStartTime, ParameterEndTime) ->
    {ok, ResCol, ResData} = eadm_pgpool:equery(pool_pg,
        "select to_char(c.ptime, 'yyyy-mm-dd hh24:mi:ss') as \"utcTime\",
                c.deviceno as \"deviceNo\",
                c.lng,
                c.lat
        from lc_carlocdaily c
        join eadm_userdevice ud on c.deviceno = ud.deviceno
        where c.ptime >= $1
          and c.ptime < $2
          and ud.loginname = $3
          and ud.deleted is false
        order by c.ptime asc;",
        [ParameterStartTime, ParameterEndTime, LoginName]),
    {ok, as_list(ResCol, ResData)}.

search_device(LoginName, DeviceNo, ParameterStartTime, ParameterEndTime) ->
    case can_access_device(LoginName, DeviceNo) of
        true ->
            {ok, ResCol, ResData} = eadm_pgpool:equery(pool_pg,
                "select to_char(ptime, 'yyyy-mm-dd hh24:mi:ss') as \"utcTime\",
                        deviceno as \"deviceNo\",
                        lng,
                        lat
                from lc_carlocdaily
                where ptime >= $1
                  and ptime < $2
                  and deviceno = $3
                order by ptime asc;",
                [ParameterStartTime, ParameterEndTime, DeviceNo]),
            {ok, as_list(ResCol, ResData)};
        false ->
            {error, forbidden, <<"没有查看该设备轨迹的权限">>}
    end.

as_list(ResCol, ResData) ->
    Items = eadm_utils:pg_as_map(ResCol, ResData),
    #{
        <<"items">> => Items,
        <<"total">> => length(Items)
    }.

can_access_device(LoginName, DeviceNo) ->
    {ok, _, AuthData} = eadm_pgpool:equery(pool_pg,
        "select count(*) from eadm_userdevice
        where deviceno = $1 and loginname = $2 and deleted is false;",
        [DeviceNo, LoginName]),
    AuthData =/= [{0}].
