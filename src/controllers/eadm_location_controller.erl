%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%
%%% 轨迹回放逻辑处理
%%%
%%% @end
%%% Created : 2024-03-07 11:28:18
%%%-------------------------------------------------------------------
-module(eadm_location_controller).
-author("wangcw").

%%%===================================================================
%%% 函数导出
%%%===================================================================
-export([index/1, search/1]).


%%====================================================================
%% API 函数
%%====================================================================

%% @doc
%% 主函数
%% @end
index(#{auth_data := #{<<"authed">> := true, <<"username">> := UserName,
      <<"permission">> := #{<<"locate">> := true}}}) ->
    {ok, [{username, UserName}]};

index(#{auth_data := #{<<"permission">> := #{<<"locate">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};

index(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 查询返回数据结果
%% @end
search(#{auth_data := #{<<"authed">> := true, <<"loginname">> := LoginName,
      <<"permission">> := #{<<"locate">> := true}},
    parsed_qs := #{<<"startTime">> := StartTime, <<"endTime">> := EndTime} = Params}) ->
    try
        case {eadm_utils:validate_date_time(StartTime), eadm_utils:validate_date_time(EndTime)} of
            {false, _} ->
                {json, [#{<<"Alert">> => unicode:characters_to_binary("开始时间格式错误！", utf8)}]};
            {_, false} ->
                {json, [#{<<"Alert">> => unicode:characters_to_binary("结束时间格式错误！", utf8)}]};
            {_, _} ->
                MaxSearchSpan = application:get_env(restwong_cfg, max_search_span, 3),
                TimeDiff = eadm_utils:time_diff(StartTime, EndTime),
                CtsStartTime = eadm_utils:cts_to_utc(StartTime),
                CtsEndTime = eadm_utils:cts_to_utc(EndTime),
                ParameterStartTime = eadm_utils:parse_date_time(CtsStartTime),
                ParameterEndTime = eadm_utils:parse_date_time(CtsEndTime),
                case TimeDiff > (MaxSearchSpan * 86400) of
                    true ->
                        {json, [#{<<"Alert">> => unicode:characters_to_binary(("查询时长超过 "
                          ++ erlang:integer_to_list(MaxSearchSpan) ++ " 天，禁止查询!"), utf8)}]};
                    _ ->
                        % 检查是否指定了设备号
                        DeviceNo = maps:get(<<"deviceNo">>, Params, <<"">>),
                        case DeviceNo of
                            <<"">> ->
                                % 未指定设备号，查询用户有权限的所有设备轨迹
                                {ok, _, ResData} = eadm_pgpool:equery(pool_pg,
                                    "select c.lng, c.lat
                                    from lc_carlocdaily c
                                    join eadm_userdevice ud on c.deviceno = ud.deviceno
                                    where c.ptime >= $1
                                      and c.ptime < $2
                                      and ud.loginname = $3
                                      and ud.deleted is false
                                    order by c.ptime asc;",
                                    [ParameterStartTime, ParameterEndTime, LoginName]),
                                {json, eadm_utils:convert_to_array(ResData)};
                            _ ->
                                % 指定了设备号，检查用户是否有权限访问该设备
                                {ok, _, AuthData} = eadm_pgpool:equery(pool_pg,
                                    "select count(*) from eadm_userdevice
                                    where deviceno = $1 and loginname = $2 and deleted is false;",
                                    [DeviceNo, LoginName]),
                                case AuthData of
                                    [{0}] ->
                                        % 用户无权限访问该设备
                                        {json, [#{<<"Alert">> => unicode:characters_to_binary("您没有权限查看该设备的轨迹！", utf8)}]};
                                    _ ->
                                        % 用户有权限，查询指定设备的轨迹
                                        {ok, _, ResData} = eadm_pgpool:equery(pool_pg,
                                            "select lng, lat
                                            from lc_carlocdaily
                                            where ptime >= $1
                                              and ptime < $2
                                              and deviceno = $3
                                            order by ptime asc;",
                                            [ParameterStartTime, ParameterEndTime, DeviceNo]),
                                        {json, eadm_utils:convert_to_array(ResData)}
                                end
                        end
                end
        end
    catch
        _:Error ->
            lager:error("数据查询失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("数据查询失败！", utf8)}]}
    end;

search(#{auth_data := #{<<"permission">> := #{<<"locate">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};

search(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%%====================================================================
%% 内部函数
%%====================================================================
