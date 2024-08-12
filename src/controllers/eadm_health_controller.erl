%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%
%%% 我的健康逻辑处理
%%%
%%% @end
%%% Created : 2024-02-27 18:37:00
%%%-------------------------------------------------------------------
-module(eadm_health_controller).
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
% index(#{auth_data := #{<<"permission">> := Permission}}) ->
%    io:format("Permission: ~p~n", [Permission]);

index(#{auth_data := #{<<"authed">> := true, <<"username">> := UserName,
      <<"permission">> := #{<<"health">> := true}}}) ->
    {ok, [{username, UserName}]};

index(#{auth_data := #{<<"permission">> := #{<<"health">> := false}}}) ->
    Alert = #{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)},
    {json, [Alert]};

index(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 查询返回数据结果
%% @end
search(#{auth_data := #{<<"authed">> := true,
      <<"permission">> := #{<<"health">> := true}},
    parsed_qs := #{<<"dataType">> := DataType, <<"startTime">> := StartTime, <<"endTime">> := EndTime}}) ->
    try
        case {eadm_utils:validate_date_time(StartTime), eadm_utils:validate_date_time(EndTime)} of
            {false, _} ->
                Alert = #{<<"Alert">> => unicode:characters_to_binary("开始时间格式错误！", utf8)},
                {json, [Alert]};
            {_, false} ->
                Alert = #{<<"Alert">> => unicode:characters_to_binary("结束时间格式错误！", utf8)},
                {json, [Alert]};
            {_, _} ->
                ParameterStartTime = eadm_utils:parse_date_time(StartTime),
                ParameterEndTime = eadm_utils:parse_date_time(EndTime),
                MaxSearchSpan = application:get_env(restwong_cfg, max_search_span, 3),
                TimeDiff = eadm_utils:time_diff(StartTime, EndTime),
                case TimeDiff > (MaxSearchSpan * 86400) of
                    true ->
                        Alert = #{<<"Alert">> => unicode:characters_to_binary(("查询时长超过 "
                            ++ erlang:integer_to_list(MaxSearchSpan) ++ " 天，禁止查询!"), utf8)},
                        {json, [Alert]};
                    _ ->
                        case DataType of
                            <<"1">> ->
                                {ok, ResCol, ResData} = eadm_pgpool:equery(pool_pg,
                                    "select to_char(ptime, 'yyyy-mm-dd hh24:mi:ss') as utctime, steps
                                    from lc_watchdaily
                                    where ptime >= $1
                                      and ptime < $2
                                      and steps is not null
                                    order by ptime desc;",
                                    [ParameterStartTime, ParameterEndTime]);
                            <<"2">> ->
                                {ok, ResCol, ResData} = eadm_pgpool:equery(pool_pg,
                                    "select to_char(ptime, 'yyyy-mm-dd hh24:mi:ss') as utctime, heartbeat
                                    from lc_watchdaily
                                    where ptime >= $1
                                      and ptime < $2
                                      and heartbeat is not null
                                    order by ptime desc;",
                                    [ParameterStartTime, ParameterEndTime]);
                            <<"3">> ->
                                {ok, ResCol, ResData} = eadm_pgpool:equery(pool_pg,
                                    "select to_char(ptime, 'yyyy-mm-dd hh24:mi:ss') as utctime,
                                    bodytemperature, wristtemperature
                                    from lc_watchdaily
                                    where ptime >= $1
                                      and ptime < $2
                                      and bodytemperature is not null
                                    order by ptime desc;",
                                    [ParameterStartTime, ParameterEndTime]);
                            <<"4">> ->
                                {ok, ResCol, ResData} = eadm_pgpool:equery(pool_pg,
                                    "select to_char(ptime, 'yyyy-mm-dd hh24:mi:ss') as utctime,
                                    diastolic, shrink
                                    from lc_watchdaily
                                    where ptime >= $1
                                    and ptime < $2
                                    and diastolic is not null
                                    order by ptime desc;",
                                    [ParameterStartTime, ParameterEndTime]);
                            <<"5">> ->
                                {ok, ResCol, ResData} = eadm_pgpool:equery(pool_pg,
                                    "select to_char(ptime, 'yyyy-mm-dd hh24:mi:ss') as utctime,
                                    sleeptype, sleepstarttime, sleependtime, sleepminute
                                    from lc_watchdaily
                                    where ptime >= $1
                                    and ptime < $2
                                    and sleeptype is not null
                                    order by ptime desc;",
                                    [ParameterStartTime, ParameterEndTime]);
                            <<"6">> ->
                                {ok, ResCol, ResData} = eadm_pgpool:equery(pool_pg,
                                    "select to_char(ptime, 'yyyy-mm-dd hh24:mi:ss') as utctime, battery
                                    from lc_watchdaily
                                    where ptime >= $1
                                    and ptime < $2
                                    and battery is not null
                                    order by ptime desc;",
                                    [ParameterStartTime, ParameterEndTime]);
                            _ ->
                                {ResCol, ResData} = {undefined, undefined}
                        end,
                        Response = eadm_utils:pg_as_json(ResCol, ResData),
                        {json, Response}
                end
        end
    catch
        _E:Error ->
            lager:error("数据查询失败：~p~n", [Error]),
            Alert = #{<<"Alert">> => unicode:characters_to_binary("数据查询失败！", utf8)},
            {json, [Alert]}
    end;

search(#{auth_data := #{<<"permission">> := #{<<"health">> := false}}}) ->
    Alert = #{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)},
    {json, [Alert]};

search(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%%====================================================================
%% 内部函数
%%====================================================================
