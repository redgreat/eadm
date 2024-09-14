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
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};

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
                {json, [#{<<"Alert">> => unicode:characters_to_binary("开始时间格式错误！", utf8)}]};
            {_, false} ->
                {json, [#{<<"Alert">> => unicode:characters_to_binary("结束时间格式错误！", utf8)}]};
            {_, _} ->
                ParameterStartTime = eadm_utils:parse_date_time(StartTime),
                ParameterEndTime = eadm_utils:parse_date_time(EndTime),
                MaxSearchSpan = application:get_env(restwong_cfg, max_search_span, 3),
                TimeDiff = eadm_utils:time_diff(StartTime, EndTime),
                case TimeDiff > (MaxSearchSpan * 86400) of
                    true ->
                        {json, [#{<<"Alert">> => unicode:characters_to_binary(("查询时长超过 "
                            ++ erlang:integer_to_list(MaxSearchSpan) ++ " 天，禁止查询!"), utf8)}]};
                    _ ->
                        case DataType of
                            <<"1">> ->
                                % 步数
                                {ok, ResCol, ResData} = eadm_pgpool:equery(pool_pg,
                                    "select to_char(ptime, 'yyyy-mm-dd hh24:mi:ss') as utctime, steps
                                    from lc_watchstep
                                    where ptime >= $1
                                      and ptime < $2
                                      and steps is not null
                                    order by ptime desc;",
                                    [ParameterStartTime, ParameterEndTime]);
                            <<"2">> ->
                                % 心率
                                {ok, ResCol, ResData} = eadm_pgpool:equery(pool_pg,
                                    "select to_char(ptime, 'yyyy-mm-dd hh24:mi:ss') as utctime, heartbeat
                                    from lc_watchhb
                                    where ptime >= $1
                                      and ptime < $2
                                      and heartbeat is not null
                                    order by ptime desc;",
                                    [ParameterStartTime, ParameterEndTime]);
                            <<"3">> ->
                                % 体温
                                {ok, ResCol, ResData} = eadm_pgpool:equery(pool_pg,
                                    "select to_char(ptime, 'yyyy-mm-dd hh24:mi:ss') as utctime,
                                    bodytemperature, wristtemperature
                                    from lc_watchbt
                                    where ptime >= $1
                                      and ptime < $2
                                      and bodytemperature is not null
                                    order by ptime desc;",
                                    [ParameterStartTime, ParameterEndTime]);
                            <<"4">> ->
                                % 血压
                                {ok, ResCol, ResData} = eadm_pgpool:equery(pool_pg,
                                    "select to_char(ptime, 'yyyy-mm-dd hh24:mi:ss') as utctime,
                                    diastolic, shrink
                                    from lc_watchbp
                                    where ptime >= $1
                                    and ptime < $2
                                    and diastolic is not null
                                    order by ptime desc;",
                                    [ParameterStartTime, ParameterEndTime]);
                            <<"5">> ->
                                % 睡眠
                                {ok, ResCol, ResData} = eadm_pgpool:equery(pool_pg,
                                    "select to_char(ptime, 'yyyy-mm-dd hh24:mi:ss') as utctime,
                                    sleeptype, starttime, endtime, minute
                                    from lc_watchsleep
                                    where ptime >= $1
                                    and ptime < $2
                                    and sleeptype is not null
                                    order by ptime desc;",
                                    [ParameterStartTime, ParameterEndTime]);
                            <<"6">> ->
                                % 信号/电量
                                {ok, ResCol, ResData} = eadm_pgpool:equery(pool_pg,
                                    "select to_char(ptime, 'yyyy-mm-dd hh24:mi:ss') as utctime,
                                     battery, signal
                                    from lc_watchsb
                                    where ptime >= $1
                                    and ptime < $2
                                    and battery is not null
                                    order by ptime desc;",
                                    [ParameterStartTime, ParameterEndTime]);
                            _ ->
                                {ResCol, ResData} = {undefined, undefined}
                        end,
                        {json, eadm_utils:pg_as_json(ResCol, ResData)}
                end
        end
    catch
        _:Error ->
            lager:error("数据查询失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("数据查询失败！", utf8)}]}
    end;

search(#{auth_data := #{<<"permission">> := #{<<"health">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};

search(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%%====================================================================
%% 内部函数
%%====================================================================
