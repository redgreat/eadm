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
%%% Application callbacks
%%%===================================================================
-export([index/1, search/1]).


%%====================================================================
%% API functions
%%====================================================================

%% @doc
%% index
%% @end
index(#{auth_data := #{<<"authed">> := true, <<"username">> := UserName,
      <<"permission">> := #{<<"health">> := true}}}) ->
    {ok, [{username, UserName}]};

index(#{auth_data := #{<<"permission">> := #{<<"health">> := false}}}) ->
    Alert = #{<<"Alert">> => unicode:characters_to_binary("API鉴权失败! ")},
    {json, [Alert]};

index(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 查询返回数据结果
%% @end
search(#{auth_data := #{<<"authed">> := true,
      <<"permission">> := #{<<"health">> := true}},
    parsed_qs := #{<<"dataType">> := DataType, <<"startTime">> := StartTime, <<"endTime">> := EndTime}}) ->
    case {StartTime, EndTime} of
        {undefined, _} ->
            Alert = #{<<"Alert">> => unicode:characters_to_binary("开始时间查询条件缺失！")},
            {json, [Alert]};
        {_, undefined} ->
            Alert = #{<<"Alert">> => unicode:characters_to_binary("结束时间查询条件缺失！")},
            {json, [Alert]};
        _ ->
            ValidateStartTime = eadm_utils:validate_date_time(StartTime),
            ValidateEndTime = eadm_utils:validate_date_time(EndTime),
            case {ValidateStartTime, ValidateEndTime} of
                {false, _} ->
                    Alert = #{<<"Alert">> => unicode:characters_to_binary("开始时间格式错误！")},
                    {json, [Alert]};
                {_, false} ->
                    Alert = #{<<"Alert">> => unicode:characters_to_binary("结束时间格式错误！")},
                    {json, [Alert]};
                {_, _} ->
                    MaxSearchSpan = application:get_env(restwong_cfg, max_search_span, 3),
                    TimeDiff = eadm_utils:time_diff(StartTime, EndTime),
                    case TimeDiff > (MaxSearchSpan * 86400) of
                        true ->
                            Alert = #{<<"Alert">> => unicode:characters_to_binary("查询时长超过 " ++ integer_to_list(MaxSearchSpan) ++ " 天，禁止查询!")},
                            {json, [Alert]};
                        _ ->
                            case DataType of
                                <<"1">> ->
                                    {ok, ResCol, ResData} = mysql_pool:query(pool_db,
                                        "SELECT DATE_FORMAT(CONVERT_TZ(`UtcTime`,'+00:00','+08:00'), '%Y-%m-%d %H:%i:%s') AS UtcTime, Steps
                                        FROM watchdaily
                                        WHERE UtcTime >= ?
                                        AND UtcTime < ?
                                        AND Steps IS NOT NULL
                                        ORDER BY UtcTime DESC;",
                                        [StartTime, EndTime]);
                                <<"2">> ->
                                    {ok, ResCol, ResData} = mysql_pool:query(pool_db,
                                        "SELECT DATE_FORMAT(CONVERT_TZ(`UtcTime`,'+00:00','+08:00'), '%Y-%m-%d %H:%i:%s') AS UtcTime, Heartbeat
                                        FROM watchdaily
                                        WHERE UtcTime >= ?
                                        AND UtcTime < ?
                                        AND Heartbeat IS NOT NULL
                                        ORDER BY UtcTime DESC;",
                                        [StartTime, EndTime]);
                                <<"3">> ->
                                    {ok, ResCol, ResData} = mysql_pool:query(pool_db,
                                        "SELECT DATE_FORMAT(CONVERT_TZ(`UtcTime`,'+00:00','+08:00'), '%Y-%m-%d %H:%i:%s') AS UtcTime,
                                        BodyTemperature, WristTemperature
                                        FROM watchdaily
                                        WHERE UtcTime >= ?
                                        AND UtcTime < ?
                                        AND BodyTemperature IS NOT NULL
                                        ORDER BY UtcTime DESC;",
                                        [StartTime, EndTime]);
                                <<"4">> ->
                                    {ok, ResCol, ResData} = mysql_pool:query(pool_db,
                                        "SELECT DATE_FORMAT(CONVERT_TZ(`UtcTime`,'+00:00','+08:00'), '%Y-%m-%d %H:%i:%s') AS UtcTime,
                                        Diastolic, Shrink
                                        FROM watchdaily
                                        WHERE UtcTime >= ?
                                        AND UtcTime < ?
                                        AND Diastolic IS NOT NULL
                                        ORDER BY UtcTime DESC;",
                                        [StartTime, EndTime]);
                                <<"5">> ->
                                    {ok, ResCol, ResData} = mysql_pool:query(pool_db,
                                        "SELECT DATE_FORMAT(CONVERT_TZ(`UtcTime`,'+00:00','+08:00'), '%Y-%m-%d %H:%i:%s') AS UtcTime,
                                        SleepType, SleepStartTime, SleepEndTime, SleepMinute
                                        FROM watchdaily
                                        WHERE UtcTime >= ?
                                        AND UtcTime < ?
                                        AND SleepType IS NOT NULL
                                        ORDER BY UtcTime DESC;",
                                        [StartTime, EndTime]);
                                <<"6">> ->
                                    {ok, ResCol, ResData} = mysql_pool:query(pool_db,
                                        "SELECT DATE_FORMAT(CONVERT_TZ(`UtcTime`,'+00:00','+08:00'), '%Y-%m-%d %H:%i:%s') AS UtcTime, Battery
                                        FROM watchdaily
                                        WHERE UtcTime >= ?
                                        AND UtcTime < ?
                                        AND Battery IS NOT NULL
                                        ORDER BY UtcTime DESC;",
                                        [StartTime, EndTime]);
                                _ ->
                                    {ok, ResCol, ResData} = _
                            end,
                            Response = eadm_utils:return_as_json(ResCol, ResData),
                            {json, Response}
                    end
            end
    end;

search(#{auth_data := #{<<"permission">> := #{<<"health">> := false}}}) ->
    Alert = #{<<"Alert">> => unicode:characters_to_binary("API鉴权失败! ")},
    {json, [Alert]};

search(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%%====================================================================
%% Internal functions
%%====================================================================
