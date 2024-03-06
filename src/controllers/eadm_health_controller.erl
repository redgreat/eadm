%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%% @doc
%%
%% 我的健康逻辑处理
%%
%% @end
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
index(#{auth_data := #{<<"authed">> := true, <<"username">> := Username}}) ->
    {ok, [{username, Username}]};

index(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 查询返回数据结果
%% @end
search(#{parsed_qs := #{<<"dataType">> := DataType, <<"startTime">> := StartTime, <<"endTime">> := EndTime}}) ->
    case DataType of
        <<"1">> ->
            {ok, Res_Col, Res_Data} = mysql_pool:query(pool_db,
                "SELECT DATE_FORMAT(CONVERT_TZ(`UtcTime`,'+00:00','+08:00'), '%Y-%m-%d %H:%i:%s') AS UtcTime, Steps
                FROM watchdaily
                WHERE UtcTime >= ?
                AND UtcTime < ?
                AND Steps IS NOT NULL
                ORDER BY UtcTime DESC;",
                [StartTime, EndTime]);
        <<"2">> ->
            {ok, Res_Col, Res_Data} = mysql_pool:query(pool_db,
                "SELECT DATE_FORMAT(CONVERT_TZ(`UtcTime`,'+00:00','+08:00'), '%Y-%m-%d %H:%i:%s') AS UtcTime, Heartbeat
                Steps AS steps
                FROM watchdaily
                WHERE UtcTime >= ?
                AND UtcTime < ?
                AND Heartbeat IS NOT NULL
                ORDER BY UtcTime DESC;",
                [StartTime, EndTime]);
        <<"3">> ->
            {ok, Res_Col, Res_Data} = mysql_pool:query(pool_db,
                "SELECT DATE_FORMAT(CONVERT_TZ(`UtcTime`,'+00:00','+08:00'), '%Y-%m-%d %H:%i:%s') AS utctime,
                Steps AS steps
                FROM watchdaily
                WHERE UtcTime >= ?
                AND UtcTime < ?
                AND Steps IS NOT NULL
                ORDER BY UtcTime DESC;",
                [StartTime, EndTime]);
        <<"4">> ->
            {ok, Res_Col, Res_Data} = mysql_pool:query(pool_db,
                "SELECT DATE_FORMAT(CONVERT_TZ(`UtcTime`,'+00:00','+08:00'), '%Y-%m-%d %H:%i:%s') AS utctime,
                Steps AS steps
                FROM watchdaily
                WHERE UtcTime >= ?
                AND UtcTime < ?
                AND Steps IS NOT NULL
                ORDER BY UtcTime DESC;",
                [StartTime, EndTime]);
        <<"5">> ->
            {ok, Res_Col, Res_Data} = mysql_pool:query(pool_db,
                "SELECT DATE_FORMAT(CONVERT_TZ(`UtcTime`,'+00:00','+08:00'), '%Y-%m-%d %H:%i:%s') AS utctime,
                Steps AS steps
                FROM watchdaily
                WHERE UtcTime >= ?
                AND UtcTime < ?
                AND Steps IS NOT NULL
                ORDER BY UtcTime DESC;",
                [StartTime, EndTime]);
        <<"6">> ->
            {ok, Res_Col, Res_Data} = mysql_pool:query(pool_db,
                "SELECT DATE_FORMAT(CONVERT_TZ(`UtcTime`,'+00:00','+08:00'), '%Y-%m-%d %H:%i:%s') AS utctime,
                Steps AS steps
                FROM watchdaily
                WHERE UtcTime >= ?
                AND UtcTime < ?
                AND Steps IS NOT NULL
                ORDER BY UtcTime DESC;",
                [StartTime, EndTime]);
        _ ->
            {ok, Res_Col, Res_Data} = {}
    end,
    Response = eadm_utils:return_as_json(Res_Col, Res_Data),
    {json, Response}.


%%====================================================================
%% Internal functions
%%====================================================================
