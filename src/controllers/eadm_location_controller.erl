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
%%% Application callbacks
%%%===================================================================
-export([index/1, search/1]).


%%====================================================================
%% API functions
%%====================================================================

%% @doc
%% index
%% @end
index(#{auth_data := #{<<"authed">> := true, <<"username">> := UserName}}) ->
    {ok, [{username, UserName}]};

index(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 查询返回数据结果
%% @end
search(#{auth_data := #{<<"authed">> := true},
    parsed_qs := #{<<"startTime">> := StartTime, <<"endTime">> := EndTime}}) ->
        MaxSearchSpan = application:get_env(restwong_cfg, max_search_span, 3),
        TimeDiff = eadm_utils:time_diff(StartTime, EndTime),
        CtsStartTime = eadm_utils:cts_to_utc(StartTime),
        CtsEndTime = eadm_utils:cts_to_utc(EndTime),
        case TimeDiff > (MaxSearchSpan * 86400) of
            true ->
                Alert = #{<<"Alert">> => unicode:characters_to_binary("查询时长超过 " ++ integer_to_list(MaxSearchSpan) ++ " 天，禁止查询!")},
                io:format("Alert: ~p~n", [Alert]),
                {json, [Alert]};
            _ ->
                try
                    {ok, _, Res_Data} = mysql_pool:query(pool_db,
                        "SELECT lng, lat
                        FROM carlocdaily
                        WHERE dev_upload >= ?
                          AND dev_upload < ?
                        ORDER BY dev_upload DESC;",
                        [CtsStartTime, CtsEndTime]),
                    {json, Res_Data}
                catch
                    _:Error ->
                        Alert = #{<<"Alert">> => unicode:characters_to_binary("查询失败! " ++ Error)},
                        {json, [Alert]}
                end
        end;

search(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%%====================================================================
%% Internal functions
%%====================================================================
