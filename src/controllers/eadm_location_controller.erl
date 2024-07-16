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
    Alert = #{<<"Alert">> => unicode:characters_to_binary("API鉴权失败! ")},
    {json, [Alert]};

index(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 查询返回数据结果
%% @end
search(#{auth_data := #{<<"authed">> := true,
      <<"permission">> := #{<<"locate">> := true}},
    parsed_qs := #{<<"startTime">> := StartTime, <<"endTime">> := EndTime}}) ->
    case {eadm_utils:validate_date_time(StartTime), eadm_utils:validate_date_time(EndTime)} of
        {false, _} ->
            Alert = #{<<"Alert">> => unicode:characters_to_binary("开始时间格式错误！")},
            {json, [Alert]};
        {_, false} ->
            Alert = #{<<"Alert">> => unicode:characters_to_binary("结束时间格式错误！")},
            {json, [Alert]};
        {_, _} ->
            MaxSearchSpan = application:get_env(restwong_cfg, max_search_span, 3),
            TimeDiff = eadm_utils:time_diff(StartTime, EndTime),
            CtsStartTime = eadm_utils:cts_to_utc(StartTime),
            CtsEndTime = eadm_utils:cts_to_utc(EndTime),
            ParameterStartTime = eadm_utils:parse_date_time(CtsStartTime),
            ParameterEndTime = eadm_utils:parse_date_time(CtsEndTime),
            case TimeDiff > (MaxSearchSpan * 86400) of
                true ->
                    Alert = #{<<"Alert">> => unicode:characters_to_binary("查询时长超过 " ++ integer_to_list(MaxSearchSpan) ++ " 天，禁止查询!")},
                    {json, [Alert]};
                _ ->
                    try
                        {ok, _, ResData} = eadm_pgpool:equery(pool_pg,
                            "select lng, lat
                            from lc_carlocdaily
                            where ptime >= $1
                              and ptime < $2
                            order by ptime desc;",
                            [ParameterStartTime, ParameterEndTime]),
                            Response = eadm_utils:convert_to_array(ResData),
                        {json, Response}
                    catch
                        _:Error ->
                            Alert = #{<<"Alert">> => unicode:characters_to_binary("查询失败! " ++ Error)},
                            {json, [Alert]}
                    end
            end
    end;

search(#{auth_data := #{<<"permission">> := #{<<"locate">> := false}}}) ->
    Alert = #{<<"Alert">> => unicode:characters_to_binary("API鉴权失败! ")},
    {json, [Alert]};

search(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%%====================================================================
%% 内部函数
%%====================================================================
