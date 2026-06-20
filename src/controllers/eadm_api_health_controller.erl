%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%  Health APIs for the new SolidJS frontend.
%%% @end
%%%-------------------------------------------------------------------
-module(eadm_api_health_controller).
-author("wangcw").

-export([search/1]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc
%% 查询健康数据。
%% @end
search(#{auth_data := #{<<"authed">> := true, <<"permission">> := #{<<"health">> := true}},
    parsed_qs := Query}) ->
    DataType = maps:get(<<"dataType">>, Query, <<"1">>),
    StartTime = maps:get(<<"startTime">>, Query, <<>>),
    EndTime = maps:get(<<"endTime">>, Query, <<>>),
    do_search(DataType, StartTime, EndTime);

search(#{auth_data := #{<<"authed">> := true, <<"permission">> := #{<<"health">> := false}}}) ->
    eadm_api_response:nova_json(eadm_api_response:forbidden());

search(#{auth_data := #{<<"authed">> := false}}) ->
    eadm_api_response:nova_json(eadm_api_response:unauthorized());

search(_) ->
    eadm_api_response:nova_json(eadm_api_response:unauthorized()).

%%====================================================================
%% Internal functions
%%====================================================================

do_search(_DataType, <<>>, _EndTime) ->
    eadm_api_response:nova_json(eadm_api_response:validation_error(<<"请选择开始时间">>));
do_search(_DataType, _StartTime, <<>>) ->
    eadm_api_response:nova_json(eadm_api_response:validation_error(<<"请选择结束时间">>));
do_search(DataType, StartTime, EndTime) ->
    try
        case {eadm_utils:validate_date_time(StartTime), eadm_utils:validate_date_time(EndTime)} of
            {false, _} ->
                eadm_api_response:nova_json(eadm_api_response:validation_error(<<"开始时间格式错误">>));
            {_, false} ->
                eadm_api_response:nova_json(eadm_api_response:validation_error(<<"结束时间格式错误">>));
            {_, _} ->
                validate_span_and_query(DataType, StartTime, EndTime)
        end
    catch
        _:Error ->
            lager:error("API健康数据查询失败：~p~n", [Error]),
            eadm_api_response:nova_json(eadm_api_response:error(<<"internal_error">>, <<"健康数据查询失败">>))
    end.

validate_span_and_query(DataType, StartTime, EndTime) ->
    MaxSearchSpan = application:get_env(restwong_cfg, max_search_span, 3),
    TimeDiff = eadm_utils:time_diff(StartTime, EndTime),
    case TimeDiff > (MaxSearchSpan * 86400) of
        true ->
            Message = unicode:characters_to_binary(
                "查询时长超过 " ++ integer_to_list(MaxSearchSpan) ++ " 天，禁止查询", utf8),
            eadm_api_response:nova_json(eadm_api_response:validation_error(Message));
        false ->
            run_query(DataType, eadm_utils:parse_date_time(StartTime), eadm_utils:parse_date_time(EndTime))
    end.

run_query(DataType, ParameterStartTime, ParameterEndTime) ->
    StartTime = datetime_to_binary(ParameterStartTime),
    EndTime = datetime_to_binary(ParameterEndTime),
    case eadm_health_service:search(DataType, StartTime, EndTime) of
        {ok, Data} ->
            eadm_api_response:nova_json(eadm_api_response:ok(Data));
        {error, _Code, Message} ->
            eadm_api_response:nova_json(eadm_api_response:validation_error(Message))
    end.

datetime_to_binary({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    unicode:characters_to_binary(
        io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
            [Year, Month, Day, Hour, Minute, Second]), utf8).
