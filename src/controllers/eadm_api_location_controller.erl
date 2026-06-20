%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%  Location APIs for the new SolidJS frontend.
%%% @end
%%%-------------------------------------------------------------------
-module(eadm_api_location_controller).
-author("wangcw").

-export([search/1]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc
%% 查询轨迹坐标。
%% @end
search(#{auth_data := #{<<"authed">> := true, <<"loginname">> := LoginName,
      <<"permission">> := #{<<"locate">> := true}},
    parsed_qs := Query}) ->
    StartTime = maps:get(<<"startTime">>, Query, <<>>),
    EndTime = maps:get(<<"endTime">>, Query, <<>>),
    DeviceNo = maps:get(<<"deviceNo">>, Query, <<>>),
    do_search(LoginName, DeviceNo, StartTime, EndTime);

search(#{auth_data := #{<<"authed">> := true, <<"permission">> := #{<<"locate">> := false}}}) ->
    eadm_api_response:nova_json(eadm_api_response:forbidden());

search(#{auth_data := #{<<"authed">> := false}}) ->
    eadm_api_response:nova_json(eadm_api_response:unauthorized());

search(_) ->
    eadm_api_response:nova_json(eadm_api_response:unauthorized()).

%%====================================================================
%% Internal functions
%%====================================================================

do_search(_LoginName, _DeviceNo, <<>>, _EndTime) ->
    eadm_api_response:nova_json(eadm_api_response:validation_error(<<"请选择开始时间">>));
do_search(_LoginName, _DeviceNo, _StartTime, <<>>) ->
    eadm_api_response:nova_json(eadm_api_response:validation_error(<<"请选择结束时间">>));
do_search(LoginName, DeviceNo, StartTime, EndTime) ->
    try
        case {eadm_utils:validate_date_time(StartTime), eadm_utils:validate_date_time(EndTime)} of
            {false, _} ->
                eadm_api_response:nova_json(eadm_api_response:validation_error(<<"开始时间格式错误">>));
            {_, false} ->
                eadm_api_response:nova_json(eadm_api_response:validation_error(<<"结束时间格式错误">>));
            {_, _} ->
                validate_span_and_query(LoginName, DeviceNo, StartTime, EndTime)
        end
    catch
        _:Error ->
            lager:error("API轨迹查询失败：~p~n", [Error]),
            eadm_api_response:nova_json(eadm_api_response:error(<<"internal_error">>, <<"轨迹查询失败">>))
    end.

validate_span_and_query(LoginName, DeviceNo, StartTime, EndTime) ->
    MaxSearchSpan = application:get_env(restwong_cfg, max_search_span, 3),
    TimeDiff = eadm_utils:time_diff(StartTime, EndTime),
    case TimeDiff > (MaxSearchSpan * 86400) of
        true ->
            Message = unicode:characters_to_binary(
                "查询时长超过 " ++ integer_to_list(MaxSearchSpan) ++ " 天，禁止查询", utf8),
            eadm_api_response:nova_json(eadm_api_response:validation_error(Message));
        false ->
            CtsStartTime = eadm_utils:cts_to_utc(StartTime),
            CtsEndTime = eadm_utils:cts_to_utc(EndTime),
            run_query(LoginName, DeviceNo, eadm_utils:parse_date_time(CtsStartTime), eadm_utils:parse_date_time(CtsEndTime))
    end.

run_query(LoginName, <<>>, ParameterStartTime, ParameterEndTime) ->
    respond(eadm_location_service:search(LoginName, <<>>, ParameterStartTime, ParameterEndTime));
run_query(LoginName, DeviceNo, ParameterStartTime, ParameterEndTime) ->
    respond(eadm_location_service:search(LoginName, DeviceNo, ParameterStartTime, ParameterEndTime)).

respond({ok, Data}) ->
    eadm_api_response:nova_json(eadm_api_response:ok(Data));
respond({error, forbidden, _Message}) ->
            eadm_api_response:nova_json(eadm_api_response:forbidden())
.
