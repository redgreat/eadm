%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%  Finance APIs for the new SolidJS frontend.
%%% @end
%%%-------------------------------------------------------------------
-module(eadm_api_finance_controller).
-author("wangcw").

-export([search/1]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc
%% 查询财务流水。
%% @end
search(#{auth_data := #{<<"authed">> := true,
      <<"permission">> := #{<<"finance">> := #{<<"finlist">> := true}}},
      parsed_qs := Query}) ->
    SourceType = maps:get(<<"sourceType">>, Query, <<"0">>),
    InOrOut = maps:get(<<"inOrOut">>, Query, <<"0">>),
    StartTime = maps:get(<<"startTime">>, Query, <<>>),
    EndTime = maps:get(<<"endTime">>, Query, <<>>),
    do_search(SourceType, InOrOut, StartTime, EndTime);

search(#{auth_data := #{<<"authed">> := true,
      <<"permission">> := #{<<"finance">> := #{<<"finlist">> := false}}}}) ->
    eadm_api_response:nova_json(eadm_api_response:forbidden());

search(#{auth_data := #{<<"authed">> := false}}) ->
    eadm_api_response:nova_json(eadm_api_response:unauthorized());

search(_) ->
    eadm_api_response:nova_json(eadm_api_response:unauthorized()).

%%====================================================================
%% Internal functions
%%====================================================================

do_search(_SourceType, _InOrOut, <<>>, _EndTime) ->
    eadm_api_response:nova_json(eadm_api_response:validation_error(<<"请选择开始时间">>));
do_search(_SourceType, _InOrOut, _StartTime, <<>>) ->
    eadm_api_response:nova_json(eadm_api_response:validation_error(<<"请选择结束时间">>));
do_search(SourceType, InOrOut, StartTime, EndTime) ->
    try
        case {eadm_utils:validate_date_time(StartTime), eadm_utils:validate_date_time(EndTime)} of
            {false, _} ->
                eadm_api_response:nova_json(eadm_api_response:validation_error(<<"开始时间格式错误">>));
            {_, false} ->
                eadm_api_response:nova_json(eadm_api_response:validation_error(<<"结束时间格式错误">>));
            {_, _} ->
                validate_span_and_query(SourceType, InOrOut, StartTime, EndTime)
        end
    catch
        _:Error ->
            lager:error("API财务数据查询失败：~p~n", [Error]),
            eadm_api_response:nova_json(eadm_api_response:error(<<"internal_error">>, <<"财务数据查询失败">>))
    end.

validate_span_and_query(SourceType, InOrOut, StartTime, EndTime) ->
    MaxSearchSpan = application:get_env(restwong_cfg, max_fin_search_span, 366),
    TimeDiff = eadm_utils:time_diff(StartTime, EndTime),
    case TimeDiff > (MaxSearchSpan * 86400) of
        true ->
            Message = unicode:characters_to_binary(
                "查询时长超过 " ++ integer_to_list(MaxSearchSpan) ++ " 天，禁止查询", utf8),
            eadm_api_response:nova_json(eadm_api_response:validation_error(Message));
        false ->
            run_query(to_int(SourceType), in_or_out(InOrOut),
                eadm_utils:parse_date_time(StartTime), eadm_utils:parse_date_time(EndTime))
    end.

run_query(SourceType, InOrOut, ParameterStartTime, ParameterEndTime) ->
    Data = eadm_finance_service:search(SourceType, InOrOut, ParameterStartTime, ParameterEndTime),
    eadm_api_response:nova_json(eadm_api_response:ok(Data)).

in_or_out(<<"1">>) -> <<"收入">>;
in_or_out(<<"2">>) -> <<"支出">>;
in_or_out(<<"3">>) -> <<"其他">>;
in_or_out(_) -> 0.

to_int(Value) when is_binary(Value) ->
    try binary_to_integer(Value) catch _:_ -> 0 end;
to_int(Value) when is_integer(Value) ->
    Value;
to_int(_) ->
    0.
