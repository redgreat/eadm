%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%  Native Cowboy finance endpoint used during migration.
%%% @end
%%%-------------------------------------------------------------------
-module(eadm_cowboy_finance_handler).
-author("wangcw").

-export([init/2]).

%%====================================================================
%% Cowboy callbacks
%%====================================================================

init(Req, State) ->
    case eadm_cowboy_guard:allow_internal_or_require(Req, [<<"finance">>, <<"finlist">>]) of
        {ok, _User} -> reply_finance(Req, State);
        {error, unauthorized} -> {ok, eadm_api_response:cowboy_json(Req, 401, eadm_api_response:unauthorized()), State};
        {error, forbidden} -> {ok, eadm_api_response:cowboy_json(Req, 403, eadm_api_response:forbidden()), State}
    end.

reply_finance(Req, State) ->
    Query = eadm_cowboy_req:query(Req),
    SourceType = maps:get(<<"sourceType">>, Query, <<"0">>),
    InOrOut = maps:get(<<"inOrOut">>, Query, <<"0">>),
    StartTime = maps:get(<<"startTime">>, Query, <<>>),
    EndTime = maps:get(<<"endTime">>, Query, <<>>),
    Reply = handle_search(SourceType, InOrOut, StartTime, EndTime),
    {ok, eadm_api_response:cowboy_json(Req, Reply), State}.

%%====================================================================
%% Internal functions
%%====================================================================

handle_search(_SourceType, _InOrOut, <<>>, _EndTime) ->
    eadm_api_response:validation_error(<<"请选择开始时间">>);
handle_search(_SourceType, _InOrOut, _StartTime, <<>>) ->
    eadm_api_response:validation_error(<<"请选择结束时间">>);
handle_search(SourceType, InOrOut, StartTime, EndTime) ->
    try
        case {eadm_utils:validate_date_time(StartTime), eadm_utils:validate_date_time(EndTime)} of
            {false, _} ->
                eadm_api_response:validation_error(<<"开始时间格式错误">>);
            {_, false} ->
                eadm_api_response:validation_error(<<"结束时间格式错误">>);
            {_, _} ->
                Data = eadm_finance_service:search(
                    to_int(SourceType),
                    in_or_out(InOrOut),
                    eadm_utils:parse_date_time(StartTime),
                    eadm_utils:parse_date_time(EndTime)
                ),
                eadm_api_response:ok(Data)
        end
    catch
        _:Error ->
            lager:error("Cowboy finance endpoint failed: ~p~n", [Error]),
            eadm_api_response:error(<<"internal_error">>, <<"财务数据查询失败">>)
    end.

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
