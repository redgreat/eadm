%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%  Native Cowboy health-data endpoint used during migration.
%%% @end
%%%-------------------------------------------------------------------
-module(eadm_cowboy_health_handler).
-author("wangcw").

-export([init/2]).

%%====================================================================
%% Cowboy callbacks
%%====================================================================

init(Req, State) ->
    case eadm_cowboy_guard:allow_internal_or_require(Req, <<"health">>) of
        {ok, _User} -> reply_health(Req, State);
        {error, unauthorized} -> {ok, eadm_api_response:cowboy_json(Req, 401, eadm_api_response:unauthorized()), State};
        {error, forbidden} -> {ok, eadm_api_response:cowboy_json(Req, 403, eadm_api_response:forbidden()), State}
    end.

reply_health(Req, State) ->
    Query = eadm_cowboy_req:query(Req),
    DataType = maps:get(<<"dataType">>, Query, <<"1">>),
    StartTime = maps:get(<<"startTime">>, Query, <<>>),
    EndTime = maps:get(<<"endTime">>, Query, <<>>),
    Reply = handle_search(DataType, StartTime, EndTime),
    {ok, eadm_api_response:cowboy_json(Req, Reply), State}.

%%====================================================================
%% Internal functions
%%====================================================================

handle_search(_DataType, <<>>, _EndTime) ->
    eadm_api_response:validation_error(<<"请选择开始时间">>);
handle_search(_DataType, _StartTime, <<>>) ->
    eadm_api_response:validation_error(<<"请选择结束时间">>);
handle_search(DataType, StartTime, EndTime) ->
    try
        case {eadm_utils:validate_date_time(StartTime), eadm_utils:validate_date_time(EndTime)} of
            {false, _} ->
                eadm_api_response:validation_error(<<"开始时间格式错误">>);
            {_, false} ->
                eadm_api_response:validation_error(<<"结束时间格式错误">>);
            {_, _} ->
                case eadm_health_service:search(DataType, StartTime, EndTime) of
                    {ok, Data} -> eadm_api_response:ok(Data);
                    {error, _Code, Message} -> eadm_api_response:validation_error(Message)
                end
        end
    catch
        _:Error ->
            lager:error("Cowboy health endpoint failed: ~p~n", [Error]),
            eadm_api_response:error(<<"internal_error">>, <<"健康数据查询失败">>)
    end.
