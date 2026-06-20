%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%  Native Cowboy location endpoint used during migration.
%%% @end
%%%-------------------------------------------------------------------
-module(eadm_cowboy_location_handler).
-author("wangcw").

-export([init/2]).

%%====================================================================
%% Cowboy callbacks
%%====================================================================

init(Req, State) ->
    case eadm_cowboy_guard:allow_internal_or_require(Req, <<"locate">>) of
        {ok, internal} -> reply_location(Req, State, internal);
        {ok, User} -> reply_location(Req, State, User);
        {error, unauthorized} -> {ok, eadm_api_response:cowboy_json(Req, 401, eadm_api_response:unauthorized()), State};
        {error, forbidden} -> {ok, eadm_api_response:cowboy_json(Req, 403, eadm_api_response:forbidden()), State}
    end.

reply_location(Req, State, UserOrInternal) ->
    Query = eadm_cowboy_req:query(Req),
    LoginName = login_name(UserOrInternal, Query),
    DeviceNo = maps:get(<<"deviceNo">>, Query, <<>>),
    StartTime = maps:get(<<"startTime">>, Query, <<>>),
    EndTime = maps:get(<<"endTime">>, Query, <<>>),
    Reply = handle_search(LoginName, DeviceNo, StartTime, EndTime),
    {ok, eadm_api_response:cowboy_json(Req, Reply), State}.

%%====================================================================
%% Internal functions
%%====================================================================

handle_search(<<>>, _DeviceNo, _StartTime, _EndTime) ->
    eadm_api_response:validation_error(<<"迁移验证接口需要 loginName 参数">>);
handle_search(_LoginName, _DeviceNo, <<>>, _EndTime) ->
    eadm_api_response:validation_error(<<"请选择开始时间">>);
handle_search(_LoginName, _DeviceNo, _StartTime, <<>>) ->
    eadm_api_response:validation_error(<<"请选择结束时间">>);
handle_search(LoginName, DeviceNo, StartTime, EndTime) ->
    try
        case {eadm_utils:validate_date_time(StartTime), eadm_utils:validate_date_time(EndTime)} of
            {false, _} ->
                eadm_api_response:validation_error(<<"开始时间格式错误">>);
            {_, false} ->
                eadm_api_response:validation_error(<<"结束时间格式错误">>);
            {_, _} ->
                CtsStartTime = eadm_utils:cts_to_utc(StartTime),
                CtsEndTime = eadm_utils:cts_to_utc(EndTime),
                Result = eadm_location_service:search(
                    LoginName,
                    DeviceNo,
                    eadm_utils:parse_date_time(CtsStartTime),
                    eadm_utils:parse_date_time(CtsEndTime)
                ),
                respond(Result)
        end
    catch
        _:Error ->
            lager:error("Cowboy location endpoint failed: ~p~n", [Error]),
            eadm_api_response:error(<<"internal_error">>, <<"轨迹查询失败">>)
    end.

respond({ok, Data}) ->
    eadm_api_response:ok(Data);
respond({error, forbidden, _Message}) ->
    eadm_api_response:forbidden().

login_name(internal, Query) ->
    maps:get(<<"loginName">>, Query, <<>>);
login_name(User, _Query) ->
    maps:get(<<"loginName">>, User, <<>>).
