%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%  Native Cowboy devices endpoint used during migration.
%%% @end
%%%-------------------------------------------------------------------
-module(eadm_cowboy_devices_handler).
-author("wangcw").

-export([init/2]).

%%====================================================================
%% Cowboy callbacks
%%====================================================================

init(Req, State) ->
    case eadm_cowboy_guard:allow_internal_or_require(Req, [<<"device">>, <<"devlist">>]) of
        {ok, _User} -> reply_devices(Req, State);
        {error, unauthorized} -> {ok, eadm_api_response:cowboy_json(Req, 401, eadm_api_response:unauthorized()), State};
        {error, forbidden} -> {ok, eadm_api_response:cowboy_json(Req, 403, eadm_api_response:forbidden()), State}
    end.

reply_devices(Req, State) ->
    Query = eadm_cowboy_req:query(Req),
    DeviceNo = maps:get(<<"deviceNo">>, Query, <<>>),
    try
        Body = eadm_api_response:ok(eadm_device_service:list(DeviceNo)),
        {ok, eadm_api_response:cowboy_json(Req, Body), State}
    catch
        _:Error ->
            lager:error("Cowboy devices endpoint failed: ~p~n", [Error]),
            ErrorBody = eadm_api_response:error(<<"internal_error">>, <<"设备查询失败">>),
            {ok, eadm_api_response:cowboy_json(Req, 500, ErrorBody), State}
    end.
