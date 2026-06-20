%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%  Native Cowboy crontabs endpoint used during migration.
%%% @end
%%%-------------------------------------------------------------------
-module(eadm_cowboy_crontabs_handler).
-author("wangcw").

-export([init/2]).

%%====================================================================
%% Cowboy callbacks
%%====================================================================

init(Req, State) ->
    case eadm_cowboy_guard:allow_internal_or_require(Req, <<"crontab">>) of
        {ok, _User} -> reply_crontabs(Req, State);
        {error, unauthorized} -> {ok, eadm_api_response:cowboy_json(Req, 401, eadm_api_response:unauthorized()), State};
        {error, forbidden} -> {ok, eadm_api_response:cowboy_json(Req, 403, eadm_api_response:forbidden()), State}
    end.

reply_crontabs(Req, State) ->
    Query = eadm_cowboy_req:query(Req),
    CronName = maps:get(<<"cronName">>, Query, <<>>),
    try
        Body = eadm_api_response:ok(eadm_crontab_service:list(CronName)),
        {ok, eadm_api_response:cowboy_json(Req, Body), State}
    catch
        _:Error ->
            lager:error("Cowboy crontabs endpoint failed: ~p~n", [Error]),
            ErrorBody = eadm_api_response:error(<<"internal_error">>, <<"任务查询失败">>),
            {ok, eadm_api_response:cowboy_json(Req, 500, ErrorBody), State}
    end.
