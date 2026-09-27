%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%  Dashboard summary endpoint.
%%% @end
%%%-------------------------------------------------------------------
-module(eadm_cowboy_dashboard_handler).
-author("wangcw").

-export([init/2]).

%%====================================================================
%% Cowboy callbacks
%%====================================================================

init(Req, State) ->
    case eadm_cowboy_guard:require(Req, <<"dashboard">>) of
        {ok, User} -> reply_summary(Req, State, User);
        {error, unauthorized} -> {ok, eadm_api_response:cowboy_json(Req, 401, eadm_api_response:unauthorized()), State};
        {error, forbidden} -> {ok, eadm_api_response:cowboy_json(Req, 403, eadm_api_response:forbidden()), State}
    end.

reply_summary(Req, State, User) ->
    LoginName = eadm_auth_service:login_name(User),
    try
        Body = eadm_api_response:ok(eadm_dashboard_service:summary(LoginName)),
        {ok, eadm_api_response:cowboy_json(Req, Body), State}
    catch
        _:Error ->
            lager:error("Cowboy dashboard endpoint failed: ~p~n", [Error]),
            ErrorBody = eadm_api_response:error(<<"internal_error">>, <<"首页数据查询失败">>),
            {ok, eadm_api_response:cowboy_json(Req, 500, ErrorBody), State}
    end.
