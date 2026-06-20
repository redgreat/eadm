%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%  Native Cowboy roles endpoint used during migration.
%%% @end
%%%-------------------------------------------------------------------
-module(eadm_cowboy_roles_handler).
-author("wangcw").

-export([init/2]).

%%====================================================================
%% Cowboy callbacks
%%====================================================================

init(Req, State) ->
    case eadm_cowboy_guard:allow_internal_or_require(Req, <<"usermanage">>) of
        {ok, _User} -> reply_roles(Req, State);
        {error, unauthorized} -> {ok, eadm_api_response:cowboy_json(Req, 401, eadm_api_response:unauthorized()), State};
        {error, forbidden} -> {ok, eadm_api_response:cowboy_json(Req, 403, eadm_api_response:forbidden()), State}
    end.

reply_roles(Req, State) ->
    try
        Body = eadm_api_response:ok(eadm_role_service:list()),
        {ok, eadm_api_response:cowboy_json(Req, Body), State}
    catch
        _:Error ->
            lager:error("Cowboy roles endpoint failed: ~p~n", [Error]),
            ErrorBody = eadm_api_response:error(<<"internal_error">>, <<"角色查询失败">>),
            {ok, eadm_api_response:cowboy_json(Req, 500, ErrorBody), State}
    end.
