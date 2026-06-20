%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%  Native Cowboy system-info endpoint used during migration.
%%% @end
%%%-------------------------------------------------------------------
-module(eadm_cowboy_system_handler).
-author("wangcw").

-export([init/2]).

%%====================================================================
%% Cowboy callbacks
%%====================================================================

init(Req, State) ->
    case eadm_cowboy_guard:allow_internal_or_require(Req, any) of
        {ok, _User} ->
            Body = eadm_api_response:ok(#{<<"items">> => eadm_system_service:info()}),
            {ok, eadm_api_response:cowboy_json(Req, Body), State};
        {error, unauthorized} ->
            {ok, eadm_api_response:cowboy_json(Req, 401, eadm_api_response:unauthorized()), State};
        {error, forbidden} ->
            {ok, eadm_api_response:cowboy_json(Req, 403, eadm_api_response:forbidden()), State}
    end.
