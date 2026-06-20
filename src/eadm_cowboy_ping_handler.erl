%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%  Native Cowboy ping endpoint for migration verification.
%%% @end
%%%-------------------------------------------------------------------
-module(eadm_cowboy_ping_handler).
-author("wangcw").

-export([init/2]).

%%====================================================================
%% Cowboy callbacks
%%====================================================================

init(Req, State) ->
    Body = eadm_api_response:ok(#{
        <<"service">> => <<"eadm">>,
        <<"runtime">> => <<"cowboy">>
    }),
    {ok, eadm_api_response:cowboy_json(Req, Body), State}.
