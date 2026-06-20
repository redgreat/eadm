%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%  Cowboy request helpers for native API handlers.
%%% @end
%%%-------------------------------------------------------------------
-module(eadm_cowboy_req).
-author("wangcw").

-export([json_body/1, query/1, method/1]).

%%====================================================================
%% API functions
%%====================================================================

method(Req) ->
    cowboy_req:method(Req).

query(Req) ->
    maps:from_list(cowboy_req:parse_qs(Req)).

json_body(Req) ->
    case cowboy_req:read_body(Req) of
        {ok, <<>>, Req1} ->
            {ok, #{}, Req1};
        {ok, Body, Req1} ->
            decode_json(Body, Req1);
        {more, Body, Req1} ->
            read_more_body(Body, Req1)
    end.

%%====================================================================
%% Internal functions
%%====================================================================

read_more_body(Acc, Req) ->
    case cowboy_req:read_body(Req) of
        {ok, Body, Req1} ->
            decode_json(<<Acc/binary, Body/binary>>, Req1);
        {more, Body, Req1} ->
            read_more_body(<<Acc/binary, Body/binary>>, Req1)
    end.

decode_json(Body, Req) ->
    try
        {ok, Json} = thoas:decode(Body),
        {ok, Json, Req}
    catch
        _:_ ->
            {error, invalid_json, Req}
    end.
