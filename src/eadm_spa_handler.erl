%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%  Cowboy handler that serves the SolidJS SPA fallback.
%%% @end
%%%-------------------------------------------------------------------
-module(eadm_spa_handler).
-author("wangcw").

-export([init/2]).

%%====================================================================
%% Cowboy callbacks
%%====================================================================

init(Req, State) ->
    IndexPath = spa_index_path(),
    case file:read_file(IndexPath) of
        {ok, Body} ->
            Headers = #{
                <<"content-type">> => <<"text/html; charset=utf-8">>,
                <<"cache-control">> => <<"no-store">>
            },
            {ok, cowboy_req:reply(200, Headers, Body, Req), State};
        {error, enoent} ->
            Body = <<"SolidJS frontend has not been built. Run npm run build in frontend/.">>,
            {ok, cowboy_req:reply(503, #{<<"content-type">> => <<"text/plain; charset=utf-8">>}, Body, Req), State};
        {error, Reason} ->
            Body = unicode:characters_to_binary(io_lib:format("Failed to read SPA entry: ~p", [Reason]), utf8),
            {ok, cowboy_req:reply(500, #{<<"content-type">> => <<"text/plain; charset=utf-8">>}, Body, Req), State}
    end.

%%====================================================================
%% Internal functions
%%====================================================================

spa_index_path() ->
    Candidates = [
        filename:join([code:priv_dir(eadm), "spa", "index.html"]),
        filename:join(["priv", "spa", "index.html"]),
        "/opt/eadm/priv/spa/index.html"
    ],
    first_existing(Candidates).

first_existing([Path | Rest]) ->
    case filelib:is_regular(Path) of
        true -> Path;
        false -> first_existing(Rest)
    end;
first_existing([]) ->
    filename:join([code:priv_dir(eadm), "spa", "index.html"]).
