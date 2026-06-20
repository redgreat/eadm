%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%  Optional Cowboy listener used during the Nova to Cowboy migration.
%%% @end
%%%-------------------------------------------------------------------
-module(eadm_cowboy_http).
-author("wangcw").

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(LISTENER, eadm_cowboy_http).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    Port = application:get_env(eadm, cowboy_port, 8091),
    {ok, _} = application:ensure_all_started(cowboy),
    Dispatch = cowboy_router:compile(routes()),
    {ok, _} = cowboy:start_clear(?LISTENER, [{port, Port}], #{env => #{dispatch => Dispatch}}),
    lager:info("EADM optional Cowboy listener started on port ~p", [Port]),
    {ok, #{port => Port}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    cowboy:stop_listener(?LISTENER),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

routes() ->
    [
        {'_', [
            {"/api/ping", eadm_cowboy_ping_handler, []},
            {"/api/auth/[...]", eadm_cowboy_auth_handler, []},
            {"/api/system/info", eadm_cowboy_system_handler, []},
            {"/api/users", eadm_cowboy_users_handler, []},
            {"/api/roles", eadm_cowboy_roles_handler, []},
            {"/api/devices", eadm_cowboy_devices_handler, []},
            {"/api/crontabs", eadm_cowboy_crontabs_handler, []},
            {"/api/health", eadm_cowboy_health_handler, []},
            {"/api/location", eadm_cowboy_location_handler, []},
            {"/api/finance", eadm_cowboy_finance_handler, []},
            {"/api/internal/auth/[...]", eadm_cowboy_auth_handler, []},
            {"/api/internal/system/info", eadm_cowboy_system_handler, []},
            {"/api/internal/users", eadm_cowboy_users_handler, []},
            {"/api/internal/roles", eadm_cowboy_roles_handler, []},
            {"/api/internal/devices", eadm_cowboy_devices_handler, []},
            {"/api/internal/crontabs", eadm_cowboy_crontabs_handler, []},
            {"/api/internal/health", eadm_cowboy_health_handler, []},
            {"/api/internal/location", eadm_cowboy_location_handler, []},
            {"/api/internal/finance", eadm_cowboy_finance_handler, []},
            {"/app/assets/[...]", cowboy_static, {dir, spa_assets_dir()}},
            {"/app/[...]", eadm_spa_handler, []},
            {"/", eadm_spa_handler, []}
        ]}
    ].

spa_assets_dir() ->
    Candidates = [
        filename:join([code:priv_dir(eadm), "spa", "assets"]),
        filename:join(["priv", "spa", "assets"]),
        "/opt/eadm/priv/spa/assets"
    ],
    first_existing_dir(Candidates).

first_existing_dir([Path | Rest]) ->
    case filelib:is_dir(Path) of
        true -> Path;
        false -> first_existing_dir(Rest)
    end;
first_existing_dir([]) ->
    filename:join([code:priv_dir(eadm), "spa", "assets"]).
