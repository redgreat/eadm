%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%  Main Cowboy listener for the EADM API and SolidJS SPA.
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
    Port = application:get_env(eadm, cowboy_port, 8090),
    {ok, _} = application:ensure_all_started(cowboy),
    Dispatch = cowboy_router:compile(routes()),
    {ok, _} = cowboy:start_clear(?LISTENER, [{port, Port}], #{env => #{dispatch => Dispatch}}),
    lager:info("EADM Cowboy listener started on port ~p", [Port]),
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
            {"/api/v1/ping", eadm_cowboy_ping_handler, []},
            {"/api/v1/auth/[...]", eadm_cowboy_auth_handler, []},
            {"/api/v1/dashboard/summary", eadm_cowboy_dashboard_handler, []},
            {"/api/v1/admin/users", eadm_cowboy_users_handler, []},
            {"/api/v1/admin/roles", eadm_cowboy_roles_handler, []},
            {"/api/v1/devices", eadm_cowboy_devices_handler, []},
            {"/api/v1/jobs/crontabs", eadm_cowboy_crontabs_handler, []},
            {"/api/v1/health/records", eadm_cowboy_health_handler, []},
            {"/api/v1/location/points", eadm_cowboy_location_handler, []},
            {"/api/v1/finance/records", eadm_cowboy_finance_handler, []},
            {"/api/v1/system/info", eadm_cowboy_system_handler, []},
            {"/favicon.ico", cowboy_static, {file, spa_file_path("favicon.ico")}},
            {"/assets/[...]", cowboy_static, {dir, spa_assets_dir()}},
            {"/[...]", eadm_spa_handler, []}
        ]}
    ].

spa_file_path(FileName) ->
    Candidates = [
        filename:join([code:priv_dir(eadm), "spa", FileName]),
        filename:join(["priv", "spa", FileName]),
        filename:join(["/opt/eadm/priv/spa", FileName])
    ],
    first_existing_file(Candidates).

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

first_existing_file([Path | Rest]) ->
    case filelib:is_regular(Path) of
        true -> Path;
        false -> first_existing_file(Rest)
    end;
first_existing_file([]) ->
    filename:join([code:priv_dir(eadm), "spa", "favicon.ico"]).
