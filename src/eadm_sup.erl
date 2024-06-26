%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%% @doc
%%
%% eadm top level supervisor.
%%
%% @end
%%% Created : 2024-01-23 17:30:14
%%%-------------------------------------------------------------------
-module(eadm_sup).
-author("wangcw").

%%%===================================================================
%%% Behaviour
%%%===================================================================
-behaviour(supervisor).

%%%===================================================================
%%% Application callbacks
%%%===================================================================
-export([start_link/0, add_pool/3]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
-export([init/1]).

%%%===================================================================
%%% Define
%%%===================================================================
-define(SERVER, ?MODULE).
%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%%====================================================================
%% API functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% start_link.
%%
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}.
%%
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, Pools} = application:get_env(pgpool, pools),
    PoolSpec = lists:map(fun ({PoolName, SizeArgs, WorkerArgs}) ->
        PoolArgs = [{name, {local, PoolName}},
            {worker_module, eadm_pgpool_worker}] ++ SizeArgs,
        poolboy:child_spec(PoolName, PoolArgs, WorkerArgs)
                         end, Pools),
    {ok, { {one_for_one, 10, 10}, PoolSpec} }.

add_pool(Name, PoolArgs, WorkerArgs) ->
    ChildSpec = poolboy:child_spec(Name, PoolArgs, WorkerArgs),
    supervisor:start_child(?MODULE, ChildSpec).

%%    {ok, { {one_for_all, 0, 1}, []} }.
