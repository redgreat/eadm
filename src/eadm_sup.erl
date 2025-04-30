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
%%% 函数行为
%%%===================================================================
-behaviour(supervisor).

%%%===================================================================
%%% 函数导出
%%%===================================================================
-export([start_link/0, add_pool/3]).
-export([init/1]).

%%%===================================================================
%%% 宏定义
%%%===================================================================
% -define(SERVER, ?MODULE).

%%====================================================================
%% API 函数
%%====================================================================

%% @private
%% @doc
%% 启动监督者进程
%% @end
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @private
%% @doc
%% 初始化监督者，配置子进程规范
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
%% @end
init([]) ->
    Pools = application:get_env(epgsql, pools, []),
    %% lager:info("数据库连接参数：~p~n", [Pools]),
    PoolSpec = lists:map(fun ({PoolName, SizeArgs, WorkerArgs}) ->
        PoolArgs = [{name, {local, PoolName}},
            {worker_module, eadm_pgpool_worker}] ++ SizeArgs,
        poolboy:child_spec(PoolName, PoolArgs, WorkerArgs)
                         end, Pools),
    {ok, { {one_for_one, 10, 10}, PoolSpec} }.

%% @doc
%% 添加数据库连接池
%% @end
add_pool(Name, PoolArgs, WorkerArgs) ->
    ChildSpec = poolboy:child_spec(Name, PoolArgs, WorkerArgs),
    supervisor:start_child(?MODULE, ChildSpec).
