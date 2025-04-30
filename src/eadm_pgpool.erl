%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%% pgpool app 来自： https://github.com/epgsql/pgapp/blob/master/src/pgapp.erl#L74
%%% @end
%%% Created : 2024-06-17 上午11:41
%%%-------------------------------------------------------------------
-module(eadm_pgpool).
-author("wangcw").

%%%===================================================================
%%% 函数导出
%%%===================================================================
-export([connect/1, connect/2,
         equery/2, equery/3, equery/4,
         squery/1, squery/2, squery/3,
         with_transaction/1, with_transaction/2, with_transaction/3]).

%%%===================================================================
%%% API 函数
%%%===================================================================

%% @doc
%% 连接数据库，使用默认连接池名称
%% @end
connect(Settings) ->
    connect(epgsql_pool, Settings).

%% @doc
%% 连接数据库，指定连接池名称
%% @end
connect(PoolName, Settings) ->
    PoolSize    = proplists:get_value(size, Settings, 5),
    MaxOverflow = proplists:get_value(max_overflow, Settings, 5),
    eadm_sup:add_pool(PoolName, [{name, {local, PoolName}},
                                  {worker_module, eadm_pgpool_worker},
                                  {size, PoolSize},
                                  {max_overflow, MaxOverflow}], Settings).

%% @doc
%% 执行参数化SQL查询，使用默认连接池
%% @end
-spec equery(Sql    :: epgsql:sql_query(),
             Params :: list(epgsql:bind_param()))
            -> epgsql:reply(epgsql:equery_row()) | {error, Reason :: any()}.
equery(Sql, Params) ->
    eadm_pgpool_worker:equery(Sql, Params).

%% @doc
%% 执行参数化SQL查询，指定超时或连接池
%% @end
-spec equery(Sql     :: epgsql:sql_query(),
             Params  :: list(epgsql:bind_param()),
             Timeout :: atom() | integer())
            -> epgsql:reply(epgsql:equery_row()) | {error, Reason :: any()};
            (PoolName :: atom(),
             Sql::epgsql:sql_query(),
             Params   :: list(epgsql:bind_param()))
            -> epgsql:reply(epgsql:equery_row()) | {error, Reason :: any()}.
equery(P1, P2, P3) ->
    eadm_pgpool_worker:equery(P1, P2, P3).

%% @doc
%% 执行参数化SQL查询，指定连接池和超时
%% @end
-spec equery(PoolName :: atom(),
             Sql::epgsql:sql_query(),
             Params   :: list(epgsql:bind_param()),
             Timeout  :: atom() | integer())
            -> epgsql:reply(epgsql:equery_row()) | {error, Reason :: any()}.
equery(PoolName, Sql, Params, Timeout) ->
    eadm_pgpool_worker:equery(PoolName, Sql, Params, Timeout).

%% @doc
%% 执行简单SQL查询，使用默认连接池
%% @end
-spec squery(Sql :: epgsql:sql_query())
            -> epgsql:reply(epgsql:squery_row()) |
               [epgsql:reply(epgsql:squery_row())] | {error, Reason :: any()}.
squery(Sql) ->
    eadm_pgpool_worker:squery(Sql).

%% @doc
%% 执行简单SQL查询，指定超时或连接池
%% @end
-spec squery(Sql::epgsql:sql_query(),
             Timeout :: atom() | integer())
            -> epgsql:reply(epgsql:squery_row()) |
               [epgsql:reply(epgsql:squery_row())] | {error, Reason :: any()};
            (PoolName :: atom(),
             Sql::epgsql:sql_query())
            -> epgsql:reply(epgsql:squery_row()) |
               [epgsql:reply(epgsql:squery_row())] | {error, Reason :: any()}.
squery(PoolName, Sql) when is_atom(PoolName) ->
    eadm_pgpool_worker:squery(PoolName, Sql);
squery(Sql, Timeout) ->
    eadm_pgpool_worker:squery(Sql, Timeout).

%% @doc
%% 执行简单SQL查询，指定连接池和超时
%% @end
-spec squery(PoolName :: atom(),
             Sql      :: epgsql:sql_query(),
             Timeout  :: atom() | integer())
            -> epgsql:reply(epgsql:squery_row()) |
               [epgsql:reply(epgsql:squery_row())] | {error, Reason :: any()}.
squery(PoolName, Sql, Timeout) ->
    eadm_pgpool_worker:squery(PoolName, Sql, Timeout).

%% @doc
%% 在事务中执行函数，使用默认连接池
%% @end
-spec with_transaction(Function :: fun(() -> Reply))
                      -> Reply | {rollback | error, any()} when Reply :: any().
with_transaction(Fun) when is_function(Fun, 0) ->
    with_transaction(epgsql_pool, Fun).

%% @doc
%% 在事务中执行函数，指定连接池或超时
%% @end
-spec with_transaction(PoolName :: atom(),
                       Function :: fun(() -> Reply))
                      -> Reply | {rollback | error, any()} when Reply :: any();
                      (Function :: fun(() -> Reply),
                       Timeout  :: timeout())
                      -> Reply | {rollback | error, any()} when Reply :: any().
with_transaction(PoolName, Fun) when is_function(Fun, 0);
                                     is_atom(PoolName) ->
    eadm_pgpool_worker:with_transaction(PoolName, Fun);
with_transaction(Fun, Timeout) when is_function(Fun, 0) ->
    eadm_pgpool_worker:with_transaction(epgsql_pool, Fun, Timeout).

%% @doc
%% 在事务中执行函数，指定连接池和超时
%% @end
-spec with_transaction(PoolName :: atom(),
                       Function :: fun(() -> Reply),
                       Timeout  :: atom() | non_neg_integer())
                      -> Reply | {rollback | error, any()} when Reply :: any().
with_transaction(PoolName, Fun, Timeout) when is_function(Fun, 0) ->
    eadm_pgpool_worker:with_transaction(PoolName, Fun, Timeout).

%%%===================================================================
%%% 内部函数
%%%===================================================================
