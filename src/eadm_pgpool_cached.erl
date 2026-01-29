%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%
%%% PostgreSQL API缓存包装器
%%% 在eadm_pgpool基础上添加缓存功能
%%%
%%% @end
%%% Created : 2024-01-XX
%%%-------------------------------------------------------------------
-module(eadm_pgpool_cached).
-author("wangcw").

%%%===================================================================
%%% 函数导出
%%%===================================================================
-export([
    equery/3,
    equery/4,
    equery_cached/4,
    equery_cached/5,
    invalidate_pg_cache/2,
    invalidate_pg_cache/3
]).

%%%===================================================================
%%% 宏定义
%%%===================================================================

% 默认查询缓存TTL：5分钟
-define(DEFAULT_TTL_QUERY, 300).

%%%===================================================================
%%% API 函数
%%%===================================================================

%% @doc
%% 执行参数化SQL查询（带缓存，使用默认TTL）
%% 注意：此函数会自动缓存查询结果，适用于读操作
%% 对于写操作（INSERT/UPDATE/DELETE），请使用 eadm_pgpool:equery/3
%% @end
-spec equery(PoolName :: atom(), Sql :: epgsql:sql_query(), Params :: list()) ->
    epgsql:reply(epgsql:equery_row()) | {error, Reason :: any()}.
equery(PoolName, Sql, Params) ->
    equery_cached(PoolName, Sql, Params, ?DEFAULT_TTL_QUERY).

%% @doc
%% 执行参数化SQL查询（带缓存，指定TTL）
%% @end
-spec equery(PoolName :: atom(), Sql :: epgsql:sql_query(), Params :: list(), TTL :: integer()) ->
    epgsql:reply(epgsql:equery_row()) | {error, Reason :: any()}.
equery(PoolName, Sql, Params, TTL) ->
    equery_cached(PoolName, Sql, Params, TTL).

%% @doc
%% 执行参数化SQL查询（带缓存，使用默认TTL和自动生成的缓存键）
%% @end
-spec equery_cached(
    PoolName :: atom(), Sql :: epgsql:sql_query(), Params :: list(), TTL :: integer()
) ->
    epgsql:reply(epgsql:equery_row()) | {error, Reason :: any()}.
equery_cached(PoolName, Sql, Params, TTL) ->
    equery_cached(PoolName, Sql, Params, TTL, undefined).

%% @doc
%% 执行参数化SQL查询（带缓存，指定TTL和自定义缓存键）
%% CacheKey: 如果提供，将使用此键作为缓存键；否则基于SQL和参数自动生成
%% @end
-spec equery_cached(
    PoolName :: atom(),
    Sql :: epgsql:sql_query(),
    Params :: list(),
    TTL :: integer(),
    CacheKey :: any() | undefined
) ->
    epgsql:reply(epgsql:equery_row()) | {error, Reason :: any()}.
equery_cached(PoolName, Sql, Params, TTL, CacheKey) ->
    % 判断是否为写操作（INSERT/UPDATE/DELETE），写操作不缓存
    case is_write_operation(Sql) of
        true ->
            % 写操作直接执行，不缓存
            eadm_pgpool:equery(PoolName, Sql, Params);
        false ->
            % 读操作，使用缓存
            FinalCacheKey =
                case CacheKey of
                    undefined ->
                        % 基于SQL和参数生成缓存键
                        {pg_query, PoolName, erlang:phash2({Sql, Params})};
                    _ ->
                        {pg_query, PoolName, CacheKey}
                end,

            CacheType = pg_query,

            case eadm_cache:get(CacheType, FinalCacheKey) of
                {ok, CachedResult} ->
                    CachedResult;
                {error, not_found} ->
                    % 查询数据库
                    Result = eadm_pgpool:equery(PoolName, Sql, Params),
                    case Result of
                        {ok, _Columns, _Rows} ->
                            % 只缓存成功的结果
                            eadm_cache:set(CacheType, FinalCacheKey, Result, TTL),
                            Result;
                        {error, _} ->
                            % 错误不缓存
                            Result
                    end;
                {error, expired} ->
                    % 缓存过期，重新查询
                    Result = eadm_pgpool:equery(PoolName, Sql, Params),
                    case Result of
                        {ok, _Columns, _Rows} ->
                            eadm_cache:set(CacheType, FinalCacheKey, Result, TTL),
                            Result;
                        {error, _} ->
                            Result
                    end
            end
    end.

%% @doc
%% 失效指定连接池的PostgreSQL查询缓存
%% Pattern: 缓存键模式（可以是具体值或'_'通配符）
%% @end
-spec invalidate_pg_cache(PoolName :: atom(), Pattern :: any()) -> integer().
invalidate_pg_cache(PoolName, Pattern) ->
    invalidate_pg_cache(pg_query, PoolName, Pattern).

%% @doc
%% 失效指定连接池的PostgreSQL查询缓存（指定缓存类型）
%% @end
-spec invalidate_pg_cache(CacheType :: atom(), PoolName :: atom(), Pattern :: any()) -> integer().
invalidate_pg_cache(CacheType, PoolName, Pattern) ->
    % 构建匹配模式
    case Pattern of
        '_' ->
            % 失效该连接池的所有缓存
            % 需要遍历所有缓存键，找出匹配的
            % 由于ETS不支持复杂的模式匹配，这里使用简化方案
            % 实际使用中，建议在业务层明确指定要失效的缓存键
            lager:warning("通配符失效缓存功能受限，建议使用明确的缓存键"),
            0;
        _ ->
            % 精确匹配失效
            CacheKey = {pg_query, PoolName, Pattern},
            eadm_cache:delete(CacheType, CacheKey),
            1
    end.

%%%===================================================================
%%% 内部函数
%%%===================================================================

%% @private
%% 判断SQL是否为写操作（INSERT/UPDATE/DELETE）
-spec is_write_operation(Sql :: string() | binary()) -> boolean().
is_write_operation(Sql) when is_binary(Sql) ->
    SqlStr = erlang:binary_to_list(Sql),
    is_write_operation(SqlStr);
is_write_operation(Sql) when is_list(Sql) ->
    % 转换为小写并去除前后空白
    SqlLower = string:to_lower(string:trim(Sql)),
    % 检查是否以写操作关键字开头
    case SqlLower of
        "insert" ++ _ -> true;
        "update" ++ _ -> true;
        "delete" ++ _ -> true;
        "create" ++ _ -> true;
        "drop" ++ _ -> true;
        "alter" ++ _ -> true;
        "truncate" ++ _ -> true;
        _ -> false
    end.
