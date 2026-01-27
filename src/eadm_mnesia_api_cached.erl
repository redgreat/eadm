%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%
%%% Mnesia API缓存包装器
%%% 在eadm_mnesia_api基础上添加缓存功能
%%%
%%% @end
%%% Created : 2024-01-XX
%%%-------------------------------------------------------------------
-module(eadm_mnesia_api_cached).
-author("wangcw").

%%%===================================================================
%%% 函数导出
%%%===================================================================
-export([
    read/2,
    read/3,
    query_all/1,
    query_all/2,
    find_by_field/3,
    find_by_field/4,
    create/2,
    update/3,
    delete/2,
    count/1,
    count/2
]).

%%%===================================================================
%%% 宏定义
%%%===================================================================
-define(DEFAULT_TTL_READ, 1800).      % 默认读取缓存TTL：30分钟
-define(DEFAULT_TTL_QUERY_ALL, 600).  % 默认查询所有缓存TTL：10分钟
-define(DEFAULT_TTL_FIND_BY_FIELD, 600). % 默认字段查询缓存TTL：10分钟

%%%===================================================================
%%% API 函数
%%%===================================================================

%% @doc
%% 带缓存的读取记录（使用默认TTL）
%% @end
-spec read(Table :: atom(), Key :: any()) -> [tuple()] | {error, any()}.
read(Table, Key) ->
    read(Table, Key, ?DEFAULT_TTL_READ).

%% @doc
%% 带缓存的读取记录（指定TTL）
%% @end
-spec read(Table :: atom(), Key :: any(), TTL :: integer()) ->
    [tuple()] | {error, any()}.
read(Table, Key, TTL) ->
    CacheType = mnesia_read,
    CacheKey = {mnesia, Table, Key},
    
    case eadm_cache:get(CacheType, CacheKey) of
        {ok, CachedValue} ->
            CachedValue;
        {error, not_found} ->
            Result = eadm_mnesia_api:read(Table, Key),
            case Result of
                {error, _} ->
                    % 错误不缓存
                    Result;
                _ ->
                    % 成功结果缓存
                    eadm_cache:set(CacheType, CacheKey, Result, TTL),
                    Result
            end;
        {error, expired} ->
            % 缓存过期，重新查询
            Result = eadm_mnesia_api:read(Table, Key),
            case Result of
                {error, _} ->
                    Result;
                _ ->
                    eadm_cache:set(CacheType, CacheKey, Result, TTL),
                    Result
            end
    end.

%% @doc
%% 带缓存的查询所有记录（使用默认TTL）
%% @end
-spec query_all(Table :: atom()) -> [tuple()] | {error, any()}.
query_all(Table) ->
    query_all(Table, ?DEFAULT_TTL_QUERY_ALL).

%% @doc
%% 带缓存的查询所有记录（指定TTL）
%% @end
-spec query_all(Table :: atom(), TTL :: integer()) ->
    [tuple()] | {error, any()}.
query_all(Table, TTL) ->
    CacheType = mnesia_query_all,
    CacheKey = {mnesia, Table, all},
    
    case eadm_cache:get(CacheType, CacheKey) of
        {ok, CachedValue} ->
            CachedValue;
        {error, not_found} ->
            Result = eadm_mnesia_api:query_all(Table),
            case Result of
                {error, _} ->
                    Result;
                _ ->
                    eadm_cache:set(CacheType, CacheKey, Result, TTL),
                    Result
            end;
        {error, expired} ->
            Result = eadm_mnesia_api:query_all(Table),
            case Result of
                {error, _} ->
                    Result;
                _ ->
                    eadm_cache:set(CacheType, CacheKey, Result, TTL),
                    Result
            end
    end.

%% @doc
%% 带缓存的根据字段查找记录（使用默认TTL）
%% @end
-spec find_by_field(Table :: atom(), FieldName :: atom(), Value :: any()) ->
    [tuple()] | {error, any()}.
find_by_field(Table, FieldName, Value) ->
    find_by_field(Table, FieldName, Value, ?DEFAULT_TTL_FIND_BY_FIELD).

%% @doc
%% 带缓存的根据字段查找记录（指定TTL）
%% @end
-spec find_by_field(Table :: atom(), FieldName :: atom(), Value :: any(), TTL :: integer()) ->
    [tuple()] | {error, any()}.
find_by_field(Table, FieldName, Value, TTL) ->
    CacheType = mnesia_find_by_field,
    CacheKey = {mnesia, Table, FieldName, Value},
    
    case eadm_cache:get(CacheType, CacheKey) of
        {ok, CachedValue} ->
            CachedValue;
        {error, not_found} ->
            Result = eadm_mnesia_api:find_by_field(Table, FieldName, Value),
            case Result of
                {error, _} ->
                    Result;
                _ ->
                    eadm_cache:set(CacheType, CacheKey, Result, TTL),
                    Result
            end;
        {error, expired} ->
            Result = eadm_mnesia_api:find_by_field(Table, FieldName, Value),
            case Result of
                {error, _} ->
                    Result;
                _ ->
                    eadm_cache:set(CacheType, CacheKey, Result, TTL),
                    Result
            end
    end.

%% @doc
%% 创建记录（自动失效相关缓存）
%% @end
-spec create(Table :: atom(), Record :: tuple()) -> ok | {error, any()}.
create(Table, Record) ->
    Result = eadm_mnesia_api:create(Table, Record),
    case Result of
        ok ->
            % 失效该表的所有查询缓存
            invalidate_table_cache(Table);
        _ ->
            ok
    end,
    Result.

%% @doc
%% 更新记录（自动失效相关缓存）
%% @end
-spec update(Table :: atom(), Key :: any(), UpdateFun :: fun((tuple()) -> tuple())) ->
    ok | {error, any()}.
update(Table, Key, UpdateFun) ->
    Result = eadm_mnesia_api:update(Table, Key, UpdateFun),
    case Result of
        ok ->
            % 失效该记录的缓存
            eadm_cache:delete(mnesia_read, {mnesia, Table, Key}),
            % 失效该表的所有查询缓存
            invalidate_table_cache(Table);
        _ ->
            ok
    end,
    Result.

%% @doc
%% 删除记录（自动失效相关缓存）
%% @end
-spec delete(Table :: atom(), Key :: any()) -> ok | {error, any()}.
delete(Table, Key) ->
    Result = eadm_mnesia_api:delete(Table, Key),
    case Result of
        ok ->
            % 失效该记录的缓存
            eadm_cache:delete(mnesia_read, {mnesia, Table, Key}),
            % 失效该表的所有查询缓存
            invalidate_table_cache(Table);
        _ ->
            ok
    end,
    Result.

%% @doc
%% 统计记录数（带缓存）
%% @end
-spec count(Table :: atom()) -> integer() | {error, any()}.
count(Table) ->
    CacheType = mnesia_count,
    CacheKey = {mnesia, Table, count},
    TTL = 300, % 5分钟TTL
    
    case eadm_cache:get(CacheType, CacheKey) of
        {ok, CachedValue} ->
            CachedValue;
        {error, _} ->
            Result = eadm_mnesia_api:count(Table),
            case Result of
                {error, _} ->
                    Result;
                _ ->
                    eadm_cache:set(CacheType, CacheKey, Result, TTL),
                    Result
            end
    end.

%% @doc
%% 条件统计记录数（带缓存）
%% @end
-spec count(Table :: atom(), MatchSpec :: list()) -> integer() | {error, any()}.
count(Table, MatchSpec) ->
    CacheType = mnesia_count,
    CacheKey = {mnesia, Table, count, erlang:phash2(MatchSpec)},
    TTL = 300, % 5分钟TTL
    
    case eadm_cache:get(CacheType, CacheKey) of
        {ok, CachedValue} ->
            CachedValue;
        {error, _} ->
            Result = eadm_mnesia_api:count(Table, MatchSpec),
            case Result of
                {error, _} ->
                    Result;
                _ ->
                    eadm_cache:set(CacheType, CacheKey, Result, TTL),
                    Result
            end
    end.

%%%===================================================================
%%% 内部函数
%%%===================================================================

%% @private
%% 失效指定表的所有相关缓存
invalidate_table_cache(Table) ->
    % 失效query_all缓存
    eadm_cache:delete(mnesia_query_all, {mnesia, Table, all}),
    % 失效count缓存
    eadm_cache:invalidate_pattern({mnesia_count, {mnesia, Table, '_'}}),
    % 失效find_by_field缓存（该表的所有字段查询）
    eadm_cache:invalidate_pattern({mnesia_find_by_field, {mnesia, Table, '_', '_'}}),
    % 注意：read缓存按需失效，不在这里批量失效
    ok.
