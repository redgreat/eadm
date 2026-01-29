%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%
%%% 缓存辅助函数模块
%%% 提供缓存键生成、TTL检查等辅助功能
%%%
%%% @end
%%% Created : 2024-01-XX
%%%-------------------------------------------------------------------
-module(eadm_cache_helper).
-author("wangcw").

%%%===================================================================
%%% 函数导出
%%%===================================================================
-export([
    make_key/3,
    make_user_key/2,
    make_global_key/1,
    check_expired/1,
    calculate_expires_at/1,
    normalize_key/1
]).

%%%===================================================================
%%% 宏定义
%%%===================================================================

% 最大TTL：24小时
-define(MAX_TTL, 86400).

%%%===================================================================
%%% API 函数
%%%===================================================================

%% @doc
%% 生成缓存键
%% @end
-spec make_key(CacheType :: atom(), Scope :: atom(), Identifier :: any()) ->
    {atom(), atom(), any()}.
make_key(CacheType, Scope, Identifier) ->
    {CacheType, Scope, Identifier}.

%% @doc
%% 生成用户相关缓存键
%% @end
-spec make_user_key(CacheType :: atom(), LoginName :: binary() | string()) ->
    {atom(), user, binary()}.
make_user_key(CacheType, LoginName) when is_binary(LoginName) ->
    {CacheType, user, LoginName};
make_user_key(CacheType, LoginName) when is_list(LoginName) ->
    {CacheType, user, erlang:list_to_binary(LoginName)}.

%% @doc
%% 生成全局缓存键
%% @end
-spec make_global_key(CacheType :: atom()) ->
    {atom(), global, all}.
make_global_key(CacheType) ->
    {CacheType, global, all}.

%% @doc
%% 检查缓存是否过期
%% @end
-spec check_expired(ExpiresAt :: integer()) -> boolean().
check_expired(0) ->
    % 0表示永不过期
    false;
check_expired(ExpiresAt) when is_integer(ExpiresAt) ->
    Now = erlang:system_time(second),
    Now >= ExpiresAt.

%% @doc
%% 计算过期时间戳
%% @end
-spec calculate_expires_at(TTL :: integer()) -> integer().
calculate_expires_at(0) ->
    % 0表示永不过期
    0;
calculate_expires_at(TTL) when is_integer(TTL), TTL > 0, TTL =< ?MAX_TTL ->
    erlang:system_time(second) + TTL;
calculate_expires_at(TTL) when is_integer(TTL), TTL > ?MAX_TTL ->
    % 超过最大TTL，限制为最大TTL
    erlang:system_time(second) + ?MAX_TTL;
calculate_expires_at(_) ->
    erlang:error({invalid_ttl, "TTL must be a positive integer"}).

%% @doc
%% 规范化缓存键（确保键的一致性）
%% @end
-spec normalize_key(Key :: any()) -> any().
normalize_key(Key) when is_tuple(Key) ->
    Key;
normalize_key(Key) when is_binary(Key) ->
    Key;
normalize_key(Key) when is_list(Key) ->
    erlang:list_to_binary(Key);
normalize_key(Key) when is_atom(Key) ->
    Key;
normalize_key(Key) ->
    erlang:error({invalid_key, Key}).
