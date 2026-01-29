%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%
%%% 缓存核心模块
%%% 提供统一的缓存操作API（get/set/delete/invalidate等）
%%%
%%% @end
%%% Created : 2024-01-XX
%%%-------------------------------------------------------------------
-module(eadm_cache).
-author("wangcw").

%%%===================================================================
%%% 头文件引用
%%%===================================================================
-include("eadm_cache.hrl").

%%%===================================================================
%%% 函数导出
%%%===================================================================
-export([
    get/2,
    get/3,
    set/4,
    delete/2,
    invalidate/2,
    invalidate_pattern/1,
    clear/1,
    get_or_set/4,
    get_or_set/5,
    stats/1,
    stats_detail/1,
    all_stats/0,
    all_stats_detail/0,
    cache_info/0,
    mget/1,
    mset/1,
    mdelete/1
]).

%%%===================================================================
%%% 宏定义
%%%===================================================================
-define(CACHE_TABLE, eadm_cache_data).
-define(STATS_TABLE, eadm_cache_stats).

%%%===================================================================
%%% API 函数
%%%===================================================================

%% @doc
%% 获取缓存值
%% @end
-spec get(CacheType :: atom(), Key :: any()) ->
    {ok, Value :: any()} | {error, not_found | expired}.
get(CacheType, Key) ->
    NormalizedKey = eadm_cache_helper:normalize_key(Key),
    CacheKey = {NormalizedKey, CacheType},

    case ets:lookup(?CACHE_TABLE, CacheKey) of
        [{CacheKey, #cache_entry{value = Value, expires_at = ExpiresAt}}] ->
            case eadm_cache_helper:check_expired(ExpiresAt) of
                false ->
                    % 更新最后访问时间和命中次数
                    update_access_time(CacheKey),
                    update_hit_count(CacheType, true),
                    {ok, Value};
                true ->
                    % 过期，删除并返回未找到
                    ets:delete(?CACHE_TABLE, CacheKey),
                    update_hit_count(CacheType, false),
                    {error, expired}
            end;
        [] ->
            update_hit_count(CacheType, false),
            {error, not_found}
    end.

%% @doc
%% 获取缓存值（带默认值）
%% @end
-spec get(CacheType :: atom(), Key :: any(), Default :: any()) -> any().
get(CacheType, Key, Default) ->
    case get(CacheType, Key) of
        {ok, Value} -> Value;
        {error, _} -> Default
    end.

%% @doc
%% 设置缓存值
%% @end
-spec set(CacheType :: atom(), Key :: any(), Value :: any(), TTL :: integer()) -> ok.
set(CacheType, Key, Value, TTL) ->
    NormalizedKey = eadm_cache_helper:normalize_key(Key),
    CacheKey = {NormalizedKey, CacheType},
    Now = erlang:system_time(second),
    ExpiresAt = eadm_cache_helper:calculate_expires_at(TTL),

    Entry = #cache_entry{
        key = CacheKey,
        value = Value,
        created_at = Now,
        expires_at = ExpiresAt,
        ttl = TTL,
        hit_count = 0,
        last_access = Now
    },

    ets:insert(?CACHE_TABLE, {CacheKey, Entry}),
    update_set_count(CacheType),
    ok.

%% @doc
%% 删除缓存
%% @end
-spec delete(CacheType :: atom(), Key :: any()) -> ok.
delete(CacheType, Key) ->
    NormalizedKey = eadm_cache_helper:normalize_key(Key),
    CacheKey = {NormalizedKey, CacheType},
    ets:delete(?CACHE_TABLE, CacheKey),
    update_delete_count(CacheType),
    ok.

%% @doc
%% 失效缓存（同delete，语义更明确）
%% @end
-spec invalidate(CacheType :: atom(), Key :: any()) -> ok.
invalidate(CacheType, Key) ->
    delete(CacheType, Key).

%% @doc
%% 按模式失效缓存
%% Pattern格式: {CacheType, Scope, Identifier} 或 {CacheType, '_', '_'}
%% 示例: invalidate_pattern({user_permission, user, '_'}) - 失效所有用户权限缓存
%%      invalidate_pattern({user_permission, '_', '_'}) - 失效所有user_permission类型缓存
%% @end
-spec invalidate_pattern(Pattern :: tuple()) -> integer().
invalidate_pattern({CacheType, '_', '_'}) ->
    % 失效所有该CacheType的缓存
    MatchSpec = [
        {
            {'$1', '$2'},
            [{'==', '$2', CacheType}],
            ['$1']
        }
    ],
    Matched = ets:select(?CACHE_TABLE, MatchSpec),
    lists:foreach(fun(CacheKey) -> ets:delete(?CACHE_TABLE, CacheKey) end, Matched),
    length(Matched);
invalidate_pattern({CacheType, Scope, '_'}) ->
    % 失效指定Scope的所有缓存
    MatchSpec = [
        {
            {'$1', '$2'},
            [{'==', '$2', CacheType}],
            ['$1']
        }
    ],
    AllKeys = ets:select(?CACHE_TABLE, MatchSpec),
    % 过滤出符合Scope的键
    FilteredKeys = lists:filter(
        fun(CacheKey) ->
            case CacheKey of
                {NormalizedKey, _Type} ->
                    case NormalizedKey of
                        {S, _} when S =:= Scope -> true;
                        _ -> false
                    end;
                _ ->
                    false
            end
        end,
        AllKeys
    ),
    lists:foreach(fun(CacheKey) -> ets:delete(?CACHE_TABLE, CacheKey) end, FilteredKeys),
    length(FilteredKeys);
invalidate_pattern({CacheType, Scope, Identifier}) ->
    % 精确匹配失效
    NormalizedKey = eadm_cache_helper:normalize_key(Identifier),
    Key =
        case Scope of
            user -> eadm_cache_helper:make_user_key(CacheType, NormalizedKey);
            global -> eadm_cache_helper:make_global_key(CacheType);
            _ -> {CacheType, Scope, NormalizedKey}
        end,
    CacheKey = {Key, CacheType},
    case ets:lookup(?CACHE_TABLE, CacheKey) of
        [_] ->
            ets:delete(?CACHE_TABLE, CacheKey),
            1;
        [] ->
            0
    end;
invalidate_pattern(Pattern) ->
    % 其他格式的模式匹配
    lager:warning("未知的缓存失效模式: ~p", [Pattern]),
    0.

%% @doc
%% 清空指定类型的所有缓存
%% @end
-spec clear(CacheType :: atom()) -> integer().
clear(CacheType) ->
    invalidate_pattern({CacheType, '_', '_'}).

%% @doc
%% 获取或设置缓存（带回调函数）
%% @end
-spec get_or_set(CacheType :: atom(), Key :: any(), FetchFun :: fun(() -> any()), TTL :: integer()) ->
    any().
get_or_set(CacheType, Key, FetchFun, TTL) ->
    case get(CacheType, Key) of
        {ok, Value} ->
            Value;
        {error, _} ->
            Value = FetchFun(),
            set(CacheType, Key, Value, TTL),
            Value
    end.

%% @doc
%% 获取或设置缓存（带默认值）
%% @end
-spec get_or_set(
    CacheType :: atom(),
    Key :: any(),
    FetchFun :: fun(() -> any()),
    TTL :: integer(),
    Default :: any()
) ->
    any().
get_or_set(CacheType, Key, FetchFun, TTL, Default) ->
    case get(CacheType, Key) of
        {ok, Value} ->
            Value;
        {error, _} ->
            try
                Value = FetchFun(),
                set(CacheType, Key, Value, TTL),
                Value
            catch
                _:_ ->
                    Default
            end
    end.

%% @doc
%% 获取指定缓存类型的统计信息
%% @end
-spec stats(CacheType :: atom()) -> {ok, #cache_stats{}} | {error, not_found}.
stats(CacheType) ->
    case ets:lookup(?STATS_TABLE, CacheType) of
        [{CacheType, Stats}] ->
            {ok, Stats};
        [] ->
            {error, not_found}
    end.

%% @doc
%% 获取指定缓存类型的详细统计信息（包含命中率、内存使用等）
%% @end
-spec stats_detail(CacheType :: atom()) ->
    {ok, #{
        cache_type => atom(),
        total_hits => integer(),
        total_misses => integer(),
        total_sets => integer(),
        total_deletes => integer(),
        hit_rate => float(),
        entry_count => integer(),
        memory_size => integer()
    }}
    | {error, not_found}.
stats_detail(CacheType) ->
    case stats(CacheType) of
        {ok, #cache_stats{
            cache_type = Type,
            total_hits = Hits,
            total_misses = Misses,
            total_sets = Sets,
            total_deletes = Deletes
        }} ->
            % 计算命中率
            Total = Hits + Misses,
            HitRate =
                case Total > 0 of
                    true -> (Hits / Total) * 100.0;
                    false -> 0.0
                end,

            % 统计该类型的缓存条目数
            MatchSpec = [
                {
                    {'$1', '$2'},
                    [{'==', '$2', Type}],
                    [true]
                }
            ],
            EntryCount = length(ets:select(?CACHE_TABLE, MatchSpec)),

            % 估算内存使用（粗略计算）

            % 假设每个条目约1KB
            MemorySize = EntryCount * 1024,

            {ok, #{
                cache_type => Type,
                total_hits => Hits,
                total_misses => Misses,
                total_sets => Sets,
                total_deletes => Deletes,
                hit_rate => HitRate,
                entry_count => EntryCount,
                memory_size => MemorySize
            }};
        {error, not_found} ->
            {error, not_found}
    end.

%% @doc
%% 获取所有缓存类型的统计信息
%% @end
-spec all_stats() -> [#cache_stats{}].
all_stats() ->
    ets:tab2list(?STATS_TABLE).

%% @doc
%% 获取所有缓存类型的详细统计信息
%% @end
-spec all_stats_detail() -> [#{}].
all_stats_detail() ->
    AllTypes = [Type || {Type, _} <- ets:tab2list(?STATS_TABLE)],
    lists:foldl(
        fun(Type, Acc) ->
            case stats_detail(Type) of
                {ok, Detail} -> [Detail | Acc];
                {error, _} -> Acc
            end
        end,
        [],
        AllTypes
    ).

%% @doc
%% 获取缓存总体信息（所有类型的汇总）
%% @end
-spec cache_info() ->
    #{
        total_entries => integer(),
        total_memory => integer(),
        cache_types => integer(),
        overall_hit_rate => float()
    }.
cache_info() ->
    % 获取所有缓存条目
    TotalEntries = ets:info(?CACHE_TABLE, size),

    % 获取所有统计信息
    AllStats = all_stats(),
    TotalHits = lists:sum([S#cache_stats.total_hits || S <- AllStats]),
    TotalMisses = lists:sum([S#cache_stats.total_misses || S <- AllStats]),

    % 计算总体命中率
    TotalRequests = TotalHits + TotalMisses,
    OverallHitRate =
        case TotalRequests > 0 of
            true -> (TotalHits / TotalRequests) * 100.0;
            false -> 0.0
        end,

    % 估算总内存使用

    % 假设每个条目约1KB
    TotalMemory = TotalEntries * 1024,

    #{
        total_entries => TotalEntries,
        total_memory => TotalMemory,
        cache_types => length(AllStats),
        overall_hit_rate => OverallHitRate
    }.

%% @doc
%% 批量获取缓存值
%% Keys: [{CacheType, Key}, ...]
%% @end
-spec mget(Keys :: [{atom(), any()}]) -> [{ok, any()} | {error, not_found | expired}].
mget(Keys) ->
    lists:map(
        fun({CacheType, Key}) ->
            get(CacheType, Key)
        end,
        Keys
    ).

%% @doc
%% 批量设置缓存值
%% Entries: [{CacheType, Key, Value, TTL}, ...]
%% @end
-spec mset(Entries :: [{atom(), any(), any(), integer()}]) -> ok.
mset(Entries) ->
    lists:foreach(
        fun({CacheType, Key, Value, TTL}) ->
            set(CacheType, Key, Value, TTL)
        end,
        Entries
    ),
    ok.

%% @doc
%% 批量删除缓存
%% Keys: [{CacheType, Key}, ...]
%% @end
-spec mdelete(Keys :: [{atom(), any()}]) -> ok.
mdelete(Keys) ->
    lists:foreach(
        fun({CacheType, Key}) ->
            delete(CacheType, Key)
        end,
        Keys
    ),
    ok.

%%%===================================================================
%%% 内部函数
%%%===================================================================

%% @private
%% 更新最后访问时间
update_access_time(CacheKey) ->
    case ets:lookup(?CACHE_TABLE, CacheKey) of
        [{CacheKey, Entry}] ->
            Now = erlang:system_time(second),
            NewEntry = Entry#cache_entry{last_access = Now},
            ets:insert(?CACHE_TABLE, {CacheKey, NewEntry});
        [] ->
            ok
    end.

%% @private
%% 更新命中次数统计
update_hit_count(CacheType, Hit) ->
    case ets:lookup(?STATS_TABLE, CacheType) of
        [{CacheType, Stats}] ->
            NewStats =
                case Hit of
                    true ->
                        Stats#cache_stats{total_hits = Stats#cache_stats.total_hits + 1};
                    false ->
                        Stats#cache_stats{total_misses = Stats#cache_stats.total_misses + 1}
                end,
            ets:insert(?STATS_TABLE, {CacheType, NewStats});
        [] ->
            % 创建新的统计记录
            NewStats =
                case Hit of
                    true ->
                        #cache_stats{cache_type = CacheType, total_hits = 1, total_misses = 0};
                    false ->
                        #cache_stats{cache_type = CacheType, total_hits = 0, total_misses = 1}
                end,
            ets:insert(?STATS_TABLE, {CacheType, NewStats})
    end.

%% @private
%% 更新设置次数统计
update_set_count(CacheType) ->
    case ets:lookup(?STATS_TABLE, CacheType) of
        [{CacheType, Stats}] ->
            NewStats = Stats#cache_stats{total_sets = Stats#cache_stats.total_sets + 1},
            ets:insert(?STATS_TABLE, {CacheType, NewStats});
        [] ->
            NewStats = #cache_stats{
                cache_type = CacheType,
                total_sets = 1,
                total_hits = 0,
                total_misses = 0,
                total_deletes = 0
            },
            ets:insert(?STATS_TABLE, {CacheType, NewStats})
    end.

%% @private
%% 更新删除次数统计
update_delete_count(CacheType) ->
    case ets:lookup(?STATS_TABLE, CacheType) of
        [{CacheType, Stats}] ->
            NewStats = Stats#cache_stats{total_deletes = Stats#cache_stats.total_deletes + 1},
            ets:insert(?STATS_TABLE, {CacheType, NewStats});
        [] ->
            ok
    end.
