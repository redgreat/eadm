%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%
%%% 缓存管理器模块
%%% 负责ETS表的创建、监控、过期清理等管理功能
%%%
%%% @end
%%% Created : 2024-01-XX
%%%-------------------------------------------------------------------
-module(eadm_cache_manager).
-author("wangcw").

%%%===================================================================
%%% 头文件引用
%%%===================================================================
-include("eadm_cache.hrl").

%%%===================================================================
%%% 行为
%%%===================================================================
-behaviour(gen_server).

%%%===================================================================
%%% 函数导出
%%%===================================================================
-export([start_link/0, start/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%===================================================================
%%% 宏定义
%%%===================================================================
-define(SERVER, ?MODULE).
-define(CACHE_TABLE, eadm_cache_data).
-define(STATS_TABLE, eadm_cache_stats).
-define(CLEANUP_INTERVAL, 60000). % 清理间隔：60秒
-define(MAX_CACHE_SIZE, 100000). % 最大缓存条目数：10万
-define(MEMORY_CHECK_INTERVAL, 300000). % 内存检查间隔：5分钟

%%%===================================================================
%%% API 函数
%%%===================================================================

%% @doc
%% 启动缓存管理器
%% @end
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call(?SERVER, stop).

%%%===================================================================
%%% gen_server 回调函数
%%%===================================================================

init([]) ->
    % 创建ETS缓存表
    CacheTable = ets:new(?CACHE_TABLE, [
        set,
        public,
        named_table,
        {read_concurrency, true},
        {write_concurrency, false}
    ]),
    
    % 创建ETS统计表
    StatsTable = ets:new(?STATS_TABLE, [
        set,
        public,
        named_table,
        {read_concurrency, true},
        {write_concurrency, false}
    ]),
    
    lager:info("缓存管理器启动成功，缓存表: ~p, 统计表: ~p", [CacheTable, StatsTable]),
    
    % 启动定时清理进程
    timer:send_interval(?CLEANUP_INTERVAL, cleanup_expired),
    % 启动内存检查进程
    timer:send_interval(?MEMORY_CHECK_INTERVAL, check_memory),
    
    {ok, #{cache_table => CacheTable, stats_table => StatsTable}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup_expired, State) ->
    cleanup_expired_cache(),
    {noreply, State};
handle_info(check_memory, State) ->
    check_and_cleanup_memory(),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    % 清理ETS表
    case ets:info(?CACHE_TABLE) of
        undefined -> ok;
        _ -> ets:delete(?CACHE_TABLE)
    end,
    case ets:info(?STATS_TABLE) of
        undefined -> ok;
        _ -> ets:delete(?STATS_TABLE)
    end,
    lager:info("缓存管理器已停止"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% 内部函数
%%%===================================================================

%% @private
%% 清理过期缓存
cleanup_expired_cache() ->
    try
        Now = erlang:system_time(second),
        
        % 查找所有过期的缓存项
        MatchSpec = [
            {
                {'$1', #cache_entry{expires_at = '$2'}},
                [{'=/=', '$2', 0}, {'=<', '$2', Now}],
                ['$1']
            }
        ],
        
        ExpiredKeys = ets:select(?CACHE_TABLE, MatchSpec),
        
        % 删除过期缓存
        Count = lists:foldl(fun(CacheKey, Acc) ->
            ets:delete(?CACHE_TABLE, CacheKey),
            Acc + 1
        end, 0, ExpiredKeys),
        
        if
            Count > 0 ->
                lager:debug("清理了 ~p 个过期缓存项", [Count]);
            true ->
                ok
        end
    catch
        Error:Reason ->
            lager:error("清理过期缓存失败: ~p:~p", [Error, Reason])
    end.

%% @private
%% 检查内存使用并清理
check_and_cleanup_memory() ->
    try
        CacheSize = ets:info(?CACHE_TABLE, size),
        
        if
            CacheSize > ?MAX_CACHE_SIZE ->
                % 缓存条目数超过限制，清理最久未访问的缓存
                lager:warning("缓存条目数 ~p 超过限制 ~p，开始清理最久未访问的缓存", 
                             [CacheSize, ?MAX_CACHE_SIZE]),
                cleanup_lru_cache(CacheSize - ?MAX_CACHE_SIZE);
            true ->
                ok
        end
    catch
        Error:Reason ->
            lager:error("内存检查失败: ~p:~p", [Error, Reason])
    end.

%% @private
%% 清理最久未访问的缓存（LRU策略）
cleanup_lru_cache(Count) ->
    try
        % 获取所有缓存条目，按最后访问时间排序
        AllEntries = ets:tab2list(?CACHE_TABLE),
        SortedEntries = lists:sort(fun({_Key1, Entry1}, {_Key2, Entry2}) ->
            Entry1#cache_entry.last_access < Entry2#cache_entry.last_access
        end, AllEntries),
        
        % 删除最久未访问的缓存
        ToDelete = lists:sublist(SortedEntries, Count),
        lists:foreach(fun({Key, _Entry}) ->
            ets:delete(?CACHE_TABLE, Key)
        end, ToDelete),
        
        lager:info("清理了 ~p 个最久未访问的缓存项", [length(ToDelete)])
    catch
        Error:Reason ->
            lager:error("LRU清理失败: ~p:~p", [Error, Reason])
    end.
