%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%
%%% 缓存模块头文件
%%% 定义缓存相关的record结构
%%%
%%% @end
%%% Created : 2024-01-XX
%%%-------------------------------------------------------------------

%% 缓存条目记录
-record(cache_entry, {
    key,           % 缓存键 {NormalizedKey, CacheType}
    value,         % 缓存值
    created_at,    % 创建时间（timestamp，秒）
    expires_at,    % 过期时间（timestamp，秒，0表示永不过期）
    ttl,           % TTL秒数
    hit_count = 0, % 命中次数（统计用）
    last_access    % 最后访问时间（timestamp，秒）
}).

%% 缓存统计记录
-record(cache_stats, {
    cache_type,      % 缓存类型（atom）
    total_hits = 0,  % 总命中次数
    total_misses = 0,% 总未命中次数
    total_sets = 0,  % 总设置次数
    total_deletes = 0% 总删除次数
}).
