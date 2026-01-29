%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%
%%% 缓存预热模块
%%% 在应用启动时预加载常用数据到缓存
%%%
%%% @end
%%% Created : 2024-01-XX
%%%-------------------------------------------------------------------
-module(eadm_cache_preload).
-author("wangcw").

%%%===================================================================
%%% 头文件引用
%%%===================================================================
-include("eadm_mnesia.hrl").

%%%===================================================================
%%% 函数导出
%%%===================================================================
-export([preload_all/0, preload_tenants/0, preload_roles/0]).

%%%===================================================================
%%% API 函数
%%%===================================================================

%% @doc
%% 预热所有常用缓存
%% @end
-spec preload_all() -> ok.
preload_all() ->
    lager:info("开始缓存预热..."),
    try
        preload_tenants(),
        preload_roles(),
        lager:info("缓存预热完成"),
        ok
    catch
        Error:Reason ->
            lager:error("缓存预热失败: ~p:~p", [Error, Reason]),
            ok
    end.

%% @doc
%% 预热租户信息缓存
%% @end
-spec preload_tenants() -> ok.
preload_tenants() ->
    try
        Tenants = eadm_mnesia_api:query_all(eadm_tenant),
        Count = lists:foldl(
            fun(Tenant, Acc) ->
                case Tenant of
                    #eadm_tenant{id = TenantId, deleted = false} ->
                        % 使用缓存包装器预加载，TTL 60分钟
                        eadm_mnesia_api_cached:read(eadm_tenant, TenantId, 3600),
                        Acc + 1;
                    _ ->
                        Acc
                end
            end,
            0,
            Tenants
        ),
        lager:info("预热了 ~p 个租户信息缓存", [Count]),
        ok
    catch
        Error:Reason ->
            lager:error("预热租户信息缓存失败: ~p:~p", [Error, Reason]),
            ok
    end.

%% @doc
%% 预热角色信息缓存
%% @end
-spec preload_roles() -> ok.
preload_roles() ->
    try
        % 预加载角色列表，TTL 10分钟
        eadm_mnesia_api_cached:query_all(eadm_role, 600),
        lager:info("预热了角色列表缓存"),
        ok
    catch
        Error:Reason ->
            lager:error("预热角色信息缓存失败: ~p:~p", [Error, Reason]),
            ok
    end.
