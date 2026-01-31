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
    try
        preload_tenants(),
        preload_roles(),
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
        eadm_pgpool_cached:equery_cached(
            pool_pg,
            "select id, tenantname from eadm_tenant where deleted is false;",
            [],
            3600,
            {tenant_list, all}
        ),
        lager:info("预热了租户列表缓存"),
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
        eadm_pgpool_cached:equery_cached(
            pool_pg,
            "select id, rolename, rolestatus, createdat from eadm_role where deleted is false order by createdat desc;",
            [],
            600,
            {role_list, all}
        ),
        lager:info("预热了角色列表缓存"),
        ok
    catch
        Error:Reason ->
            lager:error("预热角色信息缓存失败: ~p:~p", [Error, Reason]),
            ok
    end.
