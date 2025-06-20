%%% @author wangcw
%%% @copyright (C) 2025, REDGREAT
%%% @doc
%%%
%%% eadm mnesia数据库初始化
%%%
%%% @end
%%% Created : 2025-06-20
%%%-------------------------------------------------------------------
-module(eadm_mnesia_init).
-author("wangcw").

%%%===================================================================
%%% 函数导出
%%%===================================================================
-export([init_data/0]).

%%====================================================================
%% API 函数
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% 初始化示例数据
%% @end
%%--------------------------------------------------------------------
init_data() ->
    lager:info("开始初始化角色和用户数据"),
    
    %% 创建角色
    {ok, AdminRole} = eadm_mnesia:create_role(<<"admin">>, <<"系统管理员">>, 1),
    {ok, UserRole} = eadm_mnesia:create_role(<<"user">>, <<"普通用户">>, 2),
    {ok, GuestRole} = eadm_mnesia:create_role(<<"guest">>, <<"访客">>, 3),
    
    lager:info("创建角色成功: ~p, ~p, ~p", [AdminRole, UserRole, GuestRole]),
    
    %% 创建用户
    {ok, User1} = eadm_mnesia:create_user(<<"admin">>, <<"admin123">>, <<"admin@example.com">>, 1),
    {ok, User2} = eadm_mnesia:create_user(<<"user1">>, <<"user123">>, <<"user1@example.com">>, 2),
    {ok, User3} = eadm_mnesia:create_user(<<"guest1">>, <<"guest123">>, <<"guest1@example.com">>, 3),
    
    lager:info("创建用户成功: ~p, ~p, ~p", [User1, User2, User3]),
    
    %% 分配角色给用户
    ok = eadm_mnesia:assign_role_to_user(1, 1), % admin用户分配admin角色
    ok = eadm_mnesia:assign_role_to_user(2, 2), % user1用户分配user角色
    ok = eadm_mnesia:assign_role_to_user(3, 3), % guest1用户分配guest角色
    
    lager:info("角色分配完成"),
    
    ok.
