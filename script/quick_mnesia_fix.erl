% =====================================================
% EADM Mnesia权限查询快速参考
% 在rebar3 shell中使用
% =====================================================

-module(quick_mnesia_fix).
-include("../src/eadm_mnesia.hrl").
-export([
    run/0,
    check_all_roles/0,
    check_wangcw_permissions/0,
    fix_sports_permissions/0,
    fix_all_sports_permissions/0,
    verify_fixes/0
]).

% 启动
run() ->
    application:ensure_all_started(eadm).

% --- 核心权限查询 ---

% 1. 查看所有角色和权限
check_all_roles() ->
    lists:foreach(
        fun(RoleId) ->
            case mnesia:dirty_read(eadm_role, RoleId) of
                [#eadm_role{rolename = RoleName, rolepermission = Permission}] ->
                    io:format("角色: ~ts~n", [RoleName]),
                    io:format("  sports权限: ~p~n", [maps:get(<<"sports">>, Permission, false)]),
                    io:format("  usermanage权限: ~p~n", [
                        maps:get(<<"usermanage">>, Permission, false)
                    ]);
                _ ->
                    ok
            end
        end,
        mnesia:dirty_all_keys(eadm_role)
    ).

% 2. 检查wangcw用户的权限
check_wangcw_permissions() ->
    case mnesia:dirty_match_object(#eadm_user{loginname = <<"wangcw">>, _ = '_'}) of
        [#eadm_user{id = UserId}] ->
            case mnesia:dirty_match_object(#eadm_userrole{userid = UserId, _ = '_'}) of
                [#eadm_userrole{roleid = RoleId}] ->
                    case mnesia:dirty_read(eadm_role, RoleId) of
                        [#eadm_role{rolename = RoleName, rolepermission = Permission}] ->
                            io:format("wangcw用户角色: ~ts~n", [RoleName]),
                            io:format("sports权限: ~p~n", [maps:get(<<"sports">>, Permission, false)]);
                        _ ->
                            io:format("角色不存在~n")
                    end;
                _ ->
                    io:format("用户没有角色~n")
            end;
        _ ->
            io:format("用户不存在~n")
    end.

% 3. 快速修复sports权限（超级管理员设为true）
fix_sports_permissions() ->
    case mnesia:dirty_read(eadm_role, <<"er0000000001">>) of
        [#eadm_role{rolepermission = Permission} = Role] ->
            NewPermission = Permission#{<<"sports">> => true},
            mnesia:dirty_write(Role#eadm_role{rolepermission = NewPermission}),
            io:format("已修复超级管理员sports权限~n");
        _ ->
            io:format("超级管理员角色不存在~n")
    end.

% 4. 批量修复所有角色的sports权限
fix_all_sports_permissions() ->
    lists:foreach(
        fun(RoleId) ->
            case mnesia:dirty_read(eadm_role, RoleId) of
                [#eadm_role{rolename = RoleName, rolepermission = Permission} = Role] ->
                    case maps:is_key(<<"sports">>, Permission) of
                        false ->
                            SportsValue =
                                case RoleName of
                                    <<"超级管理员"/utf8>> -> true;
                                    _ -> false
                                end,
                            NewPermission = Permission#{<<"sports">> => SportsValue},
                            mnesia:dirty_write(Role#eadm_role{rolepermission = NewPermission}),
                            io:format("修复 ~ts sports权限: ~p~n", [RoleName, SportsValue]);
                        true ->
                            ok
                    end;
                _ ->
                    ok
            end
        end,
        mnesia:dirty_all_keys(eadm_role)
    ).

% 5. 验证修复结果
verify_fixes() ->
    io:format("=== 权限修复验证 ===~n"),
    lists:foreach(
        fun(RoleId) ->
            case mnesia:dirty_read(eadm_role, RoleId) of
                [#eadm_role{rolename = RoleName, rolepermission = Permission}] ->
                    Sports = maps:get(<<"sports">>, Permission, false),
                    io:format("~ts: sports=~p~n", [RoleName, Sports]);
                _ ->
                    ok
            end
        end,
        mnesia:dirty_all_keys(eadm_role)
    ).
