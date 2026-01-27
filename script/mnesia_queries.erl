% =====================================================
% EADM Mnesia数据库查询语句集合
% 在rebar3 shell中使用
% =====================================================

% 启动rebar3 shell的命令：
% rebar3 shell

% 1. 启动应用和Mnesia
application:ensure_all_started(eadm).

% 2. 查看所有Mnesia表
mnesia:system_info(tables).

% =====================================================
% 权限相关查询
% =====================================================

% 3. 查询所有角色
mnesia:dirty_all_keys(eadm_role).

% 4. 查询特定角色详情（超级管理员）
mnesia:dirty_read(eadm_role, <<"er0000000001">>).

% 5. 查询所有角色的权限配置
lists:foreach(
    fun(RoleId) ->
        case mnesia:dirty_read(eadm_role, RoleId) of
            [#eadm_role{rolename = RoleName, rolepermission = Permission}] ->
                io:format("角色: ~ts, 权限: ~p~n", [RoleName, Permission]);
            _ ->
                ok
        end
    end,
    mnesia:dirty_all_keys(eadm_role)
).

% 6. 检查角色是否有sports权限
mnesia:dirty_match_object(#eadm_role{rolepermission = #{<<"sports">> => true}, _ = '_'}).

% 7. 查询缺少sports权限的角色
lists:foreach(
    fun(RoleId) ->
        case mnesia:dirty_read(eadm_role, RoleId) of
            [#eadm_role{rolename = RoleName, rolepermission = Permission}] ->
                case maps:is_key(<<"sports">>, Permission) of
                    false -> io:format("角色 ~ts 缺少sports权限~n", [RoleName]);
                    true -> ok
                end;
            _ ->
                ok
        end
    end,
    mnesia:dirty_all_keys(eadm_role)
).

% =====================================================
% 用户相关查询
% =====================================================

% 8. 查询所有用户
mnesia:dirty_all_keys(eadm_user).

% 9. 查询特定用户（wangcw）
mnesia:dirty_match_object(#eadm_user{loginname = <<"wangcw">>, _ = '_'}).

% 10. 查询用户wangcw的角色关联
% 首先获取用户ID
case mnesia:dirty_match_object(#eadm_user{loginname = <<"wangcw">>, _ = '_'}) of
    [#eadm_user{id = UserId}] ->
        % 查询用户角色关联
        mnesia:dirty_match_object(#eadm_userrole{userid = UserId, _ = '_'});
    _ ->
        io:format("用户wangcw不存在~n")
end.

% 11. 查询用户wangcw的完整权限信息
case mnesia:dirty_match_object(#eadm_user{loginname = <<"wangcw">>, _ = '_'}) of
    [#eadm_user{id = UserId}] ->
        case mnesia:dirty_match_object(#eadm_userrole{userid = UserId, _ = '_'}) of
            [#eadm_userrole{roleid = RoleId}] ->
                case mnesia:dirty_read(eadm_role, RoleId) of
                    [#eadm_role{rolename = RoleName, rolepermission = Permission}] ->
                        io:format("用户wangcw的角色: ~ts~n", [RoleName]),
                        io:format("权限配置: ~p~n", [Permission]);
                    _ ->
                        io:format("角色不存在~n")
                end;
            _ ->
                io:format("用户没有分配角色~n")
        end;
    _ ->
        io:format("用户不存在~n")
end.

% =====================================================
% 手动修复权限（如果需要）
% =====================================================

% 12. 为超级管理员添加sports权限（如果缺失）
case mnesia:dirty_read(eadm_role, <<"er0000000001">>) of
    [#eadm_role{rolepermission = Permission} = Role] ->
        case maps:is_key(<<"sports">>, Permission) of
            false ->
                NewPermission = Permission#{<<"sports">> => true},
                UpdatedRole = Role#eadm_role{rolepermission = NewPermission},
                mnesia:dirty_write(UpdatedRole),
                io:format("已为超级管理员添加sports权限~n");
            true ->
                io:format("超级管理员已有sports权限~n")
        end;
    _ ->
        io:format("超级管理员角色不存在~n")
end.

% 13. 批量为所有角色添加sports权限
lists:foreach(
    fun(RoleId) ->
        case mnesia:dirty_read(eadm_role, RoleId) of
            [#eadm_role{rolename = RoleName, rolepermission = Permission} = Role] ->
                case maps:is_key(<<"sports">>, Permission) of
                    false ->
                        % 超级管理员设为true，其他设为false
                        SportsValue =
                            case RoleName of
                                <<"超级管理员"/utf8>> -> true;
                                _ -> false
                            end,
                        NewPermission = Permission#{<<"sports">> => SportsValue},
                        UpdatedRole = Role#eadm_role{rolepermission = NewPermission},
                        mnesia:dirty_write(UpdatedRole),
                        io:format("为角色 ~ts 添加sports权限: ~p~n", [RoleName, SportsValue]);
                    true ->
                        io:format("角色 ~ts 已有sports权限~n", [RoleName])
                end;
            _ ->
                ok
        end
    end,
    mnesia:dirty_all_keys(eadm_role)
).

% =====================================================
% 其他有用的查询
% =====================================================

% 14. 查看表结构
mnesia:table_info(eadm_role, attributes).
mnesia:table_info(eadm_user, attributes).
mnesia:table_info(eadm_userrole, attributes).

% 15. 统计信息
io:format("角色总数: ~p~n", [length(mnesia:dirty_all_keys(eadm_role))]),
io:format("用户总数: ~p~n", [length(mnesia:dirty_all_keys(eadm_user))]),
io:format("用户角色关联总数: ~p~n", [length(mnesia:dirty_all_keys(eadm_userrole))]).

% 16. 检查数据库状态
mnesia:system_info(is_running).
mnesia:system_info(directory).
mnesia:system_info(use_dir).

% =====================================================
% 简化的权限检查函数
% =====================================================

% 检查用户是否有sports权限的函数
CheckUserSports = fun(LoginName) ->
    case mnesia:dirty_match_object(#eadm_user{loginname = LoginName, _ = '_'}) of
        [#eadm_user{id = UserId}] ->
            case mnesia:dirty_match_object(#eadm_userrole{userid = UserId, _ = '_'}) of
                [#eadm_userrole{roleid = RoleId}] ->
                    case mnesia:dirty_read(eadm_role, RoleId) of
                        [#eadm_role{rolename = RoleName, rolepermission = Permission}] ->
                            SportsPermission = maps:get(<<"sports">>, Permission, false),
                            io:format("用户 ~ts (角色: ~ts) sports权限: ~p~n", [
                                LoginName, RoleName, SportsPermission
                            ]);
                        _ ->
                            io:format("角色不存在~n")
                    end;
                _ ->
                    io:format("用户没有分配角色~n")
            end;
        _ ->
            io:format("用户 ~ts 不存在~n", [LoginName])
    end
end.

% 使用函数检查wangcw用户
CheckUserSports(<<"wangcw">>).
