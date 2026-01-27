%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%
%%% 角色信息逻辑处理
%%%
%%% @end
%%% Created : 2024-03-26 16:07:25
%%%-------------------------------------------------------------------
-module(eadm_role_controller).
-author("wangcw").

-include("eadm_mnesia.hrl").

%%%===================================================================
%%% 函数导出
%%%===================================================================
-export([
    index/1,
    search/1,
    add/1,
    disable/1,
    delete/1,
    loadpermission/1,
    updatepermission/1,
    getrolelist/1
]).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc
%% 主函数
%% @end
index(#{
    auth_data := #{
        <<"authed">> := true,
        <<"username">> := UserName,
        <<"permission">> := #{<<"usermanage">> := true}
    }
}) ->
    {ok, [{username, UserName}]};
index(#{auth_data := #{<<"permission">> := #{<<"usermanage">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};
index(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 查询返回数据结果
%% @end
search(#{
    auth_data := #{
        <<"authed">> := true,
        <<"permission">> := #{<<"usermanage">> := true}
    }
}) ->
    try
        % 使用缓存包装器，TTL 10分钟
        Roles = eadm_mnesia_api_cached:query_all(eadm_role, 600),
        ActiveRoles = [
            #{
                <<"id">> => Id,
                <<"rolename">> => Name,
                <<"rolestatus">> => Status,
                <<"createdat">> => CreatedAt
            }
         || #eadm_role{
                id = Id,
                rolename = Name,
                rolestatus = Status,
                createdat = CreatedAt,
                deleted = false
            } <- Roles
        ],
        {json, #{
            <<"columns">> => [<<"id">>, <<"rolename">>, <<"rolestatus">>, <<"createdat">>],
            <<"data">> => ActiveRoles
        }}
    catch
        _:Error ->
            lager:error("数据查询失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("数据查询失败！", utf8)}]}
    end;
search(#{auth_data := #{<<"permission">> := #{<<"usermanage">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};
search(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 新增角色数据
%% @end
add(#{
    auth_data := #{
        <<"authed">> := true,
        <<"loginname">> := CreatedUser,
        <<"permission">> := #{<<"usermanage">> := true}
    },
    params := #{<<"roleName">> := RoleName}
}) ->
    try
        NewId = eadm_mnesia_api:get_next_id(eadm_role),
        Role = #eadm_role{
            id = NewId,
            rolename = RoleName,
            createduser = CreatedUser,
            createdat = erlang:system_time(second)
        },
        ok = eadm_mnesia_api:create(eadm_role, Role),
        A = unicode:characters_to_binary("角色【", utf8),
        B = unicode:characters_to_binary("】新增成功！", utf8),
        {json, [#{<<"Alert">> => <<A/binary, RoleName/binary, B/binary>>}]}
    catch
        _:Error ->
            lager:error("角色新增失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("角色新增失败！", utf8)}]}
    end;
add(#{auth_data := #{<<"permission">> := #{<<"usermanage">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};
add(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 获取角色权限数据
%% @end
loadpermission(#{
    auth_data := #{
        <<"authed">> := true,
        <<"permission">> := #{<<"usermanage">> := true}
    },
    bindings := #{<<"roleId">> := RoleId}
}) ->
    try
        case eadm_mnesia_api:read(eadm_role, RoleId) of
            [#eadm_role{rolepermission = Permission, deleted = false}] ->
                {json, [#{<<"rolepermission">> => Permission}]};
            _ ->
                {json, [#{<<"Alert">> => unicode:characters_to_binary("角色不存在！", utf8)}]}
        end
    catch
        _:Error ->
            lager:error("角色权限查询失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("角色权限查询失败！", utf8)}]}
    end;
loadpermission(#{auth_data := #{<<"permission">> := #{<<"usermanage">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};
loadpermission(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 更新角色权限信息
%% @end
updatepermission(#{
    auth_data := #{
        <<"authed">> := true,
        <<"loginname">> := LoginName,
        <<"permission">> := #{<<"usermanage">> := true}
    },
    params := #{
        <<"roleId">> := RoleId,
        <<"dashBoard">> := DashBoard,
        <<"health">> := Health,
        <<"locate">> := Locate,
        <<"finance">> := Finance,
        <<"finimp">> := Finimp,
        <<"findel">> := Findel,
        <<"crontab">> := Crontab,
        <<"userManage">> := Usermanage,
        <<"sports">> := Sports,
        <<"devlist">> := Devlist,
        <<"devadd">> := Devadd,
        <<"devedit">> := Devedit,
        <<"devdel">> := Devdel,
        <<"devassign">> := Devassign
    }
}) ->
    try
        RolePermissionMap = #{
            <<"dashboard">> => erlang:binary_to_atom(DashBoard),
            <<"health">> => erlang:binary_to_atom(Health),
            <<"locate">> => erlang:binary_to_atom(Locate),
            <<"finance">> => #{
                <<"finlist">> => erlang:binary_to_atom(Finance),
                <<"finimp">> => erlang:binary_to_atom(Finimp),
                <<"findel">> => erlang:binary_to_atom(Findel)
            },
            <<"device">> => #{
                <<"devlist">> => erlang:binary_to_atom(Devlist),
                <<"devadd">> => erlang:binary_to_atom(Devadd),
                <<"devedit">> => erlang:binary_to_atom(Devedit),
                <<"devdel">> => erlang:binary_to_atom(Devdel),
                <<"devassign">> => erlang:binary_to_atom(Devassign)
            },
            <<"crontab">> => erlang:binary_to_atom(Crontab),
            <<"sports">> => erlang:binary_to_atom(Sports),
            <<"usermanage">> => erlang:binary_to_atom(Usermanage)
        },
        ok = eadm_mnesia_api:update(eadm_role, RoleId, fun(R) ->
            R#eadm_role{
                rolepermission = RolePermissionMap,
                updateduser = LoginName,
                updatedat = erlang:system_time(second)
            }
        end),
        {json, [#{<<"Alert">> => unicode:characters_to_binary("权限更新成功！", utf8)}]}
    catch
        _:Error ->
            lager:error("权限更新失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("权限更新失败！", utf8)}]}
    end;
updatepermission(#{auth_data := #{<<"permission">> := #{<<"usermanage">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};
updatepermission(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 禁用角色
%% @end
disable(#{
    auth_data := #{
        <<"authed">> := true,
        <<"loginname">> := LoginName,
        <<"permission">> := #{<<"usermanage">> := true}
    },
    bindings := #{<<"roleId">> := RoleId}
}) ->
    try
        ok = eadm_mnesia_api:update(eadm_role, RoleId, fun(R) ->
            R#eadm_role{
                rolestatus = 1 - R#eadm_role.rolestatus,
                updateduser = LoginName,
                updatedat = erlang:system_time(second)
            }
        end),
        {json, [#{<<"Alert">> => unicode:characters_to_binary("角色启禁用成功！", utf8)}]}
    catch
        _:Error ->
            lager:error("角色启禁用失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("角色启禁用失败！", utf8)}]}
    end;
disable(#{auth_data := #{<<"permission">> := #{<<"usermanage">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};
disable(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 删除角色数据
%% @end
delete(#{
    auth_data := #{
        <<"authed">> := true,
        <<"loginname">> := LoginName,
        <<"permission">> := #{<<"usermanage">> := true}
    },
    bindings := #{<<"roleId">> := RoleId}
}) ->
    try
        ok = eadm_mnesia_api:update(eadm_role, RoleId, fun(R) ->
            R#eadm_role{
                deleted = true,
                deleteduser = LoginName,
                deletedat = erlang:system_time(second)
            }
        end),
        {json, [#{<<"Alert">> => unicode:characters_to_binary("角色删除成功！", utf8)}]}
    catch
        _:Error ->
            lager:error("角色删除失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("角色删除失败！", utf8)}]}
    end;
delete(#{auth_data := #{<<"permission">> := #{<<"usermanage">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败, utf8", utf8)}]};
delete(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 查询角色列表
%% @end
getrolelist(#{
    auth_data := #{
        <<"authed">> := true,
        <<"permission">> := #{<<"usermanage">> := true}
    },
    bindings := #{<<"userId">> := UserId}
}) ->
    try
        % 使用缓存包装器
        Roles = eadm_mnesia_api_cached:query_all(eadm_role, 600),
        UserRoles = eadm_mnesia_api_cached:find_by_field(eadm_userrole, userid, UserId, 600),
        AssignedRoleIds = [RoleId || #eadm_userrole{roleid = RoleId, deleted = false} <- UserRoles],

        AvailableRoles = [
            #{
                <<"id">> => Id,
                <<"rolename">> => Name,
                <<"createdat">> => CreatedAt
            }
         || #eadm_role{id = Id, rolename = Name, createdat = CreatedAt, deleted = false} <- Roles,
            not lists:member(Id, AssignedRoleIds)
        ],

        {json, #{
            <<"columns">> => [<<"id">>, <<"rolename">>, <<"createdat">>],
            <<"data">> => AvailableRoles
        }}
    catch
        _:Error ->
            lager:error("角色列表查询失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("角色列表查询失败！", utf8)}]}
    end;
getrolelist(#{auth_data := #{<<"permission">> := #{<<"usermanage">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};
getrolelist(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%%====================================================================
%% 内部函数
%%====================================================================

