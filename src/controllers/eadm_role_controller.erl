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
        {ok, Columns, ResData} =
            eadm_pgpool_cached:equery_cached(
                pool_pg,
                "select id, rolename, rolestatus, createdat from eadm_role where deleted is false order by createdat desc;",
                [],
                600,
                {role_list, all}
            ),
        {json, eadm_utils:to_json(eadm_utils:pg_as_json(Columns, ResData))}
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
        Sql =
            "insert into eadm_role(rolename, createduser, updateduser) values($1,$2,$2);",
        {ok, _} = eadm_pgpool:equery(pool_pg, Sql, [RoleName, CreatedUser]),
        eadm_pgpool_cached:invalidate_pg_cache(pool_pg, {role_list, all}),
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
        Sql = "select rolepermission from eadm_role where id = $1 and deleted is false limit 1;",
        case eadm_pgpool:equery(pool_pg, Sql, [RoleId]) of
            {ok, _, [{Permission}]} ->
                {json, [#{<<"rolepermission">> => normalize_permission(Permission)}]};
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
        RolePermissionJson = json:encode(RolePermissionMap),
        Sql =
            "update eadm_role set rolepermission = $1, updateduser = $2, updatedat = current_timestamp where id = $3 and deleted is false;",
        {ok, _} = eadm_pgpool:equery(pool_pg, Sql, [RolePermissionJson, LoginName, RoleId]),
        eadm_pgpool_cached:invalidate_pg_cache(pool_pg, {role_list, all}),
        eadm_pgpool_cached:invalidate_pg_cache(pool_pg, {role_permission, RoleId}),
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
        Sql =
            "update eadm_role set rolestatus = case rolestatus when 0 then 1 else 0 end, updateduser = $1, updatedat = current_timestamp where id = $2 and deleted is false;",
        {ok, _} = eadm_pgpool:equery(pool_pg, Sql, [LoginName, RoleId]),
        eadm_pgpool_cached:invalidate_pg_cache(pool_pg, {role_list, all}),
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
        Sql =
            "update eadm_role set deleted = true, deleteduser = $1, deletedat = current_timestamp where id = $2;",
        {ok, _} = eadm_pgpool:equery(pool_pg, Sql, [LoginName, RoleId]),
        eadm_pgpool_cached:invalidate_pg_cache(pool_pg, {role_list, all}),
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
        {ok, Columns, ResData} =
            eadm_pgpool_cached:equery_cached(
                pool_pg,
                "select id, rolename, createdat from eadm_role where deleted is false and id not in (select roleid from eadm_userrole where userid = $1 and deleted is false) order by createdat desc;",
                [UserId],
                600,
                {role_available, UserId}
            ),
        {json, eadm_utils:to_json(eadm_utils:pg_as_json(Columns, ResData))}
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

normalize_permission(Permission) when is_binary(Permission) ->
    case json:decode(Permission) of
        {ok, Map} -> Map;
        _ -> #{}
    end;
normalize_permission(Permission) when is_map(Permission) ->
    Permission;
normalize_permission(_) ->
    #{}.
