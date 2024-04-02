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
%%% Application callbacks
%%%===================================================================
-export([index/1, search/1, disable/1, delete/1, loadpermission/1, updatepermission/1, getrolelist/1]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc
%% index
%% @end
index(#{auth_data := #{<<"authed">> := true, <<"username">> := UserName}}) ->
    {ok, [{username, UserName}]};

index(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 查询返回数据结果
%% @end
search(#{auth_data := #{<<"authed">> := true}}) ->
    try
        {ok, Res_Col, Res_Data} = mysql_pool:query(pool_db,
            "SELECT Id, RoleName, RoleStatus, CreatedAt
            FROM vi_role
            ORDER BY CreatedAt;", []),
        Response = eadm_utils:return_as_json(Res_Col, Res_Data),
        {json, Response}
    catch
        _:Error ->
            Alert = #{<<"Alert">> => unicode:characters_to_binary("数据查询失败! " ++ Error)},
            {json, [Alert]}
    end;

search(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 获取角色权限数据
%% @end
loadpermission(#{auth_data := #{<<"authed">> := true}, bindings := #{<<"roleId">> := RoleId}}) ->
    try
        {ok, _, ResData} = mysql_pool:query(pool_db,
            "SELECT RolePermission
            FROM eadm_role
            WHERE Id = ?
              AND Deleted = 0;",
            [RoleId]),
        ResBin = list_to_binary(ResData),
        {ok, ResJson} = thoas:decode(ResBin),
        {json, ResJson}
    catch
        _:Error ->
            Alert = #{<<"Alert">> => unicode:characters_to_binary("数据查询失败! " ++ Error)},
            {json, [Alert]}
    end;

loadpermission(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 更新角色权限信息
%% @end
updatepermission(#{auth_data := #{<<"authed">> := true, <<"loginname">> := LoginName},
      params := #{<<"roleId">> := RoleId, <<"dashBoard">> := DashBoard, <<"health">> := Health,
        <<"locate">> := Locate, <<"finance">> := Finance, <<"finimp">> := Finimp,
        <<"findel">> := Findel, <<"crontab">> := Crontab, <<"userManage">> := Usermanage}}) ->
    RolePermissionMap = #{<<"dashboard">> => binary_to_atom(DashBoard),
        <<"health">> => binary_to_atom(Health),
        <<"locate">> => binary_to_atom(Locate),
        <<"finance">> =>  #{
           <<"finlist">> => binary_to_atom(Finance),
           <<"finimp">> => binary_to_atom(Finimp),
           <<"findel">> => binary_to_atom(Findel)
        },
        <<"crontab">> => binary_to_atom(Crontab),
        <<"usermanage">> => binary_to_atom(Usermanage)
    },
    RolePermissionJson = thoas:encode(RolePermissionMap),
    try
        mysql_pool:query(pool_db,
            "UPDATE eadm_role
            SET RolePermission = ?,
                UpdatedUser = ?,
                UpdatedAt = CURRENT_TIMESTAMP()
            WHERE Id = ?;", [RolePermissionJson, LoginName, RoleId]),
        Info = #{<<"Alert">> => unicode:characters_to_binary("权限更新成功! ")},
        {json, [Info]}
    catch
        _:Error ->
            Alert = #{<<"Alert">> => unicode:characters_to_binary("权限更新失败! " ++ Error)},
            {json, [Alert]}
    end;

updatepermission(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 禁用角色
%% @end
disable(#{auth_data := #{<<"authed">> := true, <<"loginname">> := LoginName},
      bindings := #{<<"roleId">> := RoleId}}) ->
    try
        mysql_pool:query(pool_db,
            "UPDATE eadm_role
            SET RoleStatus = 1 - RoleStatus,
                UpdatedUser = ?,
                UpdatedAt = CURRENT_TIMESTAMP()
            WHERE Id = ?
              AND Deleted = 0;",
            [LoginName, RoleId]),
        Info = #{<<"Alert">> => unicode:characters_to_binary("角色启禁用成功! ")},
        {json, [Info]}
    catch
        _:Error ->
            Alert = #{<<"Alert">> => unicode:characters_to_binary("角色操作失败! ") ++ binary_to_list(Error)},
            {json, [Alert]}
    end;

disable(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 删除角色数据
%% @end
delete(#{auth_data := #{<<"authed">> := true, <<"loginname">> := LoginName},
      bindings := #{<<"roleId">> := RoleId}}) ->
    try
        mysql_pool:query(pool_db,
            "UPDATE eadm_role
            SET DeletedUser = ?,
              DeletedAt = NOW(),
              Deleted = 1
            WHERE Id = ?;",
            [LoginName, RoleId]),
        Info = #{<<"Alert">> => unicode:characters_to_binary("角色删除成功! ")},
        {json, [Info]}
    catch
        _:Error ->
            Alert = #{<<"Alert">> => unicode:characters_to_binary("角色删除失败! " ++ Error)},
            {json, [Alert]}
    end;

delete(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 查询角色列表
%% @end
getrolelist(#{auth_data := #{<<"authed">> := true},
      bindings := #{<<"userId">> := UserId}}) ->
    try
        {ok, Res_Col, Res_Data} = mysql_pool:query(pool_db,
            "SELECT A.Id, A.RoleName, A.CreatedAt
            FROM vi_role A
            WHERE NOT EXISTS(SELECT 1
                FROM eadm_userrole B
                WHERE B.RoleId=A.Id
                AND B.UserId=?
                AND B.Deleted=0)
            ORDER BY A.CreatedAt;", [UserId]),
        Response = eadm_utils:return_as_json(Res_Col, Res_Data),
        {json, Response}
    catch
        _:Error ->
            Alert = #{<<"Alert">> => unicode:characters_to_binary("数据查询失败! " ++ Error)},
            {json, [Alert]}
    end;

getrolelist(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%%====================================================================
%% Internal functions
%%====================================================================

