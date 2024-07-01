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
-export([index/1, search/1, disable/1, delete/1, loadpermission/1, updatepermission/1, getrolelist/1]).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc
%% 主函数
%% @end
index(#{auth_data := #{<<"authed">> := true, <<"username">> := UserName,
      <<"permission">> := #{<<"usermanage">> := true}}}) ->
    {ok, [{username, UserName}]};

index(#{auth_data := #{<<"permission">> := #{<<"usermanage">> := false}}}) ->
    Alert = #{<<"Alert">> => unicode:characters_to_binary("API鉴权失败! ")},
    {json, [Alert]};

index(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 查询返回数据结果
%% @end
search(#{auth_data := #{<<"authed">> := true,
      <<"permission">> := #{<<"usermanage">> := true}}}) ->
    try
        {ok, Res_Col, Res_Data} = eadm_pgpool:equery(pool_pg,
            "select id, rolename, rolestatus, createdat
            from vi_role
            order by createdat;", []),
        Response = eadm_utils:return_as_json(Res_Col, Res_Data),
        {json, Response}
    catch
        _:Error ->
            Alert = #{<<"Alert">> => unicode:characters_to_binary("数据查询失败! " ++ Error)},
            {json, [Alert]}
    end;

search(#{auth_data := #{<<"permission">> := #{<<"usermanage">> := false}}}) ->
    Alert = #{<<"Alert">> => unicode:characters_to_binary("API鉴权失败! ")},
    {json, [Alert]};

search(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 获取角色权限数据
%% @end
loadpermission(#{auth_data := #{<<"authed">> := true,
      <<"permission">> := #{<<"usermanage">> := true}},
      bindings := #{<<"roleId">> := RoleId}}) ->
    try
        {ok, _, ResData} = eadm_pgpool:equery(pool_pg,
            "select rolepermission
            from eadm_role
            where id = $1
              and deleted is false;",
            [RoleId]),
        ResBin = hd(ResData),
        {ResJson} = json:decode(ResBin),
        {json, ResJson}
    catch
        _:Error ->
            Alert = #{<<"Alert">> => unicode:characters_to_binary("数据查询失败! " ++ Error)},
            {json, [Alert]}
    end;

loadpermission(#{auth_data := #{<<"permission">> := #{<<"usermanage">> := false}}}) ->
    Alert = #{<<"Alert">> => unicode:characters_to_binary("API鉴权失败! ")},
    {json, [Alert]};

loadpermission(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 更新角色权限信息
%% @end
updatepermission(#{auth_data := #{<<"authed">> := true, <<"loginname">> := LoginName,
      <<"permission">> := #{<<"usermanage">> := true}},
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
        eadm_pgpool:equery(pool_pg,
            "update eadm_role
            set rolepermission = $1,
                updateduser = $2,
                updatedat = current_timestamp
            where id = $3;", [RolePermissionJson, LoginName, RoleId]),
        Info = #{<<"Alert">> => unicode:characters_to_binary("权限更新成功! ")},
        {json, [Info]}
    catch
        _:Error ->
            Alert = #{<<"Alert">> => unicode:characters_to_binary("权限更新失败! " ++ Error)},
            {json, [Alert]}
    end;

updatepermission(#{auth_data := #{<<"permission">> := #{<<"usermanage">> := false}}}) ->
    Alert = #{<<"Alert">> => unicode:characters_to_binary("API鉴权失败! ")},
    {json, [Alert]};

updatepermission(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 禁用角色
%% @end
disable(#{auth_data := #{<<"authed">> := true, <<"loginname">> := LoginName,
      <<"permission">> := #{<<"usermanage">> := true}},
      bindings := #{<<"roleId">> := RoleId}}) ->
    try
        eadm_pgpool:equery(pool_pg,
            "update eadm_role
            set rolestatus = 1 - rolestatus,
                updateduser = $1,
                updatedat = current_timestamp
            where id = $2
              and deleted is false;",
            [LoginName, RoleId]),
        Info = #{<<"Alert">> => unicode:characters_to_binary("角色启禁用成功! ")},
        {json, [Info]}
    catch
        _:Error ->
            Alert = #{<<"Alert">> => unicode:characters_to_binary("角色操作失败! ") ++ binary_to_list(Error)},
            {json, [Alert]}
    end;

disable(#{auth_data := #{<<"permission">> := #{<<"usermanage">> := false}}}) ->
    Alert = #{<<"Alert">> => unicode:characters_to_binary("API鉴权失败! ")},
    {json, [Alert]};

disable(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 删除角色数据
%% @end
delete(#{auth_data := #{<<"authed">> := true, <<"loginname">> := LoginName,
      <<"permission">> := #{<<"usermanage">> := true}},
      bindings := #{<<"roleId">> := RoleId}}) ->
    try
        eadm_pgpool:equery(pool_pg,
            "update eadm_role
            set deleteduser = $1,
              deletedat = now(),
              deleted = true
            where id = $2;",
            [LoginName, RoleId]),
        Info = #{<<"Alert">> => unicode:characters_to_binary("角色删除成功! ")},
        {json, [Info]}
    catch
        _:Error ->
            Alert = #{<<"Alert">> => unicode:characters_to_binary("角色删除失败! " ++ Error)},
            {json, [Alert]}
    end;

delete(#{auth_data := #{<<"permission">> := #{<<"usermanage">> := false}}}) ->
    Alert = #{<<"Alert">> => unicode:characters_to_binary("API鉴权失败! ")},
    {json, [Alert]};

delete(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 查询角色列表
%% @end
getrolelist(#{auth_data := #{<<"authed">> := true,
      <<"permission">> := #{<<"usermanage">> := true}},
      bindings := #{<<"userId">> := UserId}}) ->
    try
        {ok, Res_Col, Res_Data} = eadm_pgpool:equery(pool_pg,
            "select a.id, a.rolename, a.createdat
            from vi_role a
            where not exists(select 1
                from eadm_userrole b
                where b.roleid=a.id
                and b.userid=$1
                and b.deleted is false)
            order by a.createdat;", [UserId]),
        Response = eadm_utils:return_as_json(Res_Col, Res_Data),
        {json, Response}
    catch
        _:Error ->
            Alert = #{<<"Alert">> => unicode:characters_to_binary("数据查询失败! " ++ Error)},
            {json, [Alert]}
    end;

getrolelist(#{auth_data := #{<<"permission">> := #{<<"usermanage">> := false}}}) ->
    Alert = #{<<"Alert">> => unicode:characters_to_binary("API鉴权失败! ")},
    {json, [Alert]};

getrolelist(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%%====================================================================
%% 内部函数
%%====================================================================

