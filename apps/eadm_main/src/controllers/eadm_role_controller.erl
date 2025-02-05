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
-export([index/1, search/1, add/1, disable/1, delete/1, loadpermission/1, updatepermission/1, getrolelist/1]).

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
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};

index(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 查询返回数据结果
%% @end
search(#{auth_data := #{<<"authed">> := true,
      <<"permission">> := #{<<"usermanage">> := true}}}) ->
    try
        {ok, ResCol, ResData} = eadm_pgpool:equery(pool_pg,
            "select id, rolename, rolestatus, createdat
            from vi_role
            order by createdat;", []),
        Response = eadm_utils:pg_as_json(ResCol, ResData),
        {json, Response}
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
add(#{auth_data := #{<<"authed">> := true, <<"loginname">> := CreatedUser,
    <<"permission">> := #{<<"usermanage">> := true}},
    params := #{<<"roleName">> := RoleName}}) ->
    try
        eadm_pgpool:equery(pool_pg, "insert into eadm_role(rolename, createduser) values($1, $2);",
            [RoleName, CreatedUser]),
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
        RetuenJson = eadm_utils:pg_as_jsondata(ResData),
        {json, RetuenJson}
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
updatepermission(#{auth_data := #{<<"authed">> := true, <<"loginname">> := LoginName,
      <<"permission">> := #{<<"usermanage">> := true}},
      params := #{<<"roleId">> := RoleId, <<"dashBoard">> := DashBoard, <<"health">> := Health,
        <<"locate">> := Locate, <<"finance">> := Finance, <<"finimp">> := Finimp,
        <<"findel">> := Findel, <<"crontab">> := Crontab, <<"userManage">> := Usermanage}}) ->
    try
        RolePermissionMap = #{<<"dashboard">> => erlang:binary_to_atom(DashBoard),
            <<"health">> => erlang:binary_to_atom(Health),
            <<"locate">> => erlang:binary_to_atom(Locate),
            <<"finance">> =>  #{
                <<"finlist">> => erlang:binary_to_atom(Finance),
                <<"finimp">> => erlang:binary_to_atom(Finimp),
                <<"findel">> => erlang:binary_to_atom(Findel)
            },
            <<"crontab">> => erlang:binary_to_atom(Crontab),
            <<"usermanage">> => erlang:binary_to_atom(Usermanage)
        },
        RolePermissionJson = thoas:encode(RolePermissionMap),
        eadm_pgpool:equery(pool_pg,
            "update eadm_role
            set rolepermission = $1,
                updateduser = $2,
                updatedat = current_timestamp
            where id = $3;", [RolePermissionJson, LoginName, RoleId]),
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
        {json, [#{<<"Alert">> => unicode:characters_to_binary("角色启禁用成功！", utf8)}]}
    catch
        _:Error ->
            lager:error("任务新增失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("任务新增失败！", utf8)}]}
    end;

disable(#{auth_data := #{<<"permission">> := #{<<"usermanage">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};

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
getrolelist(#{auth_data := #{<<"authed">> := true,
      <<"permission">> := #{<<"usermanage">> := true}},
      bindings := #{<<"userId">> := UserId}}) ->
    try
        {ok, ResCol, ResData} = eadm_pgpool:equery(pool_pg,
            "select a.id, a.rolename, a.createdat
            from vi_role a
            where not exists(select 1
                from eadm_userrole b
                where b.roleid=a.id
                and b.userid=$1
                and b.deleted is false)
            order by a.createdat;", [UserId]),
        {json, eadm_utils:pg_as_json(ResCol, ResData)}
    catch
        _:Error ->
            lager:error("数据查询失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("数据查询失败！", utf8)}]}
    end;

getrolelist(#{auth_data := #{<<"permission">> := #{<<"usermanage">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};

getrolelist(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%%====================================================================
%% 内部函数
%%====================================================================

