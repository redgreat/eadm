%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%
%%% 基于mnesia数据库的角色信息逻辑处理
%%%
%%% @end
%%% Created : 2024-12-19
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
        case eadm_mnesia:list_roles() of
            {ok, Roles} ->
                Response = format_roles_response(Roles),
                {json, Response};
            {error, Reason} ->
                lager:error("角色查询失败：~p~n", [Reason]),
                {json, [#{<<"Alert">> => unicode:characters_to_binary("角色查询失败！", utf8)}]}
        end
    catch
        _:Error ->
            lager:error("角色查询失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("角色查询失败！", utf8)}]}
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
        RoleId = generate_role_id(),
        case eadm_mnesia:create_role(RoleName, <<"系统角色">>, RoleId) of
            {ok, _Role} ->
                A = unicode:characters_to_binary("角色【", utf8),
                B = unicode:characters_to_binary("】新增成功！", utf8),
                {json, [#{<<"Alert">> => <<A/binary, RoleName/binary, B/binary>>}]};
            {error, Reason} ->
                lager:error("角色新增失败：~p~n", [Reason]),
                {json, [#{<<"Alert">> => unicode:characters_to_binary("角色新增失败！", utf8)}]}
        end
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
%% 禁用角色
%% @end
disable(#{auth_data := #{<<"authed">> := true, <<"loginname">> := LoginName,
        <<"permission">> := #{<<"usermanage">> := true}},
        bindings := #{<<"roleId">> := RoleId}}) ->
    lager:info("用户~p禁用了角色~p~n", [LoginName, RoleId]),
    try
        case eadm_mnesia:update_role(RoleId, [{status, disabled}]) of
            {ok, _Role} ->
                {json, [#{<<"Alert">> => unicode:characters_to_binary("角色禁用成功！", utf8)}]};
            {error, not_found} ->
                {json, [#{<<"Alert">> => unicode:characters_to_binary("角色不存在！", utf8)}]};
            {error, Reason} ->
                lager:error("角色禁用失败：~p~n", [Reason]),
                {json, [#{<<"Alert">> => unicode:characters_to_binary("角色禁用失败！", utf8)}]}
        end
    catch
        _:Error ->
            lager:error("角色禁用失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("角色禁用失败！", utf8)}]}
    end;

disable(#{auth_data := #{<<"permission">> := #{<<"usermanage">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};

disable(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 删除角色
%% @end
delete(#{auth_data := #{<<"authed">> := true, <<"loginname">> := LoginName,
       <<"permission">> := #{<<"usermanage">> := true}},
       bindings := #{<<"roleId">> := RoleId}}) ->
    lager:info("用户~p删除了角色~p~n", [LoginName, RoleId]),
    try
        case eadm_mnesia:delete_role(RoleId) of
            ok ->
                {json, [#{<<"Alert">> => unicode:characters_to_binary("角色删除成功！", utf8)}]};
            {error, Reason} ->
                lager:error("角色删除失败：~p~n", [Reason]),
                {json, [#{<<"Alert">> => unicode:characters_to_binary("角色删除失败！", utf8)}]}
        end
    catch
        _:Error ->
            lager:error("角色删除失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("角色删除失败！", utf8)}]}
    end;

delete(#{auth_data := #{<<"permission">> := #{<<"usermanage">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};

delete(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 获取角色权限数据
%% @end
loadpermission(#{auth_data := #{<<"authed">> := true,
        <<"permission">> := #{<<"usermanage">> := true}},
        bindings := #{<<"roleId">> := RoleId}}) ->
    try
        case eadm_mnesia:get_role(RoleId) of
            {ok, Role} ->
                %% 这里可以扩展角色权限字段
                Response = #{<<"permissions">> => <<"{}">>},
                {json, [Response]};
            {error, not_found} ->
                {json, [#{<<"Alert">> => unicode:characters_to_binary("角色不存在！", utf8)}]};
            {error, Reason} ->
                lager:error("角色权限查询失败：~p~n", [Reason]),
                {json, [#{<<"Alert">> => unicode:characters_to_binary("角色权限查询失败！", utf8)}]}
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
%% 更新角色权限
%% @end
updatepermission(#{auth_data := #{<<"authed">> := true, <<"loginname">> := LoginName,
                 <<"permission">> := #{<<"usermanage">> := true}},
                 params := #{<<"roleId">> := RoleId, <<"permissions">> := Permissions}}) ->
    lager:info("用户~p更新了角色~p的权限~n", [LoginName, RoleId]),
    try
        case eadm_mnesia:update_role(RoleId, [{permissions, Permissions}]) of
            {ok, _Role} ->
                {json, [#{<<"Alert">> => unicode:characters_to_binary("角色权限更新成功！", utf8)}]};
            {error, not_found} ->
                {json, [#{<<"Alert">> => unicode:characters_to_binary("角色不存在！", utf8)}]};
            {error, Reason} ->
                lager:error("角色权限更新失败：~p~n", [Reason]),
                {json, [#{<<"Alert">> => unicode:characters_to_binary("角色权限更新失败！", utf8)}]}
        end
    catch
        _:Error ->
            lager:error("角色权限更新失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("角色权限更新失败！", utf8)}]}
    end;

updatepermission(#{auth_data := #{<<"permission">> := #{<<"usermanage">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};

updatepermission(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 获取角色列表
%% @end
getrolelist(#{auth_data := #{<<"authed">> := true,
            <<"permission">> := #{<<"usermanage">> := true}}}) ->
    try
        case eadm_mnesia:list_roles() of
            {ok, Roles} ->
                Response = format_simple_roles_response(Roles),
                {json, Response};
            {error, Reason} ->
                lager:error("角色列表查询失败：~p~n", [Reason]),
                {json, [#{<<"Alert">> => unicode:characters_to_binary("角色列表查询失败！", utf8)}]}
        end
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

%% @doc
%% 格式化角色响应数据
%% @end
format_roles_response(Roles) ->
    lists:map(fun(Role) ->
        #{<<"id">> => element(2, Role),
          <<"rolename">> => element(3, Role),
          <<"description">> => element(4, Role),
          <<"rolestatus">> => <<"active">>,
          <<"createdat">> => element(5, Role)}
    end, Roles).

%% @doc
%% 格式化简单角色响应数据
%% @end
format_simple_roles_response(Roles) ->
    lists:map(fun(Role) ->
        #{<<"id">> => element(2, Role),
          <<"name">> => element(3, Role)}
    end, Roles).

%% @doc
%% 生成角色ID
%% @end
generate_role_id() ->
    erlang:system_time(microsecond).