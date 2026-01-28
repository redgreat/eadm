%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%
%%% 用户信息逻辑处理
%%%
%%% @end
%%% Created : 2024-03-20 16:20:17
%%%-------------------------------------------------------------------
-module(eadm_user_controller).
-author("wangcw").

-include("eadm_mnesia.hrl").
-export([
    index/1,
    search/1,
    add/1,
    edit/1,
    reset/1,
    delete/1,
    disable/1,
    userrole/1,
    userroleadd/1,
    userroledel/1,
    userpermission/1
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
search(#{auth_data := #{<<"authed">> := true, <<"permission">> := #{<<"usermanage">> := true}}}) ->
    try
        % 使用缓存包装器，TTL 10分钟
        Users = eadm_mnesia_api_cached:query_all(eadm_user, 600),
        % 过滤已删除的用户并转换格式
        ActiveUsers = [
            #{
                <<"id">> => Id,
                <<"tenantname">> => get_tenant_name(TenantId),
                <<"loginname">> => LoginName,
                <<"username">> => UserName,
                <<"email">> => Email,
                <<"userstatus">> => Status,
                <<"createdat">> => CreatedAt
            }
         || #eadm_user{
                id = Id,
                tenantid = TenantId,
                loginname = LoginName,
                username = UserName,
                email = Email,
                userstatus = Status,
                createdat = CreatedAt,
                deleted = false
            } <- Users
        ],
        {json, #{
            <<"columns">> => [
                <<"id">>,
                <<"tenantname">>,
                <<"loginname">>,
                <<"username">>,
                <<"email">>,
                <<"userstatus">>,
                <<"createdat">>
            ],
            <<"data">> => ActiveUsers
        }}
    catch
        _:Error ->
            lager:error("用户查询失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("用户查询失败！", utf8)}]}
    end;
search(#{auth_data := #{<<"permission">> := #{<<"usermanage">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！")}]};
search(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 新增用户数据
%% @end
add(#{
    auth_data := #{
        <<"authed">> := true,
        <<"loginname">> := CreatedUser,
        <<"permission">> := #{<<"usermanage">> := true}
    },
    params := #{
        <<"loginName">> := LoginName,
        <<"email">> := Email,
        <<"userName">> := UserName,
        <<"password">> := PassWord
    }
}) ->
    try
        case validate_password(PassWord) of
            {ok} ->
                case validate_addloginname(LoginName) of
                    {ok} ->
                        case re:run(Email, "^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+$") of
                            {match, _} ->
                                CryptoGram = eadm_utils:pass_encrypt(PassWord),
                                NewId = eadm_mnesia_api:get_next_id(eadm_user),
                                User = #eadm_user{
                                    id = NewId,
                                    tenantid = <<"et0000000002">>,
                                    loginname = LoginName,
                                    username = UserName,
                                    email = Email,
                                    passwd = CryptoGram,
                                    createduser = CreatedUser,
                                    createdat = erlang:system_time(second)
                                },
                                ok = eadm_mnesia_api_cached:create(eadm_user, User),
                                % 失效用户列表缓存
                                eadm_cache:clear(mnesia_query_all),
                                A = unicode:characters_to_binary("用户【", utf8),
                                B = unicode:characters_to_binary("】新增成功！", utf8),
                                {json, [#{<<"Alert">> => <<A/binary, UserName/binary, B/binary>>}]};
                            _ ->
                                A = unicode:characters_to_binary("邮箱【", utf8),
                                B = unicode:characters_to_binary("】格式错误！", utf8),
                                {json, [#{<<"Alert">> => <<A/binary, Email/binary, B/binary>>}]}
                        end;
                    {error, 1} ->
                        A = unicode:characters_to_binary("登录名【", utf8),
                        B = unicode:characters_to_binary("】不能少于6位！", utf8),
                        {json, [#{<<"Alert">> => <<A/binary, LoginName/binary, B/binary>>}]};
                    {error, 2} ->
                        A = unicode:characters_to_binary("登录名【", utf8),
                        B = unicode:characters_to_binary("】不能大于18位！", utf8),
                        {json, [#{<<"Alert">> => <<A/binary, LoginName/binary, B/binary>>}]};
                    {error, 3} ->
                        A = unicode:characters_to_binary("登录名【", utf8),
                        B = unicode:characters_to_binary("】已存在！", utf8),
                        {json, [#{<<"Alert">> => <<A/binary, LoginName/binary, B/binary>>}]};
                    {error, 6} ->
                        A = unicode:characters_to_binary("登录名【", utf8),
                        B = unicode:characters_to_binary("】仅支持英文+数字！", utf8),
                        {json, [#{<<"Alert">> => <<A/binary, LoginName/binary, B/binary>>}]};
                    _ ->
                        {json, [#{<<"Alert">> => unicode:characters_to_binary("用户新增失败！", utf8)}]}
                end;
            {error, ErrInfo} ->
                {json, [#{<<"Alert">> => unicode:characters_to_binary(ErrInfo, utf8)}]};
            _ ->
                {json, [#{<<"Alert">> => unicode:characters_to_binary("用户新增失败！", utf8)}]}
        end
    catch
        _:Error ->
            lager:error("用户新增失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("用户新增失败！", utf8)}]}
    end;
add(#{auth_data := #{<<"permission">> := #{<<"usermanage">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};
add(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 编辑用户数据
%% @end
edit(#{
    auth_data := #{
        <<"authed">> := true,
        <<"loginname">> := CreatedUser,
        <<"permission">> := #{<<"usermanage">> := true}
    },
    params := #{
        <<"userId">> := UserId,
        <<"loginName">> := LoginName,
        <<"email">> := Email,
        <<"userName">> := UserName
    }
}) ->
    case validate_editloginname(UserId, LoginName) of
        {ok} ->
            case re:run(Email, "^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+$") of
                {match, _} ->
                    try
                        ok = eadm_mnesia_api_cached:update(eadm_user, UserId, fun(U) ->
                            U#eadm_user{
                                loginname = LoginName,
                                username = UserName,
                                email = Email,
                                updateduser = CreatedUser,
                                updatedat = erlang:system_time(second)
                            }
                        end),
                        % 失效相关缓存
                        eadm_cache:clear(mnesia_query_all),
                        eadm_cache:invalidate(user_permission, LoginName),
                        A = unicode:characters_to_binary("用户【", utf8),
                        B = unicode:characters_to_binary("】编辑成功！", utf8),
                        {json, [#{<<"Alert">> => <<A/binary, UserName/binary, B/binary>>}]}
                    catch
                        _:Error ->
                            lager:error("用户编辑失败：~p~n", [Error]),
                            {json, [
                                #{<<"Alert">> => unicode:characters_to_binary("用户编辑失败！", utf8)}
                            ]}
                    end;
                _ ->
                    A = unicode:characters_to_binary("邮箱【", utf8),
                    B = unicode:characters_to_binary("】格式错误！", utf8),
                    {json, [#{<<"Alert">> => <<A/binary, Email/binary, B/binary>>}]}
            end;
        {error, 1} ->
            A = unicode:characters_to_binary("登录名【", utf8),
            B = unicode:characters_to_binary("】不能少于6位！", utf8),
            {json, [#{<<"Alert">> => <<A/binary, LoginName/binary, B/binary>>}]};
        {error, 2} ->
            A = unicode:characters_to_binary("登录名【", utf8),
            B = unicode:characters_to_binary("】不能大于18位！", utf8),
            {json, [#{<<"Alert">> => <<A/binary, LoginName/binary, B/binary>>}]};
        {error, 3} ->
            A = unicode:characters_to_binary("登录名【", utf8),
            B = unicode:characters_to_binary("】已存在！", utf8),
            {json, [#{<<"Alert">> => <<A/binary, LoginName/binary, B/binary>>}]};
        {error, 6} ->
            A = unicode:characters_to_binary("登录名【", utf8),
            B = unicode:characters_to_binary("】仅支持英文+数字！", utf8),
            {json, [#{<<"Alert">> => <<A/binary, LoginName/binary, B/binary>>}]};
        _ ->
            {json, [#{<<"Alert">> => unicode:characters_to_binary("用户编辑失败！", utf8)}]}
    end;
edit(#{auth_data := #{<<"permission">> := #{<<"usermanage">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};
edit(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 重置用户密码
%% @end
reset(#{
    auth_data := #{
        <<"authed">> := true,
        <<"loginname">> := LoginName,
        <<"permission">> := #{<<"usermanage">> := true}
    },
    bindings := #{<<"userId">> := UserId}
}) ->
    % 重置密码(123456)
    CryptoGram = eadm_utils:pass_encrypt(<<"123456">>),
    lager:info("用户~p重置了密码~n", [LoginName]),
    try
        ok = eadm_mnesia_api_cached:update(eadm_user, UserId, fun(U) ->
            U#eadm_user{
                passwd = CryptoGram,
                updateduser = LoginName,
                updatedat = erlang:system_time(second)
            }
        end),
        {json, [#{<<"Alert">> => unicode:characters_to_binary("用户密码重置成功！", utf8)}]}
    catch
        _:Error ->
            lager:error("用户密码重置失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("用户密码重置失败！", utf8)}]}
    end;
reset(#{auth_data := #{<<"permission">> := #{<<"usermanage">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};
reset(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 禁用用户
%% @end
disable(#{
    auth_data := #{
        <<"authed">> := true,
        <<"loginname">> := LoginName,
        <<"permission">> := #{<<"usermanage">> := true}
    },
    bindings := #{<<"userId">> := UserId}
}) ->
    try
        ok = eadm_mnesia_api_cached:update(eadm_user, UserId, fun(U) ->
            U#eadm_user{
                userstatus = 1 - U#eadm_user.userstatus,
                updateduser = LoginName,
                updatedat = erlang:system_time(second)
            }
        end),
        % 失效用户列表缓存
        eadm_cache:clear(mnesia_query_all),
        {json, [#{<<"Alert">> => unicode:characters_to_binary("用户启禁用成功！", utf8)}]}
    catch
        _:Error ->
            lager:error("用户操作失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("用户操作失败！", utf8)}]}
    end;
disable(#{auth_data := #{<<"permission">> := #{<<"usermanage">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};
disable(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 删除用户数据
%% @end
delete(#{
    auth_data := #{
        <<"authed">> := true,
        <<"loginname">> := LoginName,
        <<"permission">> := #{<<"usermanage">> := true}
    },
    bindings := #{<<"userId">> := UserId}
}) ->
    try
        % 先读取用户信息，用于失效缓存
        DeletedLoginName = case eadm_mnesia_api_cached:read(eadm_user, UserId) of
            [#eadm_user{loginname = LName}] -> LName;
            _ -> <<>>
        end,
        ok = eadm_mnesia_api_cached:update(eadm_user, UserId, fun(U) ->
            U#eadm_user{
                deleted = true,
                deleteduser = LoginName,
                deletedat = erlang:system_time(second)
            }
        end),
        % 失效相关缓存
        eadm_cache:clear(mnesia_query_all),
        case DeletedLoginName of
            <<>> -> ok;
            _ -> eadm_cache:invalidate(user_permission, DeletedLoginName)
        end,
        {json, [#{<<"Alert">> => unicode:characters_to_binary("用户删除成功！", utf8)}]}
    catch
        _:Error ->
            lager:error("用户删除失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("用户删除失败！", utf8)}]}
    end;
delete(#{auth_data := #{<<"permission">> := #{<<"usermanage">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};
delete(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 获取用户角色
%% @end
userrole(#{
    auth_data := #{
        <<"authed">> := true,
        <<"permission">> := #{<<"usermanage">> := true}
    },
    bindings := #{<<"userId">> := UserId}
}) ->
    try
        {ok, ResCol, ResData} = eadm_pgpool:equery(
            pool_pg,
            "select id, rolename, updatedat
\n"
            "            from vi_userrole
\n"
            "            where userid = $1;",
            [UserId]
        ),
        {json, eadm_utils:pg_as_json(ResCol, ResData)}
    catch
        _:Error ->
            lager:error("用户角色查询失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("用户角色查询失败！", utf8)}]}
    end;
userrole(#{auth_data := #{<<"permission">> := #{<<"usermanage">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};
userrole(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 新增用户角色
%% @end
userroleadd(#{
    auth_data := #{
        <<"authed">> := true,
        <<"loginname">> := LoginName,
        <<"permission">> := #{<<"usermanage">> := true}
    },
    params := RoleIdMap
}) ->
    [{RoleIds, _Value}] = maps:to_list(RoleIdMap),
    {ok, RoleIdList} = thoas:decode(RoleIds),
    InsertQuery = "insert into eadm_userrole(userid, roleid, createduser) values($1, $2, $3);",
    try
        lists:foreach(
            fun(Map) ->
                eadm_pgpool:equery(
                    pool_pg,
                    InsertQuery,
                    [maps:get(<<"userId">>, Map), maps:get(<<"roleId">>, Map), LoginName]
                )
            end,
            RoleIdList
        ),
        {json, [#{<<"Alert">> => unicode:characters_to_binary("用户角色新增成功！", utf8)}]}
    catch
        _:Error ->
            lager:error("用户角色新增失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("用户角色新增失败！", utf8)}]}
    end;
userroleadd(#{auth_data := #{<<"permission">> := #{<<"usermanage">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};
userroleadd(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 删除用户角色数据
%% @end
userroledel(#{
    auth_data := #{
        <<"authed">> := true,
        <<"loginname">> := LoginName,
        <<"permission">> := #{<<"usermanage">> := true}
    },
    bindings := #{<<"userRoleId">> := UserRoleId}
}) ->
    try
        eadm_pgpool:equery(
            pool_pg,
            "update eadm_userrole
\n"
            "                                  set deleteduser = $1,
\n"
            "                                  deletedat = current_timestamp,
\n"
            "                                  deleted = true
\n"
            "                                  where id = $2;",
            [LoginName, erlang:binary_to_integer(UserRoleId)]
        ),
        {json, [#{<<"Alert">> => unicode:characters_to_binary("用户角色删除成功！", utf8)}]}
    catch
        _:Error ->
            lager:error("用户角色删除失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("用户角色删除失败！", utf8)}]}
    end;
userroledel(#{auth_data := #{<<"permission">> := #{<<"usermanage">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};
userroledel(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 获取角色权限数据
%% 需特殊处理权限验证，需要登录成功所以要验authed=true，无需数据权限不需验permission
%% @end
userpermission(#{auth_data := #{<<"authed">> := true, <<"loginname">> := LoginName}}) ->
    Permission = get_permission(LoginName),
    {json, [Permission]};
userpermission(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%%====================================================================
%% 内部函数
%%====================================================================

%% @doc
%% 验证登录名是否有重复(新增)
%% @end
validate_addloginname(LoginName) ->
    AllowedChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-",
    Regex = "^[" ++ AllowedChars ++ "]+$",
    try
        case re:run(LoginName, Regex, [global, {capture, none}]) of
            match ->
                case erlang:byte_size(LoginName) of
                    L when L < 6 ->
                        {error, 1};
                    L when L > 18 ->
                        {error, 2};
                    _ ->
                        try
                            case
                                eadm_pgpool:equery(
                                    pool_pg, "select 1 from eadm_user where loginname = $1;", [
                                        LoginName
                                    ]
                                )
                            of
                                {ok, _, []} ->
                                    {ok};
                                {ok, _, _} ->
                                    {error, 3};
                                _ ->
                                    {error, 4}
                            end
                        catch
                            _ ->
                                {error, 5}
                        end
                end;
            _ ->
                {error, 6}
        end
    catch
        _:Error ->
            lager:error("用户名验证失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("用户名验证失败！", utf8)}]}
    end.

%% @doc
%% 验证登录名是否有重复(修改)
%% @end
validate_editloginname(UserId, LoginName) ->
    % io:format("UserId: ~p, LoginName: ~p~n", [UserId, LoginName]),
    AllowedChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-",
    Regex = "^[" ++ AllowedChars ++ "]+$",
    try
        case re:run(LoginName, Regex, [global, {capture, none}]) of
            match ->
                case erlang:byte_size(LoginName) of
                    L when L < 6 ->
                        {error, 1};
                    L when L > 18 ->
                        {error, 2};
                    _ ->
                        try
                            case
                                eadm_pgpool:equery(
                                    pool_pg,
                                    "select 1 from eadm_user where id != $1 and loginname = $2 and deleted is false;",
                                    [UserId, LoginName]
                                )
                            of
                                {ok, _, []} ->
                                    {ok};
                                {ok, _, _} ->
                                    {error, 3};
                                _ ->
                                    {error, 4}
                            end
                        catch
                            _ ->
                                {error, 5}
                        end
                end;
            _ ->
                {error, 6}
        end
    catch
        _:Error ->
            lager:error("用户名验证失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("用户名验证失败！", utf8)}]}
    end.

%% @doc
%% 验证二进制密码数据
%% @end
validate_password(PassWordBin) when erlang:is_binary(PassWordBin) ->
    PassWord = erlang:binary_to_list(PassWordBin),
    AllowedChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789,\._-",
    Regex = "^[" ++ AllowedChars ++ "]+$",
    try
        case re:run(PassWord, Regex, [global, {capture, none}]) of
            match ->
                case erlang:byte_size(PassWordBin) of
                    L when L < 6 ->
                        {error, "密码不能少于6位！"};
                    L when L > 36 ->
                        {error, "密码不能大于36位！"};
                    _ ->
                        {ok}
                end;
            _ ->
                {error, "密码仅支持【英文、数字、符号：,._-】"}
        end
    catch
        _:Error ->
            lager:error("密码验证失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("密码验证失败！", utf8)}]}
    end;
validate_password(_) ->
    {error, "密码格式错误！"}.

%% @doc
%% 获取用户权限（带缓存）
%% @end
get_permission(LoginName) ->
    % 使用缓存包装器，TTL 30分钟
    CacheType = user_permission,
    CacheKey = LoginName,
    TTL = 1800, % 30分钟
    
    eadm_cache:get_or_set(
        CacheType,
        CacheKey,
        fun() ->
    try
                case eadm_mnesia_api_cached:find_by_field(eadm_user, loginname, LoginName, 1800) of
            [#eadm_user{id = UserId}] ->
                        UserRoles = eadm_mnesia_api_cached:find_by_field(eadm_userrole, userid, UserId, 1800),
                case UserRoles of
                    [] ->
                        #{<<"data">> => #{}};
                    [#eadm_userrole{roleid = RoleId} | _] ->
                                case eadm_mnesia_api_cached:read(eadm_role, RoleId, 1800) of
                            [#eadm_role{rolepermission = Permission, rolestatus = 0}] ->
                                #{<<"data">> => Permission};
                            _ ->
                                #{<<"data">> => #{}}
                        end
                end;
            [] ->
                #{<<"data">> => #{}}
        end
    catch
        _:Error ->
            lager:error("用户权限获取失败：~p~n", [Error]),
            #{<<"data">> => #{}}
            end
        end,
        TTL
    ).

%% @doc
%% 获取租户名称（带缓存）
%% @end
get_tenant_name(TenantId) ->
    % 使用缓存包装器，TTL 60分钟
    case eadm_mnesia_api_cached:read(eadm_tenant, TenantId, 3600) of
        [#eadm_tenant{tenantname = Name}] -> Name;
        _ -> <<"未知租户"/utf8>>
    end.
