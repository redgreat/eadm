%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%  用户登录模块
%%% @end
%%% Created : 2024-02-15 23:54
%%%-------------------------------------------------------------------
-module(eadm_login_controller).
-author("wangcw").

%%%===================================================================
%%% 头文件引用
%%%===================================================================
-include("eadm_mnesia.hrl").

%%%===================================================================
%%% 函数导出
%%%===================================================================
-export([login/1, logout/1, userinfo/1, useredit/1, userpwd/1]).

%%%===================================================================
%%% API 函数
%%%===================================================================

%% @doc
%% 用户登录
%% @end
login(Req) ->
    try
        Method = cowboy_req:method(Req),
        case Method of
            <<"GET">> ->
                {ok, []};
            <<"POST">> ->
                {ok, _, #{params := Params}} = cowboy_req:read_body(Req),
                LoginName = maps:get(<<"loginName">>, Params),
                Password = maps:get(<<"password">>, Params),
                case eadm_utils:validate_login(LoginName, Password) of
                    true ->
                        UserName = getusername(LoginName),
                        Permission = getpermission(LoginName),
                        NewExp = eadm_utils:get_exp_bin(),
                        nova_session:set(Req, <<"loginname">>, LoginName),
                        nova_session:set(Req, <<"username">>, UserName),
                        nova_session:set(Req, <<"permission">>, Permission),
                        nova_session:set(Req, <<"exp">>, NewExp),
                        lager:info("User: ~ts, Login Success! New Exp: ~p", [UserName, NewExp]),
                        A = unicode:characters_to_binary("欢迎【", utf8),
                        B = unicode:characters_to_binary("】登录! ", utf8),
                        {json, [
                            #{
                                <<"Alert">> => <<A/binary, UserName/binary, B/binary>>,
                                <<"logined">> => 1
                            }
                        ]};
                    2 ->
                        lager:info("User Not Fond!"),
                        {json, [
                            #{
                                <<"Alert">> => unicode:characters_to_binary("用户不存在，请联系管理员！", utf8),
                                <<"logined">> => 0
                            }
                        ]};
                    3 ->
                        lager:info("User Disable!"),
                        {json, [
                            #{
                                <<"Alert">> => unicode:characters_to_binary("用户已禁用，请联系管理员！", utf8),
                                <<"logined">> => 0
                            }
                        ]};
                    _ ->
                        lager:info("User Login Failed!"),
                        {json, [
                            #{
                                <<"Alert">> => unicode:characters_to_binary(
                                    "用户名或密码错误，请重新登录！", utf8
                                ),
                                <<"logined">> => 0
                            }
                        ]}
                end
        end
    catch
        _:Error ->
            lager:error("用户登录失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("用户登录失败！", utf8)}]}
    end.

%% @doc
%% 退出登录
%% @end
logout(Req) ->
    lager:info("User Logout!~n"),
    nova_session:delete(Req).

%% @doc
%% 查询返回数据结果
%% @end
userinfo(#{auth_data := #{<<"authed">> := true, <<"loginname">> := LoginName}}) ->
    try
        case eadm_mnesia_api:find_by_field(eadm_user, loginname, LoginName) of
            [
                #eadm_user{
                    userstatus = 0,
                    deleted = false,
                    loginname = LName,
                    username = UName,
                    email = Email
                }
                | _
            ] ->
                {json, [#{<<"loginname">> => LName, <<"username">> => UName, <<"email">> => Email}]};
            _ ->
                {json, [#{<<"Alert">> => unicode:characters_to_binary("用户不存在或已禁用！", utf8)}]}
        end
    catch
        _:Error ->
            lager:error("用户查询失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("用户查询失败！", utf8)}]}
    end;
userinfo(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 编辑用户数据
%% @end
useredit(#{
    auth_data := #{<<"authed">> := true, <<"loginname">> := CreatedUser},
    params := #{<<"loginName">> := LoginName, <<"email">> := Email, <<"userName">> := NewUserName}
}) ->
    case re:run(Email, "^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+$") of
        {match, _} ->
            try
                case eadm_mnesia_api:find_by_field(eadm_user, loginname, CreatedUser) of
                    [User] ->
                        ok = eadm_mnesia_api:update(eadm_user, User#eadm_user.id, fun(U) ->
                            U#eadm_user{
                                loginname = LoginName,
                                username = NewUserName,
                                email = Email,
                                updateduser = CreatedUser,
                                updatedat = erlang:system_time(second)
                            }
                        end),
                        A = unicode:characters_to_binary("用户【", utf8),
                        B = unicode:characters_to_binary("】编辑成功！", utf8),
                        {json, [#{<<"Alert">> => <<A/binary, NewUserName/binary, B/binary>>}]};
                    [] ->
                        {json, [#{<<"Alert">> => unicode:characters_to_binary("用户不存在！", utf8)}]}
                end
            catch
                _:Error ->
                    lager:error("用户编辑失败：~p~n", [Error]),
                    {json, [#{<<"Alert">> => unicode:characters_to_binary("用户编辑失败！", utf8)}]}
            end;
        _ ->
            A = unicode:characters_to_binary("邮箱【", utf8),
            B = unicode:characters_to_binary("】格式错误！", utf8),
            {json, [#{<<"Alert">> => <<A/binary, Email/binary, B/binary>>}]}
    end;
useredit(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 修改用户密码
%% @end
userpwd(#{
    auth_data := #{<<"authed">> := true, <<"loginname">> := LoginName},
    params := #{<<"passwordOld">> := PasswordOld, <<"passwordNew">> := PasswordNew}
}) ->
    case validate_password(PasswordNew) of
        {ok} ->
            case eadm_utils:validate_login(LoginName, PasswordOld) of
                true ->
                    CryptoGram = eadm_utils:pass_encrypt(PasswordNew),
                    try
                        case eadm_mnesia_api:find_by_field(eadm_user, loginname, LoginName) of
                            [User] ->
                                ok = eadm_mnesia_api:update(eadm_user, User#eadm_user.id, fun(U) ->
                                    U#eadm_user{
                                        passwd = CryptoGram,
                                        updateduser = LoginName,
                                        updatedat = erlang:system_time(second)
                                    }
                                end),
                                {json, [
                                    #{<<"Alert">> => unicode:characters_to_binary("密码修改成功！", utf8)}
                                ]};
                            [] ->
                                {json, [
                                    #{<<"Alert">> => unicode:characters_to_binary("用户不存在！", utf8)}
                                ]}
                        end
                    catch
                        _:Error ->
                            lager:error("用户密码修改失败：~p~n", [Error]),
                            {json, [
                                #{<<"Alert">> => unicode:characters_to_binary("用户密码修改失败！", utf8)}
                            ]}
                    end;
                2 ->
                    lager:info("User Not Fond!"),
                    {json, [
                        #{
                            <<"Alert">> => unicode:characters_to_binary("用户不存在，请联系管理员！", utf8),
                            <<"logined">> => 0
                        }
                    ]};
                3 ->
                    lager:info("User Disable!"),
                    {json, [
                        #{
                            <<"Alert">> => unicode:characters_to_binary("用户已禁用，请联系管理员！", utf8),
                            <<"logined">> => 0
                        }
                    ]};
                _ ->
                    lager:info("User Login Failed!"),
                    {json, [
                        #{
                            <<"Alert">> => unicode:characters_to_binary("用户名或密码错误，请重新登录！", utf8),
                            <<"logined">> => 0
                        }
                    ]}
            end;
        {error, ErrInfo} ->
            {json, [#{<<"Alert">> => unicode:characters_to_binary(ErrInfo, utf8)}]};
        _ ->
            {json, [#{<<"Alert">> => unicode:characters_to_binary("用户新增失败！", utf8)}]}
    end;
userpwd(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%%===================================================================
%% 内部函数
%%===================================================================

%% @doc
%% 获取用户权限
%% @end
getpermission(LoginName) ->
    try
        case eadm_mnesia_api:find_by_field(eadm_user, loginname, LoginName) of
            [#eadm_user{id = UserId}] ->
                UserRoles = eadm_mnesia_api:find_by_field(eadm_userrole, userid, UserId),
                case UserRoles of
                    [] ->
                        #{};
                    [#eadm_userrole{roleid = RoleId} | _] ->
                        case eadm_mnesia_api:read(eadm_role, RoleId) of
                            [#eadm_role{rolepermission = Permission, rolestatus = 0}] ->
                                Permission;
                            _ ->
                                #{}
                        end
                end;
            [] ->
                #{}
        end
    catch
        _:Error ->
            lager:error("用户权限获取失败：~p~n", [Error]),
            #{}
    end.

%% @doc
%% 根据登陆名获取显示
%% @end
getusername(LoginName) ->
    try
        case eadm_mnesia_api:find_by_field(eadm_user, loginname, LoginName) of
            [#eadm_user{username = UserName} | _] ->
                UserName;
            [] ->
                <<"未知用户"/utf8>>
        end
    catch
        _:Error ->
            lager:error("登录名称获取失败：~p~n", [Error]),
            <<"未知用户"/utf8>>
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
