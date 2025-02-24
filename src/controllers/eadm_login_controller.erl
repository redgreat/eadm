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
                        {json, [#{<<"Alert">> => <<A/binary, UserName/binary, B/binary>>,
                            <<"logined">> => 1}]};
                    2 ->
                        lager:info("User Not Fond!"),
                        {json, [#{<<"Alert">> => unicode:characters_to_binary("用户不存在，请联系管理员！", utf8),
                            <<"logined">> => 0}]};
                    3 ->
                        lager:info("User Disable!"),
                        {json, [#{<<"Alert">> => unicode:characters_to_binary("用户已禁用，请联系管理员！", utf8),
                            <<"logined">> => 0}]};
                    _ ->
                        lager:info("User Login Failed!"),
                        {json, [#{<<"Alert">> => unicode:characters_to_binary("用户名或密码错误，请重新登录！", utf8),
                            <<"logined">> => 0}]}
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
        {ok, _, ResData} = eadm_pgpool:equery(pool_pg,
            "select loginname, username, email
            from eadm_user
            where loginname = $1
                and userstatus = 0
                and deleted is false
            limit 1", [LoginName]),
        ResList = eadm_utils:pg_as_list(ResData),
        {json, ResList}
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
useredit(#{auth_data := #{<<"authed">> := true, <<"loginname">> := CreatedUser},
    params := #{<<"loginName">> := LoginName, <<"email">> := Email, <<"userName">> := NewUserName}}) ->
    case re:run(Email, "^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+$") of
        {match, _} ->
            try
                eadm_pgpool:equery(pool_pg,
                    "update eadm_user
                    set loginname = $1,
                    username = $2,
                    email = $3,
                    updateduser = $4
                    where loginname = $5;",
                    [LoginName, NewUserName, Email, CreatedUser, CreatedUser]),
                A = unicode:characters_to_binary("用户【", utf8),
                B = unicode:characters_to_binary("】编辑成功！", utf8),
                {json, [#{<<"Alert">> => <<A/binary, NewUserName/binary, B/binary>>}]}
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
userpwd(#{auth_data := #{<<"authed">> := true, <<"loginname">> := LoginName},
    params := #{<<"passwordOld">> := PasswordOld, <<"passwordNew">> := PasswordNew}}) ->
    case validate_password(PasswordNew) of
        {ok} ->
            case eadm_utils:validate_login(LoginName, PasswordOld) of
                true ->
                    CryptoGram = eadm_utils:pass_encrypt(PasswordNew),
                    try
                        eadm_pgpool:equery(pool_pg,
                            "update eadm_user
                            set updateduser = $1,
                            updatedat = current_timestamp,
                            passwd = $2
                            where loginname = $3
                                and userstatus = 0
                                and deleted is false;",
                            [LoginName, CryptoGram, LoginName]),
                        {json, [#{<<"Alert">> => unicode:characters_to_binary("密码修改成功！", utf8)}]}
                    catch
                        _:Error ->
                            lager:error("用户密码修改失败：~p~n", [Error]),
                            {json, [#{<<"Alert">> => unicode:characters_to_binary("用户密码修改失败！", utf8)}]}
                    end;
                2 ->
                    lager:info("User Not Fond!"),
                    {json, [#{<<"Alert">> => unicode:characters_to_binary("用户不存在，请联系管理员！", utf8),
                        <<"logined">> => 0}]};
                3 ->
                    lager:info("User Disable!"),
                    {json, [#{<<"Alert">> => unicode:characters_to_binary("用户已禁用，请联系管理员！", utf8),
                        <<"logined">> => 0}]};
                _ ->
                    lager:info("User Login Failed!"),
                    {json, [#{<<"Alert">> => unicode:characters_to_binary("用户名或密码错误，请重新登录！", utf8),
                        <<"logined">> => 0}]}
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
        {ok, _, ResData} = eadm_pgpool:equery(pool_pg,
            "select rolepermission
            from vi_userpermission
            where loginname = $1
            limit 1;", [LoginName]),
        eadm_utils:pg_as_jsondata(ResData)
    catch
        _:Error ->
            lager:error("用户权限获取失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("用户权限获取失败！", utf8)}]}
    end.

%% @doc
%% 根据登陆名获取显示
%% @end
getusername(LoginName) ->
    try
        {ok, _, ResData} = eadm_pgpool:equery(pool_pg,
            "select username
            from eadm_user
            where loginname = $1
            limit 1;", [LoginName]),
        eadm_utils:pg_as_jsonmap(ResData)
    catch
        _:Error ->
            lager:error("登录名称获取失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("登录名称获取失败！", utf8)}]}
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
