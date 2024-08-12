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
-export([login/1, logout/1]).

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
        _E:Error ->
            lager:error("用户登录失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("用户登录失败！", utf8)}]}
    end.

%% @doc
%% 退出登录
%% @end
logout(Req) ->
    lager:info("User Logout!~n"),
    nova_session:delete(Req).

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
        _E:Error ->
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
        _E:Error ->
            lager:error("登录名称获取失败：~p~n", [Error]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("登录名称获取失败！", utf8)}]}
    end.
