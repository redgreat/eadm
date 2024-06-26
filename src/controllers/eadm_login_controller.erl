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

%% Application callbacks
-export([login/1, logout/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%% @doc
%%
%% @end
login(Req) ->
    Method = cowboy_req:method(Req),
    case Method of
        <<"GET">> ->
            {ok, []};
        <<"POST">> ->
            {ok, _, #{params := Params}} = cowboy_req:read_body(Req),
            LoginName = maps:get(<<"loginName">>, Params),
            Password = maps:get(<<"password">>, Params),
            lager:info("InputLoginName:~p~n", [LoginName]),
            lager:info("InputPassword:~p~n", [Password]),
            case eadm_utils:validate_login(LoginName, Password) of
                true ->
                    UserName = get_username(LoginName),
                    Permission = get_permission(LoginName),
                    NewExp = eadm_utils:get_exp_bin(),
                    nova_session:set(Req, <<"loginname">>, LoginName),
                    nova_session:set(Req, <<"username">>, UserName),
                    nova_session:set(Req, <<"permission">>, Permission),
                    nova_session:set(Req, <<"exp">>, NewExp),
                    lager:info("User: ~ts, Login Success! New Exp: ~p", [UserName, NewExp]),
                    A = unicode:characters_to_binary("欢迎【"),
                    B = unicode:characters_to_binary("】登录! "),
                    Info = #{<<"Alert">> => <<A/binary, UserName/binary, B/binary>>,
                        <<"logined">> => 1},
                    {json, [Info]};
                2 ->
                    lager:info("User Not Fond!"),
                    Alert = #{<<"Alert">> => unicode:characters_to_binary("用户不存在，请联系管理员！"),
                        <<"logined">> => 0},
                    {json, [Alert]};
                3 ->
                    lager:info("User Disable!"),
                    Alert = #{<<"Alert">> => unicode:characters_to_binary("用户已禁用，请联系管理员！"),
                        <<"logined">> => 0},
                    {json, [Alert]};
                _ ->
                    lager:info("User Login Failed!"),
                    Alert = #{<<"Alert">> => unicode:characters_to_binary("用户名或密码错误，请重新登录！"),
                        <<"logined">> => 0},
                    {json, [Alert]}
            end
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
get_permission(LoginName) ->
    lager:info("GetPermissionLoginName:~p~n", [LoginName]),
    {ok, _, Res_Data} = eadm_pgpool:equery(pool_pg,
        "select rolepermission
        from vi_userpermission
        where loginname=?
        limit 1;", [LoginName]),
    lager:info("GetPermissionRes_Data:~p~n", [Res_Data]),
    {ok, Response} = thoas:decode(list_to_binary(Res_Data)),
    Response.

%% @doc
%% 根据登陆名获取显示
%% @end
get_username(LoginName) ->
    lager:info("GetUserNameLoginName:~p~n", [LoginName]),
    {ok, _, Res_Data} = eadm_pgpool:equery(pool_pg,
        "select username
        from eadm_user
        where loginname=?
        limit 1;", [LoginName]),
    lager:info("GetUserNameRes_Data:~p~n", [Res_Data]),
    {Return_Data} = hd(Res_Data),
    Return_Data.

