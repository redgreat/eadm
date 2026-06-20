%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%  Authentication service shared by Nova controllers and future Cowboy handlers.
%%% @end
%%%-------------------------------------------------------------------
-module(eadm_auth_service).
-author("wangcw").

-export([authenticate/2, current_user/1]).

%%====================================================================
%% API functions
%%====================================================================

authenticate(LoginName, Password) ->
    try
        case eadm_utils:validate_login(LoginName, Password) of
            true ->
                UserName = get_username(LoginName),
                Permission = get_permission(LoginName),
                {ok, #{
                    <<"authed">> => true,
                    <<"loginName">> => LoginName,
                    <<"userName">> => UserName,
                    <<"permission">> => Permission
                }};
            2 ->
                {error, not_found, utf8("用户不存在，请联系管理员")};
            3 ->
                {error, forbidden, utf8("用户已禁用，请联系管理员")};
            _ ->
                {error, invalid_credentials, utf8("用户名或密码错误")}
        end
    catch
        _:Error ->
            lager:error("认证失败：~p~n", [Error]),
            {error, internal_error, utf8("用户登录失败")}
    end.

current_user(#{<<"authed">> := true} = AuthData) ->
    {ok, #{
        <<"authed">> => true,
        <<"loginName">> => maps:get(<<"loginname">>, AuthData, <<>>),
        <<"userName">> => maps:get(<<"username">>, AuthData, <<>>),
        <<"permission">> => maps:get(<<"permission">>, AuthData, #{})
    }};
current_user(_) ->
    {error, unauthorized, <<"请先登录">>}.

%%====================================================================
%% Internal functions
%%====================================================================

get_permission(LoginName) ->
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
            #{}
    end.

get_username(LoginName) ->
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
            LoginName
    end.

utf8(Text) ->
    unicode:characters_to_binary(Text, utf8).
