%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%  Auth API for the new SolidJS frontend.
%%% @end
%%%-------------------------------------------------------------------
-module(eadm_api_auth_controller).
-author("wangcw").

-export([login/1, logout/1, me/1]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc
%% 新前端登录接口。
%% @end
login(#{params := Params} = Req) ->
    do_login(Req, Params);

login(#{json := Json} = Req) ->
    do_login(Req, Json);

login(Req) ->
    try
        {ok, _, #{params := Params} = Req1} = cowboy_req:read_body(Req),
        do_login(Req1, Params)
    catch
        _:Error ->
            lager:error("API登录请求解析失败：~p~n", [Error]),
            eadm_api_response:nova_json(eadm_api_response:validation_error(<<"登录参数错误">>))
    end.

%% @doc
%% 新前端登出接口。
%% @end
logout(Req) ->
    try
        nova_session:delete(Req)
    catch
        _:Error ->
            lager:error("API退出登录失败：~p~n", [Error])
    end,
    eadm_api_response:nova_json(eadm_api_response:ok(#{}, <<"已退出登录">>)).

%% @doc
%% 获取当前登录用户信息。
%% @end
me(#{auth_data := #{<<"authed">> := true} = AuthData}) ->
    {ok, Data} = eadm_auth_service:current_user(AuthData),
    eadm_api_response:nova_json(eadm_api_response:ok(Data));

me(#{auth_data := #{<<"authed">> := false}}) ->
    eadm_api_response:nova_json(eadm_api_response:unauthorized());

me(_) ->
    eadm_api_response:nova_json(eadm_api_response:unauthorized()).

%%====================================================================
%% Internal functions
%%====================================================================

do_login(Req, Params) ->
    LoginName = maps:get(<<"loginName">>, Params, <<>>),
    Password = maps:get(<<"password">>, Params, <<>>),
    case {LoginName, Password} of
        {<<>>, _} ->
            eadm_api_response:nova_json(eadm_api_response:validation_error(<<"请输入登录名">>));
        {_, <<>>} ->
            eadm_api_response:nova_json(eadm_api_response:validation_error(<<"请输入密码">>));
        _ ->
            authenticate(Req, LoginName, Password)
    end.

authenticate(Req, LoginName, Password) ->
    case eadm_auth_service:authenticate(LoginName, Password) of
        {ok, Data} ->
            NewExp = eadm_utils:get_exp_bin(),
            UserName = maps:get(<<"userName">>, Data),
            Permission = maps:get(<<"permission">>, Data),
            nova_session:set(Req, <<"loginname">>, LoginName),
            nova_session:set(Req, <<"username">>, UserName),
            nova_session:set(Req, <<"permission">>, Permission),
            nova_session:set(Req, <<"exp">>, NewExp),
            lager:info("API login success: ~ts, exp: ~p", [UserName, NewExp]),
            eadm_api_response:nova_json(eadm_api_response:ok(Data, <<"登录成功">>));
        {error, Code, Message} ->
            eadm_api_response:nova_json(eadm_api_response:error(Code, Message))
    end.
