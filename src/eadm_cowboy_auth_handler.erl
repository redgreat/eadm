%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%  Cowboy auth endpoints.
%%% @end
%%%-------------------------------------------------------------------
-module(eadm_cowboy_auth_handler).
-author("wangcw").

-export([init/2]).

%%====================================================================
%% Cowboy callbacks
%%====================================================================

init(Req, State) ->
    Path = cowboy_req:path(Req),
    Method = cowboy_req:method(Req),
    handle(Method, Path, Req, State).

%%====================================================================
%% Internal functions
%%====================================================================

handle(<<"POST">>, <<"/api/v1/auth/login">>, Req, State) ->
    login_request(Req, State);
handle(<<"POST">>, <<"/api/v1/auth/logout">>, Req, State) ->
    Req1 = eadm_cowboy_session:clear_cookie(Req),
    Reply = eadm_api_response:ok(#{}, utf8("已退出登录")),
    {ok, eadm_api_response:cowboy_json(Req1, Reply), State};
handle(<<"GET">>, <<"/api/v1/auth/me">>, Req, State) ->
    me_request(Req, State);
handle(_Method, _Path, Req, State) ->
    Reply = eadm_api_response:not_found(),
    {ok, eadm_api_response:cowboy_json(Req, 404, Reply), State}.

login_request(Req, State) ->
    case login_params(Req) of
        {ok, Body, Req1} ->
            LoginName = maps:get(<<"loginName">>, Body, <<>>),
            Password = maps:get(<<"password">>, Body, <<>>),
            login(LoginName, Password, Req1, State);
        {error, invalid_body, Req1} ->
            Reply = eadm_api_response:validation_error(utf8("请求参数格式错误")),
            {ok, eadm_api_response:cowboy_json(Req1, 400, Reply), State}
    end.

login_params(Req) ->
    ContentType = cowboy_req:header(<<"content-type">>, Req, <<>>),
    case binary:match(ContentType, <<"application/x-www-form-urlencoded">>) of
        nomatch ->
            json_login_params(Req);
        _ ->
            form_login_params(Req)
    end.

json_login_params(Req) ->
    case eadm_cowboy_req:json_body(Req) of
        {ok, Body, Req1} ->
            {ok, Body, Req1};
        {error, invalid_json, Req1} ->
            {error, invalid_body, Req1}
    end.

form_login_params(Req) ->
    try
        {ok, Params, Req1} = cowboy_req:read_urlencoded_body(Req),
        {ok, maps:from_list(Params), Req1}
    catch
        _:_ ->
            {error, invalid_body, Req}
    end.

me_request(Req, State) ->
    case eadm_cowboy_guard:current_user(Req) of
        {ok, Data} ->
            {ok, eadm_api_response:cowboy_json(Req, eadm_api_response:ok(Data)), State};
        {error, _Reason} ->
            {ok, eadm_api_response:cowboy_json(Req, 401, eadm_api_response:unauthorized()), State}
    end.

login(<<>>, _Password, Req, State) ->
    Reply = eadm_api_response:validation_error(utf8("请输入登录名")),
    {ok, eadm_api_response:cowboy_json(Req, 400, Reply), State};
login(_LoginName, <<>>, Req, State) ->
    Reply = eadm_api_response:validation_error(utf8("请输入密码")),
    {ok, eadm_api_response:cowboy_json(Req, 400, Reply), State};
login(LoginName, Password, Req, State) ->
    case eadm_auth_service:authenticate(LoginName, Password) of
        {ok, Data} ->
            Req1 = eadm_cowboy_session:set_cookie(Req, Data),
            Reply = eadm_api_response:ok(Data, utf8("登录成功")),
            {ok, eadm_api_response:cowboy_json(Req1, Reply), State};
        {error, Code, Message} ->
            Reply = eadm_api_response:error(Code, Message),
            {ok, eadm_api_response:cowboy_json(Req, 401, Reply), State}
    end.

utf8(Text) ->
    unicode:characters_to_binary(Text, utf8).

