%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%  Native Cowboy auth endpoints used during migration.
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

handle(<<"POST">>, <<"/api/internal/auth/login">>, Req, State) ->
    login_request(Req, State);
handle(<<"POST">>, <<"/api/auth/login">>, Req, State) ->
    login_request(Req, State);
handle(<<"POST">>, <<"/api/internal/auth/logout">>, Req, State) ->
    Req1 = eadm_cowboy_session:clear_cookie(Req),
    Reply = eadm_api_response:ok(#{}, utf8("已退出登录")),
    {ok, eadm_api_response:cowboy_json(Req1, Reply), State};
handle(<<"POST">>, <<"/api/auth/logout">>, Req, State) ->
    Req1 = eadm_cowboy_session:clear_cookie(Req),
    Reply = eadm_api_response:ok(#{}, utf8("已退出登录")),
    {ok, eadm_api_response:cowboy_json(Req1, Reply), State};
handle(<<"GET">>, <<"/api/internal/auth/me">>, Req, State) ->
    me_request(Req, State);
handle(<<"GET">>, <<"/api/auth/me">>, Req, State) ->
    me_request(Req, State);
handle(_Method, _Path, Req, State) ->
    Reply = eadm_api_response:not_found(),
    {ok, eadm_api_response:cowboy_json(Req, 404, Reply), State}.

login_request(Req, State) ->
    case eadm_cowboy_req:json_body(Req) of
        {ok, Body, Req1} ->
            LoginName = maps:get(<<"loginName">>, Body, <<>>),
            Password = maps:get(<<"password">>, Body, <<>>),
            login(LoginName, Password, Req1, State);
        {error, invalid_json, Req1} ->
            Reply = eadm_api_response:validation_error(utf8("JSON格式错误")),
            {ok, eadm_api_response:cowboy_json(Req1, 400, Reply), State}
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
