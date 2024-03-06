%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%
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
            case validate_login(Params) of
                true ->
                    Username = maps:get(<<"username">>, Params),
                    NewExp = eadm_utils:get_exp_bin(),
                    nova_session:set(Req, <<"username">>, Username),
                    nova_session:set(Req, <<"exp">>, NewExp),
                    lager:info("User: ~p, Login Success! New Exp: ~p~n", [Username, NewExp]),
                    {redirect, "/"};
                false ->
                    lager:info("User Login Failed!~n"),
                    {redirect, "/login?error=invalid_credentials"}
            end
    end.

%% @doc
%% 退出登录
%% @end
logout(Req) ->
    lager:info("User Session Deleted!~n"),
    nova_session:delete(Req).
    % {redirect, "/login"}.

%%===================================================================
%% 内部函数
%%===================================================================
validate_login(ParamsPwd) ->
    {ok, _, DbPassE} = mysql_pool:query(pool_db,
        "SELECT upw FROM eadm_user WHERE un = ? LIMIT 1;",
        [maps:get(<<"username">>, ParamsPwd)]),
        DbPassB = list_to_binary(DbPassE),
        PaPass = maps:get(<<"password">>, ParamsPwd),
        verify_password(PaPass, DbPassB).

%% @doc
%% 密码加密解密-验证密码
%% @end
verify_password(Pwd, DbPwd) ->
    Secret_Key = application:get_env(nova, secret_key, <<>>),
    HPwd = crypto:hash(sha256, <<Secret_Key/binary, Pwd/binary>>),
    DbPwdBin = base64:decode(DbPwd),
    HPwd =:= DbPwdBin.
