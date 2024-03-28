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
                    lager:info("User: ~p, Login Success! New Exp: ~p", [Username, NewExp]),
                    {redirect, "/"};
                2 ->
                    lager:info("User Disable Failed!"),
                    {redirect, "/login?error=user_notfond"};
                3 ->
                    lager:info("User Disable Failed!"),
                    {redirect, "/login?error=user_disable"};
                _ ->
                    lager:info("User Login Failed!"),
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
validate_login(ParamsVal) ->
    {ok, _, DbPassE} = mysql_pool:query(pool_db,
        "SELECT CryptoGram, UserStatus
        FROM eadm_user
        WHERE LoginName = ?
          AND Deleted = 0
        ORDER BY UpdatedAt DESC
        LIMIT 1;",
        [maps:get(<<"username">>, ParamsVal)]),
        case DbPassE of
            [] ->
                2;
            _ ->
                case tl(hd(DbPassE)) of
                    [0] ->
                      ParamsPass = maps:get(<<"password">>, ParamsVal),
                      verify_password(ParamsPass, hd(hd(DbPassE)));
                    [1] ->
                      3;
                    _ ->
                      4
                end
        end.

%% @doc
%% 密码加密解密-验证密码
%% @end
verify_password(Pwd, DbPwd) ->
    Secret_Key = application:get_env(nova, secret_key, <<>>),
    HPwd = crypto:hash(sha256, <<Secret_Key/binary, Pwd/binary>>),
    DbPwdBin = base64:decode(DbPwd),
    HPwd =:= DbPwdBin.
