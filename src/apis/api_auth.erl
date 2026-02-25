%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2025, REDGREAT
%%% @doc
%%%
%%% 移动端认证 API
%%%
%%% @end
%%% Created : 2025-02-24 16:00:00
%%%-------------------------------------------------------------------
-module(api_auth).
-author("wangcw").

%%%===================================================================
%%% 函数导出
%%%===================================================================
-export([login/1, userinfo/1, logout/1]).
-export([verify_token_from_req/1]).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc
%% 移动端登录，返回 Token
%% @end
login(#{params := Params}) ->
    try
        LoginName = maps:get(<<"loginName">>, Params, <<>>),
        Password = maps:get(<<"password">>, Params, <<>>),
        case eadm_utils:validate_login(LoginName, Password) of
            true ->
                UserName = get_username(LoginName),
                Permission = get_permission(LoginName),
                Token = generate_token(LoginName),
                lager:info("API Login Success: ~ts", [LoginName]),
                {json, #{
                    <<"code">> => 200,
                    <<"data">> => #{
                        <<"token">> => Token,
                        <<"username">> => UserName,
                        <<"loginname">> => LoginName,
                        <<"permission">> => Permission
                    }
                }};
            2 ->
                {json, #{
                    <<"code">> => 401,
                    <<"message">> => unicode:characters_to_binary("用户不存在", utf8)
                }};
            3 ->
                {json, #{
                    <<"code">> => 403,
                    <<"message">> => unicode:characters_to_binary("用户已禁用", utf8)
                }};
            _ ->
                {json, #{
                    <<"code">> => 401,
                    <<"message">> => unicode:characters_to_binary("用户名或密码错误", utf8)
                }}
        end
    catch
        _:Error ->
            lager:error("API登录失败：~p", [Error]),
            {json, #{
                <<"code">> => 500,
                <<"message">> => unicode:characters_to_binary("登录失败", utf8)
            }}
    end;
login(_Req) ->
    {json, #{
        <<"code">> => 400,
        <<"message">> => unicode:characters_to_binary("请求参数错误", utf8)
    }}.

%% @doc
%% 获取用户信息（需 Token 认证）
%% @end
userinfo(Req) ->
    case verify_token_from_req(Req) of
        {ok, LoginName} ->
            try
                {ok, _, ResData} = eadm_pgpool:equery(
                    pool_pg,
                    "select loginname, username, email
                    from eadm_user
                    where loginname = $1
                        and userstatus = 0
                        and deleted is false
                    limit 1",
                    [LoginName]
                ),
                case ResData of
                    [{LN, UN, EM}] ->
                        {json, #{
                            <<"code">> => 200,
                            <<"data">> => #{
                                <<"loginname">> => LN,
                                <<"username">> => UN,
                                <<"email">> => EM
                            }
                        }};
                    _ ->
                        {json, #{
                            <<"code">> => 404,
                            <<"message">> => unicode:characters_to_binary("用户不存在", utf8)
                        }}
                end
            catch
                _:Error ->
                    lager:error("API用户信息查询失败：~p", [Error]),
                    {json, #{
                        <<"code">> => 500,
                        <<"message">> => unicode:characters_to_binary("查询失败", utf8)
                    }}
            end;
        {error, Reason} ->
            {json, #{
                <<"code">> => 401,
                <<"message">> => unicode:characters_to_binary(Reason, utf8)
            }}
    end.

%% @doc
%% 退出登录
%% @end
logout(Req) ->
    case verify_token_from_req(Req) of
        {ok, LoginName} ->
            lager:info("API Logout: ~ts", [LoginName]),
            {json, #{<<"code">> => 200, <<"message">> => <<"ok">>}};
        {error, _} ->
            {json, #{<<"code">> => 200, <<"message">> => <<"ok">>}}
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @doc
%% 生成 Token
%% Token 格式: Base64(LoginName).Timestamp.HMAC
%% @end
generate_token(LoginName) ->
    SecretKey = get_secret_key(),
    Timestamp = integer_to_binary(erlang:system_time(seconds)),
    Payload = base64:encode(LoginName),
    Data = <<Payload/binary, ".", Timestamp/binary>>,
    Hmac = crypto:mac(hmac, sha256, SecretKey, Data),
    HmacHex = binary_to_hex(Hmac),
    <<Data/binary, ".", HmacHex/binary>>.

%% @doc
%% 验证 Token
%% @end
verify_token(Token) ->
    try
        case binary:split(Token, <<".">>, [global]) of
            [Payload, Timestamp, HmacHex] ->
                SecretKey = get_secret_key(),
                Data = <<Payload/binary, ".", Timestamp/binary>>,
                ExpectedHmac = crypto:mac(hmac, sha256, SecretKey, Data),
                ExpectedHex = binary_to_hex(ExpectedHmac),
                case ExpectedHex =:= HmacHex of
                    true ->
                        Ts = binary_to_integer(Timestamp),
                        Now = erlang:system_time(seconds),
                        TokenExpire = application:get_env(nova, api_token_expire, 86400 * 7),
                        case (Now - Ts) < TokenExpire of
                            true ->
                                LoginName = base64:decode(Payload),
                                {ok, LoginName};
                            false ->
                                {error, "Token已过期"}
                        end;
                    false ->
                        {error, "Token无效"}
                end;
            _ ->
                {error, "Token格式错误"}
        end
    catch
        _:_ ->
            {error, "Token验证失败"}
    end.

%% @doc
%% 从请求头中提取并验证 Token
%% @end
verify_token_from_req(Req) ->
    case cowboy_req:header(<<"authorization">>, Req, <<>>) of
        <<"Bearer ", Token/binary>> ->
            verify_token(Token);
        _ ->
            case maps:get(<<"authorization">>, maps:get(params, Req, #{}), <<>>) of
                <<>> ->
                    {error, "缺少认证信息"};
                Token ->
                    verify_token(Token)
            end
    end.

%% @doc
%% 获取密钥
%% @end
get_secret_key() ->
    case application:get_env(nova, secret_key, <<>>) of
        SKey when is_binary(SKey) -> SKey;
        _ -> <<>>
    end.

%% @doc
%% 获取用户名
%% @end
get_username(LoginName) ->
    try
        {ok, _, ResData} = eadm_pgpool:equery(
            pool_pg,
            "select username from eadm_user where loginname = $1 limit 1;",
            [LoginName]
        ),
        case ResData of
            [{Name}] -> Name;
            _ -> LoginName
        end
    catch
        _:_ -> LoginName
    end.

%% @doc
%% 获取用户权限
%% @end
get_permission(LoginName) ->
    try
        {ok, _, ResData} = eadm_pgpool:equery(
            pool_pg,
            "select rolepermission from vi_userpermission where loginname = $1 limit 1;",
            [LoginName]
        ),
        eadm_utils:pg_as_jsondata(ResData)
    catch
        _:_ -> #{}
    end.

%% @doc
%% 二进制转十六进制字符串
%% @end
binary_to_hex(Bin) ->
    list_to_binary(
        lists:flatten(
            [io_lib:format("~2.16.0b", [B]) || <<B:8>> <= Bin]
        )
    ).
