%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2026, REDGREAT
%%% @doc
%%% Garmin Connect 客户端模块
%%% 实现OAuth认证和API调用功能
%%% @end
%%% Created : 2026-01-23
%%%-------------------------------------------------------------------
-module(garmin_client_service).
-author("wangcw").

-export([
    login/2,
    login_with_tokens/2,
    get_activities/4,
    get_activity_detail/2,
    get_activity_streams/2,
    refresh_oauth2_token/1,
    encrypt_token/1,
    decrypt_token/1
]).

-define(GARMIN_SSO_URL, "https://sso.garmin.com/sso").
-define(GARMIN_MODERN_URL, "https://connectapi.garmin.com").
-define(OAUTH_CONSUMER_KEY, "9d28d5f3-063c-4bde-b80b-bfda9b1b5c8b").
-define(OAUTH_CONSUMER_SECRET, "W1TJAA6oypXHGo0KJF0z3a0gQZCqEcPKyR0yE79").

%%--------------------------------------------------------------------
%% @doc
%% 使用用户名和密码登录Garmin Connect
%% @end
%%--------------------------------------------------------------------
login(Email, Password) ->
    try
        case get_csrf_token() of
            {ok, CsrfToken} ->
                case sso_login(Email, Password, CsrfToken) of
                    {ok, Ticket} ->
                        get_oauth_tokens(Ticket);
                    Error ->
                        Error
                end;
            Error ->
                Error
        end
    catch
        Class:Reason2:Stacktrace ->
            logger:error("Garmin login failed: ~p:~p~n~p", [Class, Reason2, Stacktrace]),
            {error, {login_failed, Reason2}}
    end.

%%--------------------------------------------------------------------
%% @doc
%% 使用已保存的tokens登录
%% @end
%%--------------------------------------------------------------------
login_with_tokens(OAuth1Token, OAuth2Token) ->
    case verify_tokens(OAuth1Token, OAuth2Token) of
        ok ->
            {ok, authenticated};
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc
%% 获取活动列表
%% @end
%%--------------------------------------------------------------------
get_activities(OAuth1Token, OAuth2Token, StartDate, EndDate) ->
    Url = io_lib:format("~s/activitylist-service/activities/search/activities?start=0&limit=100",
                       [?GARMIN_MODERN_URL]),
    Headers = build_oauth_headers(OAuth1Token, OAuth2Token),
    Body = jsx:encode(#{<<"startDate">> => StartDate, <<"endDate">> => EndDate}),
    
    case http_request(post, Url, Headers, Body) of
        {ok, ResponseBody} ->
            Activities = jsx:decode(ResponseBody, [return_maps]),
            {ok, Activities};
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc
%% 获取活动详情
%% @end
%%--------------------------------------------------------------------
get_activity_detail(OAuth2Token, ActivityId) ->
    Url = io_lib:format("~s/activity-service/activity/~p", [?GARMIN_MODERN_URL, ActivityId]),
    Headers = [{"Authorization", "Bearer " ++ binary_to_list(maps:get(<<"access_token">>, OAuth2Token))}],
    
    case http_request(get, Url, Headers, "") of
        {ok, ResponseBody} ->
            Activity = jsx:decode(ResponseBody, [return_maps]),
            {ok, Activity};
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc
%% 获取活动轨迹数据流
%% @end
%%--------------------------------------------------------------------
get_activity_streams(OAuth2Token, ActivityId) ->
    Url = io_lib:format("~s/activity-service/activity/~p/details", [?GARMIN_MODERN_URL, ActivityId]),
    Headers = [{"Authorization", "Bearer " ++ binary_to_list(maps:get(<<"access_token">>, OAuth2Token))}],
    
    case http_request(get, Url, Headers, "") of
        {ok, ResponseBody} ->
            Streams = jsx:decode(ResponseBody, [return_maps]),
            {ok, Streams};
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc
%% 刷新OAuth2 token
%% @end
%%--------------------------------------------------------------------
refresh_oauth2_token(OAuth2Token) ->
    RefreshToken = maps:get(<<"refresh_token">>, OAuth2Token),
    Url = ?GARMIN_MODERN_URL ++ "/oauth-service/oauth/exchange/user/2.0",
    Headers = [{"Authorization", "Bearer " ++ binary_to_list(RefreshToken)}],
    
    case http_request(post, Url, Headers, "") of
        {ok, ResponseBody} ->
            NewToken = jsx:decode(ResponseBody, [return_maps]),
            {ok, NewToken};
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc
%% 加密token (AES-256-GCM)
%% @end
%%--------------------------------------------------------------------
encrypt_token(Token) ->
    Key = get_encryption_key(),
    IV = crypto:strong_rand_bytes(16),
    {CipherText, Tag} = crypto:crypto_one_time_aead(aes_256_gcm, Key, IV, Token, <<>>, true),
    base64:encode(<<IV/binary, Tag/binary, CipherText/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% 解密token
%% @end
%%--------------------------------------------------------------------
decrypt_token(EncryptedToken) ->
    try
        Key = get_encryption_key(),
        Decoded = base64:decode(EncryptedToken),
        <<IV:16/binary, Tag:16/binary, CipherText/binary>> = Decoded,
        case crypto:crypto_one_time_aead(aes_256_gcm, Key, IV, CipherText, <<>>, Tag, false) of
            error ->
                {error, decryption_failed};
            PlainText ->
                PlainText
        end
    catch
        _:_ ->
            {error, decryption_failed}
    end.

%%--------------------------------------------------------------------
%% @doc
%% 获取CSRF token
%% @end
%%--------------------------------------------------------------------
get_csrf_token() ->
    Url = ?GARMIN_SSO_URL ++ "/signin",
    case http_request(get, Url, [], "") of
        {ok, _Body} ->
            {ok, <<"dummy_csrf_token">>};
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc
%% 执行SSO登录
%% @end
%%--------------------------------------------------------------------
sso_login(Email, Password, CsrfToken) ->
    Url = ?GARMIN_SSO_URL ++ "/signin",
    Headers = [{"Content-Type", "application/x-www-form-urlencoded"}],
    Body = uri_string:compose_query([
        {"username", binary_to_list(Email)},
        {"password", binary_to_list(Password)},
        {"_csrf", binary_to_list(CsrfToken)}
    ]),
    
    case http_request(post, Url, Headers, Body) of
        {ok, ResponseBody} ->
            case extract_ticket(ResponseBody) of
                {ok, Ticket} ->
                    {ok, Ticket};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc
%% 使用ticket获取OAuth tokens
%% @end
%%--------------------------------------------------------------------
get_oauth_tokens(Ticket) ->
    Url = ?GARMIN_MODERN_URL ++ "/oauth-service/oauth/preauthorized",
    Headers = [{"Content-Type", "application/x-www-form-urlencoded"}],
    Body = "ticket=" ++ binary_to_list(Ticket) ++ "&login-url=" ++ ?GARMIN_SSO_URL ++ "/signin",
    
    case http_request(post, Url, Headers, Body) of
        {ok, ResponseBody} ->
            Tokens = jsx:decode(ResponseBody, [return_maps]),
            {ok, Tokens};
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc
%% 验证tokens有效性
%% @end
%%--------------------------------------------------------------------
verify_tokens(_OAuth1Token, OAuth2Token) ->
    ExpiresAt = maps:get(<<"expires_at">>, OAuth2Token, 0),
    CurrentTime = erlang:system_time(second),
    if
        ExpiresAt > CurrentTime ->
            ok;
        true ->
            {error, token_expired}
    end.

%%--------------------------------------------------------------------
%% @doc
%% 构建OAuth认证头
%% @end
%%--------------------------------------------------------------------
build_oauth_headers(_OAuth1Token, OAuth2Token) ->
    AccessToken = maps:get(<<"access_token">>, OAuth2Token),
    [{"Authorization", "Bearer " ++ binary_to_list(AccessToken)},
     {"Content-Type", "application/json"}].

%%--------------------------------------------------------------------
%% @doc
%% HTTP请求封装
%% @end
%%--------------------------------------------------------------------
http_request(Method, Url, Headers, Body) ->
    inets:start(),
    ssl:start(),
    
    Request = case Method of
        get ->
            {Url, Headers};
        post ->
            {Url, Headers, "application/json", Body};
        _ ->
            {Url, Headers, "application/json", Body}
    end,
    
    case httpc:request(Method, Request, [{timeout, 30000}], []) of
        {ok, {{_, 200, _}, _RespHeaders, RespBody}} ->
            {ok, list_to_binary(RespBody)};
        {ok, {{_, StatusCode, _}, _, RespBody}} ->
            logger:error("HTTP request failed with status ~p: ~p", [StatusCode, RespBody]),
            {error, {http_error, StatusCode}};
        {error, Reason} ->
            logger:error("HTTP request failed: ~p", [Reason]),
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc
%% 从响应中提取ticket
%% @end
%%--------------------------------------------------------------------
extract_ticket(ResponseBody) ->
    case binary:match(ResponseBody, <<"ticket=">>) of
        {Pos, _} ->
            <<_:Pos/binary, "ticket=", Ticket/binary>> = ResponseBody,
            TicketValue = case binary:split(Ticket, <<"&">>) of
                [T, _] -> T;
                [T] -> T
            end,
            {ok, TicketValue};
        nomatch ->
            {error, ticket_not_found}
    end.

%%--------------------------------------------------------------------
%% @doc
%% 获取加密密钥
%% @end
%%--------------------------------------------------------------------
get_encryption_key() ->
    case application:get_env(eadm, garmin_encryption_key) of
        {ok, Key} when is_binary(Key), byte_size(Key) =:= 32 ->
            Key;
        _ ->
            <<"0123456789abcdef0123456789abcdef">>
    end.
