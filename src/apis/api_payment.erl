%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2025, REDGREAT
%%% @doc
%%% 支付宝和微信支付API集成模块
%%% @end
%%% Created : 2025-5-16
%%%-------------------------------------------------------------------
-module(api_payment).

-export([
    fetch_alipay_transactions/2,
    fetch_wechat_transactions/2,
    schedule_transaction_sync/0
]).

-include_lib("kernel/include/logger.hrl").

%%%===================================================================
%%% 宏定义
%%%===================================================================
-define(ALIPAY_API_URL, application:get_env(nova, alipay_api_url, "https://openapi.alipay.com/gateway.do")).
-define(ALIPAY_APP_ID, application:get_env(nova, alipay_app_id, "")).
-define(ALIPAY_PRIVATE_KEY, application:get_env(nova, alipay_private_key, "")).
-define(ALIPAY_PUBLIC_KEY, application:get_env(nova, alipay_public_key, "")).

-define(WECHAT_API_URL, application:get_env(nova, wechat_api_url, "https://api.mch.weixin.qq.com")).
-define(WECHAT_APP_ID, application:get_env(nova, wechat_app_id, "")).
-define(WECHAT_MCH_ID, application:get_env(nova, wechat_mch_id, "")).
-define(WECHAT_API_KEY, application:get_env(nova, wechat_api_key, "")).
-define(WECHAT_API_V3_KEY, application:get_env(nova, wechat_api_v3_key, "")).
-define(WECHAT_SERIAL_NO, application:get_env(nova, wechat_serial_no, "")).
-define(WECHAT_PRIVATE_KEY, application:get_env(nova, wechat_private_key, "")).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc
%% 获取支付宝交易记录
%% @end
fetch_alipay_transactions(StartDate, EndDate) ->
    case ?ALIPAY_APP_ID of
        "" ->
            {error, "支付宝API配置未设置"};
        _ ->
            try
                % 构建请求参数
                Params = #{
                    app_id => ?ALIPAY_APP_ID,
                    method => "alipay.trade.query",
                    charset => "utf-8",
                    sign_type => "RSA2",
                    timestamp => format_datetime(erlang:localtime()),
                    version => "1.0",
                    biz_content => thoas:encode(#{
                        start_time => StartDate,
                        end_time => EndDate
                    })
                },

                % 生成签名
                SignContent = build_sign_content(Params),
                Sign = sign_with_private_key(SignContent, ?ALIPAY_PRIVATE_KEY),

                % 添加签名到参数
                ParamsWithSign = Params#{sign => Sign},

                % 发送请求
                {ok, Response} = httpc:request(post, {
                    ?ALIPAY_API_URL,
                    [{"Content-Type", "application/x-www-form-urlencoded;charset=utf-8"}],
                    "application/x-www-form-urlencoded",
                    uri_string:compose_query(maps:to_list(ParamsWithSign))
                }, [], []),

                % 解析响应
                {_, _, Body} = Response,
                JsonResponse = thoas:decode(Body),

                % 处理响应数据
                case maps:get(<<"alipay_trade_query_response">>, JsonResponse, undefined) of
                    undefined ->
                        {error, "支付宝API返回格式错误"};
                    ApiResponse ->
                        case maps:get(<<"code">>, ApiResponse) of
                            <<"10000">> ->
                                {ok, ApiResponse};
                            _ ->
                                {error, maps:get(<<"msg">>, ApiResponse, "未知错误")}
                        end
                end
            catch
                Exception:Error ->
                    ?LOG_ERROR("获取支付宝交易记录失败: ~p:~p", [Exception, Error]),
                    {error, "获取支付宝交易记录失败"}
            end
    end.

%% @doc
%% 获取微信支付交易记录
%% @end
fetch_wechat_transactions(StartDate, _EndDate) ->
    case ?WECHAT_APP_ID of
        "" ->
            {error, "微信支付API配置未设置"};
        _ ->
            try
                % 构建请求URL
                Url = ?WECHAT_API_URL ++ "/v3/bill/tradebill",

                % 构建请求参数 (微信API只需要开始日期)
                QueryParams = [
                    {"bill_date", StartDate},
                    {"bill_type", "ALL"}
                ],
                FullUrl = Url ++ "?" ++ uri_string:compose_query(QueryParams),

                % 生成随机数和时间戳
                Nonce = generate_nonce(),
                Timestamp = integer_to_list(erlang:system_time(second)),

                % 构建签名字符串
                Method = "GET",
                ParsedUrl = uri_string:parse(FullUrl),
                Path = maps:get(path, ParsedUrl, ""),
                SignString = Method ++ "\n" ++
                             Path ++ "\n" ++
                             Timestamp ++ "\n" ++
                             Nonce ++ "\n\n",

                % 使用私钥签名
                Signature = sign_with_private_key(SignString, ?WECHAT_PRIVATE_KEY),

                % 构建认证头
                AuthHeader = "WECHATPAY2-SHA256-RSA2048 " ++
                             "mchid=\"" ++ ?WECHAT_MCH_ID ++ "\"," ++
                             "nonce_str=\"" ++ Nonce ++ "\"," ++
                             "signature=\"" ++ Signature ++ "\"," ++
                             "timestamp=\"" ++ Timestamp ++ "\"," ++
                             "serial_no=\"" ++ ?WECHAT_SERIAL_NO ++ "\"",

                % 发送请求
                {ok, Response} = httpc:request(get, {
                    FullUrl,
                    [{"Authorization", AuthHeader}, {"Accept", "application/json"}]
                }, [], []),

                % 解析响应
                {_, _, Body} = Response,
                JsonResponse = thoas:decode(Body),

                % 处理响应数据
                case maps:get(<<"download_url">>, JsonResponse, undefined) of
                    undefined ->
                        {error, maps:get(<<"message">>, JsonResponse, "未知错误")};
                    DownloadUrl ->
                        % 下载账单文件
                        {ok, download_bill_file(binary_to_list(DownloadUrl), AuthHeader)}
                end
            catch
                Exception:Error ->
                    ?LOG_ERROR("获取微信支付交易记录失败: ~p:~p", [Exception, Error]),
                    {error, "获取微信支付交易记录失败"}
            end
    end.

%% @doc
%% 定时同步交易数据
%% @end
schedule_transaction_sync() ->
    % 获取当前日期
    {{Year, Month, Day}, _} = erlang:localtime(),
    StartDate = lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0w", [Year, Month, Day - 1])),
    EndDate = lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0w", [Year, Month, Day])),

    % 同步支付宝交易
    case fetch_alipay_transactions(StartDate, EndDate) of
        {ok, AlipayData} ->
            process_alipay_transactions(AlipayData);
        {error, AlipayError} ->
            ?LOG_ERROR("同步支付宝交易失败: ~p", [AlipayError])
    end,

    % 同步微信支付交易
    case fetch_wechat_transactions(StartDate, EndDate) of
        {ok, WechatData} ->
            process_wechat_transactions(WechatData);
        {error, WechatError} ->
            ?LOG_ERROR("同步微信支付交易失败: ~p", [WechatError])
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private
%% @doc
%% 处理支付宝交易数据并保存到数据库
%% @end
process_alipay_transactions(_Data) ->
    % TODO: 实现支付宝交易数据处理和保存
    ?LOG_INFO("处理支付宝交易数据"),
    ok.

%% @private
%% @doc
%% 处理微信支付交易数据并保存到数据库
%% @end
process_wechat_transactions(_Data) ->
    % TODO: 实现微信支付交易数据处理和保存
    ?LOG_INFO("处理微信支付交易数据"),
    ok.

%% @private
%% @doc
%% 下载账单文件
%% @end
download_bill_file(_Url, _AuthHeader) ->
    % TODO: 实现下载账单文件
    ?LOG_INFO("下载账单文件"),
    {}.

%% @private
%% @doc
%% 生成随机字符串
%% @end
generate_nonce() ->
    % 生成32位随机字符串
    binary_to_list(base64:encode(crypto:strong_rand_bytes(24))).

%% @private
%% @doc
%% 构建签名内容
%% @end
build_sign_content(_Params) ->
    % TODO: 实现签名内容构建
    ?LOG_INFO("构建签名内容"),
    "".

%% @private
%% @doc
%% 使用私钥签名
%% @end
sign_with_private_key(_Content, _PrivateKey) ->
    % TODO: 实现私钥签名
    ?LOG_INFO("使用私钥签名"),
    "".

%% @private
%% @doc
%% 格式化日期时间
%% @end
format_datetime({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
                               [Year, Month, Day, Hour, Minute, Second])).
