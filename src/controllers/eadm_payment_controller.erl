%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%% 财务API控制器
%%% @end
%%% Created : 2024-07-01
%%%-------------------------------------------------------------------
-module(eadm_payment_controller).

-export([
    alipay/1,
    wechat/1,
    config/1
]).

-include_lib("kernel/include/logger.hrl").

%% @doc
%% 获取支付宝交易数据
%% @end
alipay(#{auth_data := #{<<"authed">> := true,
      <<"permission">> := #{<<"finance">> := #{<<"finlist">> := true}}},
      bindings := #{<<"startDate">> := StartDate, <<"endDate">> := EndDate}}) ->
    try
        case api_payment:fetch_alipay_transactions(StartDate, EndDate) of
            {ok, Transactions} ->
                % 处理交易数据并保存到数据库
                process_alipay_transactions(Transactions),
                {json, #{<<"success">> => true, <<"count">> => length(Transactions)}};
            {error, Reason} ->
                {json, #{<<"success">> => false, <<"message">> => Reason}}
        end
    catch
        Exception:Error ->
            ?LOG_ERROR("获取支付宝交易数据失败: ~p:~p", [Exception, Error]),
            {json, #{<<"success">> => false, <<"message">> => "获取支付宝交易数据失败"}}
    end;

alipay(#{auth_data := #{<<"permission">> := #{<<"finance">> := #{<<"finlist">> := false}}}}) ->
    {json, #{<<"success">> => false, <<"message">> => "API鉴权失败"}};

alipay(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 获取微信支付交易数据
%% @end
wechat(#{auth_data := #{<<"authed">> := true,
      <<"permission">> := #{<<"finance">> := #{<<"finlist">> := true}}},
      bindings := #{<<"startDate">> := StartDate, <<"endDate">> := EndDate}}) ->
    try
        case api_payment:fetch_wechat_transactions(StartDate, EndDate) of
            {ok, Transactions} ->
                % 处理交易数据并保存到数据库
                process_wechat_transactions(Transactions),
                {json, #{<<"success">> => true, <<"count">> => length(Transactions)}};
            {error, Reason} ->
                {json, #{<<"success">> => false, <<"message">> => Reason}}
        end
    catch
        Exception:Error ->
            ?LOG_ERROR("获取微信支付交易数据失败: ~p:~p", [Exception, Error]),
            {json, #{<<"success">> => false, <<"message">> => "获取微信支付交易数据失败"}}
    end;

wechat(#{auth_data := #{<<"permission">> := #{<<"finance">> := #{<<"finlist">> := false}}}}) ->
    {json, #{<<"success">> => false, <<"message">> => "API鉴权失败"}};

wechat(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%% @doc
%% 保存API配置
%% @end
config(#{auth_data := #{<<"authed">> := true,
      <<"permission">> := #{<<"finance">> := #{<<"finlist">> := true}}},
      json := #{<<"type">> := Type} = Config}) ->
    try
        % 根据不同的API类型保存配置
        case Type of
            <<"alipay">> ->
                save_alipay_config(Config),
                {json, #{<<"success">> => true}};
            <<"wechat">> ->
                save_wechat_config(Config),
                {json, #{<<"success">> => true}};
            _ ->
                {json, #{<<"success">> => false, <<"message">> => "不支持的API类型"}}
        end
    catch
        Exception:Error ->
            ?LOG_ERROR("保存API配置失败: ~p:~p", [Exception, Error]),
            {json, #{<<"success">> => false, <<"message">> => "保存API配置失败"}}
    end;

config(#{auth_data := #{<<"permission">> := #{<<"finance">> := #{<<"finlist">> := false}}}}) ->
    {json, #{<<"success">> => false, <<"message">> => "API鉴权失败"}};

config(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private
%% @doc
%% 处理支付宝交易数据并保存到数据库
%% @end
process_alipay_transactions(Transactions) ->
    % 遍历交易数据并保存到数据库
    lists:foreach(
        fun(Transaction) ->
            try
                % 转换交易数据为数据库格式
                Map = convert_alipay_transaction(Transaction),

                % 插入数据库
                eadm_pgpool:equery(pool_pg,
                    "insert into fn_paybilldetail(owner, sourcetype, tradetime, tradetype, counterparty, goodscomment,
                    inorout, amount, paymethod, paystatus, tradeorderno, counterorderno, billcomment)
                    values ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13);",
                    [maps:get(<<"Owner">>, Map, null),
                     maps:get(<<"Source">>, Map, null),
                     maps:get(<<"TradeTime">>, Map, null),
                     maps:get(<<"TradeType">>, Map, null),
                     maps:get(<<"CounterParty">>, Map, null),
                     maps:get(<<"GoodsComment">>, Map, null),
                     maps:get(<<"InOrOut">>, Map, null),
                     maps:get(<<"Amount">>, Map, null),
                     maps:get(<<"PayMethod">>, Map, null),
                     maps:get(<<"PayStatus">>, Map, null),
                     maps:get(<<"TradeOrderNo">>, Map, null),
                     maps:get(<<"CounterOrderNo">>, Map, null),
                     maps:get(<<"BillComment">>, Map, null)])
            catch
                E:R ->
                    ?LOG_ERROR("保存支付宝交易数据失败: ~p:~p", [E, R])
            end
        end,
        Transactions).

%% @private
%% @doc
%% 处理微信支付交易数据并保存到数据库
%% @end
process_wechat_transactions(Transactions) ->
    % 遍历交易数据并保存到数据库
    lists:foreach(
        fun(Transaction) ->
            try
                % 转换交易数据为数据库格式
                Map = convert_wechat_transaction(Transaction),

                % 插入数据库
                eadm_pgpool:equery(pool_pg,
                    "insert into fn_paybilldetail(owner, sourcetype, tradeorderno, counterorderno, tradetime,
                    paymethod, counterparty, goodscomment, amount, inorout, paystatus, billcomment)
                    values ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12);",
                    [maps:get(<<"Owner">>, Map, null),
                     maps:get(<<"Source">>, Map, null),
                     maps:get(<<"TradeOrderNo">>, Map, null),
                     maps:get(<<"CounterOrderNo">>, Map, null),
                     maps:get(<<"TradeTime">>, Map, null),
                     maps:get(<<"PayMethod">>, Map, null),
                     maps:get(<<"CounterParty">>, Map, null),
                     maps:get(<<"GoodsComment">>, Map, null),
                     maps:get(<<"Amount">>, Map, null),
                     maps:get(<<"InOrOut">>, Map, null),
                     maps:get(<<"PayStatus">>, Map, null),
                     maps:get(<<"BillComment">>, Map, null)])
            catch
                E:R ->
                    ?LOG_ERROR("保存微信支付交易数据失败: ~p:~p", [E, R])
            end
        end,
        Transactions).

%% @private
%% @doc
%% 转换支付宝交易数据为数据库格式
%% @end
convert_alipay_transaction(_Transaction) ->
    % TODO: 实现支付宝交易数据转换
    #{
        <<"Owner">> => "系统导入",
        <<"Source">> => 1,  % 支付宝
        <<"TradeTime">> => calendar:universal_time(),
        <<"TradeType">> => "支付宝交易",
        <<"CounterParty">> => "支付宝用户",
        <<"GoodsComment">> => "API导入",
        <<"InOrOut">> => "收入",
        <<"Amount">> => 0.0,
        <<"PayMethod">> => "支付宝",
        <<"PayStatus">> => "成功",
        <<"TradeOrderNo">> => "",
        <<"CounterOrderNo">> => "",
        <<"BillComment">> => "API自动导入"
    }.

%% @private
%% @doc
%% 转换微信支付交易数据为数据库格式
%% @end
convert_wechat_transaction(_Transaction) ->
    % TODO: 实现微信支付交易数据转换
    #{
        <<"Owner">> => "系统导入",
        <<"Source">> => 2,  % 微信
        <<"TradeOrderNo">> => "",
        <<"CounterOrderNo">> => "",
        <<"TradeTime">> => calendar:universal_time(),
        <<"PayMethod">> => "微信支付",
        <<"CounterParty">> => "微信用户",
        <<"GoodsComment">> => "API导入",
        <<"Amount">> => 0.0,
        <<"InOrOut">> => "收入",
        <<"PayStatus">> => "成功",
        <<"BillComment">> => "API自动导入"
    }.

%% @private
%% @doc
%% 保存支付宝API配置
%% @end
save_alipay_config(Config) ->
    % 获取配置参数
    AppId = maps:get(<<"appId">>, Config, ""),
    PrivateKey = maps:get(<<"privateKey">>, Config, ""),
    PublicKey = maps:get(<<"publicKey">>, Config, ""),

    % 保存配置到应用环境
    application:set_env(nova, alipay_app_id, AppId),
    application:set_env(nova, alipay_private_key, PrivateKey),
    application:set_env(nova, alipay_public_key, PublicKey).

%% @private
%% @doc
%% 保存微信支付API配置
%% @end
save_wechat_config(Config) ->
    % 获取配置参数
    AppId = maps:get(<<"appId">>, Config, ""),
    MchId = maps:get(<<"mchId">>, Config, ""),
    ApiKey = maps:get(<<"apiKey">>, Config, ""),
    ApiV3Key = maps:get(<<"apiV3Key">>, Config, ""),
    SerialNo = maps:get(<<"serialNo">>, Config, ""),
    PrivateKey = maps:get(<<"privateKey">>, Config, ""),

    % 保存配置到应用环境
    application:set_env(nova, wechat_app_id, AppId),
    application:set_env(nova, wechat_mch_id, MchId),
    application:set_env(nova, wechat_api_key, ApiKey),
    application:set_env(nova, wechat_api_v3_key, ApiV3Key),
    application:set_env(nova, wechat_serial_no, SerialNo),
    application:set_env(nova, wechat_private_key, PrivateKey).
