%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2025, REDGREAT
%%% @doc
%%%
%%% 推送加消息推送模块
%%%
%%% @end
%%% Created : 2025-1-16 09:46:17
%%%-------------------------------------------------------------------
-module(eadm_push).
-author("wangcw").

%%%===================================================================
%%% 函数导出
%%%===================================================================
-export([send_msg/1]).

-define(PUSH_TOKEN, application:get_env(nova, push_token, "f9f695f545524ebd89927ddfbce5d9b1")).
-define(PUSH_URL, "http://www.pushplus.plus/send/").
-define(PUSH_HEADERS, [{"Content-Type", "application/json"}]).

%%====================================================================
%% API 函数
%%====================================================================
%% @doc
%% 消息发送
%% 中文发送方法：
%% Content = unicode:characters_to_binary("测试消息！").
%% eadm_wechat:send_msg(Content).
%% @end
send_msg(Content) ->
    try
        JsonData = thoas:encode(#{token => ?PUSH_TOKEN,
            title => unicode:characters_to_binary("手表提醒消息"),
            content => Content}),
        httpc:request(post, {?PUSH_URL, ?PUSH_HEADERS, "application/json", JsonData}, [], []),
        #{<<"success">> => true}
    catch
        Exception:Error ->
            lager:error("Message Send Failed: ~p:~p", [Exception, Error]),
            #{<<"success">> => false}
    end.

%%====================================================================
%% 内部函数
%%====================================================================

