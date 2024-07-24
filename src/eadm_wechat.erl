%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%
%%% 微信消息推送模块
%%%
%%% @end
%%% Created : 2024-07-01 下午2:38
%%%-------------------------------------------------------------------
-module(eadm_wechat).
-author("wangcw").

%%%===================================================================
%%% 函数导出
%%%===================================================================
-export([send_msg/1]).

-define(WX_KEY, application:get_env(nova, wx_key, "ea50a2d0-2d51-4ba3-a90c-66919a51ca01")).
-define(WX_URL, "https://qyapi.weixin.qq.com/cgi-bin/webhook/send?key=" ++ ?WX_KEY).
-define(WX_HEADERS, [{"Content-Type", "application/json"}]).
-define(WX_MENTIONS, []).

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
        JsonData = thoas:encode(#{msgtype => 'text',
            text => #{content => Content, mentioned_list => ?WX_MENTIONS}}),
        httpc:request(post, {?WX_URL, ?WX_HEADERS, "application/json", JsonData}, [], []),
        #{<<"success">> => true}
    catch
        Exception:Error ->
            lager:error("Message Send Failed: ~p:~p", [Exception, Error]),


            #{<<"success">> => false}
    end.

%%====================================================================
%% 内部函数
%%====================================================================

