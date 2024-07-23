%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%
%%% 手表 API
%%%
%%% @end
%%% Created : 2024-06-29 下午5:29
%%%-------------------------------------------------------------------
-module(api_watch).
-author("wangcw").

%%%===================================================================
%%% 函数导出
%%%===================================================================
-export([index/1]).

%%====================================================================
%% API 函数
%%====================================================================
%% @doc
%% 主函数
%% @end
% index(#{auth_data := #{<<"authed">> := true, <<"username">> := UserName,
%       <<"permission">> := #{<<"api">> := #{<<"watch">> := true}}}}) ->
%    {ok, [{username, UserName}]};
index(Req) ->
    lager:info("请求内容: ~p~n", [Req]),
    io:format("请求内容: ~p~n", [Req]).

