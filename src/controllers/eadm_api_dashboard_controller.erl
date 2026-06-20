%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%  Dashboard API for the new SolidJS frontend.
%%% @end
%%%-------------------------------------------------------------------
-module(eadm_api_dashboard_controller).
-author("wangcw").

-export([summary/1]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc
%% 获取首页汇总数据。
%% @end
summary(#{auth_data := #{<<"authed">> := true, <<"loginname">> := LoginName}}) ->
    try
        Data = eadm_dashboard_service:summary(LoginName),
        eadm_api_response:nova_json(eadm_api_response:ok(Data))
    catch
        _:Error ->
            lager:error("API首页信息查询失败：~p~n", [Error]),
            eadm_api_response:nova_json(eadm_api_response:error(<<"internal_error">>, <<"首页信息查询失败">>))
    end;

summary(#{auth_data := #{<<"authed">> := false}}) ->
    eadm_api_response:nova_json(eadm_api_response:unauthorized());

summary(_) ->
    eadm_api_response:nova_json(eadm_api_response:unauthorized()).
