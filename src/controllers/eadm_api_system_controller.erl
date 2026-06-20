%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%  System APIs for the new SolidJS frontend.
%%% @end
%%%-------------------------------------------------------------------
-module(eadm_api_system_controller).
-author("wangcw").

-export([info/1]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc
%% 获取 Erlang VM 系统信息。
%% @end
info(#{auth_data := #{<<"authed">> := true}}) ->
    eadm_api_response:nova_json(eadm_api_response:ok(#{<<"items">> => eadm_system_service:info()}));

info(#{auth_data := #{<<"authed">> := false}}) ->
    eadm_api_response:nova_json(eadm_api_response:unauthorized());

info(_) ->
    eadm_api_response:nova_json(eadm_api_response:unauthorized()).
