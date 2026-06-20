%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%  User APIs for the new SolidJS frontend.
%%% @end
%%%-------------------------------------------------------------------
-module(eadm_api_user_controller).
-author("wangcw").

-export([list/1]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc
%% 获取用户列表。
%% @end
list(#{auth_data := #{<<"authed">> := true, <<"permission">> := #{<<"usermanage">> := true}}}) ->
    try
        Data = eadm_user_service:list(),
        eadm_api_response:nova_json(eadm_api_response:ok(Data))
    catch
        _:Error ->
            lager:error("API用户查询失败：~p~n", [Error]),
            eadm_api_response:nova_json(eadm_api_response:error(<<"internal_error">>, <<"用户查询失败">>))
    end;

list(#{auth_data := #{<<"authed">> := true, <<"permission">> := #{<<"usermanage">> := false}}}) ->
    eadm_api_response:nova_json(eadm_api_response:forbidden());

list(#{auth_data := #{<<"authed">> := false}}) ->
    eadm_api_response:nova_json(eadm_api_response:unauthorized());

list(_) ->
    eadm_api_response:nova_json(eadm_api_response:unauthorized()).
