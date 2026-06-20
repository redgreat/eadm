%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%  Crontab APIs for the new SolidJS frontend.
%%% @end
%%%-------------------------------------------------------------------
-module(eadm_api_crontab_controller).
-author("wangcw").

-export([list/1]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc
%% 获取定时任务列表。
%% @end
list(#{auth_data := #{<<"authed">> := true, <<"permission">> := #{<<"crontab">> := true}}} = Req) ->
    Query = maps:get(parsed_qs, Req, #{}),
    CronName = maps:get(<<"cronName">>, Query, <<>>),
    try
        Data = eadm_crontab_service:list(CronName),
        eadm_api_response:nova_json(eadm_api_response:ok(Data))
    catch
        ErrorType:ErrorReason:Stacktrace ->
            lager:error("API任务查询失败：~p:~p~n~p~n", [ErrorType, ErrorReason, Stacktrace]),
            eadm_api_response:nova_json(eadm_api_response:error(<<"internal_error">>, <<"任务查询失败">>))
    end;

list(#{auth_data := #{<<"authed">> := true, <<"permission">> := #{<<"crontab">> := false}}}) ->
    eadm_api_response:nova_json(eadm_api_response:forbidden());

list(#{auth_data := #{<<"authed">> := false}}) ->
    eadm_api_response:nova_json(eadm_api_response:unauthorized());

list(_) ->
    eadm_api_response:nova_json(eadm_api_response:unauthorized()).
