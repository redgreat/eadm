%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%  Device APIs for the new SolidJS frontend.
%%% @end
%%%-------------------------------------------------------------------
-module(eadm_api_device_controller).
-author("wangcw").

-export([list/1]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc
%% 获取设备列表。
%% @end
list(#{auth_data := #{<<"authed">> := true, <<"permission">> := Permission}} = Req) ->
    case can_list_devices(Permission) of
        true -> do_list(Req);
        false -> eadm_api_response:nova_json(eadm_api_response:forbidden())
    end;

list(#{auth_data := #{<<"authed">> := false}}) ->
    eadm_api_response:nova_json(eadm_api_response:unauthorized());

list(_) ->
    eadm_api_response:nova_json(eadm_api_response:unauthorized()).

%%====================================================================
%% Internal functions
%%====================================================================

do_list(Req) ->
    Query = maps:get(parsed_qs, Req, #{}),
    DeviceNo = maps:get(<<"deviceNo">>, Query, <<>>),
    try
        Data = eadm_device_service:list(DeviceNo),
        eadm_api_response:nova_json(eadm_api_response:ok(Data))
    catch
        _:Error ->
            lager:error("API设备查询失败：~p~n", [Error]),
            eadm_api_response:nova_json(eadm_api_response:error(<<"internal_error">>, <<"设备查询失败">>))
    end.

can_list_devices(#{<<"device">> := #{<<"devlist">> := true}}) ->
    true;
can_list_devices(_) ->
    false.
