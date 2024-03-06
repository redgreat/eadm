%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%% @doc
%%
%% eadm Controller
%%
%% @end
%%% Created : 2024-01-25 09:28:21
%%%-------------------------------------------------------------------
-module(eadm_sys_ports_controller).
-author("wangcw").

%%%===================================================================
%%% Application callbacks
%%%===================================================================
-export([index/1]).

%%====================================================================
%% API functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% index
%%
%% @end
%%--------------------------------------------------------------------
index(_Req) ->
    Ports = [ maps:from_list(X) || X <- observer_backend:get_port_list()],
    Ports0 = lists:map(fun(A = #{port_id := PortId, connected := Connected}) ->
                               A#{port_id => port_to_list(PortId), connected => erlang:pid_to_list(Connected)}
                       end, Ports),
    {ok, [{ports, Ports0}]}.
