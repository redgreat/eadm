%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%  System information service shared by Nova controllers and Cowboy handlers.
%%% @end
%%%-------------------------------------------------------------------
-module(eadm_system_service).
-author("wangcw").

-export([info/0]).

%%====================================================================
%% API functions
%%====================================================================

info() ->
    {{input, IOInput}, {output, IOOutput}} = erlang:statistics(io),
    {Uptime, _} = erlang:statistics(wall_clock),
    [
        item(<<"otpRelease">>, erlang:system_info(otp_release)),
        item(<<"version">>, erlang:system_info(version)),
        item(<<"systemArchitecture">>, erlang:system_info(system_architecture)),
        item(<<"schedulers">>, erlang:system_info(schedulers)),
        item(<<"schedulersOnline">>, erlang:system_info(schedulers_online)),
        item(<<"runQueue">>, erlang:statistics(run_queue)),
        item(<<"processCount">>, erlang:system_info(process_count)),
        item(<<"processLimit">>, erlang:system_info(process_limit)),
        item(<<"portCount">>, erlang:system_info(port_count)),
        item(<<"portLimit">>, erlang:system_info(port_limit)),
        item(<<"etsCount">>, erlang:system_info(ets_count)),
        item(<<"etsLimit">>, erlang:system_info(ets_limit)),
        item(<<"memoryTotal">>, erlang:memory(total)),
        item(<<"memoryProcessesUsed">>, erlang:memory(processes_used)),
        item(<<"memoryBinary">>, erlang:memory(binary)),
        item(<<"memoryCode">>, erlang:memory(code)),
        item(<<"memoryEts">>, erlang:memory(ets)),
        item(<<"ioInput">>, IOInput),
        item(<<"ioOutput">>, IOOutput),
        item(<<"uptimeSeconds">>, Uptime div 1000)
    ].

%%====================================================================
%% Internal functions
%%====================================================================

item(Key, Value) ->
    #{<<"key">> => Key, <<"value">> => to_json(Value)}.

to_json(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
to_json(Value) when is_list(Value) ->
    unicode:characters_to_binary(Value, utf8);
to_json(Value) ->
    Value.
