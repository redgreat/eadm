%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%
%%% eadm 系统信息查询
%%%
%%% @end
%%% Created : 2024-01-25 09:18:34
%%%-------------------------------------------------------------------
-module(eadm_sys_sysinfo_controller).
-author("wangcw").

%%%===================================================================
%%% 函数导出
%%%===================================================================
-export([index/1,route_table/1]).

%%%===================================================================
%%% Includes
%%%===================================================================
-include_lib("routing_tree/include/routing_tree.hrl").

%%%===================================================================
%%% API 函数
%%%===================================================================

%% @doc
%% 主函数
%% @end
index(#{auth_data := #{<<"authed">> := true, <<"username">> := Username}}) ->
    % SysInfo = observer_backend:sys_info(),
    % Uptime = observer_lib:to_str({time_ms, proplists:get_value(uptime, SysInfo)}),

    SysInfo = [
        {otp_release, erlang:system_info(otp_release)},
        {version, erlang:system_info(version)},
        {system_architecture, erlang:system_info(system_architecture)},
        {wordsize_internal, erlang:system_info({wordsize, internal})},
        {wordsize_external, erlang:system_info({wordsize, external})},
        {smp_support, erlang:system_info(smp_support)},
        {threads, erlang:system_info(threads)},
        {thread_pool_size, erlang:system_info(thread_pool_size)},
        {logical_processors, erlang:system_info(logical_processors)},
        {logical_processors_online, erlang:system_info(logical_processors_online)},
        {logical_processors_available, erlang:system_info(logical_processors_available)},
        {schedulers, erlang:system_info(schedulers)},
        {schedulers_online, erlang:system_info(schedulers_online)},
        {schedulers_available, case erlang:system_info(multi_scheduling) of
            enabled -> erlang:system_info(schedulers_online);
            _ -> 1
        end},
        {run_queue, erlang:statistics(run_queue)},
        {atom_count, erlang:system_info(atom_count)},
        {atom_limit, erlang:system_info(atom_limit)},
        {process_count, erlang:system_info(process_count)},
        {process_limit, erlang:system_info(process_limit)},
        {port_count, erlang:system_info(port_count)},
        {port_limit, erlang:system_info(port_limit)},
        {ets_count, erlang:system_info(ets_count)},
        {ets_limit, erlang:system_info(ets_limit)},
        {total, erlang:memory(total)},
        {processes_used, erlang:memory(processes_used)},
        {atom_used, erlang:memory(atom_used)},
        {binary, erlang:memory(binary)},
        {code, erlang:memory(code)},
        {ets, erlang:memory(ets)},
        {dist_buf_busy_limit, erlang:system_info(dist_buf_busy_limit)}
    ],

    {{input, IOInput}, {output, IOOutput}} = erlang:statistics(io),
    SysInfoWithIO = lists:append(SysInfo, [{io_input, IOInput}, {io_output, IOOutput}]),

    {Uptime, _} = erlang:statistics(wall_clock),
    UptimeStr = io_lib:format("~p Secs", [Uptime div 1000]),

    {ok, [{sys_info, SysInfoWithIO}, {uptime, UptimeStr}, {username, Username}]};

index(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% route_table
%% @end
%%--------------------------------------------------------------------
route_table(#{auth_data := #{<<"authed">> := true, <<"username">> := Username}}) ->
    #host_tree{hosts = Hosts} = persistent_term:get(nova_dispatch),
    [Routes|_] = lists:map(fun({Host, #routing_tree{tree = Tree}}) ->
                                #{"text" => to_string(Host), "children" => flatten_routes(Tree)}
                        end, Hosts),
    {ok, [{routes, thoas:encode(Routes)}, {username, Username}], #{view => eadm_sys_routes}};

route_table(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%%===================================================================
%% 内部函数
%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% flatten_routes
%%
%% @end
%%--------------------------------------------------------------------
flatten_routes([]) -> [];
flatten_routes([#node{segment = <<>>}|Tl]) -> flatten_routes(Tl);
flatten_routes([#node{segment = Segment, value = Values, children = Children, is_binding = IsBinding, is_wildcard = IsWildcard}|Tl]) ->
    HTMLClass = case IsWildcard of
                    true ->
                        <<"wildcard">>;
                    _ ->
                        case IsBinding of
                            true ->
                                <<"binding">>;
                            _ ->
                                case Values of
                                    [] ->
                                        <<"">>;
                                    _ ->
                                        <<"endpoint">>
                                end
                        end
                end,
    case Values of
        [] ->
            [#{"HTMLclass" => HTMLClass, "text" => #{"route" => to_string(Segment)}, "children" => flatten_routes(Children)}|flatten_routes(Tl)];
        _Values ->
            [#{"HTMLclass" => HTMLClass, "text" => #{"route" => to_string(Segment)}, "children" => flatten_routes(Children)}|flatten_routes(Tl)]
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% to_string
%%
%% @end
%%--------------------------------------------------------------------
to_string('_') -> <<"/">>;
to_string(S) when erlang:is_list(S) -> S;
to_string(I) when erlang:is_integer(I) ->
    SCode = erlang:integer_to_binary(I),
    << <<"StatusCode: ">>/binary,
        SCode/binary >>;
to_string(B) -> B.
