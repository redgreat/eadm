%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%
%%% eadm Controller
%%%
%%% @end
%%% Created : 2024-01-25 09:18:34
%%%-------------------------------------------------------------------
-module(eadm_sys_sysinfo_controller).
-author("wangcw").

%%%===================================================================
%%% Application callbacks
%%%===================================================================
-export([index/1,route_table/1]).

%%%===================================================================
%%% Includes
%%%===================================================================
-include_lib("routing_tree/include/routing_tree.hrl").

%%====================================================================
%% API functions
%%====================================================================

%% @doc
%% index
%% @end
index(#{auth_data := #{<<"authed">> := true, <<"username">> := Username}}) ->
    SysInfo = observer_backend:sys_info(),
    Uptime = observer_lib:to_str({time_ms, proplists:get_value(uptime, SysInfo)}),
    {ok, [{sys_info, SysInfo}, {uptime, Uptime}, {username, Username}]};

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

%%====================================================================
%% Internal functions
%%====================================================================

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
to_string(S) when is_list(S) -> S;
to_string(I) when is_integer(I) ->
    SCode = erlang:integer_to_binary(I),
    << <<"StatusCode: ">>/binary,
       SCode/binary >>;
to_string(B) -> B.
