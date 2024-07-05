%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%% @doc
%%
%% eadm handler
%%
%% @end
%%% Created : 2024-01-23 17:32:22
%%%-------------------------------------------------------------------
-module(eadm_handler).
-author("wangcw").

%%%===================================================================
%%% 函数导出
%%%===================================================================
-export([debug_toolbar/3]).

%%====================================================================
%% API 函数
%%====================================================================

%% @private
%% @doc
%% Fetches the regular view
%% @end
debug_toolbar({ok, Variables}, MF, Req) ->
    debug_toolbar({ok, Variables, #{}}, MF, Req);
debug_toolbar({ok, Variables, _Options}, {Mod, Func}, Req = #{parsed_qs := #{<<"d">> := <<"1">>}}) ->
    %% Fetches the regular view
    nova_basic_handler:handle_ok({ok, Variables}, {Mod, Func}, Req);
debug_toolbar({ok, Variables, Options}, {Mod, _Func}, Req) ->
    io:format("~p~n", [Req]),
    {ok, DebugToolbarVars} = eadm_toolbar_controller:overlay(Req),
    nova_basic_handler:handle_ok({ok, DebugToolbarVars#{}, #{view => eadm_debug_toolbar}},
                                 {eadm_debug_toolbar, overlay}, Req).

%% @private
%% @doc
%% Fetches the regular view
%% @end
get_view_name(Mod) when is_atom(Mod) ->
    StrName = get_view_name(erlang:atom_to_list(Mod)),
    erlang:list_to_atom(StrName);
get_view_name([$_, $c, $o, $n, $t, $r, $o, $l, $l, $e, $r]) ->
    "_dtl";
get_view_name([H|T]) ->
    [H|get_view_name(T)].

%%====================================================================
%% 内部函数
%%====================================================================
