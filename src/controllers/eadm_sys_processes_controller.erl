%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%
%%% eadm Controller
%%%
%%% @end
%%% Created : 2024-01-25 09:28:14
%%%-------------------------------------------------------------------
-module(eadm_sys_processes_controller).
-author("wangcw").

%%%===================================================================
%%% Application callbacks
%%%===================================================================
-export([index/1, process_info/1]).

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
    Processes = [ maps:put(pid, erlang:pid_to_list(Pid), maps:from_list(erlang:process_info(Pid))) || Pid <- erlang:processes()],
    {ok, [{procs, Processes}]}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% process_info
%%
%% @end
%%--------------------------------------------------------------------
process_info(#{bindings := #{<<"pid">> := Pid}}) ->
    Pid0 = uri_string:percent_decode(Pid),
    Pid1 = erlang:binary_to_list(Pid0),
    Pid2 = erlang:list_to_pid(Pid1),

    ProcInfo = erlang:process_info(Pid2),
    MapInfo = maps:from_list(ProcInfo),
    {json, eadm_utils:to_json(MapInfo)}.
