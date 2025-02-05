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
-module(eadm_debug_toolbar_controller).
-author("wangcw").

%%%===================================================================
%%% Application callbacks
%%%===================================================================
-export([overlay/1]).

%%====================================================================
%% API functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% overlay
%%
%% @end
%%--------------------------------------------------------------------
overlay(_Req) ->
    {ok, #{}}.
