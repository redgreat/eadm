%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%% @doc
%%
%% eadm public API
%%
%% @end
%%% Created : 2024-01-23 17:27:13
%%%-------------------------------------------------------------------
-module(eadm_app).
-author("wangcw").

%%%===================================================================
%%% Behaviour
%%%===================================================================
-behaviour(application).

%%%===================================================================
%%% Application callbacks
%%%===================================================================
-export([start/2, stop/1]).

%%====================================================================
%% API functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    application:start(lager),
    eadm_sup:start_link().

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    application:stop(lager),
    ok.
