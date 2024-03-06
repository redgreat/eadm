%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%% @doc
%%
%% eadm public API
%%
%% @end
%%% Created : 2024-01-23 17:30:14
%%%-------------------------------------------------------------------
-module(eadm_router).
-author("wangcw").

%%%===================================================================
%%% Behaviour
%%%===================================================================
-behaviour(nova_router).

%%%===================================================================
%%% Application callbacks
%%%===================================================================
-export([routes/1]).

%%====================================================================
%% API functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% The Environment-variable is defined in your sys.config in {nova, [{environment, Value}]}
%%
%% @end
%%--------------------------------------------------------------------
routes(_Environment) ->
    [#{prefix => "",
      security => false,
      routes => [
          {"/login", { eadm_login_controller, login }, #{methods => [get, post]}},
          {"/logout", { eadm_login_controller, logout }, #{methods => [post]}},
          {"/assets/[...]", "assets"}
        ]
      },
      #{prefix => "",
      security => {eadm_auth, auth},
      routes => [
          {"/", { eadm_dashboard_controller, index }, #{methods => [get]}}
        ]
      },
      #{prefix => "data",
      security => false,
      routes => [
          {"/health", { eadm_health_controller, search }, #{methods => [get]}}
        ]
      },
      #{prefix => "daily",
      security => {eadm_auth, auth},
      routes => [
          {"/health", { eadm_health_controller, index }, #{methods => [get]}}
        ]
      },
      #{prefix => "/sys",
      security => {eadm_auth, auth},
      routes => [
          {"/sysinfo", { eadm_sys_sysinfo_controller, index }, #{methods => [get]}},
          {"/route_table", { eadm_sys_sysinfo_controller, route_table }, #{methods => [get]}},
          {"/processes", { eadm_sys_processes_controller, index }, #{methods => [get]}},
          {"/processes/:pid", { eadm_sys_processes_controller, process_info }, #{methods => [get]}},
          {"/ports", { eadm_sys_ports_controller, index }, #{methods => [get]}},
          {"/tables", { eadm_sys_tv_controller, index }, #{methods => [get]}}
        ]
      }].
