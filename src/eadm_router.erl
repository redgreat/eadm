%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%
%%% eadm public API
%%%
%%% @end
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
      #{prefix => "daily",
      security => {eadm_auth, auth},
      routes => [
          {"/health", { eadm_health_controller, index }, #{methods => [get]}},
          {"/location", { eadm_location_controller, index }, #{methods => [get]}},
          {"/finance", { eadm_finance_controller, index }, #{methods => [get]}},
          {"/crontab", { eadm_crontab_controller, index }, #{methods => [get]}}
        ]
      },
      #{prefix => "user",
      security => {eadm_auth, auth},
      routes => [
          {"/", { eadm_user_controller, index }, #{methods => [get]}},
          {"/role", { eadm_role_controller, index }, #{methods => [get]}}
        ]
      },
      #{prefix => "data",
      security => {eadm_auth, auth},
      routes => [
          {"/dashboard", { eadm_dashboard_controller, search }, #{methods => [get]}},
          {"/health", { eadm_health_controller, search }, #{methods => [get]}},
          {"/location", { eadm_location_controller, search }, #{methods => [get]}},
          {"/finance", { eadm_finance_controller, search }, #{methods => [get]}},
          {"/finance/:detailId", { eadm_finance_controller, delete }, #{methods => [delete]}},
          {"/finance/:detailId", { eadm_finance_controller, searchdetail }, #{methods => [get]}},
          {"/crontab/:cronName", { eadm_crontab_controller, search }, #{methods => [get]}},
          {"/useradd", { eadm_user_controller, add }, #{methods => [post]}},
          {"/useredit", { eadm_user_controller, edit }, #{methods => [post]}},
          {"/usereditself", { eadm_user_controller, editself }, #{methods => [post]}},
          {"/userpass", { eadm_user_controller, password }, #{methods => [post]}},
          {"/user/:userId", { eadm_user_controller, delete }, #{methods => [delete]}},
          {"/user/reset/:userId", { eadm_user_controller, reset }, #{methods => [post]}},
          {"/user/disable/:userId", { eadm_user_controller, disable }, #{methods => [post]}},
          {"/user", { eadm_user_controller, search }, #{methods => [get]}},
          {"/userself", { eadm_user_controller, searchself }, #{methods => [get]}},
          {"/password", { eadm_user_controller, password }, #{methods => [post]}},
          {"/userrole/:userId", { eadm_user_controller, userrole }, #{methods => [get]}},
          {"/userrole/delete/:userRoleId", { eadm_user_controller, userroledel }, #{methods => [delete]}},
          {"/userroleadd", { eadm_user_controller, userroleadd }, #{methods => [post]}},
          {"/userpermission", { eadm_user_controller, userpermission }, #{methods => [get]}},
          {"/role/:roleId", { eadm_role_controller, delete }, #{methods => [delete]}},
          {"/role/disable/:roleId", { eadm_role_controller, disable }, #{methods => [post]}},
          {"/role", { eadm_role_controller, search }, #{methods => [get]}},
          {"/rolelist/:userId", { eadm_role_controller, getrolelist }, #{methods => [get]}},
          {"/permission", { eadm_role_controller, updatepermission }, #{methods => [post]}},
          {"/permission/:roleId", { eadm_role_controller, loadpermission }, #{methods => [get]}}
        ]
      },#{prefix => "upload",
      security => {eadm_auth, auth},
      routes => [
          {"/finance", { eadm_finance_controller, upload }, #{methods => [post]}}
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
      },
      #{prefix => "api",
      security => false,
      routes => [
          {"/watch", { api_watch, index }, #{methods => [post]}}
        ]
      }].
