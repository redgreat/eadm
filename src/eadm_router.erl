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
          {"/login", fun eadm_login_controller:login/1, #{methods => [get, post]}},
          {"/logout", fun eadm_login_controller:logout/1, #{methods => [post]}},
          {"/assets/[...]", "assets"}
        ]
      },
      #{prefix => "",
      security => {eadm_auth, auth},
      routes => [
          {"/", fun eadm_dashboard_controller:index/1, #{methods => [get]}}
        ]
      },
      #{prefix => "daily",
      security => {eadm_auth, auth},
      routes => [
          {"/health", fun eadm_health_controller:index/1, #{methods => [get]}},
          {"/location", fun eadm_location_controller:index/1, #{methods => [get]}},
          {"/finance", fun eadm_finance_controller:index/1, #{methods => [get]}},
          {"/crontab", fun eadm_crontab_controller:index/1, #{methods => [get]}}
        ]
      },
      #{prefix => "user",
      security => {eadm_auth, auth},
      routes => [
          {"/", fun eadm_user_controller:index/1, #{methods => [get]}},
          {"/role", fun eadm_role_controller:index/1, #{methods => [get]}}
        ]
      },
      #{prefix => "data",
      security => {eadm_auth, auth},
      routes => [
          {"/dashboard", fun eadm_dashboard_controller:search/1, #{methods => [get]}},
          {"/health", fun eadm_health_controller:search/1, #{methods => [get]}},
          {"/location", fun eadm_location_controller:search/1, #{methods => [get]}},
          {"/finance", fun eadm_finance_controller:search/1, #{methods => [get]}},
          {"/finance/:detailId", fun eadm_finance_controller:delete/1, #{methods => [delete]}},
          {"/finance/:detailId", fun eadm_finance_controller:searchdetail/1, #{methods => [get]}},
          {"/crontab/:cronName", fun eadm_crontab_controller:search/1, #{methods => [get]}},
          {"/useradd", fun eadm_user_controller:add/1, #{methods => [post]}},
          {"/useredit", fun eadm_user_controller:edit/1, #{methods => [post]}},
          {"/usereditself", fun eadm_user_controller:editself/1, #{methods => [post]}},
          {"/userpass", fun eadm_user_controller:password/1, #{methods => [post]}},
          {"/user/:userId", fun eadm_user_controller:delete/1, #{methods => [delete]}},
          {"/user/reset/:userId", fun eadm_user_controller:reset/1, #{methods => [post]}},
          {"/user/disable/:userId", fun eadm_user_controller:disable/1, #{methods => [post]}},
          {"/user", fun eadm_user_controller:search/1, #{methods => [get]}},
          {"/userself", fun eadm_user_controller:searchself/1, #{methods => [get]}},
          {"/password", fun eadm_user_controller:password/1, #{methods => [post]}},
          {"/userrole/:userId", fun eadm_user_controller:userrole/1, #{methods => [get]}},
          {"/userrole/delete/:userRoleId", fun eadm_user_controller:userroledel/1, #{methods => [delete]}},
          {"/userroleadd", fun eadm_user_controller:userroleadd/1, #{methods => [post]}},
          {"/userpermission", fun eadm_user_controller:userpermission/1, #{methods => [get]}},
          {"/role/:roleId", fun eadm_role_controller:delete/1, #{methods => [delete]}},
          {"/role/disable/:roleId", fun eadm_role_controller:disable/1, #{methods => [post]}},
          {"/role", fun eadm_role_controller:search/1, #{methods => [get]}},
          {"/role/add", fun eadm_role_controller:add/1, #{methods => [post]}},
          {"/rolelist/:userId", fun eadm_role_controller:getrolelist/1, #{methods => [get]}},
          {"/permission", fun eadm_role_controller:updatepermission/1, #{methods => [post]}},
          {"/permission/:roleId", fun eadm_role_controller:loadpermission/1, #{methods => [get]}}
        ]
      },#{prefix => "upload",
      security => {eadm_auth, auth},
      routes => [
          {"/finance", fun eadm_finance_controller:upload/1, #{methods => [post]}}
        ]
      },
      #{prefix => "sys",
      security => {eadm_auth, auth},
      routes => [
          {"/sysinfo", fun eadm_sys_sysinfo_controller:index/1, #{methods => [get]}},
          {"/route_table", fun eadm_sys_sysinfo_controller:route_table/1, #{methods => [get]}},
          {"/processes", fun eadm_sys_processes_controller:index/1, #{methods => [get]}},
          {"/processes/:pid", fun eadm_sys_processes_controller:process_info/1, #{methods => [get]}},
          {"/ports", fun eadm_sys_ports_controller:index/1, #{methods => [get]}},
          {"/tables", fun eadm_sys_tv_controller:index/1, #{methods => [get]}}
        ]
      },
      #{prefix => "watch",
      security => false,
      routes => [
          {"/receive", fun api_watch:index/1, #{methods => [post]}}
        ]
      }].
