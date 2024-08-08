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
          {"/list", fun eadm_user_controller:search/1, #{methods => [get]}},
          {"/info", fun eadm_user_controller:searchself/1, #{methods => [get]}},
          {"/pwd", fun eadm_user_controller:password/1, #{methods => [post]}},

          {"/add", fun eadm_user_controller:add/1, #{methods => [post]}},
          {"/edit", fun eadm_user_controller:edit/1, #{methods => [post]}},
          {"/editself", fun eadm_user_controller:editself/1, #{methods => [post]}},

          {"/:userId", fun eadm_user_controller:delete/1, #{methods => [delete]}},
          {"/reset/:userId", fun eadm_user_controller:reset/1, #{methods => [post]}},
          {"/disable/:userId", fun eadm_user_controller:disable/1, #{methods => [post]}},

          {"/userrole/:userId", fun eadm_user_controller:userrole/1, #{methods => [get]}},
          {"/userrole/:userRoleId", fun eadm_user_controller:userroledel/1, #{methods => [delete]}},
          {"/userrole/add", fun eadm_user_controller:userroleadd/1, #{methods => [post]}},

          {"/role", fun eadm_role_controller:index/1, #{methods => [get]}},
          {"/role/list", fun eadm_role_controller:search/1, #{methods => [get]}},
          {"/role/list/:userId", fun eadm_role_controller:getrolelist/1, #{methods => [get]}},
          {"/role/add", fun eadm_role_controller:add/1, #{methods => [post]}},
          {"/role/:roleId", fun eadm_role_controller:delete/1, #{methods => [delete]}},
          {"/role/disable/:roleId", fun eadm_role_controller:disable/1, #{methods => [post]}},

          {"/permission", fun eadm_user_controller:userpermission/1, #{methods => [get]}},
          {"/permission/edit", fun eadm_role_controller:updatepermission/1, #{methods => [post]}},
          {"/permission/:roleId", fun eadm_role_controller:loadpermission/1, #{methods => [get]}}
        ]
      },
      #{prefix => "finance",
      security => {eadm_auth, auth},
      routes => [
          {"/", fun eadm_finance_controller:search/1, #{methods => [get]}},
          {"/:detailId", fun eadm_finance_controller:delete/1, #{methods => [delete]}},
          {"/:detailId", fun eadm_finance_controller:searchdetail/1, #{methods => [get]}},
          {"/upload", fun eadm_finance_controller:upload/1, #{methods => [post]}}
      ]
      },
      #{prefix => "data",
      security => {eadm_auth, auth},
      routes => [
          {"/dashboard", fun eadm_dashboard_controller:search/1, #{methods => [get]}},
          {"/health", fun eadm_health_controller:search/1, #{methods => [get]}},
          {"/location", fun eadm_location_controller:search/1, #{methods => [get]}},
          {"/crontab", fun eadm_crontab_controller:search/1, #{methods => [get]}}
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
