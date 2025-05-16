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
%% 路由配置函数，定义系统的URL路由规则
%% 环境变量在sys.config中定义：{nova, [{environment, Value}]}
%%
%% @end
%%--------------------------------------------------------------------
routes(_Environment) ->
    [#{prefix => "",
    security => false,
    routes => [
        {"/login", fun eadm_login_controller:login/1, #{methods => [get, post, options]}},
        {"/logout", fun eadm_login_controller:logout/1, #{methods => [post]}},
        {"/assets/[...]", "assets"}
      ]
    },
    #{prefix => "menu",
    security => {eadm_auth, auth},
    routes => [
        {"/health", fun eadm_health_controller:index/1, #{methods => [get]}},
        {"/location", fun eadm_location_controller:index/1, #{methods => [get]}},
        {"/finance", fun eadm_finance_controller:index/1, #{methods => [get]}},
        {"/crontab", fun eadm_crontab_controller:index/1, #{methods => [get]}},
        {"/user", fun eadm_user_controller:index/1, #{methods => [get]}},
        {"/role", fun eadm_role_controller:index/1, #{methods => [get]}},
        {"/device", fun eadm_device_controller:index/1, #{methods => [get]}}
      ]
    },
    #{prefix => "",
    security => {eadm_auth, auth},
    routes => [
        {"/", fun eadm_dashboard_controller:index/1, #{methods => [get]}},
        {"/userinfo", fun eadm_login_controller:userinfo/1, #{methods => [get]}},
        {"/userpwd", fun eadm_login_controller:userpwd/1, #{methods => [post]}},
        {"/useredit", fun eadm_login_controller:useredit/1, #{methods => [post]}},
        {"/dashboard", fun eadm_dashboard_controller:search/1, #{methods => [get]}},
        {"/health", fun eadm_health_controller:search/1, #{methods => [get]}},
        {"/location", fun eadm_location_controller:search/1, #{methods => [get]}},
        {"/finance", fun eadm_finance_controller:search/1, #{methods => [get]}},
        {"/crontab", fun eadm_crontab_controller:search/1, #{methods => [get]}},
        {"/user", fun eadm_user_controller:search/1, #{methods => [get]}},
        {"/role", fun eadm_role_controller:search/1, #{methods => [get]}}
      ]
    },
    #{prefix => "finance",
    security => {eadm_auth, auth},
    routes => [
        {"/:detailId", fun eadm_finance_controller:searchdetail/1, #{methods => [get]}},
        {"/:detailId", fun eadm_finance_controller:delete/1, #{methods => [delete]}},
        {"/upload", fun eadm_finance_controller:upload/1, #{methods => [post]}}
      ]
    },
    #{prefix => "device",
    security => {eadm_auth, auth},
    routes => [
        {"/", fun eadm_device_controller:search/1, #{methods => [get]}},
        {"/add", fun eadm_device_controller:add/1, #{methods => [post]}},
        {"/edit", fun eadm_device_controller:edit/1, #{methods => [post]}},
        {"/delete/:deviceNo", fun eadm_device_controller:delete/1, #{methods => [delete]}},
        {"/toggle", fun eadm_device_controller:toggle_status/1, #{methods => [post]}},
        {"/assign", fun eadm_device_controller:assign/1, #{methods => [post]}},
        {"/unassign/:id", fun eadm_device_controller:unassign/1, #{methods => [delete]}},
        {"/users/:deviceNo", fun eadm_device_controller:device_users/1, #{methods => [get]}},
        {"/user_devices", fun eadm_device_controller:user_devices/1, #{methods => [get]}}
      ]
    },
    #{prefix => "crontab",
    security => {eadm_auth, auth},
    routes => [
        {"/detail/:cronId", fun eadm_crontab_controller:detail/1, #{methods => [get]}},
        {"/add", fun eadm_crontab_controller:add/1, #{methods => [post]}},
        {"/edit", fun eadm_crontab_controller:edit/1, #{methods => [post]}},
        {"/toggle", fun eadm_crontab_controller:toggle/1, #{methods => [post]}},
        {"/delete/:cronId", fun eadm_crontab_controller:delete/1, #{methods => [delete]}}
      ]
    },
    #{prefix => "user",
    security => {eadm_auth, auth},
    routes => [
        {"/add", fun eadm_user_controller:add/1, #{methods => [post]}},
        {"/edit", fun eadm_user_controller:edit/1, #{methods => [post]}},
        {"/delete/:userId", fun eadm_user_controller:delete/1, #{methods => [delete]}},
        {"/reset/:userId", fun eadm_user_controller:reset/1, #{methods => [post]}},
        {"/disable/:userId", fun eadm_user_controller:disable/1, #{methods => [post]}}
      ]
    },
    #{prefix => "userrole",
    security => {eadm_auth, auth},
    routes => [
        {"/:userId", fun eadm_user_controller:userrole/1, #{methods => [get]}},
        {"/delete/:userRoleId", fun eadm_user_controller:userroledel/1, #{methods => [delete]}},
        {"/add", fun eadm_user_controller:userroleadd/1, #{methods => [post]}}
      ]
    },
    #{prefix => "role",
    security => {eadm_auth, auth},
    routes => [
        {"/:userId", fun eadm_role_controller:getrolelist/1, #{methods => [get]}},
        {"/add", fun eadm_role_controller:add/1, #{methods => [post]}},
        {"/disable/:roleId", fun eadm_role_controller:disable/1, #{methods => [post]}}
      ]
    },
    #{prefix => "permission",
    security => {eadm_auth, auth},
    routes => [
        {"/", fun eadm_user_controller:userpermission/1, #{methods => [get]}},
        {"/:roleId", fun eadm_role_controller:loadpermission/1, #{methods => [get]}},
        {"/edit", fun eadm_role_controller:updatepermission/1, #{methods => [post]}}
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
    #{prefix => "api",
    security => false,
    routes => [
        {"/watch", fun api_watch:index/1, #{methods => [post]}}
      ]
    },
    #{prefix => "api/finance",
    security => {eadm_auth, auth},
    routes => [
        {"/alipay", fun eadm_payment_controller:alipay/1, #{methods => [get]}},
        {"/wechat", fun eadm_payment_controller:wechat/1, #{methods => [get]}},
        {"/config", fun eadm_payment_controller:config/1, #{methods => [post]}}
      ]
    }
    ].
