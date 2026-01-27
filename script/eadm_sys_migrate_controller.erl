%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2026, REDGREAT
%%% @doc
%%%
%%% 系统迁移控制器：权限字段补齐等一次性操作
%%%
%%% @end
%%% Created : 2026-01-26
%%%-------------------------------------------------------------------
-module(eadm_sys_migrate_controller).
-author("wangcw").

-include("eadm_mnesia.hrl").

%%%===================================================================
%%% 函数导出
%%%===================================================================
-export([sports_permission/1]).

%%%===================================================================
%%% API 函数
%%%===================================================================

%% @doc
%% 为所有角色的 rolepermission 补齐 sports 字段
%% 超级管理员设置为true，其他角色默认false
%% 需具备用户管理权限
%% @end
sports_permission(#{
    auth_data := #{
        <<"authed">> := true,
        <<"permission">> := #{<<"usermanage">> := true}
    }
}) ->
    Roles = eadm_mnesia_api:query_all(eadm_role),
    lists:foreach(
        fun(#eadm_role{id = Id, rolepermission = Permission, rolename = RoleName}) ->
            case maps:is_key(<<"sports">>, Permission) of
                true ->
                    ok;
                false ->
                    % 超级管理员角色启用sports权限，其他角色禁用
                    SportsValue =
                        case RoleName of
                            <<"超级管理员"/utf8>> -> true;
                            _ -> false
                        end,
                    ok = eadm_mnesia_api:update(eadm_role, Id, fun(R) ->
                        R#eadm_role{
                            rolepermission = Permission#{<<"sports">> => SportsValue},
                            updatedat = erlang:system_time(second)
                        }
                    end)
            end
        end,
        Roles
    ),
    {json, [#{<<"Alert">> => unicode:characters_to_binary("迁移完成：已补齐 sports 权限字段（超级管理员已启用）", utf8)}]};
sports_permission(#{auth_data := #{<<"permission">> := #{<<"usermanage">> := false}}}) ->
    {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]};
sports_permission(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.
