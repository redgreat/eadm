%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2026, REDGREAT
%%% @doc
%%% 运动数据设置控制器
%%% 处理Garmin账户关联、同步配置等设置
%%% @end
%%% Created : 2026-01-23
%%%-------------------------------------------------------------------
-module(eadm_settings_controller).
-author("wangcw").

%%%===================================================================
%%% 头文件引用
%%%===================================================================

-export([
    index/1,
    search/1,
    link_garmin/1,
    unlink_garmin/1,
    garmin_status/1,
    update_sync_config/1,
    update_share_config/1,
    sync_history/1,
    trigger_sync/1
]).

%%====================================================================
%% API functions
%%====================================================================

index(#{
    auth_data := #{
        <<"authed">> := true,
        <<"username">> := UserName,
        <<"permission">> := Permission
    }
}) ->
    case maps:get(<<"sports">>, Permission, false) of
        true ->
            lager:info("Settings index called for user: ~ts", [UserName]),
            {ok, [{username, UserName}]};
        false ->
            lager:warning("User ~ts does not have sports permission", [UserName]),
            {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]}
    end;
index(#{auth_data := #{<<"authed">> := false}}) ->
    lager:info("User not authenticated, redirecting to login"),
    {redirect, "/login"};
index(Params) ->
    lager:error("Unexpected params in index: ~p", [Params]),
    {json, [#{<<"Alert">> => unicode:characters_to_binary("参数错误！", utf8)}]}.

%%--------------------------------------------------------------------
%% @doc
%% 查询设置数据（AJAX调用）
%% @end
%%--------------------------------------------------------------------
search(#{
    auth_data := #{
        <<"authed">> := true,
        <<"username">> := UserName,
        <<"loginname">> := LoginName,
        <<"permission">> := Permission
    }
}) ->
    case maps:get(<<"sports">>, Permission, false) of
        true ->
            UserId = get_user_id_from_loginname(LoginName),

            SQL =
                <<
                    "SELECT garminemail, lastsynctime, syncenable, autosync, syncdays\n"
                    "                    FROM sp_garminconf \n"
                    "                    WHERE userid = $1"
                >>,
            {Linked, Email, LastSync, AutoSync, SyncDays} =
                case eadm_pgpool:equery(pool_pg, SQL, [UserId]) of
                    {ok, _, [{Email0, LastSync0, _SyncEnabled0, AutoSync0, SyncDays0}]} ->
                        {true, Email0, format_timestamp(LastSync0), AutoSync0, SyncDays0};
                    {ok, _, []} ->
                        {false, <<"">>, null, false, 30}
                end,
            LogSql =
                <<
                    "SELECT starttime, endtime, synccount, \n"
                    "                               newcount, syncstatus, errmsg\n"
                    "                        FROM sp_garminlog \n"
                    "                        WHERE userid = $1 \n"
                    "                        ORDER BY starttime DESC \n"
                    "                        LIMIT 20"
                >>,
            Logs =
                case eadm_pgpool:equery(pool_pg, LogSql, [UserId]) of
                    {ok, _, Rows} ->
                        lists:map(fun format_sync_log_view/1, Rows);
                    _ ->
                        []
                end,
            {json, #{
                <<"code">> => 200,
                <<"data">> => #{
                    <<"username">> => UserName,
                    <<"garmin_linked">> => Linked,
                    <<"garmin_email">> => Email,
                    <<"last_sync_time">> => LastSync,
                    <<"auto_sync">> => AutoSync,
                    <<"sync_days">> => SyncDays,
                    <<"sync_logs">> => Logs
                }
            }};
        false ->
            {json, #{<<"code">> => 403, <<"message">> => <<"API鉴权失败！">>}}
    end;
search(#{auth_data := #{<<"authed">> := false}}) ->
    {json, #{<<"code">> => 401, <<"message">> => <<"未认证">>}};
search(_Params) ->
    {json, #{<<"code">> => 400, <<"message">> => <<"参数错误">>}}.

%%--------------------------------------------------------------------
%% @doc
%% 关联Garmin账户
%% @end
%%--------------------------------------------------------------------
link_garmin(#{req := Req}) ->
    UserId = get_user_id(Req),

    %% 解析请求体
    {ok, Body, _} = cowboy_req:read_body(Req),
    #{<<"email">> := Email, <<"password">> := Password} = json:decode(Body),

    %% 登录Garmin
    case garmin_client_service:login(Email, Password) of
        {ok, #{oauth1 := OAuth1Token, oauth2 := OAuth2Token}} ->
            %% 加密并保存tokens
            OAuth1Encrypted = garmin_client_service:encrypt_token(eadm_utils:to_json(OAuth1Token)),
            OAuth2Encrypted = garmin_client_service:encrypt_token(eadm_utils:to_json(OAuth2Token)),
            OAuth1Json = eadm_utils:to_json(#{<<"enc">> => OAuth1Encrypted}),
            OAuth2Json = eadm_utils:to_json(#{<<"enc">> => OAuth2Encrypted}),

            %% 保存到数据库
            SQL =
                <<
                    "INSERT INTO sp_garminconf \n"
                    "\n"
                    "\n"
                    "                    (userid, garminemail, oauth1token, oauth2token, syncenable, autosync)\n"
                    "\n"
                    "\n"
                    "                    VALUES ($1, $2, $3, $4, true, true)\n"
                    "\n"
                    "\n"
                    "                    ON CONFLICT (userid) \n"
                    "\n"
                    "\n"
                    "                    DO UPDATE SET \n"
                    "\n"
                    "\n"
                    "                        garminemail = EXCLUDED.garminemail,\n"
                    "\n"
                    "\n"
                    "                        oauth1token = EXCLUDED.oauth1token,\n"
                    "\n"
                    "\n"
                    "                        oauth2token = EXCLUDED.oauth2token,\n"
                    "\n"
                    "\n"
                    "                        syncenable = true,\n"
                    "\n"
                    "\n"
                    "                        updatedat = CURRENT_TIMESTAMP"
                >>,

            case eadm_pgpool:equery(pool_pg, SQL, [UserId, Email, OAuth1Json, OAuth2Json]) of
                {ok, _} ->
                    eadm_scheduler:schedule_user_sync(UserId),

                    {json, #{
                        <<"code">> => 200,
                        <<"message">> => <<"Garmin account linked successfully">>
                    }};
                {error, Reason} ->
                    {json, #{
                        <<"code">> => 500,
                        <<"message">> => iolist_to_binary(io_lib:format("~p", [Reason]))
                    }}
            end;
        {error, Reason} ->
            {json, #{
                <<"code">> => 400,
                <<"message">> => iolist_to_binary(io_lib:format("Login failed: ~p", [Reason]))
            }}
    end.

%%--------------------------------------------------------------------
%% @doc
%% 解除Garmin账户关联
%% @end
%%--------------------------------------------------------------------
unlink_garmin(#{req := Req} = _Params) ->
    UserId = get_user_id(Req),

    %% 取消定时任务
    eadm_scheduler:cancel_user_sync(UserId),

    %% 删除集成配置
    SQL = <<"DELETE FROM sp_garminconf WHERE userid = $1 RETURNING id">>,

    case eadm_pgpool:equery(pool_pg, SQL, [UserId]) of
        {ok, _, [{_}]} ->
            {json, #{
                <<"code">> => 200,
                <<"message">> => <<"Garmin account unlinked successfully">>
            }};
        {ok, _, []} ->
            {json, #{
                <<"code">> => 404,
                <<"message">> => <<"No Garmin account linked">>
            }};
        {error, Reason} ->
            {json, #{
                <<"code">> => 500,
                <<"message">> => iolist_to_binary(io_lib:format("~p", [Reason]))
            }}
    end.

%%--------------------------------------------------------------------
%% @doc
%% 获取Garmin关联状态
%% @end
%%--------------------------------------------------------------------
garmin_status(#{req := Req} = _Params) ->
    UserId = get_user_id(Req),

    SQL =
        <<
            "SELECT garminemail, lastsynctime, syncenable, autosync, syncdays\n"
            "\n"
            "\n"
            "            FROM sp_garminconf \n"
            "\n"
            "\n"
            "            WHERE userid = $1"
        >>,

    case eadm_pgpool:equery(pool_pg, SQL, [UserId]) of
        {ok, _, [{Email, LastSync, SyncEnabled, AutoSync, SyncDays}]} ->
            {ok, SyncStatus} = eadm_scheduler:get_sync_status(UserId),

            {json, #{
                <<"code">> => 200,
                <<"data">> => #{
                    <<"linked">> => true,
                    <<"email">> => Email,
                    <<"lastSyncTime">> => format_timestamp(LastSync),
                    <<"syncEnabled">> => SyncEnabled,
                    <<"autoSync">> => AutoSync,
                    <<"syncDays">> => SyncDays,
                    <<"syncStatus">> => atom_to_binary(SyncStatus, utf8)
                }
            }};
        {ok, _, []} ->
            {json, #{
                <<"code">> => 200,
                <<"data">> => #{
                    <<"linked">> => false
                }
            }};
        {error, Reason} ->
            {json, #{
                <<"code">> => 500,
                <<"message">> => iolist_to_binary(io_lib:format("~p", [Reason]))
            }}
    end.

%%--------------------------------------------------------------------
%% @doc
%% 更新同步配置
%% @end
%%--------------------------------------------------------------------
update_sync_config(#{req := Req} = _Params) ->
    UserId = get_user_id(Req),

    %% 解析请求体
    {ok, Body, _} = cowboy_req:read_body(Req),
    Config = json:decode(Body),

    SyncEnabled = maps:get(<<"syncEnabled">>, Config, true),
    AutoSync = maps:get(<<"autoSync">>, Config, true),
    SyncDays = maps:get(<<"syncDays">>, Config, 30),

    SQL =
        <<
            "UPDATE sp_garminconf \n"
            "\n"
            "\n"
            "            SET syncenable = $1, autosync = $2, syncdays = $3, updatedat = CURRENT_TIMESTAMP\n"
            "\n"
            "\n"
            "            WHERE userid = $4 \n"
            "\n"
            "\n"
            "            RETURNING id"
        >>,

    case eadm_pgpool:equery(pool_pg, SQL, [SyncEnabled, AutoSync, SyncDays, UserId]) of
        {ok, _, [{_}]} ->
            case {SyncEnabled, AutoSync} of
                {true, true} ->
                    eadm_scheduler:schedule_user_sync(UserId);
                _ ->
                    eadm_scheduler:cancel_user_sync(UserId)
            end,

            {json, #{
                <<"code">> => 200,
                <<"message">> => <<"Sync config updated successfully">>
            }};
        {ok, _, []} ->
            {json, #{
                <<"code">> => 404,
                <<"message">> => <<"No Garmin account linked">>
            }};
        {error, Reason} ->
            {json, #{
                <<"code">> => 500,
                <<"message">> => iolist_to_binary(io_lib:format("~p", [Reason]))
            }}
    end.

%%--------------------------------------------------------------------
%% @doc
%% 更新分享配置
%% @end
%%--------------------------------------------------------------------
update_share_config(#{req := Req} = _Params) ->
    UserId = get_user_id(Req),

    %% 解析请求体
    {ok, Body, _} = cowboy_req:read_body(Req),
    #{<<"activityId">> := ActivityId} = Config = json:decode(Body),

    IsPublic = maps:get(<<"isPublic">>, Config, false),
    HideMap = maps:get(<<"hideMap">>, Config, false),
    HideStats = maps:get(<<"hideStats">>, Config, false),
    HideLocation = maps:get(<<"hideLocation">>, Config, false),

    SQL =
        <<
            "UPDATE sp_activity \n"
            "\n"
            "\n"
            "            SET ispublic = $1, hidemap = $2, hidestats = $3, hidelocation = $4,\n"
            "\n"
            "\n"
            "                updatedat = CURRENT_TIMESTAMP\n"
            "\n"
            "\n"
            "            WHERE id = $5 AND userid = $6 \n"
            "\n"
            "\n"
            "            RETURNING sharetoken"
        >>,

    case
        eadm_pgpool:equery(pool_pg, SQL, [
            IsPublic, HideMap, HideStats, HideLocation, ActivityId, UserId
        ])
    of
        {ok, _, [{ShareToken}]} ->
            {json, #{
                <<"code">> => 200,
                <<"data">> => #{
                    <<"shareToken">> => ShareToken,
                    <<"shareUrl">> => <<"/share/", ShareToken/binary>>
                }
            }};
        {ok, _, []} ->
            {json, #{
                <<"code">> => 404,
                <<"message">> => <<"Activity not found">>
            }};
        {error, Reason} ->
            {json, #{
                <<"code">> => 500,
                <<"message">> => iolist_to_binary(io_lib:format("~p", [Reason]))
            }}
    end.

%%--------------------------------------------------------------------
%% @doc
%% 获取同步历史记录
%% @end
%%--------------------------------------------------------------------
sync_history(#{req := Req} = _Params) ->
    UserId = get_user_id(Req),

    SQL =
        <<
            "SELECT starttime, endtime, synccount, \n"
            "\n"
            "\n"
            "                   newcount, syncstatus, errmsg\n"
            "\n"
            "\n"
            "            FROM sp_garminlog \n"
            "\n"
            "\n"
            "            WHERE userid = $1 \n"
            "\n"
            "\n"
            "            ORDER BY starttime DESC \n"
            "\n"
            "\n"
            "            LIMIT 20"
        >>,

    case eadm_pgpool:equery(pool_pg, SQL, [UserId]) of
        {ok, _, Rows} ->
            Logs = lists:map(fun format_sync_log/1, Rows),
            {json, #{
                <<"code">> => 200,
                <<"data">> => Logs
            }};
        {error, Reason} ->
            {json, #{
                <<"code">> => 500,
                <<"message">> => iolist_to_binary(io_lib:format("~p", [Reason]))
            }}
    end.

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc 获取当前用户ID（从Req中获取）
%%--------------------------------------------------------------------
get_user_id(Req) ->
    case nova_session:get(Req, <<"loginname">>) of
        {ok, LoginName} ->
            get_user_id_from_loginname(LoginName);
        {error, _} ->
            erlang:error(no_loginname_in_session)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc 根据登录名获取用户ID
%%--------------------------------------------------------------------
get_user_id_from_loginname(LoginName) ->
    Sql = "select id from eadm_user where loginname = $1 and deleted is false limit 1;",
    case eadm_pgpool:equery(pool_pg, Sql, [LoginName]) of
        {ok, _, [{UserId}]} ->
            UserId;
        _ ->
            erlang:error({user_not_found, LoginName})
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc 格式化同步日志
%%--------------------------------------------------------------------
format_sync_log({StartTime, EndTime, Synced, New, Status, ErrorMsg}) ->
    #{
        <<"startTime">> => format_timestamp(StartTime),
        <<"endTime">> => format_timestamp(EndTime),
        <<"activitiesSynced">> => Synced,
        <<"activitiesNew">> => New,
        <<"status">> => Status,
        <<"errorMessage">> => ErrorMsg
    }.

format_sync_log_view({StartTime, EndTime, Synced, New, Status, ErrorMsg}) ->
    #{
        <<"start_time">> => format_timestamp(StartTime),
        <<"end_time">> => format_timestamp(EndTime),
        <<"activities_synced">> => Synced,
        <<"activities_new">> => New,
        <<"status">> => Status,
        <<"error_message">> => ErrorMsg
    }.

%%--------------------------------------------------------------------
%% @private
%% @doc 格式化时间戳
%%--------------------------------------------------------------------
format_timestamp(null) ->
    null;
format_timestamp({{Y, M, D}, {H, Mi, S}}) ->
    iolist_to_binary(
        io_lib:format(
            "~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
            [Y, M, D, H, Mi, S]
        )
    );
format_timestamp(Timestamp) when is_binary(Timestamp) ->
    Timestamp.

%%--------------------------------------------------------------------
%% @doc
%% 触发手动同步
%% @end
%%--------------------------------------------------------------------
trigger_sync(#{req := Req} = _Params) ->
    UserId = get_user_id_from_req(Req),

    %% 解析请求体
    {ok, Body, _} = cowboy_req:read_body(Req),
    Params = json:decode(Body),

    DaysBack = maps:get(<<"daysBack">>, Params, 30),

    %% 触发同步
    case eadm_scheduler:trigger_manual_sync(UserId, DaysBack) of
        {ok, _Pid} ->
            {json, #{<<"code">> => 200, <<"message">> => <<"Sync started successfully">>}};
        {error, sync_already_running} ->
            {json, #{<<"code">> => 409, <<"message">> => <<"Sync already in progress">>}};
        {error, Reason} ->
            {json, #{
                <<"code">> => 500, <<"message">> => iolist_to_binary(io_lib:format("~p", [Reason]))
            }}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc 从请求中获取用户ID
%%--------------------------------------------------------------------
get_user_id_from_req(Req) ->
    %% 从session或token中获取用户ID，这里需要根据实际的认证机制来实现
    %% 暂时返回一个默认值，实际使用时需要替换为真实的用户ID获取逻辑
    case cowboy_req:header(<<"authorization">>, Req) of
        undefined ->
            <<"default_user">>;
        _Token ->
            %% 这里需要解析token获取用户ID
            <<"default_user">>
    end.
