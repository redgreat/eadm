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

-export([
    index/1,
    link_garmin/1,
    unlink_garmin/1,
    garmin_status/1,
    update_sync_config/1,
    update_share_config/1,
    sync_history/1
]).

%%====================================================================
%% API functions
%%====================================================================

index(#{auth_data := #{<<"authed">> := true, <<"username">> := UserName,
        <<"permission">> := Permission}, req := Req}) ->
    case maps:get(<<"sports">>, Permission, false) of
        true ->
            UserId = get_user_id(Req),
            SQL = <<"SELECT garminemail, lastsynctime, syncenable, autosync, syncdays
                    FROM sp_garminconf 
                    WHERE userid = $1">>,
            {Linked, Email, LastSync, AutoSync, SyncDays} =
                case eadm_pgpool:equery(SQL, [UserId]) of
                    {ok, _, [{Email0, LastSync0, _SyncEnabled0, AutoSync0, SyncDays0}]} ->
                        {true, Email0, format_timestamp(LastSync0), AutoSync0, SyncDays0};
                    {ok, _, []} ->
                        {false, <<"">>, null, false, 30}
                end,
            LogSql = <<"SELECT starttime, endtime, synccount, 
                               newcount, syncstatus, errmsg
                        FROM sp_garminlog 
                        WHERE userid = $1 
                        ORDER BY starttime DESC 
                        LIMIT 20">>,
            Logs =
                case eadm_pgpool:equery(LogSql, [UserId]) of
                    {ok, _, Rows} ->
                        lists:map(fun format_sync_log_view/1, Rows);
                    _ ->
                        []
                end,
            {ok, [
                {username, UserName},
                {garmin_linked, Linked},
                {garmin_email, Email},
                {last_sync_time, LastSync},
                {auto_sync, AutoSync},
                {sync_days, SyncDays},
                {sync_logs, Logs}
            ], #{view => eadm_settings}};
        false ->
            {json, [#{<<"Alert">> => unicode:characters_to_binary("API鉴权失败！", utf8)}]}
    end;

index(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/login"}.

%%--------------------------------------------------------------------
%% @doc
%% 关联Garmin账户
%% @end
%%--------------------------------------------------------------------
link_garmin(#{req := Req} = _Params) ->
    UserId = get_user_id(Req),
    
    %% 解析请求体
    {ok, Body, _} = cowboy_req:read_body(Req),
    #{<<"email">> := Email, <<"password">> := Password} = jsx:decode(Body, [return_maps]),
    
    %% 登录Garmin
    case garmin_client_service:login(Email, Password) of
        {ok, #{oauth1 := OAuth1Token, oauth2 := OAuth2Token}} ->
            %% 加密并保存tokens
            OAuth1Encrypted = garmin_client_service:encrypt_token(jsx:encode(OAuth1Token)),
            OAuth2Encrypted = garmin_client_service:encrypt_token(jsx:encode(OAuth2Token)),
            OAuth1Json = jsx:encode(#{<<"enc">> => OAuth1Encrypted}),
            OAuth2Json = jsx:encode(#{<<"enc">> => OAuth2Encrypted}),
            
            %% 保存到数据库
            SQL = <<"INSERT INTO sp_garminconf 
                    (userid, garminemail, oauth1token, oauth2token, syncenable, autosync)
                    VALUES ($1, $2, $3, $4, true, true)
                    ON CONFLICT (userid) 
                    DO UPDATE SET 
                        garminemail = EXCLUDED.garminemail,
                        oauth1token = EXCLUDED.oauth1token,
                        oauth2token = EXCLUDED.oauth2token,
                        syncenable = true,
                        updatedat = CURRENT_TIMESTAMP">>,
            
            case eadm_pgpool:equery(SQL, [UserId, Email, OAuth1Json, OAuth2Json]) of
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
    
    case eadm_pgpool:equery(SQL, [UserId]) of
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
    
    SQL = <<"SELECT garminemail, lastsynctime, syncenable, autosync, syncdays
            FROM sp_garminconf 
            WHERE userid = $1">>,
    
    case eadm_pgpool:equery(SQL, [UserId]) of
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
    Config = jsx:decode(Body, [return_maps]),
    
    SyncEnabled = maps:get(<<"syncEnabled">>, Config, true),
    AutoSync = maps:get(<<"autoSync">>, Config, true),
    SyncDays = maps:get(<<"syncDays">>, Config, 30),
    
    SQL = <<"UPDATE sp_garminconf 
            SET syncenable = $1, autosync = $2, syncdays = $3, updatedat = CURRENT_TIMESTAMP
            WHERE userid = $4 
            RETURNING id">>,
    
    case eadm_pgpool:equery(SQL, [SyncEnabled, AutoSync, SyncDays, UserId]) of
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
    #{<<"activityId">> := ActivityId} = Config = jsx:decode(Body, [return_maps]),
    
    IsPublic = maps:get(<<"isPublic">>, Config, false),
    HideMap = maps:get(<<"hideMap">>, Config, false),
    HideStats = maps:get(<<"hideStats">>, Config, false),
    HideLocation = maps:get(<<"hideLocation">>, Config, false),
    
    SQL = <<"UPDATE sp_activity 
            SET ispublic = $1, hidemap = $2, hidestats = $3, hidelocation = $4,
                updatedat = CURRENT_TIMESTAMP
            WHERE id = $5 AND userid = $6 
            RETURNING sharetoken">>,
    
    case eadm_pgpool:equery(SQL, [IsPublic, HideMap, HideStats, HideLocation, ActivityId, UserId]) of
        {ok, _, [{ShareToken}]} ->
            {json, #{
                <<"code">> => 200,
                <<"data">> => #{
                    <<"shareToken">> => ShareToken,
                    <<"shareUrl">> => <<"/share/sports/", ShareToken/binary>>
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
    
    SQL = <<"SELECT starttime, endtime, synccount, 
                   newcount, syncstatus, errmsg
            FROM sp_garminlog 
            WHERE userid = $1 
            ORDER BY starttime DESC 
            LIMIT 20">>,
    
    case eadm_pgpool:equery(SQL, [UserId]) of
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
%% @doc 获取当前用户ID
%%--------------------------------------------------------------------
get_user_id(Req) ->
    #{<<"userid">> := UserId} = nova_session:get(Req),
    UserId.

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
        io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
                     [Y, M, D, H, Mi, S])
    );
format_timestamp(Timestamp) when is_binary(Timestamp) ->
    Timestamp.
