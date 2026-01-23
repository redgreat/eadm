%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2026, REDGREAT
%%% @doc
%%% Garmin数据同步模块
%%% 负责从Garmin获取数据并存储到数据库
%%% @end
%%% Created : 2026-01-23
%%%-------------------------------------------------------------------
-module(garmin_sync_service).
-author("wangcw").

-export([
    sync_user_activities/2,
    sync_activity_detail/3,
    save_activity/2,
    parse_garmin_activity/1,
    map_activity_type/1
]).

%%--------------------------------------------------------------------
%% @doc
%% 同步用户活动数据
%% @end
%%--------------------------------------------------------------------
sync_user_activities(UserId, DaysBack) ->
    try
        OAuth1Token = maps:get(<<"oauth1">>, get_user_tokens(UserId)),
        OAuth2Token = maps:get(<<"oauth2">>, get_user_tokens(UserId)),
        EndDate = calendar:universal_time(),
        StartDate = subtract_days(EndDate, DaysBack),
        case garmin_client_service:get_activities(OAuth1Token, OAuth2Token,
                                               format_date(StartDate),
                                               format_date(EndDate)) of
            {ok, Activities} ->
                lists:foreach(
                    fun(Activity) ->
                        save_activity(UserId, parse_garmin_activity(Activity))
                    end,
                    Activities),
                {ok, #{synced => length(Activities), new => 0}};
            {error, Reason} ->
                logger:error("Failed to sync activities: ~p", [Reason]),
                {error, Reason}
        end
    catch
        _:Reason2 ->
            logger:error("Sync failed: ~p", [Reason2]),
            {error, Reason2}
    end.

%%--------------------------------------------------------------------
%% @doc
%% 同步活动详情和轨迹数据
%% @end
%%--------------------------------------------------------------------
sync_activity_detail(UserId, ActivityId, OAuth2Token) ->
    case garmin_client_service:get_activity_detail(OAuth2Token, ActivityId) of
        {ok, Detail} ->
            case garmin_client_service:get_activity_streams(OAuth2Token, ActivityId) of
                {ok, Streams} ->
                    save_activity_streams(ActivityId, Streams),
                    {ok, Detail};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc
%% 保存活动到数据库
%% @end
%%--------------------------------------------------------------------
save_activity(UserId, ActivityData) ->
    ActivityName = maps:get(<<"activityName">>, ActivityData, null),
    ActivityType = map_activity_type(maps:get(<<"activityType">>, ActivityData)),
    Distance = maps:get(<<"distance">>, ActivityData, null),
    StartTime = maps:get(<<"startTimeGMT">>, ActivityData),
    Duration = maps:get(<<"duration">>, ActivityData),
    Calories = maps:get(<<"calories">>, ActivityData, null),
    AvgHR = maps:get(<<"averageHR">>, ActivityData, null),
    MaxHR = maps:get(<<"maxHR">>, ActivityData, null),
    AvgSpeed = maps:get(<<"averageSpeed">>, ActivityData, null),
    MaxSpeed = maps:get(<<"maxSpeed">>, ActivityData, null),
    ElevationGain = maps:get(<<"elevationGain">>, ActivityData, null),
    ElevationLoss = maps:get(<<"elevationLoss">>, ActivityData, null),
    GarminActivityId = maps:get(<<"activityId">>, ActivityData),
    SQL = <<"INSERT INTO sp_activity 
            (userid, actname, acttype, distance, starttime, endtime,
             timespan, elevationgain, elevationloss, avgspeed, maxspeed,
             avgheartbeat, maxheartbeat, calorie, garminactid)
            VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15)
            ON CONFLICT (garminactid) 
            DO UPDATE SET
                actname = EXCLUDED.actname,
                distance = EXCLUDED.distance,
                timespan = EXCLUDED.timespan,
                updatedat = CURRENT_TIMESTAMP
            RETURNING id">>,
    EndTime = add_seconds(StartTime, Duration),
    case eadm_pgpool:equery(SQL, [
        UserId, ActivityName, ActivityType, Distance, StartTime, EndTime,
        Duration, ElevationGain, ElevationLoss, AvgSpeed, MaxSpeed,
        AvgHR, MaxHR, Calories, GarminActivityId
    ]) of
        {ok, _, [{ActivityId}]} ->
            {ok, ActivityId};
        {error, Reason} ->
            logger:error("Failed to save activity: ~p", [Reason]),
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc
%% 保存活动轨迹数据流
%% @end
%%--------------------------------------------------------------------
save_activity_streams(_UserId, _ActivityDetail) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% 解析Garmin活动数据
%% @end
%%--------------------------------------------------------------------
parse_garmin_activity(GarminActivity) ->
    GarminActivity.

%%--------------------------------------------------------------------
%% @doc
%% 映射Garmin活动类型到系统类型
%% @end
%%--------------------------------------------------------------------
map_activity_type(GarminType) when is_binary(GarminType) ->
    case GarminType of
        <<"running">> -> 1;
        <<"cycling">> -> 2;
        <<"swimming">> -> 3;
        <<"fitness_equipment">> -> 4;
        <<"hiking">> -> 5;
        _ -> 99
    end;
map_activity_type(GarminType) when is_map(GarminType) ->
    TypeKey = maps:get(<<"typeKey">>, GarminType, <<"other">>),
    map_activity_type(TypeKey);
map_activity_type(_) ->
    99.

%%--------------------------------------------------------------------
%% @doc
%% 获取用户的OAuth tokens
%% @end
%%--------------------------------------------------------------------
get_user_tokens(UserId) ->
    SQL = <<"SELECT oauth1token, oauth2token FROM sp_garminconf WHERE userid = $1">>,
    case eadm_pgpool:equery(SQL, [UserId]) of
        {ok, _, [{OAuth1Json, OAuth2Json}]} ->
            OAuth1 = decode_token_field(OAuth1Json),
            OAuth2 = decode_token_field(OAuth2Json),
            #{<<"oauth1">> => OAuth1, <<"oauth2">> => OAuth2};
        _ ->
            {error, tokens_not_found}
    end.

%%--------------------------------------------------------------------
%% @doc
%% 日期减去天数
%% @end
%%--------------------------------------------------------------------
subtract_days(DateTime, Days) ->
    Seconds = calendar:datetime_to_gregorian_seconds(DateTime),
    NewSeconds = Seconds - (Days * 24 * 3600),
    calendar:gregorian_seconds_to_datetime(NewSeconds).

%%--------------------------------------------------------------------
%% @doc
%% 格式化日期为ISO8601字符串
%% @end
%%--------------------------------------------------------------------
format_date({{Y, M, D}, {H, Mi, S}}) ->
    list_to_binary(
        io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0wZ",
                     [Y, M, D, H, Mi, S])
    );
format_date(Timestamp) when is_binary(Timestamp) ->
    Timestamp.

%%--------------------------------------------------------------------
%% @doc
%% 给时间戳添加秒数
%% @end
%%--------------------------------------------------------------------
add_seconds(Timestamp, Duration) when is_binary(Timestamp) ->
    Timestamp;
add_seconds(DateTime, Duration) ->
    Seconds = calendar:datetime_to_gregorian_seconds(DateTime),
    NewSeconds = Seconds + round(Duration),
    calendar:gregorian_seconds_to_datetime(NewSeconds).

%%--------------------------------------------------------------------
%% @doc
%% 解码token字段并解密
%% @end
%%--------------------------------------------------------------------
decode_token_field(Json) ->
    try
        Map = jsx:decode(Json, [return_maps]),
        case maps:get(<<"enc">>, Map, undefined) of
            undefined -> Map;
            Enc -> jsx:decode(garmin_client_service:decrypt_token(Enc), [return_maps])
        end
    catch
        _:Reason2 ->
            jsx:decode(Json, [return_maps])
    end.
