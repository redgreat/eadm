%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2026, REDGREAT
%%% @doc
%%% 位置追踪API模块
%%% 提供多数据源的GPS轨迹查询和聚合功能
%%% 支持设备类型：garmin、watch、car、device、yedgns、racebox
%%% @end
%%% Created : 2026-02-27 10:00:00
%%%-------------------------------------------------------------------
-module(api_location).
-author("wangcw").

%%%===================================================================
%%% 函数导出
%%%===================================================================
-export([index/1]).

%% 导出内部函数用于测试
-ifdef(TEST).
-export([
    validate_device_type/1,
    validate_timestamp/1,
    validate_time_range/2,
    validate_params/3,
    is_valid_coordinate/2,
    haversine_distance/4,
    calculate_total_distance/1,
    calculate_duration/1,
    format_track_point/2,
    format_timestamp/1,
    to_float/1,
    to_int_or_null/1,
    format_error/1,
    parse_timestamp/1,
    check_rate_limit/1,
    check_location_permission/2,
    get_user_role/1,
    is_admin_role/1,
    handle_query_error/2,
    classify_error/1,
    sanitize_user_id/1,
    sanitize_coordinate/1
]).
-endif.

%%%===================================================================
%%% 宏定义
%%%===================================================================
-define(MAX_TRACK_POINTS, 10000).  %% 最大轨迹点数
-define(MAX_TIME_RANGE_DAYS, 7).   %% 最大查询天数
-define(EARTH_RADIUS, 6371000.0).  %% 地球半径（米）
-define(MAX_REQUESTS_PER_MINUTE, 60).  %% 每分钟最大请求数
-define(RATE_LIMIT_TABLE, rate_limit_location).  %% 速率限制ETS表名

%%====================================================================
%% API 函数
%%====================================================================

%% @doc
%% HTTP请求入口函数
%% GET /api/location/track?device_type=garmin&start_time=2024-01-01T00:00:00Z&end_time=2024-01-01T23:59:59Z
%% @end
index(Req) ->
    case api_auth:verify_token_from_req(Req) of
        {ok, LoginName} ->
            %% 检查速率限制
            case check_rate_limit(LoginName) of
                {ok, allowed} ->
                    try
                        ParsedQs = maps:get(parsed_qs, Req, #{}),
                        DeviceType = maps:get(<<"device_type">>, ParsedQs, <<>>),
                        StartTime = maps:get(<<"start_time">>, ParsedQs, <<>>),
                        EndTime = maps:get(<<"end_time">>, ParsedQs, <<>>),
                        DeviceId = maps:get(<<"device_id">>, ParsedQs, undefined),
                        
                        %% 参数验证
                        case validate_params(DeviceType, StartTime, EndTime) of
                            ok ->
                                %% 权限检查
                                case check_location_permission(LoginName, DeviceId) of
                                    true ->
                                        %% 查询轨迹数据
                                        case query_track_data(DeviceType, StartTime, EndTime) of
                                            {ok, TrackData} ->
                                                %% 记录审计日志（成功）
                                                log_location_access(LoginName, DeviceType, StartTime, EndTime, <<"success">>),
                                                {json, #{
                                                    <<"success">> => true,
                                                    <<"data">> => TrackData
                                                }};
                                            {error, Reason} ->
                                                lager:error("查询轨迹数据失败: ~p", [Reason]),
                                                %% 记录审计日志（失败）
                                                log_location_access(LoginName, DeviceType, StartTime, EndTime, <<"failure">>),
                                                {json, #{
                                                    <<"success">> => false,
                                                    <<"error">> => <<"query_failed">>,
                                                    <<"message">> => format_error(Reason)
                                                }}
                                        end;
                                    false ->
                                        %% 记录审计日志（权限拒绝）
                                        log_location_access(LoginName, DeviceType, StartTime, EndTime, <<"forbidden">>),
                                        {status, 403, #{}, #{
                                            <<"success">> => false,
                                            <<"error">> => <<"forbidden">>,
                                            <<"message">> => unicode:characters_to_binary("无权限访问该设备数据", utf8)
                                        }}
                                end;
                            {error, Reason} ->
                                {json, #{
                                    <<"success">> => false,
                                    <<"error">> => <<"invalid_params">>,
                                    <<"message">> => Reason
                                }}
                        end
                    catch
                        _:Error ->
                            lager:error("位置追踪API异常: ~p", [Error]),
                            {json, #{
                                <<"success">> => false,
                                <<"error">> => <<"internal_error">>,
                                <<"message">> => unicode:characters_to_binary("查询失败，请联系管理员", utf8)
                            }}
                    end;
                {error, rate_limit_exceeded} ->
                    lager:warning("速率限制超出 - 用户:~p", [LoginName]),
                    {status, 429, #{}, #{
                        <<"success">> => false,
                        <<"error">> => <<"rate_limit_exceeded">>,
                        <<"message">> => unicode:characters_to_binary("请求过于频繁，请稍后重试", utf8)
                    }}
            end;
        {error, Reason} ->
            {json, #{
                <<"success">> => false,
                <<"error">> => <<"unauthorized">>,
                <<"message">> => unicode:characters_to_binary(Reason, utf8)
            }}
    end.

%%====================================================================
%% 内部函数 - 参数验证
%%====================================================================

%% @doc
%% 验证请求参数
%% @end
validate_params(DeviceType, StartTime, EndTime) ->
    case validate_device_type(DeviceType) of
        false ->
            {error, unicode:characters_to_binary("不支持的设备类型", utf8)};
        true ->
            case validate_timestamp(StartTime) of
                false ->
                    {error, unicode:characters_to_binary("开始时间格式无效", utf8)};
                StartDT ->
                    case validate_timestamp(EndTime) of
                        false ->
                            {error, unicode:characters_to_binary("结束时间格式无效", utf8)};
                        EndDT ->
                            case validate_time_range(StartDT, EndDT) of
                                ok -> ok;
                                {error, Reason} -> {error, Reason}
                            end
                    end
            end
    end.

%% @doc
%% 验证设备类型
%% @end
validate_device_type(DeviceType) ->
    ValidTypes = [<<"garmin">>, <<"watch">>, <<"car">>, <<"device">>, <<"yedgns">>, <<"racebox">>],
    lists:member(DeviceType, ValidTypes).

%% @doc
%% 验证时间戳格式（ISO 8601）
%% 返回 {Year, Month, Day, Hour, Minute, Second} 或 false
%% @end
validate_timestamp(<<Y1, Y2, Y3, Y4, $-, M1, M2, $-, D1, D2, $T,
                     H1, H2, $:, Min1, Min2, $:, S1, S2, $Z>>) ->
    try
        Year = list_to_integer([Y1, Y2, Y3, Y4]),
        Month = list_to_integer([M1, M2]),
        Day = list_to_integer([D1, D2]),
        Hour = list_to_integer([H1, H2]),
        Minute = list_to_integer([Min1, Min2]),
        Second = list_to_integer([S1, S2]),
        case calendar:valid_date(Year, Month, Day) andalso
             Hour >= 0 andalso Hour =< 23 andalso
             Minute >= 0 andalso Minute =< 59 andalso
             Second >= 0 andalso Second =< 59 of
            true -> {{Year, Month, Day}, {Hour, Minute, Second}};
            false -> false
        end
    catch
        _:_ -> false
    end;
validate_timestamp(_) -> false.

%% @doc
%% 验证时间范围
%% @end
validate_time_range(StartDT, EndDT) ->
    StartSec = calendar:datetime_to_gregorian_seconds(StartDT),
    EndSec = calendar:datetime_to_gregorian_seconds(EndDT),
    DiffSec = EndSec - StartSec,
    
    if
        DiffSec =< 0 ->
            {error, unicode:characters_to_binary("时间范围无效：开始时间必须早于结束时间", utf8)};
        DiffSec > ?MAX_TIME_RANGE_DAYS * 24 * 3600 ->
            {error, unicode:characters_to_binary("时间范围超过限制：最多查询7天", utf8)};
        true ->
            ok
    end.

%%====================================================================
%% 内部函数 - 轨迹数据查询
%%====================================================================

%% @doc
%% 查询轨迹数据（主函数）
%% @end
query_track_data(DeviceType, StartTime, EndTime) ->
    try
        %% 将 ISO 8601 字符串转换为 Erlang datetime 格式
        StartDT = parse_timestamp(StartTime),
        EndDT = parse_timestamp(EndTime),
        
        case {StartDT, EndDT} of
            {false, _} ->
                {error, <<"开始时间格式无效">>};
            {_, false} ->
                {error, <<"结束时间格式无效">>};
            {StartDateTime, EndDateTime} ->
                %% 根据设备类型路由到对应的查询函数
                TrackPoints = case DeviceType of
                    <<"garmin">> -> query_garmin_track(StartDateTime, EndDateTime);
                    <<"watch">> -> query_watch_location(StartDateTime, EndDateTime);
                    <<"car">> -> query_car_location(StartDateTime, EndDateTime);
                    <<"device">> -> query_device_data(StartDateTime, EndDateTime);
                    <<"yedgns">> -> query_yedgns_data(StartDateTime, EndDateTime);
                    <<"racebox">> -> query_racebox_data(StartDateTime, EndDateTime);
                    _ -> []
                end,
                
                %% 过滤无效坐标点
                ValidPoints = lists:filter(fun(Point) ->
                    Lat = maps:get(<<"latitude">>, Point),
                    Lng = maps:get(<<"longitude">>, Point),
                    is_valid_coordinate(Lat, Lng)
                end, TrackPoints),
                
                %% 限制返回点数
                LimitedPoints = lists:sublist(ValidPoints, ?MAX_TRACK_POINTS),
                
                %% 如果超过限制，记录警告日志
                case length(ValidPoints) > ?MAX_TRACK_POINTS of
                    true ->
                        lager:warning("轨迹点数量超过限制 - 设备类型:~p, 原始数量:~p, 限制后:~p",
                                     [DeviceType, length(ValidPoints), ?MAX_TRACK_POINTS]);
                    false ->
                        ok
                end,
                
                %% 计算统计信息
                TotalDistance = calculate_total_distance(LimitedPoints),
                Duration = calculate_duration(LimitedPoints),
                
                %% 构造响应数据
                {ok, #{
                    <<"device_type">> => DeviceType,
                    <<"track_points">> => LimitedPoints,
                    <<"total_count">> => length(LimitedPoints),
                    <<"total_distance">> => TotalDistance,
                    <<"duration">> => Duration
                }}
        end
    catch
        _:Error ->
            lager:error("查询轨迹数据异常 - 设备类型:~p, 错误:~p", [DeviceType, Error]),
            {error, Error}
    end.

%% @doc
%% 查询佳明设备轨迹
%% @end
query_garmin_track(StartDateTime, EndDateTime) ->
    SQL = "
        SELECT 
            pointtime,
            latitude,
            longitude,
            elevation AS altitude,
            speed,
            heartrate
        FROM garmin_activity_detail
        WHERE pointtime >= $1 AND pointtime <= $2
        ORDER BY pointtime ASC
        LIMIT $3
    ",
    
    case eadm_pgpool:equery(pool_pg, SQL, [StartDateTime, EndDateTime, ?MAX_TRACK_POINTS]) of
        {ok, _Columns, Rows} ->
            [format_track_point(Row, <<"garmin">>) || Row <- Rows];
        {error, Reason} ->
            lager:warning("佳明轨迹查询失败: ~p", [Reason]),
            []
    end.

%% @doc
%% 查询手表定位轨迹
%% @end
query_watch_location(StartDateTime, EndDateTime) ->
    SQL = "
        SELECT 
            ptime as timestamp,
            lat AS latitude,
            lng AS longitude,
            NULL as altitude,
            NULL as speed,
            NULL as heartrate
        FROM lc_watchlocation
        WHERE ptime >= $1 AND ptime <= $2
        ORDER BY ptime ASC
        LIMIT $3
    ",
    
    case eadm_pgpool:equery(pool_pg, SQL, [StartDateTime, EndDateTime, ?MAX_TRACK_POINTS]) of
        {ok, _Columns, Rows} ->
            [format_track_point(Row, <<"watch">>) || Row <- Rows];
        {error, Reason} ->
            lager:warning("手表定位查询失败: ~p", [Reason]),
            []
    end.

%% @doc
%% 查询车辆定位轨迹
%% @end
query_car_location(StartDateTime, EndDateTime) ->
    SQL = "
        SELECT 
            ptime as timestamp,
            lat AS latitude,
            lng AS longitude,
            NULL as altitude,
            NULL as speed,
            NULL as heartrate
        FROM lc_carlocdaily
        WHERE ptime >= $1 AND ptime <= $2
        ORDER BY ptime ASC
        LIMIT $3
    ",
    
    case eadm_pgpool:equery(pool_pg, SQL, [StartDateTime, EndDateTime, ?MAX_TRACK_POINTS]) of
        {ok, _Columns, Rows} ->
            [format_track_point(Row, <<"car">>) || Row <- Rows];
        {error, Reason} ->
            lager:warning("车辆定位查询失败: ~p", [Reason]),
            []
    end.

%% @doc
%% 查询设备数据轨迹
%% @end
query_device_data(StartDateTime, EndDateTime) ->
    SQL = "
        SELECT 
            receivetime as timestamp,
            lat AS latitude,
            lng AS longitude,
            NULL as altitude,
            NULL as speed,
            NULL as heartrate
        FROM emqx_device_data
        WHERE receivetime >= $1 AND receivetime <= $2
        ORDER BY receivetime ASC
        LIMIT $3
    ",
    
    case eadm_pgpool:equery(pool_pg, SQL, [StartDateTime, EndDateTime, ?MAX_TRACK_POINTS]) of
        {ok, _Columns, Rows} ->
            [format_track_point(Row, <<"device">>) || Row <- Rows];
        {error, Reason} ->
            lager:warning("设备数据查询失败: ~p", [Reason]),
            []
    end.

%% @doc
%% 查询野点GNS数据
%% @end
query_yedgns_data(StartDateTime, EndDateTime) ->
    SQL = "
        SELECT 
            gtime as timestamp,
            COALESCE(gpslat,lbslat) AS latitude,
            COALESCE(gpslng,lbslng) AS longitude,
            NULL as altitude,
            NULL as speed,
            NULL as heartrate
        FROM lc_yedgnss
        WHERE gtime >= $1 AND gtime <= $2
        ORDER BY gtime ASC
        LIMIT $3
    ",
    
    case eadm_pgpool:equery(pool_pg, SQL, [StartDateTime, EndDateTime, ?MAX_TRACK_POINTS]) of
        {ok, _Columns, Rows} ->
            [format_track_point(Row, <<"yedgns">>) || Row <- Rows];
        {error, Reason} ->
            lager:warning("野点GNS数据查询失败: ~p", [Reason]),
            []
    end.

%% @doc
%% 查询RaceBox数据
%% @end
query_racebox_data(StartDateTime, EndDateTime) ->
    SQL = "
        SELECT 
            insert_time as timestamp,
            latitude,
            longitude,
            NULL as altitude,
            NULL as speed,
            NULL as heartrate
        FROM lc_racebox
        WHERE insert_time >= $1 AND insert_time <= $2
        ORDER BY insert_time ASC
        LIMIT $3
    ",
    
    case eadm_pgpool:equery(pool_pg, SQL, [StartDateTime, EndDateTime, ?MAX_TRACK_POINTS]) of
        {ok, _Columns, Rows} ->
            [format_track_point(Row, <<"racebox">>) || Row <- Rows];
        {error, Reason} ->
            lager:warning("RaceBox数据查询失败: ~p", [Reason]),
            []
    end.

%%====================================================================
%% 内部函数 - 数据格式化和验证
%%====================================================================

%% @doc
%% 格式化轨迹点数据
%% @end
format_track_point({Timestamp, Lat, Lng, Alt, Speed, Hr}, DeviceType) ->
    #{
        <<"timestamp">> => format_timestamp(Timestamp),
        <<"latitude">> => to_float(Lat),
        <<"longitude">> => to_float(Lng),
        <<"altitude">> => to_float_or_null(Alt),
        <<"speed">> => to_float_or_null(Speed),
        <<"heartrate">> => to_int_or_null(Hr),
        <<"device_type">> => DeviceType
    }.

%% @doc
%% 格式化时间戳为ISO 8601格式
%% @end
format_timestamp({{Y, M, D}, {H, Min, S}}) when is_integer(S) ->
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
                                    [Y, M, D, H, Min, S]));
format_timestamp({{Y, M, D}, {H, Min, S}}) when is_float(S) ->
    % 如果秒数是浮点数，转换为整数
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
                                    [Y, M, D, H, Min, trunc(S)]));
format_timestamp(Timestamp) when is_binary(Timestamp) ->
    Timestamp;
format_timestamp(_) ->
    <<"">>.

%% @doc
%% 验证坐标有效性
%% @end
is_valid_coordinate(Lat, Lng) when is_number(Lat), is_number(Lng) ->
    Lat >= -90.0 andalso Lat =< 90.0 andalso
    Lng >= -180.0 andalso Lng =< 180.0 andalso
    not (Lat == 0.0 andalso Lng == 0.0);
is_valid_coordinate(_, _) ->
    false.

%% @doc
%% 转换为浮点数
%% @end
to_float(null) -> 0.0;
to_float({decimal, Val}) when is_binary(Val) ->
    try binary_to_float(Val)
    catch _:_ -> 0.0
    end;
to_float(Val) when is_float(Val) -> Val;
to_float(Val) when is_integer(Val) -> Val * 1.0;
to_float(Val) when is_binary(Val) ->
    try binary_to_float(Val)
    catch _:_ -> 0.0
    end;
to_float(_) -> 0.0.

%% @doc
%% 转换为浮点数或null
%% @end
to_float_or_null(null) -> null;
to_float_or_null(Val) -> to_float(Val).

%% @doc
%% 转换为整数或null
%% @end
to_int_or_null(null) -> null;
to_int_or_null(Val) when is_integer(Val) -> Val;
to_int_or_null(Val) when is_float(Val) -> round(Val);
to_int_or_null(Val) when is_binary(Val) ->
    try binary_to_integer(Val)
    catch _:_ -> null
    end;
to_int_or_null(_) -> null.

%%====================================================================
%% 内部函数 - 距离和时长计算
%%====================================================================

%% @doc
%% 计算总距离（使用Haversine公式）
%% @end
calculate_total_distance([]) -> 0.0;
calculate_total_distance([_]) -> 0.0;
calculate_total_distance(TrackPoints) ->
    calculate_total_distance(TrackPoints, 0.0).

calculate_total_distance([P1, P2 | Rest], Acc) ->
    Lat1 = maps:get(<<"latitude">>, P1),
    Lng1 = maps:get(<<"longitude">>, P1),
    Lat2 = maps:get(<<"latitude">>, P2),
    Lng2 = maps:get(<<"longitude">>, P2),
    
    Distance = haversine_distance(Lat1, Lng1, Lat2, Lng2),
    calculate_total_distance([P2 | Rest], Acc + Distance);
calculate_total_distance([_], Acc) ->
    Acc.

%% @doc
%% Haversine公式计算两点间距离（米）
%% @end
haversine_distance(Lat1, Lng1, Lat2, Lng2) ->
    %% 转换为弧度
    Lat1Rad = Lat1 * math:pi() / 180.0,
    Lat2Rad = Lat2 * math:pi() / 180.0,
    DLat = (Lat2 - Lat1) * math:pi() / 180.0,
    DLng = (Lng2 - Lng1) * math:pi() / 180.0,
    
    %% Haversine公式
    A = math:sin(DLat / 2.0) * math:sin(DLat / 2.0) +
        math:cos(Lat1Rad) * math:cos(Lat2Rad) *
        math:sin(DLng / 2.0) * math:sin(DLng / 2.0),
    C = 2.0 * math:atan2(math:sqrt(A), math:sqrt(1.0 - A)),
    
    ?EARTH_RADIUS * C.

%% @doc
%% 计算持续时间（秒）
%% @end
calculate_duration([]) -> 0;
calculate_duration([_]) -> 0;
calculate_duration(TrackPoints) ->
    FirstPoint = hd(TrackPoints),
    LastPoint = lists:last(TrackPoints),
    
    FirstTime = parse_timestamp(maps:get(<<"timestamp">>, FirstPoint)),
    LastTime = parse_timestamp(maps:get(<<"timestamp">>, LastPoint)),
    
    case {FirstTime, LastTime} of
        {false, _} -> 0;
        {_, false} -> 0;
        {FT, LT} ->
            FirstSec = calendar:datetime_to_gregorian_seconds(FT),
            LastSec = calendar:datetime_to_gregorian_seconds(LT),
            max(0, LastSec - FirstSec)
    end.

%% @doc
%% 解析ISO 8601时间戳
%% @end
parse_timestamp(<<Y1, Y2, Y3, Y4, $-, M1, M2, $-, D1, D2, $T,
                  H1, H2, $:, Min1, Min2, $:, S1, S2, $Z>>) ->
    try
        Year = list_to_integer([Y1, Y2, Y3, Y4]),
        Month = list_to_integer([M1, M2]),
        Day = list_to_integer([D1, D2]),
        Hour = list_to_integer([H1, H2]),
        Minute = list_to_integer([Min1, Min2]),
        Second = list_to_integer([S1, S2]),
        {{Year, Month, Day}, {Hour, Minute, Second}}
    catch
        _:_ -> false
    end;
parse_timestamp(_) -> false.

%%====================================================================
%% 内部函数 - 安全和权限控制
%%====================================================================

%% @doc
%% 检查速率限制
%% 使用ETS表实现每分钟最多60次请求的限制
%% @end
check_rate_limit(UserId) ->
    %% 确保ETS表存在
    ensure_rate_limit_table(),
    
    Key = {rate_limit, UserId},
    Now = erlang:system_time(second),
    
    case ets:lookup(?RATE_LIMIT_TABLE, Key) of
        [] ->
            %% 首次请求，插入记录
            ets:insert(?RATE_LIMIT_TABLE, {Key, 1, Now}),
            {ok, allowed};
        [{Key, Count, Timestamp}] ->
            if
                Now - Timestamp > 60 ->
                    %% 超过1分钟，重置计数器
                    ets:insert(?RATE_LIMIT_TABLE, {Key, 1, Now}),
                    {ok, allowed};
                Count >= ?MAX_REQUESTS_PER_MINUTE ->
                    %% 超过速率限制
                    {error, rate_limit_exceeded};
                true ->
                    %% 增加计数器
                    ets:update_counter(?RATE_LIMIT_TABLE, Key, {2, 1}),
                    {ok, allowed}
            end
    end.

%% @doc
%% 确保速率限制ETS表存在
%% @end
ensure_rate_limit_table() ->
    case ets:whereis(?RATE_LIMIT_TABLE) of
        undefined ->
            try
                ets:new(?RATE_LIMIT_TABLE, [named_table, public, set, {write_concurrency, true}])
            catch
                error:badarg ->
                    %% 表已存在（并发创建）
                    ok
            end;
        _ ->
            ok
    end.

%% @doc
%% 检查位置数据访问权限
%% 管理员可以访问所有设备，普通用户只能访问绑定的设备
%% @end
check_location_permission(LoginName, DeviceId) ->
    %% 检查用户角色
    case get_user_role(LoginName) of
        admin ->
            %% 管理员可以访问所有设备
            true;
        user ->
            %% 普通用户需要检查设备绑定关系
            case DeviceId of
                undefined ->
                    %% 如果没有指定设备ID，允许查询（返回所有绑定设备的数据）
                    true;
                _ ->
                    %% 检查设备是否绑定到该用户
                    is_device_bound_to_user(LoginName, DeviceId)
            end;
        _ ->
            %% 未知角色，拒绝访问
            false
    end.

%% @doc
%% 获取用户角色
%% 通过查询用户权限判断是否为管理员
%% @end
get_user_role(LoginName) ->
    try
        {ok, _, ResData} = eadm_pgpool:equery(
            pool_pg,
            "select c.rolename, c.rolepermission
             from eadm_user a
             inner join eadm_userrole b on b.userid = a.id and b.deleted is false
             inner join eadm_role c on c.id = b.roleid and c.rolestatus = 0 and c.deleted is false
             where a.loginname = $1 and a.userstatus = 0 and a.deleted is false
             limit 1",
            [LoginName]
        ),
        case ResData of
            [{RoleName, _Permission}] ->
                %% 判断是否为管理员角色
                case is_admin_role(RoleName) of
                    true -> admin;
                    false -> user
                end;
            _ ->
                user
        end
    catch
        _:Error ->
            lager:error("获取用户角色失败 - 用户:~p, 错误:~p", [LoginName, Error]),
            user
    end.

%% @doc
%% 判断角色名称是否为管理员角色
%% @end
is_admin_role(RoleName) when is_binary(RoleName) ->
    AdminRoles = [<<"超级管理员">>, <<"admin">>, <<"administrator">>],
    lists:member(RoleName, AdminRoles);
is_admin_role(_) ->
    false.

%% @doc
%% 检查设备是否绑定到用户
%% @end
is_device_bound_to_user(LoginName, DeviceId) ->
    try
        {ok, _, ResData} = eadm_pgpool:equery(
            pool_pg,
            "select count(*) from eadm_userdevice
             where loginname = $1 and deviceno = $2 and deleted is false",
            [LoginName, DeviceId]
        ),
        case ResData of
            [{Count}] when Count > 0 -> true;
            _ -> false
        end
    catch
        _:Error ->
            lager:error("检查设备绑定失败 - 用户:~p, 设备:~p, 错误:~p", [LoginName, DeviceId, Error]),
            false
    end.

%% @doc
%% 记录位置数据访问审计日志
%% @end
log_location_access(UserId, DeviceType, StartTime, EndTime, Result) ->
    %% 脱敏处理：只记录查询参数，不记录完整GPS坐标
    SanitizedUserId = sanitize_user_id(UserId),
    
    %% 记录到lager日志
    lager:info("位置数据访问 - 用户:~s, 设备类型:~s, 时间范围:~s~s, 结果:~s",
               [SanitizedUserId, DeviceType, StartTime, EndTime, Result]),
    
    %% 写入审计日志表
    try
        %% 构造详细信息字符串
        Details = iolist_to_binary(io_lib:format(
            "{\"device_type\":\"~s\",\"start_time\":\"~s\",\"end_time\":\"~s\"}",
            [DeviceType, StartTime, EndTime]
        )),
        
        eadm_pgpool:equery(
            pool_pg,
            "insert into audit_log(user_id, action, resource, timestamp, result, details)
             values($1, $2, $3, $4, $5, $6)",
            [UserId, <<"location_access">>, DeviceType, 
             calendar:universal_time(), Result, Details]
        )
    catch
        _:Error ->
            lager:error("写入审计日志失败 - 用户:~s, 错误:~p", [SanitizedUserId, Error])
    end.

%% @doc
%% 脱敏用户ID（只显示前3位和后2位）
%% @end
sanitize_user_id(UserId) when is_binary(UserId) ->
    Size = byte_size(UserId),
    if
        Size =< 5 ->
            %% 太短，用星号替换中间部分
            <<"***">>;
        true ->
            %% 显示前3位和后2位
            MiddleSize = Size - 5,
            <<Prefix:3/binary, _:MiddleSize/binary, Suffix:2/binary>> = UserId,
            <<Prefix/binary, "***", Suffix/binary>>
    end;
sanitize_user_id(UserId) ->
    %% 非二进制，转换后脱敏
    sanitize_user_id(iolist_to_binary(io_lib:format("~p", [UserId]))).

%% @doc
%% 脱敏GPS坐标（用于日志记录）
%% 只保留整数部分和一位小数
%% @end
sanitize_coordinate(Coord) when is_float(Coord) ->
    %% 保留一位小数
    Truncated = float(trunc(Coord * 10.0) / 10.0),
    iolist_to_binary(io_lib:format("~.1f***", [Truncated]));
sanitize_coordinate(Coord) when is_integer(Coord) ->
    iolist_to_binary(io_lib:format("~p***", [Coord]));
sanitize_coordinate(_) ->
    <<"***">>.

%%====================================================================
%% 内部函数 - 错误处理
%%====================================================================

%% @doc
%% 统一错误处理函数
%% 根据错误类型返回适当的HTTP状态码和错误信息
%% @end
handle_query_error(Error, Context) ->
    %% 记录详细错误日志（不使用已废弃的get_stacktrace）
    lager:error("数据库查询错误 - 上下文:~p, 错误:~p", [Context, Error]),
    
    %% 根据错误类型返回不同的响应
    case classify_error(Error) of
        {connection_error, Msg} ->
            %% 数据库连接失败，返回503
            {status, 503, #{}, #{
                <<"success">> => false,
                <<"error">> => <<"service_unavailable">>,
                <<"message">> => Msg
            }};
        {timeout_error, Msg} ->
            %% 查询超时，返回400
            {json, #{
                <<"success">> => false,
                <<"error">> => <<"query_timeout">>,
                <<"message">> => Msg
            }};
        {unknown_error, Msg} ->
            %% 未预期错误，返回500
            {json, #{
                <<"success">> => false,
                <<"error">> => <<"internal_error">>,
                <<"message">> => Msg
            }}
    end.

%% @doc
%% 分类错误类型
%% @end
classify_error({error, timeout}) ->
    {timeout_error, unicode:characters_to_binary("数据库查询超时，请缩短查询时间范围", utf8)};
classify_error({error, no_connection}) ->
    {connection_error, unicode:characters_to_binary("数据库连接失败，请稍后重试", utf8)};
classify_error({error, closed}) ->
    {connection_error, unicode:characters_to_binary("数据库连接已关闭，请稍后重试", utf8)};
classify_error({error, econnrefused}) ->
    {connection_error, unicode:characters_to_binary("无法连接到数据库服务器", utf8)};
classify_error(timeout) ->
    {timeout_error, unicode:characters_to_binary("数据库查询超时，请缩短查询时间范围", utf8)};
classify_error(no_connection) ->
    {connection_error, unicode:characters_to_binary("数据库连接失败，请稍后重试", utf8)};
classify_error(_) ->
    {unknown_error, unicode:characters_to_binary("查询失败，请联系管理员", utf8)}.

%% @doc
%% 格式化错误信息（保留用于向后兼容）
%% @end
format_error(timeout) ->
    unicode:characters_to_binary("数据库查询超时，请缩短查询时间范围", utf8);
format_error(no_connection) ->
    unicode:characters_to_binary("数据库连接失败，请稍后重试", utf8);
format_error(_) ->
    unicode:characters_to_binary("查询失败，请联系管理员", utf8).
