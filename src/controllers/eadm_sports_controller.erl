%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2026, REDGREAT
%%% @doc
%%% 运动数据控制器
%%% 处理运动数据相关的HTTP请求
%%% @end
%%% Created : 2026-01-23
%%%-------------------------------------------------------------------
-module(eadm_sports_controller).

-author("wangcw").

-export([
    list_activities/1,
    activity_detail/1,
    public_activity/1,
    share_page/1,
    delete_activity/1
]).

%%====================================================================
%% API functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% 获取用户的活动列表
%% @end
%%--------------------------------------------------------------------
list_activities(#{req := Req} = _Params) ->
    UserId = get_user_id(Req),

    %% 获取查询参数
    Qs = cowboy_req:parse_qs(Req),
    Page = list_to_integer(binary_to_list(proplists:get_value(<<"page">>, Qs, <<"1">>))),
    PageSize =
        list_to_integer(binary_to_list(proplists:get_value(<<"pageSize">>, Qs, <<"20">>))),

    Offset = (Page - 1) * PageSize,

    %% 查询数据库
    SQL = <<
        "SELECT id, actname, acttype, distance, starttime, "
        "\r\n                   timespan, avgheartbeat, calorie, "
        "ispublic, sharetoken\r\n            FROM sp_activity "
        "\r\n            WHERE userid = $1 \r\n            ORDER BY "
        "starttime DESC \r\n            LIMIT $2 OFFSET $3"
    >>,

    case eadm_pgpool:equery(SQL, [UserId, PageSize, Offset]) of
        {ok, _, Rows} ->
            %% 统计总数
            CountSQL = <<"SELECT COUNT(*) FROM sp_activity WHERE userid = $1">>,
            {ok, _, [{Total}]} = eadm_pgpool:equery(CountSQL, [UserId]),

            Activities = lists:map(fun format_activity_row/1, Rows),

            {json, #{
                <<"code">> => 200,
                <<"data">> =>
                    #{
                        <<"list">> => Activities,
                        <<"total">> => Total,
                        <<"page">> => Page,
                        <<"pageSize">> => PageSize
                    }
            }};
        {error, Reason} ->
            {json, #{
                <<"code">> => 500, <<"message">> => iolist_to_binary(io_lib:format("~p", [Reason]))
            }}
    end.

%%--------------------------------------------------------------------
%% @doc
%% 获取活动详情
%% @end
%%--------------------------------------------------------------------
activity_detail(#{bindings := #{id := IdBin}, req := Req} = _Params) ->
    UserId = get_user_id(Req),
    ActivityId = IdBin,

    SQL = <<
        "SELECT * FROM sp_activity \r\n            WHERE id = "
        "$1 AND userid = $2"
    >>,

    case eadm_pgpool:equery(SQL, [ActivityId, UserId]) of
        {ok, Columns, [Row]} ->
            Activity = format_activity_detail(Columns, Row),

            %% 获取streams数据
            StreamsSQL =
                <<
                    "SELECT streamtype, streamjson \r\n                       "
                    "   FROM sp_stream \r\n                       "
                    "   WHERE actid = $1"
                >>,
            {ok, _, StreamRows} = eadm_pgpool:equery(StreamsSQL, [ActivityId]),

            Streams =
                lists:map(
                    fun({Type, Data}) -> #{<<"type">> => Type, <<"data">> => Data} end,
                    StreamRows
                ),

            {json, #{<<"code">> => 200, <<"data">> => Activity#{<<"streams">> => Streams}}};
        {ok, _, []} ->
            {json, #{<<"code">> => 404, <<"message">> => <<"Activity not found">>}};
        {error, Reason} ->
            {json, #{
                <<"code">> => 500, <<"message">> => iolist_to_binary(io_lib:format("~p", [Reason]))
            }}
    end.

%%--------------------------------------------------------------------
%% @doc
%% 公开访问活动(通过share_token)
%% @end
%%--------------------------------------------------------------------
public_activity(#{bindings := #{<<"shareId">> := ShareToken}} = _Params) ->
    SQL = <<
        "SELECT * FROM sp_activity \r\n            WHERE sharetoken "
        "= $1 AND ispublic = true"
    >>,

    case eadm_pgpool:equery(SQL, [ShareToken]) of
        {ok, Columns, [Row]} ->
            Activity = format_activity_detail(Columns, Row),

            %% 根据隐私设置过滤数据
            FilteredActivity = filter_by_privacy(Activity),

            %% 获取streams数据(如果允许)
            Streams =
                case maps:get(<<"hidemap">>, Activity, false) of
                    false ->
                        ActivityId = maps:get(<<"id">>, Activity),
                        StreamsSQL =
                            <<
                                "SELECT streamtype, streamjson \r\n                       "
                                "           FROM sp_stream \r\n               "
                                "                   WHERE actid = $1 AND streamtype = "
                                "'latlng'"
                            >>,
                        {ok, _, StreamRows} = eadm_pgpool:equery(StreamsSQL, [ActivityId]),
                        lists:map(
                            fun({Type, Data}) -> #{<<"type">> => Type, <<"data">> => Data} end,
                            StreamRows
                        );
                    true ->
                        []
                end,

            {json, #{<<"code">> => 200, <<"data">> => FilteredActivity#{<<"streams">> => Streams}}};
        {ok, _, []} ->
            {status, 404};
        {error, Reason} ->
            {json, #{
                <<"code">> => 500, <<"message">> => iolist_to_binary(io_lib:format("~p", [Reason]))
            }}
    end.

share_page(#{bindings := #{<<"shareId">> := ShareToken}} = _Params) ->
    SQL = <<
        "SELECT * FROM sp_activity \r\n            WHERE sharetoken "
        "= $1 AND ispublic = true"
    >>,

    case eadm_pgpool:equery(SQL, [ShareToken]) of
        {ok, Columns, [Row]} ->
            Activity = format_activity_detail(Columns, Row),
            ActivityView = format_share_activity(Activity),
            {MapCoordinates, HasMapData} =
                case maps:get(<<"hidemap">>, Activity, false) of
                    false ->
                        ActivityId = maps:get(<<"id">>, Activity),
                        StreamsSQL =
                            <<
                                "SELECT streamtype, streamjson \r\n                       "
                                "           FROM sp_stream \r\n               "
                                "                   WHERE actid = $1 AND streamtype = "
                                "'latlng'"
                            >>,
                        case eadm_pgpool:equery(StreamsSQL, [ActivityId]) of
                            {ok, _, StreamRows} ->
                                decode_stream_coordinates(StreamRows);
                            _ ->
                                {[], false}
                        end;
                    true ->
                        {[], false}
                end,
            ShareUrl = <<"/share/", ShareToken/binary>>,
            {ok,
                [
                    {activity, ActivityView},
                    {has_map_data, HasMapData},
                    {map_coordinates, json:encode(MapCoordinates)},
                    {share_url, ShareUrl}
                ],
                #{view => eadm_share}};
        {ok, _, []} ->
            {status, 404};
        {error, _Reason} ->
            {status, 500}
    end.

%%--------------------------------------------------------------------
%% @doc
%% 删除活动
%% @end
%%--------------------------------------------------------------------
delete_activity(#{bindings := #{id := IdBin}, req := Req} = _Params) ->
    UserId = get_user_id(Req),
    ActivityId = IdBin,

    SQL = <<
        "DELETE FROM sp_activity \r\n            WHERE id = $1 "
        "AND userid = $2 \r\n            RETURNING id"
    >>,

    case eadm_pgpool:equery(SQL, [ActivityId, UserId]) of
        {ok, _, [{_}]} ->
            {json, #{<<"code">> => 200, <<"message">> => <<"Activity deleted successfully">>}};
        {ok, _, []} ->
            {json, #{<<"code">> => 404, <<"message">> => <<"Activity not found">>}};
        {error, Reason} ->
            {json, #{
                <<"code">> => 500, <<"message">> => iolist_to_binary(io_lib:format("~p", [Reason]))
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
    %% 从session或token中获取用户ID
    case cowboy_req:header(<<"authorization">>, Req) of
        undefined ->
            %% 从session获取
            #{<<"userid">> := UserId} = nova_session:get(Req),
            UserId;
        _Token ->
            %% TODO: 从token解析用户ID
            1
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc 格式化活动行数据
%%--------------------------------------------------------------------
format_activity_row(
    {Id, Name, Type, Distance, StartTime, Duration, AvgHR, Calories, IsPublic, ShareToken}
) ->
    #{
        <<"id">> => Id,
        <<"name">> => Name,
        <<"type">> => Type,
        <<"distance">> => Distance,
        <<"startTime">> => format_timestamp(StartTime),
        <<"duration">> => Duration,
        <<"avgHeartRate">> => AvgHR,
        <<"calories">> => Calories,
        <<"isPublic">> => IsPublic,
        <<"shareToken">> => ShareToken,
        <<"shareUrl">> =>
            case IsPublic of
                true ->
                    <<"/share/", ShareToken/binary>>;
                false ->
                    null
            end
    }.

%%--------------------------------------------------------------------
%% @private
%% @doc 格式化活动详情
%%--------------------------------------------------------------------
format_activity_detail(Columns, Row) ->
    %% 将Column names和Row values组合成map
    ColumnNames = [Name || {column, Name, _, _, _, _} <- Columns],
    maps:from_list(
        lists:zip(ColumnNames, tuple_to_list(Row))
    ).

%%--------------------------------------------------------------------
%% @private
%% @doc 根据隐私设置过滤数据
%%--------------------------------------------------------------------
filter_by_privacy(Activity) ->
    %% 如果hidestats为true,隐藏统计数据
    case maps:get(<<"hidestats">>, Activity, false) of
        true ->
            maps:without(
                [
                    <<"avgheartbeat">>,
                    <<"maxheartbeat">>,
                    <<"avgpower">>,
                    <<"maxpower">>,
                    <<"avgcadence">>,
                    <<"maxcadence">>
                ],
                Activity
            );
        false ->
            Activity
    end.

format_share_activity(Activity) ->
    Distance = maps:get(<<"distance">>, Activity, null),
    Duration = maps:get(<<"timespan">>, Activity, null),
    AvgSpeed = maps:get(<<"avgspeed">>, Activity, null),
    #{
        activity_name => maps:get(<<"actname">>, Activity, null),
        activity_type => maps:get(<<"acttype">>, Activity, null),
        start_time => format_timestamp(maps:get(<<"starttime">>, Activity, null)),
        distance => Distance,
        duration => Duration,
        avg_heart_rate => maps:get(<<"avgheartbeat">>, Activity, null),
        calories => maps:get(<<"calorie">>, Activity, null),
        elevation_gain => maps:get(<<"elevationgain">>, Activity, null),
        avg_speed => AvgSpeed,
        hide_stats => maps:get(<<"hidestats">>, Activity, false),
        hide_map => maps:get(<<"hidemap">>, Activity, false),
        distance_km => format_distance_km(Distance),
        duration_formatted => format_duration(Duration),
        avg_speed_kmh => format_speed_kmh(AvgSpeed)
    }.

decode_stream_coordinates(StreamRows) ->
    Coordinates =
        lists:foldl(
            fun({_Type, Data}, Acc) ->
                Decoded =
                    case Data of
                        Bin when is_binary(Bin) ->
                            try json:decode(Bin) of
                                V -> V
                            catch
                                _:_ -> []
                            end;
                        V ->
                            V
                    end,
                case is_list(Decoded) of
                    true -> Acc ++ Decoded;
                    false -> Acc
                end
            end,
            [],
            StreamRows
        ),
    {Coordinates, Coordinates =/= []}.

format_distance_km(null) ->
    null;
format_distance_km(Distance) when is_number(Distance) ->
    (Distance / 1000).

format_speed_kmh(null) ->
    null;
format_speed_kmh(Speed) when is_number(Speed) ->
    (Speed * 3.6).

format_duration(null) ->
    null;
format_duration(Seconds) when is_number(Seconds) ->
    Total = trunc(Seconds),
    Hours = Total div 3600,
    Minutes = (Total rem 3600) div 60,
    Secs = Total rem 60,
    iolist_to_binary(io_lib:format("~2..0w:~2..0w:~2..0w", [Hours, Minutes, Secs])).

%%--------------------------------------------------------------------
%% @private
%% @doc 格式化时间戳
%%--------------------------------------------------------------------
format_timestamp({{Y, M, D}, {H, Mi, S}}) ->
    iolist_to_binary(
        io_lib:format(
            "~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
            [Y, M, D, H, Mi, S]
        )
    );
format_timestamp(Timestamp) when is_binary(Timestamp) ->
    Timestamp;
format_timestamp(_) ->
    null.
