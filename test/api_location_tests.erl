%%%-------------------------------------------------------------------
%%% @author eadm
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%% api_location模块的单元测试
%%% 测试轨迹数据查询、参数验证、距离计算等功能
%%% @end
%%% Created : 2024-12-20
%%%-------------------------------------------------------------------
-module(api_location_tests).
-author("eadm").

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% 测试用例 - 参数验证
%%%===================================================================

%% 测试设备类型验证
validate_device_type_test_() ->
    [
        ?_assertEqual(true, api_location:validate_device_type(<<"garmin">>)),
        ?_assertEqual(true, api_location:validate_device_type(<<"watch">>)),
        ?_assertEqual(true, api_location:validate_device_type(<<"car">>)),
        ?_assertEqual(true, api_location:validate_device_type(<<"device">>)),
        ?_assertEqual(true, api_location:validate_device_type(<<"yedgns">>)),
        ?_assertEqual(true, api_location:validate_device_type(<<"racebox">>)),
        ?_assertEqual(false, api_location:validate_device_type(<<"invalid">>)),
        ?_assertEqual(false, api_location:validate_device_type(<<>>)),
        ?_assertEqual(false, api_location:validate_device_type(<<"unknown">>))
    ].

%% 测试时间戳验证
validate_timestamp_test_() ->
    [
        %% 有效的时间戳
        ?_assertMatch({{2024, 1, 1}, {0, 0, 0}}, 
                      api_location:validate_timestamp(<<"2024-01-01T00:00:00Z">>)),
        ?_assertMatch({{2024, 12, 31}, {23, 59, 59}}, 
                      api_location:validate_timestamp(<<"2024-12-31T23:59:59Z">>)),
        
        %% 无效的时间戳
        ?_assertEqual(false, api_location:validate_timestamp(<<"2024-13-01T00:00:00Z">>)),  %% 无效月份
        ?_assertEqual(false, api_location:validate_timestamp(<<"2024-01-32T00:00:00Z">>)),  %% 无效日期
        ?_assertEqual(false, api_location:validate_timestamp(<<"2024-01-01T25:00:00Z">>)),  %% 无效小时
        ?_assertEqual(false, api_location:validate_timestamp(<<"2024-01-01T00:60:00Z">>)),  %% 无效分钟
        ?_assertEqual(false, api_location:validate_timestamp(<<"invalid">>)),
        ?_assertEqual(false, api_location:validate_timestamp(<<>>))
    ].

%% 测试时间范围验证
validate_time_range_test_() ->
    StartTime = {{2024, 1, 1}, {0, 0, 0}},
    EndTime1Day = {{2024, 1, 2}, {0, 0, 0}},
    EndTime7Days = {{2024, 1, 8}, {0, 0, 0}},
    EndTime8Days = {{2024, 1, 9}, {0, 0, 0}},
    
    [
        %% 有效的时间范围
        ?_assertEqual(ok, api_location:validate_time_range(StartTime, EndTime1Day)),
        ?_assertEqual(ok, api_location:validate_time_range(StartTime, EndTime7Days)),
        
        %% 无效的时间范围 - 超过7天
        ?_assertMatch({error, _}, api_location:validate_time_range(StartTime, EndTime8Days)),
        
        %% 无效的时间范围 - 开始时间晚于结束时间
        ?_assertMatch({error, _}, api_location:validate_time_range(EndTime1Day, StartTime))
    ].

%%%===================================================================
%%% 测试用例 - 坐标验证
%%%===================================================================

%% 测试坐标有效性验证
is_valid_coordinate_test_() ->
    [
        %% 有效坐标
        ?_assertEqual(true, api_location:is_valid_coordinate(39.916527, 116.397128)),  %% 北京
        ?_assertEqual(true, api_location:is_valid_coordinate(-33.8688, 151.2093)),     %% 悉尼
        ?_assertEqual(true, api_location:is_valid_coordinate(90.0, 180.0)),            %% 边界值
        ?_assertEqual(true, api_location:is_valid_coordinate(-90.0, -180.0)),          %% 边界值
        
        %% 无效坐标 - 零点
        ?_assertEqual(false, api_location:is_valid_coordinate(0.0, 0.0)),
        
        %% 无效坐标 - 超出范围
        ?_assertEqual(false, api_location:is_valid_coordinate(91.0, 116.0)),
        ?_assertEqual(false, api_location:is_valid_coordinate(-91.0, 116.0)),
        ?_assertEqual(false, api_location:is_valid_coordinate(39.0, 181.0)),
        ?_assertEqual(false, api_location:is_valid_coordinate(39.0, -181.0))
    ].

%%%===================================================================
%%% 测试用例 - 距离计算
%%%===================================================================

%% 测试Haversine距离计算
haversine_distance_test_() ->
    %% 北京天安门到故宫（约1公里）
    Lat1 = 39.916527,
    Lng1 = 116.397128,
    Lat2 = 39.916628,
    Lng2 = 116.397228,
    
    Distance = api_location:haversine_distance(Lat1, Lng1, Lat2, Lng2),
    
    [
        %% 距离应该大于0
        ?_assert(Distance > 0.0),
        
        %% 距离应该在合理范围内（约15米）
        ?_assert(Distance < 20.0),
        
        %% 相同点之间的距离应该为0
        ?_assertEqual(0.0, api_location:haversine_distance(Lat1, Lng1, Lat1, Lng1))
    ].

%% 测试总距离计算
calculate_total_distance_test_() ->
    %% 空轨迹
    EmptyTrack = [],
    
    %% 单点轨迹
    SinglePoint = [#{
        <<"latitude">> => 39.916527,
        <<"longitude">> => 116.397128
    }],
    
    %% 两点轨迹
    TwoPoints = [
        #{<<"latitude">> => 39.916527, <<"longitude">> => 116.397128},
        #{<<"latitude">> => 39.916628, <<"longitude">> => 116.397228}
    ],
    
    %% 三点轨迹
    ThreePoints = [
        #{<<"latitude">> => 39.916527, <<"longitude">> => 116.397128},
        #{<<"latitude">> => 39.916628, <<"longitude">> => 116.397228},
        #{<<"latitude">> => 39.916728, <<"longitude">> => 116.397328}
    ],
    
    [
        %% 空轨迹距离为0
        ?_assertEqual(0.0, api_location:calculate_total_distance(EmptyTrack)),
        
        %% 单点轨迹距离为0
        ?_assertEqual(0.0, api_location:calculate_total_distance(SinglePoint)),
        
        %% 两点轨迹距离大于0
        ?_assert(api_location:calculate_total_distance(TwoPoints) > 0.0),
        
        %% 三点轨迹距离应该大于两点轨迹
        ?_assert(api_location:calculate_total_distance(ThreePoints) > 
                 api_location:calculate_total_distance(TwoPoints))
    ].

%%%===================================================================
%%% 测试用例 - 时长计算
%%%===================================================================

%% 测试持续时间计算
calculate_duration_test_() ->
    %% 空轨迹
    EmptyTrack = [],
    
    %% 单点轨迹
    SinglePoint = [#{
        <<"timestamp">> => <<"2024-01-01T00:00:00Z">>
    }],
    
    %% 两点轨迹（相隔1小时）
    TwoPoints = [
        #{<<"timestamp">> => <<"2024-01-01T00:00:00Z">>},
        #{<<"timestamp">> => <<"2024-01-01T01:00:00Z">>}
    ],
    
    %% 两点轨迹（相隔1天）
    TwoPointsOneDay = [
        #{<<"timestamp">> => <<"2024-01-01T00:00:00Z">>},
        #{<<"timestamp">> => <<"2024-01-02T00:00:00Z">>}
    ],
    
    [
        %% 空轨迹时长为0
        ?_assertEqual(0, api_location:calculate_duration(EmptyTrack)),
        
        %% 单点轨迹时长为0
        ?_assertEqual(0, api_location:calculate_duration(SinglePoint)),
        
        %% 两点轨迹（1小时）时长为3600秒
        ?_assertEqual(3600, api_location:calculate_duration(TwoPoints)),
        
        %% 两点轨迹（1天）时长为86400秒
        ?_assertEqual(86400, api_location:calculate_duration(TwoPointsOneDay))
    ].

%%%===================================================================
%%% 测试用例 - 数据格式化
%%%===================================================================

%% 测试轨迹点格式化
format_track_point_test_() ->
    %% 完整数据
    FullData = {
        {{2024, 1, 1}, {12, 0, 0}},  %% timestamp
        39.916527,                    %% latitude
        116.397128,                   %% longitude
        50.5,                         %% altitude
        2.5,                          %% speed
        120                           %% heartrate
    },
    
    %% 部分数据（null值）
    PartialData = {
        {{2024, 1, 1}, {12, 0, 0}},
        39.916527,
        116.397128,
        null,
        null,
        null
    },
    
    FormattedFull = api_location:format_track_point(FullData, <<"garmin">>),
    FormattedPartial = api_location:format_track_point(PartialData, <<"watch">>),
    
    [
        %% 完整数据格式化
        ?_assertEqual(<<"2024-01-01T12:00:00Z">>, maps:get(<<"timestamp">>, FormattedFull)),
        ?_assertEqual(39.916527, maps:get(<<"latitude">>, FormattedFull)),
        ?_assertEqual(116.397128, maps:get(<<"longitude">>, FormattedFull)),
        ?_assertEqual(50.5, maps:get(<<"altitude">>, FormattedFull)),
        ?_assertEqual(2.5, maps:get(<<"speed">>, FormattedFull)),
        ?_assertEqual(120, maps:get(<<"heartrate">>, FormattedFull)),
        ?_assertEqual(<<"garmin">>, maps:get(<<"device_type">>, FormattedFull)),
        
        %% 部分数据格式化（null值处理）
        ?_assertEqual(null, maps:get(<<"altitude">>, FormattedPartial)),
        ?_assertEqual(null, maps:get(<<"speed">>, FormattedPartial)),
        ?_assertEqual(null, maps:get(<<"heartrate">>, FormattedPartial)),
        ?_assertEqual(<<"watch">>, maps:get(<<"device_type">>, FormattedPartial))
    ].

%% 测试时间戳格式化
format_timestamp_test_() ->
    [
        ?_assertEqual(<<"2024-01-01T00:00:00Z">>, 
                      api_location:format_timestamp({{2024, 1, 1}, {0, 0, 0}})),
        ?_assertEqual(<<"2024-12-31T23:59:59Z">>, 
                      api_location:format_timestamp({{2024, 12, 31}, {23, 59, 59}})),
        ?_assertEqual(<<"test">>, 
                      api_location:format_timestamp(<<"test">>)),
        ?_assertEqual(<<>>, 
                      api_location:format_timestamp(invalid))
    ].

%%%===================================================================
%%% 测试用例 - 类型转换
%%%===================================================================

%% 测试浮点数转换
to_float_test_() ->
    [
        ?_assertEqual(0.0, api_location:to_float(null)),
        ?_assertEqual(3.14, api_location:to_float(3.14)),
        ?_assertEqual(42.0, api_location:to_float(42)),
        ?_assertEqual(3.14, api_location:to_float(<<"3.14">>)),
        ?_assertEqual(0.0, api_location:to_float(<<"invalid">>)),
        ?_assertEqual(0.0, api_location:to_float(invalid_atom))
    ].

%% 测试整数转换
to_int_or_null_test_() ->
    [
        ?_assertEqual(null, api_location:to_int_or_null(null)),
        ?_assertEqual(42, api_location:to_int_or_null(42)),
        ?_assertEqual(3, api_location:to_int_or_null(3.14)),
        ?_assertEqual(42, api_location:to_int_or_null(<<"42">>)),
        ?_assertEqual(null, api_location:to_int_or_null(<<"invalid">>)),
        ?_assertEqual(null, api_location:to_int_or_null(invalid_atom))
    ].

%%%===================================================================
%%% 测试用例 - 错误处理
%%%===================================================================

%% 测试错误信息格式化
format_error_test_() ->
    [
        ?_assertMatch(<<_/binary>>, api_location:format_error(timeout)),
        ?_assertMatch(<<_/binary>>, api_location:format_error(no_connection)),
        ?_assertMatch(<<_/binary>>, api_location:format_error(unknown_error))
    ].

%%%===================================================================
%%% 测试用例 - 时间戳解析
%%%===================================================================

%% 测试时间戳解析
parse_timestamp_test_() ->
    [
        ?_assertEqual({{2024, 1, 1}, {0, 0, 0}}, 
                      api_location:parse_timestamp(<<"2024-01-01T00:00:00Z">>)),
        ?_assertEqual({{2024, 12, 31}, {23, 59, 59}}, 
                      api_location:parse_timestamp(<<"2024-12-31T23:59:59Z">>)),
        ?_assertEqual(false, api_location:parse_timestamp(<<"invalid">>)),
        ?_assertEqual(false, api_location:parse_timestamp(<<>>))
    ].

%%%===================================================================
%%% 集成测试用例
%%%===================================================================

%% 测试完整的参数验证流程
validate_params_integration_test_() ->
    [
        %% 有效参数
        ?_assertEqual(ok, 
                      api_location:validate_params(
                          <<"garmin">>, 
                          <<"2024-01-01T00:00:00Z">>, 
                          <<"2024-01-02T00:00:00Z">>)),
        
        %% 无效设备类型
        ?_assertMatch({error, _}, 
                      api_location:validate_params(
                          <<"invalid">>, 
                          <<"2024-01-01T00:00:00Z">>, 
                          <<"2024-01-02T00:00:00Z">>)),
        
        %% 无效时间格式
        ?_assertMatch({error, _}, 
                      api_location:validate_params(
                          <<"garmin">>, 
                          <<"invalid">>, 
                          <<"2024-01-02T00:00:00Z">>)),
        
        %% 时间范围倒置
        ?_assertMatch({error, _}, 
                      api_location:validate_params(
                          <<"garmin">>, 
                          <<"2024-01-02T00:00:00Z">>, 
                          <<"2024-01-01T00:00:00Z">>))
    ].

%%%===================================================================
%%% 测试用例 - 错误分类和处理
%%%===================================================================

%% 测试错误分类
classify_error_test_() ->
    [
        %% 超时错误
        ?_assertMatch({timeout_error, _}, 
                      api_location:classify_error({error, timeout})),
        ?_assertMatch({timeout_error, _}, 
                      api_location:classify_error(timeout)),
        
        %% 连接错误
        ?_assertMatch({connection_error, _}, 
                      api_location:classify_error({error, no_connection})),
        ?_assertMatch({connection_error, _}, 
                      api_location:classify_error({error, closed})),
        ?_assertMatch({connection_error, _}, 
                      api_location:classify_error({error, econnrefused})),
        ?_assertMatch({connection_error, _}, 
                      api_location:classify_error(no_connection)),
        
        %% 未知错误
        ?_assertMatch({unknown_error, _}, 
                      api_location:classify_error(unknown)),
        ?_assertMatch({unknown_error, _}, 
                      api_location:classify_error({error, unknown}))
    ].

%% 测试统一错误处理函数
handle_query_error_test_() ->
    [
        %% 连接错误应返回503
        ?_assertMatch({status, 503, #{}, #{
            <<"success">> := false,
            <<"error">> := <<"service_unavailable">>,
            <<"message">> := _
        }}, api_location:handle_query_error({error, no_connection}, <<"test_context">>)),
        
        %% 超时错误应返回JSON响应
        ?_assertMatch({json, #{
            <<"success">> := false,
            <<"error">> := <<"query_timeout">>,
            <<"message">> := _
        }}, api_location:handle_query_error({error, timeout}, <<"test_context">>)),
        
        %% 未知错误应返回JSON响应
        ?_assertMatch({json, #{
            <<"success">> := false,
            <<"error">> := <<"internal_error">>,
            <<"message">> := _
        }}, api_location:handle_query_error(unknown_error, <<"test_context">>))
    ].

%%%===================================================================
%%% 测试用例 - 数据脱敏
%%%===================================================================

%% 测试用户ID脱敏
sanitize_user_id_test_() ->
    [
        %% 正常长度的用户ID
        ?_assertEqual(<<"tes***er">>, api_location:sanitize_user_id(<<"testuser">>)),
        ?_assertEqual(<<"adm***01">>, api_location:sanitize_user_id(<<"admin001">>)),
        
        %% 短用户ID
        ?_assertEqual(<<"***">>, api_location:sanitize_user_id(<<"abc">>)),
        ?_assertEqual(<<"***">>, api_location:sanitize_user_id(<<"ab">>)),
        
        %% 边界情况 - 5个字符也太短
        ?_assertEqual(<<"***">>, api_location:sanitize_user_id(<<"abcde">>)),
        %% 6个字符：前3位 + *** + 后2位
        ?_assertEqual(<<"abc***ef">>, api_location:sanitize_user_id(<<"abcdef">>))
    ].

%% 测试GPS坐标脱敏
sanitize_coordinate_test_() ->
    [
        %% 浮点数坐标
        ?_assertEqual(<<"39.9***">>, api_location:sanitize_coordinate(39.916527)),
        ?_assertEqual(<<"116.3***">>, api_location:sanitize_coordinate(116.397128)),
        ?_assertEqual(<<"-33.8***">>, api_location:sanitize_coordinate(-33.8688)),
        
        %% 整数坐标
        ?_assertEqual(<<"39***">>, api_location:sanitize_coordinate(39)),
        ?_assertEqual(<<"116***">>, api_location:sanitize_coordinate(116)),
        
        %% 无效输入
        ?_assertEqual(<<"***">>, api_location:sanitize_coordinate(invalid)),
        ?_assertEqual(<<"***">>, api_location:sanitize_coordinate(null))
    ].

%%%===================================================================
%%% 测试用例 - 权限和角色
%%%===================================================================

%% 测试管理员角色判断
is_admin_role_test_() ->
    [
        %% 管理员角色
        ?_assertEqual(true, api_location:is_admin_role(<<"超级管理员">>)),
        ?_assertEqual(true, api_location:is_admin_role(<<"admin">>)),
        ?_assertEqual(true, api_location:is_admin_role(<<"administrator">>)),
        
        %% 非管理员角色
        ?_assertEqual(false, api_location:is_admin_role(<<"user">>)),
        ?_assertEqual(false, api_location:is_admin_role(<<"guest">>)),
        ?_assertEqual(false, api_location:is_admin_role(<<>>)),
        ?_assertEqual(false, api_location:is_admin_role(invalid))
    ].

%%%===================================================================
%%% 边界测试用例
%%%===================================================================

%% 测试极端坐标值
extreme_coordinates_test_() ->
    [
        %% 北极和南极
        ?_assertEqual(true, api_location:is_valid_coordinate(90.0, 0.0)),
        ?_assertEqual(true, api_location:is_valid_coordinate(-90.0, 0.0)),
        
        %% 国际日期变更线
        ?_assertEqual(true, api_location:is_valid_coordinate(0.0, 180.0)),
        ?_assertEqual(true, api_location:is_valid_coordinate(0.0, -180.0)),
        
        %% 超出边界
        ?_assertEqual(false, api_location:is_valid_coordinate(90.1, 0.0)),
        ?_assertEqual(false, api_location:is_valid_coordinate(-90.1, 0.0)),
        ?_assertEqual(false, api_location:is_valid_coordinate(0.0, 180.1)),
        ?_assertEqual(false, api_location:is_valid_coordinate(0.0, -180.1))
    ].

%% 测试大量轨迹点的距离计算性能
large_track_distance_test() ->
    %% 生成1000个轨迹点
    TrackPoints = lists:map(fun(I) ->
        #{
            <<"latitude">> => 39.916527 + I * 0.0001,
            <<"longitude">> => 116.397128 + I * 0.0001
        }
    end, lists:seq(1, 1000)),
    
    %% 计算距离应该能够快速完成
    StartTime = erlang:monotonic_time(millisecond),
    Distance = api_location:calculate_total_distance(TrackPoints),
    EndTime = erlang:monotonic_time(millisecond),
    
    %% 验证结果
    ?assert(Distance > 0.0),
    ?assert(EndTime - StartTime < 1000).  %% 应该在1秒内完成

%% 测试空值和边界情况
null_and_boundary_test_() ->
    [
        %% 空二进制
        ?_assertEqual(false, api_location:validate_device_type(<<>>)),
        ?_assertEqual(false, api_location:validate_timestamp(<<>>)),
        
        %% null值转换
        ?_assertEqual(0.0, api_location:to_float(null)),
        ?_assertEqual(null, api_location:to_int_or_null(null)),
        
        %% 边界时间
        ?_assertMatch({{2024, 1, 1}, {0, 0, 0}}, 
                      api_location:validate_timestamp(<<"2024-01-01T00:00:00Z">>)),
        ?_assertMatch({{2024, 12, 31}, {23, 59, 59}}, 
                      api_location:validate_timestamp(<<"2024-12-31T23:59:59Z">>))
    ].

%%%===================================================================
%%% 错误恢复测试
%%%===================================================================

%% 测试错误信息的完整性
error_message_completeness_test_() ->
    [
        %% 所有错误类型都应该返回非空消息
        ?_assert(byte_size(api_location:format_error(timeout)) > 0),
        ?_assert(byte_size(api_location:format_error(no_connection)) > 0),
        ?_assert(byte_size(api_location:format_error(unknown)) > 0),
        
        %% 错误分类应该返回有效的元组
        ?_assertMatch({_, Msg} when is_binary(Msg), 
                      api_location:classify_error({error, timeout})),
        ?_assertMatch({_, Msg} when is_binary(Msg), 
                      api_location:classify_error({error, no_connection})),
        ?_assertMatch({_, Msg} when is_binary(Msg), 
                      api_location:classify_error(unknown))
    ].

%% 测试日志记录不会崩溃
logging_safety_test_() ->
    [
        %% 脱敏函数应该总是返回二进制
        ?_assert(is_binary(api_location:sanitize_user_id(<<"test">>))),
        ?_assert(is_binary(api_location:sanitize_user_id(<<>>))),
        ?_assert(is_binary(api_location:sanitize_coordinate(39.916527))),
        ?_assert(is_binary(api_location:sanitize_coordinate(invalid)))
    ].
