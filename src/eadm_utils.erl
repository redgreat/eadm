%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%% @doc
%%
%% eadm utils.
%%
%% @end
%%% Created : 2024-01-23 17:36:53
%%%-------------------------------------------------------------------
-module(eadm_utils).
-author("wangcw").

%% define
-define(DATE_TIME_PATTERN, <<"^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$">>).

-type pg_col() :: {column, binary(), any(), any(), any(), any(), any(), any(), any()}.
-type pg_row() :: tuple().
-type json_map() :: #{binary() => any()}.

%%%===================================================================
%%% Application callbacks
%%%===================================================================
-export([to_json/1, get_exp_bin/0]).
-export([
    as_map/1, as_map/3,
    return_as_map/1, return_as_map/2,
    return_as_json/1, return_as_json/2,
    validate_date_time/1,
    time_diff/2,
    utc_to_cts/1,
    cts_to_utc/1,
    pass_encrypt/1,
    validate_login/2,
    verify_password/2,
    current_date_binary/0,
    yesterday_date_binary/0,
    lastyear_date_binary/0,
    parse_date_time/1,
    pg_as_map/2,
    pg_as_json/2,
    convert_to_array/1,
    pg_as_jsonmap/1,
    pg_as_jsondata/1,
    pg_as_list/1,
    binary_to_float/1,
    log_info/1,
    parse_time/1
]).

%%====================================================================
%% API 函数
%%====================================================================

%% @private
%% @doc
%% 将Erlang数据结构转换为JSON二进制字符串（使用json模块编码）
%% @end
to_json(Data) ->
    try
        Encoded = json:encode(Data),
        case Encoded of
            {ok, Bin} when is_binary(Bin) -> Bin;
            Bin when is_binary(Bin) -> Bin;
            Iolist when is_list(Iolist) -> iolist_to_binary(Iolist);
            Other -> iolist_to_binary(Other)
        end
    catch
        ErrorType:ErrorReason ->
            lager:error("JSON编码失败: ~p:~p, 数据: ~p", [ErrorType, ErrorReason, Data]),
            <<"{}">>
    end.

%% @doc
%% 获取session过期时间
%% @end
-spec get_exp_bin() -> integer().
get_exp_bin() ->
    ExpExtend =
        case application:get_env(nova, session_expire, 3600) of
            Val when is_integer(Val) -> Val;
            _ -> 3600
        end,
    erlang:system_time(seconds) + ExpExtend.

%% @doc
%% 将MySQL查询结果转换为map格式
%% @end
-spec as_map({ok, [binary()], [tuple()]}) -> [json_map()].
as_map({ok, ColumnNames, Rows}) ->
    as_map(ColumnNames, Rows, []).

-spec as_map([binary()], [tuple()], [json_map()]) -> [json_map()].
as_map(ColumnNames, [Row | RestRows], Acc) ->
    RowList = erlang:tuple_to_list(Row),
    Pairs = lists:zip(ColumnNames, RowList),
    TransformedPairs = [{K, element(1, transform_value(K, V))} || {K, V} <- Pairs],
    Map = maps:from_list(TransformedPairs),
    case is_map(Map) of
        true -> as_map(ColumnNames, RestRows, [Map | Acc]);
        false -> as_map(ColumnNames, RestRows, Acc)
    end;
as_map(_ColumnNames, [], Acc) ->
    lists:reverse(Acc).

%% @doc
%% 将MySQL查询结果转换为HTTP返回的map格式，添加返回状态
%% @end
return_as_map({ok, Columns, Rows}) ->
    return_as_map(Columns, Rows).

return_as_map(Columns, Rows) ->
    #{<<"data">> => as_map(Columns, Rows, [])}.

%% @doc
%% mysql-otp 查询结果返回nova框架所需格式数据
%% @end
return_as_json({ok, Columns, Rows}) ->
    return_as_json(Columns, Rows).

return_as_json(Columns, Rows) ->
    #{columns => Columns, data => as_map(Columns, Rows, [])}.

%% @doc
%% epgsql返回结果转换为erlang的map格式
%% @end
-spec pg_as_map([pg_col()], [pg_row()]) -> [json_map()].
pg_as_map(ResCol, ResData) ->
    ColumnNames = [Name || {column, Name, _, _, _, _, _, _, _} <- ResCol],
    [maps:from_list(lists:zip(ColumnNames, erlang:tuple_to_list(Row))) || Row <- ResData].

%% @doc
%% epgsql返回结果转换为erlang的带列名的json格式
%% @end
-spec pg_as_json([pg_col()], [pg_row()]) -> #{columns => [binary()], data => [json_map()]}.
pg_as_json(ResCol, ResData) ->
    #{
        columns => [Name || {column, Name, _, _, _, _, _, _, _} <- ResCol],
        data => pg_as_map(ResCol, ResData)
    }.

%% @doc
%% epgsql返回结果转换为map格式data
%% @end
pg_as_jsonmap(ResData) ->
    {ResBin} = erlang:hd(ResData),
    ResBin.

%% @doc
%% epgsql返回结果转换为json格式data
%% @end
-spec pg_as_jsondata([pg_row()]) -> any().
pg_as_jsondata(ResData) ->
    case erlang:hd(ResData) of
        {ResBin} when is_binary(ResBin) ->
            {ok, RetuenData} = json:decode(ResBin),
            RetuenData;
        _ ->
            #{}
    end.

%% @doc
%% epgsql返回结果转换为list格式data
%% @end
pg_as_list(ResData) ->
    [ResList] = [[A, B, C] || {A, B, C} <- ResData],
    ResList.

%% @doc
%% 校验字符串是否为时间格式
%% @end
validate_date_time(DateTimeBin) ->
    case re:run(DateTimeBin, ?DATE_TIME_PATTERN, [{capture, none}, global]) of
        match ->
            true;
        nomatch ->
            false
    end.

%% @doc
%% 计算两二进制格式时间字符串(&lt;&lt;"2024-02-12 09:16:28"&gt;&gt;)时间差(秒).
%% @end
time_diff(DateTimeStrA, DateTimeStrB) ->
    ASeconds = calendar:datetime_to_gregorian_seconds(parse_date_time(DateTimeStrA)),
    BSeconds = calendar:datetime_to_gregorian_seconds(parse_date_time(DateTimeStrB)),
    DiffSeconds = erlang:abs(BSeconds - ASeconds),
    DiffSeconds.

%% @doc
%% 将 UTC 时间转换为 +8 时区的时间.
%% @end
utc_to_cts(DateTimeBin) ->
    OraDateTime = parse_date_time(DateTimeBin),
    Seconds = calendar:datetime_to_gregorian_seconds(OraDateTime),
    NewSeconds = Seconds + 28800,
    NewDateTime = calendar:gregorian_seconds_to_datetime(NewSeconds),
    str_from_datetime(NewDateTime).

%% @doc
%% 将+8 时区的时间转换为 UTC 时间.
%% @end
cts_to_utc(DateTimeBin) ->
    OraDateTime = parse_date_time(DateTimeBin),
    Seconds = calendar:datetime_to_gregorian_seconds(OraDateTime),
    NewSeconds = Seconds - 28800,
    NewDateTime = calendar:gregorian_seconds_to_datetime(NewSeconds),
    str_from_datetime(NewDateTime).

%% @doc
%% 查询pg经纬度，转换为列表
%% @end
convert_to_array(Coords) ->
    Response = [[erlang:binary_to_float(Lat), erlang:binary_to_float(Lng)] || {Lat, Lng} <- Coords],
    Response.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private
%% @doc
%% 时间转换器，将Erlang时间格式转换为ISO8601格式
%% @end
transform_value(_, {{Year, Month, Day}, {Hour, Minute, Second}}) when
    erlang:is_integer(Year),
    erlang:is_integer(Month),
    erlang:is_integer(Day),
    erlang:is_integer(Hour),
    erlang:is_integer(Minute),
    erlang:is_integer(Second)
->
    TimeStr =
        % 带时区格式 2024-02-13T13:32:12Z
        % io_lib:fwrite("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ", [Year, Month, Day, Hour, Minute, Second]),
        % 不带时区格式 2024-02-13 13:32:20
        io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w", [
            Year, Month, Day, Hour, Minute, Second
        ]),
    {list_to_binary(TimeStr), true};
transform_value(_, Value) ->
    {Value, false}.

%% @private
%% @doc
%% 二进制时间格式(&lt;&lt;"YYYY-MM-DD HH:II:SS"&gt;&gt;)转换为erl时间{{Year, Month, Day}, {Hour, Minute, Second}}.
%% @end
parse_date_time(DateTimeBin) ->
    [DateStr, TimeStr] = re:split(DateTimeBin, <<" ">>, [{return, binary}]),
    {{Year, Month, Day}, {Hour, Minute, Second}} =
        {date_from_binary(DateStr), time_from_binary(TimeStr)},
    {{Year, Month, Day}, {Hour, Minute, Second}}.

%% @private
%% @doc
%% 日期字符串(&lt;&lt;"YYYY-MM-DD"&gt;&gt;或&lt;&lt;"YYYY/MM/DD"&gt;&gt;)转换为erl日期{{Year, Month, Day}}.
%% @end
date_from_binary(DateBin) ->
    case binary:split(DateBin, <<"-">>, [global]) of
        [Year, Month, Day] ->
            binary:split(DateBin, <<"-">>, [global]),
            {binary_to_integer(Year), binary_to_integer(Month), binary_to_integer(Day)};
        _ ->
            [Year, Month, Day] = binary:split(DateBin, <<"/">>, [global]),
            {binary_to_integer(Year), binary_to_integer(Month), binary_to_integer(Day)}
    end.

%% @private
%% @doc
%% 时间字符串(&lt;&lt;"HH:II:SS"&gt;&gt;)转换为erl时间{{Hour, Minute, Second}}.
%% @end
time_from_binary(TimeBin) ->
    [Hour, Minute, Second] = binary:split(TimeBin, <<":">>, [global]),
    {binary_to_integer(Hour), binary_to_integer(Minute), binary_to_integer(Second)}.

%% @private
%% @doc
%% erl时间{{Year, Month, Day}, {Hour, Minute, Second}}转换为时间字符串(&lt;&lt;"YYYY-MM-DD HH:II:SS"&gt;&gt;).
%% @end
str_from_datetime(DateTime) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = DateTime,
    iolist_to_binary(
        io_lib:format(
            "~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
            [Year, Month, Day, Hour, Minute, Second]
        )
    ).

%% @private
%% @doc
%% 获取当前日期二进制字符串(&lt;&lt;"YYYY-MM-DD"&gt;&gt;).
%% @end
current_date_binary() ->
    {{Year, Month, Day}, _} = calendar:universal_time(),
    DateBin = list_to_binary(io_lib:format("~4..0B-~2..0B-~2..0B", [Year, Month, Day])),
    DateBin.

%% @private
%% @doc
%% 获取昨日日期二进制字符串(&lt;&lt;"YYYY-MM-DD"&gt;&gt;).
%% @end
yesterday_date_binary() ->
    {{Year, Month, Day}, _} = calendar:universal_time(),
    TodayDays = calendar:date_to_gregorian_days({Year, Month, Day}),
    YesterdayDays = TodayDays - 1,
    {YesterdayYear, YesterdayMonth, YesterdayDay} = calendar:gregorian_days_to_date(YesterdayDays),
    YesterdayBin = list_to_binary(
        io_lib:format("~4..0B-~2..0B-~2..0B", [YesterdayYear, YesterdayMonth, YesterdayDay])
    ),
    YesterdayBin.

%% @private
%% @doc
%% 获取去年日期二进制字符串(&lt;&lt;"YYYY-MM-DD"&gt;&gt;).
%% @end
lastyear_date_binary() ->
    {{Year, Month, Day}, _} = calendar:universal_time(),
    TodayDays = calendar:date_to_gregorian_days({Year, Month, Day}),
    DaysInAYear =
        case calendar:is_leap_year(Year) of
            true -> 366;
            false -> 365
        end,
    LastYearDays = TodayDays - DaysInAYear,
    {LastYear, LastYearMonth, LastYearDay} = calendar:gregorian_days_to_date(LastYearDays),
    LastYearBin = list_to_binary(
        io_lib:format("~4..0B-~2..0B-~2..0B", [LastYear, LastYearMonth, LastYearDay])
    ),
    LastYearBin.

%% @private
%% @doc
%% 密码加密.
%% @end
-spec pass_encrypt(binary()) -> binary().
pass_encrypt(PassBin) ->
    SecretKey =
        case application:get_env(nova, secret_key, <<>>) of
            SKey when is_binary(SKey) -> SKey;
            _ -> <<>>
        end,
    EncryptPwd = crypto:hash(sha256, <<SecretKey/binary, PassBin/binary>>),
    base64:encode(EncryptPwd).

-include("eadm_mnesia.hrl").

%% @doc
%% 验证密码
%% @end
validate_login(LoginName, Password) ->
    case eadm_mnesia_api:find_by_field(eadm_user, loginname, LoginName) of
        [#eadm_user{passwd = DbPassword, userstatus = DbUserStatus, deleted = false} | _] ->
            case DbUserStatus of
                0 ->
                    verify_password(Password, DbPassword);
                1 ->
                    3;
                _ ->
                    4
            end;
        _ ->
            2
    end.

%% @doc
%% 密码加密解密-验证密码
%% @end
-spec verify_password(binary(), binary()) -> boolean().
verify_password(Pwd, DbPwd) ->
    SecretKey =
        case application:get_env(nova, secret_key, <<>>) of
            SKey when is_binary(SKey) -> SKey;
            _ -> <<>>
        end,
    HPwd = crypto:hash(sha256, <<SecretKey/binary, Pwd/binary>>),
    DbPwdBin = base64:decode(DbPwd),
    HPwd =:= DbPwdBin.

%% @doc
%% 将二进制字符串转换为浮点数
%% @end
binary_to_float(Binary) ->
    ResList = binary_to_list(Binary),
    ResFloat = list_to_float(ResList),
    ResFloat.

%% @doc
%% 记录信息日志，用于定时任务
%% @end
log_info(Message) when is_list(Message) ->
    % 获取当前执行的任务ID
    JobId =
        case erlang:get(current_job_id) of
            undefined -> <<"unknown">>;
            Id -> Id
        end,

    % 记录到日志文件
    lager:info("Crontab Task [~s]: ~s", [JobId, Message]),

    % 记录到数据库
    try
        % 将消息转换为二进制格式
        MessageBin =
            if
                is_list(Message) -> list_to_binary(Message);
                true -> Message
            end,

        % 插入到数据库
        eadm_pgpool:equery(
            pool_pg,
            "insert into sys_cronlog(cronid, cronlog) values($1, $2);",
            [JobId, MessageBin]
        )
    catch
        ErrorType:ErrorReason:Stacktrace ->
            lager:error("任务日志记录失败: ~p:~p~n~p", [ErrorType, ErrorReason, Stacktrace])
    end,
    ok;
log_info(Message) when is_binary(Message) ->
    log_info(binary_to_list(Message));
log_info(Message) ->
    log_info(io_lib:format("~p", [Message])).

%% @doc
%% 解析时间字符串为 {Hour, Minute, Second} 格式或 unlimited
%% 用于 ecron 任务的时间解析
%% @end
parse_time(null) ->
    unlimited;
parse_time(undefined) ->
    unlimited;
parse_time(<<>>) ->
    unlimited;
parse_time(TimeBin) when is_binary(TimeBin) ->
    try
        % 尝试解析时间字符串，格式为 "YYYY-MM-DD HH:MM:SS"
        [_DateStr, TimeStr] = binary:split(TimeBin, <<" ">>),
        [HourStr, MinStr, SecStr] = binary:split(TimeStr, <<":">>, [global]),
        {binary_to_integer(HourStr), binary_to_integer(MinStr), binary_to_integer(SecStr)}
    catch
        _:_ ->
            lager:info("无法解析时间字符串: ~p，使用默认值 unlimited", [TimeBin]),
            unlimited
    end;
parse_time(_) ->
    unlimited.
