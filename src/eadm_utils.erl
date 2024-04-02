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

%%%===================================================================
%%% Application callbacks
%%%===================================================================
-export([to_json/1, get_exp_bin/0]).
-export([as_map/1, as_map/3, return_as_map/1, return_as_map/2, return_as_json/1, return_as_json/2,
    validate_date_time/1, time_diff/2, utc_to_cts/1, cts_to_utc/1, pass_encrypt/1, validate_login/2, verify_password/2]).

%%====================================================================
%% API functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% to_json
%%
%% @end
%%--------------------------------------------------------------------
to_json([Hd|Tl]) ->
    [to_json(Hd)|to_json(Tl)];
to_json(Tuple) when is_tuple(Tuple) ->
    to_json(erlang:tuple_to_list(Tuple));
to_json(Map) when is_map(Map) ->
    %% What should we do here? Nothing?
    maps:map(fun(Key, Value) ->
                     to_json(Value)
             end, Map);
to_json(Pid) when is_pid(Pid) ->
    erlang:list_to_binary(erlang:pid_to_list(Pid));
to_json(Port) when is_port(Port) ->
    erlang:list_to_binary(erlang:port_to_list(Port));
to_json(Ref) when is_reference(Ref) ->
    erlang:list_to_binary(erlang:ref_to_list(Ref));
to_json(Element) ->
    Element.

%% @doc
%% 获取session过期时间
%% @end
get_exp_bin() ->
    ExpExtend = application:get_env(nova, session_expire, 3600),
    erlang:system_time(seconds) + ExpExtend.

%% @doc
%% mysql-otp result to map.
%% @end
as_map({ok, ColumnNames, Rows}) ->
    as_map(ColumnNames, Rows, []).

as_map(ColumnNames, [Row | RestRows], Acc) ->
    Map = lists:foldl(
        fun({Key, Value}, AccMap) ->
            {TransformedValue, _IsTime} = transform_value(Key, Value),
            AccMap#{Key => TransformedValue}
        end,
        #{},
        lists:zip(ColumnNames, Row)
    ),
    as_map(ColumnNames, RestRows, [Map | Acc]);
as_map(_ColumnNames, [], Acc) ->
    lists:reverse(Acc).

%% @doc
%% mysql-otp result to map for http return, add ReturnStatus.
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
%% 计算两二进制格式时间字符串(<<"2024-02-12 09:16:28">>)时间差(秒).
%% @end
time_diff(DateTimeStrA, DateTimeStrB) ->
    ASeconds = calendar:datetime_to_gregorian_seconds(parse_date_time(DateTimeStrA)),
    BSeconds = calendar:datetime_to_gregorian_seconds(parse_date_time(DateTimeStrB)),
    DiffSeconds = abs(BSeconds - ASeconds),
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

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% @doc
%% Time convertor, erl to ISO8601.
%% @end
transform_value(_, {{Year, Month, Day}, {Hour, Minute, Second}}) when
    is_integer(Year), is_integer(Month), is_integer(Day), is_integer(Hour), is_integer(Minute), is_integer(Second)
->
    TimeStr =
        % 带时区格式 2024-02-13T13:32:12Z
        % io_lib:fwrite("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ", [Year, Month, Day, Hour, Minute, Second]),
        % 不带时区格式 2024-02-13 13:32:20
        io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w", [Year, Month, Day, Hour, Minute, Second]),
    {list_to_binary(TimeStr), true};
transform_value(_, Value) ->
    {Value, false}.

%% @private
%% @doc
%% 二进制时间格式(<<"YYYY-MM-DD HH:II:SS">>)转换为erl时间{{Year, Month, Day}, {Hour, Minute, Second}}.
%% @end
parse_date_time(DateTimeBin) ->
    [DateStr, TimeStr] = re:split(DateTimeBin, <<" ">>, [{return, binary}]),
    {{Year, Month, Day}, {Hour, Minute, Second}} =
        {date_from_binary(DateStr), time_from_binary(TimeStr)},
    {{Year, Month, Day}, {Hour, Minute, Second}}.

%% @private
%% @doc
%% 日期字符串(<<"YYYY-MM-DD">>)转换为erl日期{{Year, Month, Day}}.
%% @end
date_from_binary(DateBin) ->
    [Year, Month, Day] = binary:split(DateBin, <<"-">>, [global]),
    {binary_to_integer(Year), binary_to_integer(Month), binary_to_integer(Day)}.

%% @private
%% @doc
%% 时间字符串(<<"HH:II:SS">>)转换为erl时间{{Hour, Minute, Second}}.
%% @end
time_from_binary(TimeBin) ->
    [Hour, Minute, Second] = binary:split(TimeBin, <<":">>, [global]),
    {binary_to_integer(Hour), binary_to_integer(Minute), binary_to_integer(Second)}.

%% @private
%% @doc
%% erl时间{{Year, Month, Day}, {Hour, Minute, Second}}转换为时间字符串(<<"YYYY-MM-DD HH:II:SS">>).
%% @end
str_from_datetime(DateTime) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = DateTime,
    iolist_to_binary(io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
    [Year, Month, Day, Hour, Minute, Second])).

%% @private
%% @doc
%% 密码加密.
%% @end
pass_encrypt(PassBin) ->
    SecretKey = application:get_env(nova, secret_key, <<>>),
    EncryptPwd = crypto:hash(sha256, <<SecretKey/binary, PassBin/binary>>),
    base64:encode(EncryptPwd).

%% @doc
%% 验证密码
%% @end
validate_login(LoginName, Password) ->
    {ok, _, DbPassword} = mysql_pool:query(pool_db,
        "SELECT CryptoGram, UserStatus
        FROM eadm_user
        WHERE LoginName = ?
          AND Deleted = 0
        ORDER BY UpdatedAt DESC
        LIMIT 1;",
        [LoginName]),
        case DbPassword of
            [] ->
                2;
            _ ->
                case tl(hd(DbPassword)) of
                    [0] ->
                      verify_password(Password, hd(hd(DbPassword)));
                    [1] ->
                      3;
                    _ ->
                      4
                end
        end.

%% @doc
%% 密码加密解密-验证密码
%% @end
verify_password(Pwd, DbPwd) ->
    Secret_Key = application:get_env(nova, secret_key, <<>>),
    HPwd = crypto:hash(sha256, <<Secret_Key/binary, Pwd/binary>>),
    DbPwdBin = base64:decode(DbPwd),
    HPwd =:= DbPwdBin.

%%===================================================================
%% 内部函数
%%===================================================================
