%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2026, REDGREAT
%%% @doc
%%%
%%% 移动端健康数据 API
%%%
%%% @end
%%% Created : 2026-02-26 02:00:00
%%%-------------------------------------------------------------------
-module(api_health).
-author("wangcw").

%%%===================================================================
%%% 函数导出
%%%===================================================================
-export([summary/1, heartrate/1, sleep/1, stress/1, spo2/1, respiration/1]).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc
%% 健康数据汇总（指定日期的心率/睡眠/压力/血氧/呼吸汇总）
%% GET /api/v1/health/summary?date=YYYY-MM-DD
%% @end
summary(Req) ->
    case api_auth:verify_token_from_req(Req) of
        {ok, _LoginName} ->
            try
                ParsedQs = maps:get(parsed_qs, Req, #{}),
                DateStr = maps:get(<<"date">>, ParsedQs, <<>>),
                case validate_date(DateStr) of
                    false ->
                        {json, #{<<"code">> => 400,
                                 <<"message">> => unicode:characters_to_binary("日期格式错误，请使用YYYY-MM-DD", utf8)}};
                    DateVal ->
                        %% 心率汇总
                        HrData = case eadm_pgpool:equery(pool_pg,
                            "SELECT restinghr, maxhr, minhr FROM garmin_heartrate WHERE hrdate = $1 LIMIT 1",
                            [DateVal]) of
                            {ok, _, [{RHr, MxHr, MnHr}]} ->
                                #{<<"restinghr">> => null_to_zero(RHr),
                                  <<"maxhr">> => null_to_zero(MxHr),
                                  <<"minhr">> => null_to_zero(MnHr)};
                            _ -> #{}
                        end,

                        %% 睡眠汇总
                        SleepData = case eadm_pgpool:equery(pool_pg,
                            "SELECT totalsleep, deepsleep, lightsleep, remsleep, awaketime, sleepscore,
                                    to_char(sleepstart, 'HH24:MI') as sleepstart_str,
                                    to_char(sleepend, 'HH24:MI') as sleepend_str
                             FROM garmin_sleep WHERE sleepdate = $1 LIMIT 1",
                            [DateVal]) of
                            {ok, _, [{TS, DS, LS, RS, AW, SC, SSt, SEn}]} ->
                                #{<<"totalsleep">> => null_to_zero(TS),
                                  <<"deepsleep">> => null_to_zero(DS),
                                  <<"lightsleep">> => null_to_zero(LS),
                                  <<"remsleep">> => null_to_zero(RS),
                                  <<"awaketime">> => null_to_zero(AW),
                                  <<"sleepscore">> => null_to_zero(SC),
                                  <<"sleepstart">> => null_to_empty(SSt),
                                  <<"sleepend">> => null_to_empty(SEn)};
                            _ -> #{}
                        end,

                        %% 压力汇总
                        StressData = case eadm_pgpool:equery(pool_pg,
                            "SELECT overalllevel, restduration, lowduration, mediumduration, highduration, stressscore
                             FROM garmin_stress WHERE stressdate = $1 LIMIT 1",
                            [DateVal]) of
                            {ok, _, [{OL, RD, LD, MD, HD, SS}]} ->
                                #{<<"overalllevel">> => null_to_zero(OL),
                                  <<"restduration">> => null_to_zero(RD),
                                  <<"lowduration">> => null_to_zero(LD),
                                  <<"mediumduration">> => null_to_zero(MD),
                                  <<"highduration">> => null_to_zero(HD),
                                  <<"stressscore">> => null_to_zero(SS)};
                            _ -> #{}
                        end,

                        %% 血氧汇总
                        Spo2Data = case eadm_pgpool:equery(pool_pg,
                            "SELECT avgspo2, lowspo2, highspo2, latestspo2
                             FROM garmin_spo2 WHERE spo2date = $1 LIMIT 1",
                            [DateVal]) of
                            {ok, _, [{AS, LS2, HS, LtS}]} ->
                                #{<<"avgspo2">> => null_to_zero_float(AS),
                                  <<"lowspo2">> => null_to_zero_float(LS2),
                                  <<"highspo2">> => null_to_zero_float(HS),
                                  <<"latestspo2">> => null_to_zero_float(LtS)};
                            _ -> #{}
                        end,

                        %% 呼吸汇总
                        RespData = case eadm_pgpool:equery(pool_pg,
                            "SELECT avgwaking, highwaking, lowwaking, avgsleeping, highsleeping, lowsleeping
                             FROM garmin_respiration WHERE respdate = $1 LIMIT 1",
                            [DateVal]) of
                            {ok, _, [{AW2, HW, LW, ASl, HSl, LSl}]} ->
                                #{<<"avgwaking">> => null_to_zero_float(AW2),
                                  <<"highwaking">> => null_to_zero_float(HW),
                                  <<"lowwaking">> => null_to_zero_float(LW),
                                  <<"avgsleeping">> => null_to_zero_float(ASl),
                                  <<"highsleeping">> => null_to_zero_float(HSl),
                                  <<"lowsleeping">> => null_to_zero_float(LSl)};
                            _ -> #{}
                        end,

                        {json, #{<<"code">> => 200,
                                 <<"data">> => #{
                                     <<"heartrate">> => HrData,
                                     <<"sleep">> => SleepData,
                                     <<"stress">> => StressData,
                                     <<"spo2">> => Spo2Data,
                                     <<"respiration">> => RespData
                                 }}}
                end
            catch
                _:Error ->
                    lager:error("健康汇总查询失败：~p", [Error]),
                    {json, #{<<"code">> => 500,
                             <<"message">> => unicode:characters_to_binary("查询失败", utf8)}}
            end;
        {error, Reason} ->
            {json, #{<<"code">> => 401,
                     <<"message">> => unicode:characters_to_binary(Reason, utf8)}}
    end.

%% @doc
%% 心率时序明细
%% GET /api/v1/health/heartrate?date=YYYY-MM-DD
%% @end
heartrate(Req) ->
    case api_auth:verify_token_from_req(Req) of
        {ok, _LoginName} ->
            try
                ParsedQs = maps:get(parsed_qs, Req, #{}),
                DateStr = maps:get(<<"date">>, ParsedQs, <<>>),
                case validate_date(DateStr) of
                    false ->
                        {json, #{<<"code">> => 400,
                                 <<"message">> => unicode:characters_to_binary("日期格式错误", utf8)}};
                    DateVal ->
                        case eadm_pgpool:equery(pool_pg,
                            "SELECT to_char(pointtime, 'HH24:MI') as time, heartrate
                             FROM garmin_heartrate_detail
                             WHERE hrdate = $1
                             ORDER BY pointtime ASC",
                            [DateVal]) of
                            {ok, _, ResData} ->
                                Points = [#{<<"time">> => T, <<"value">> => V} || {T, V} <- ResData],
                                {json, #{<<"code">> => 200, <<"data">> => Points}};
                            {error, Reason} ->
                                lager:warning("心率明细查询错误：~p", [Reason]),
                                {json, #{<<"code">> => 200, <<"data">> => []}}
                        end
                end
            catch
                _:Error ->
                    lager:error("心率明细查询失败：~p", [Error]),
                    {json, #{<<"code">> => 500,
                             <<"message">> => unicode:characters_to_binary("查询失败", utf8)}}
            end;
        {error, Reason} ->
            {json, #{<<"code">> => 401,
                     <<"message">> => unicode:characters_to_binary(Reason, utf8)}}
    end.

%% @doc
%% 睡眠阶段明细
%% GET /api/v1/health/sleep?date=YYYY-MM-DD
%% @end
sleep(Req) ->
    case api_auth:verify_token_from_req(Req) of
        {ok, _LoginName} ->
            try
                ParsedQs = maps:get(parsed_qs, Req, #{}),
                DateStr = maps:get(<<"date">>, ParsedQs, <<>>),
                case validate_date(DateStr) of
                    false ->
                        {json, #{<<"code">> => 400,
                                 <<"message">> => unicode:characters_to_binary("日期格式错误", utf8)}};
                    DateVal ->
                        case eadm_pgpool:equery(pool_pg,
                            "SELECT to_char(starttime, 'HH24:MI') as start_time,
                                    to_char(endtime, 'HH24:MI') as end_time,
                                    activitylevel
                             FROM garmin_sleep_detail
                             WHERE sleepdate = $1
                             ORDER BY starttime ASC",
                            [DateVal]) of
                            {ok, _, ResData} ->
                                Phases = [#{<<"startTime">> => ST, <<"endTime">> => ET,
                                            <<"level">> => level_to_float(AL)}
                                          || {ST, ET, AL} <- ResData],
                                {json, #{<<"code">> => 200, <<"data">> => Phases}};
                            {error, Reason} ->
                                lager:warning("睡眠明细查询错误：~p", [Reason]),
                                {json, #{<<"code">> => 200, <<"data">> => []}}
                        end
                end
            catch
                _:Error ->
                    lager:error("睡眠明细查询失败：~p", [Error]),
                    {json, #{<<"code">> => 500,
                             <<"message">> => unicode:characters_to_binary("查询失败", utf8)}}
            end;
        {error, Reason} ->
            {json, #{<<"code">> => 401,
                     <<"message">> => unicode:characters_to_binary(Reason, utf8)}}
    end.

%% @doc
%% 压力时序明细
%% GET /api/v1/health/stress?date=YYYY-MM-DD
%% @end
stress(Req) ->
    case api_auth:verify_token_from_req(Req) of
        {ok, _LoginName} ->
            try
                ParsedQs = maps:get(parsed_qs, Req, #{}),
                DateStr = maps:get(<<"date">>, ParsedQs, <<>>),
                case validate_date(DateStr) of
                    false ->
                        {json, #{<<"code">> => 400,
                                 <<"message">> => unicode:characters_to_binary("日期格式错误", utf8)}};
                    DateVal ->
                        case eadm_pgpool:equery(pool_pg,
                            "SELECT to_char(pointtime, 'HH24:MI') as time, stresslevel
                             FROM garmin_stress_detail
                             WHERE stressdate = $1
                             ORDER BY pointtime ASC",
                            [DateVal]) of
                            {ok, _, ResData} ->
                                Points = [#{<<"time">> => T, <<"value">> => V} || {T, V} <- ResData],
                                {json, #{<<"code">> => 200, <<"data">> => Points}};
                            {error, Reason} ->
                                lager:warning("压力明细查询错误：~p", [Reason]),
                                {json, #{<<"code">> => 200, <<"data">> => []}}
                        end
                end
            catch
                _:Error ->
                    lager:error("压力明细查询失败：~p", [Error]),
                    {json, #{<<"code">> => 500,
                             <<"message">> => unicode:characters_to_binary("查询失败", utf8)}}
            end;
        {error, Reason} ->
            {json, #{<<"code">> => 401,
                     <<"message">> => unicode:characters_to_binary(Reason, utf8)}}
    end.

%% @doc
%% 血氧时序明细
%% GET /api/v1/health/spo2?date=YYYY-MM-DD
%% @end
spo2(Req) ->
    case api_auth:verify_token_from_req(Req) of
        {ok, _LoginName} ->
            try
                ParsedQs = maps:get(parsed_qs, Req, #{}),
                DateStr = maps:get(<<"date">>, ParsedQs, <<>>),
                case validate_date(DateStr) of
                    false ->
                        {json, #{<<"code">> => 400,
                                 <<"message">> => unicode:characters_to_binary("日期格式错误", utf8)}};
                    DateVal ->
                        case eadm_pgpool:equery(pool_pg,
                            "SELECT to_char(pointtime, 'HH24:MI') as time, spo2value
                             FROM garmin_spo2_detail
                             WHERE spo2date = $1
                             ORDER BY pointtime ASC",
                            [DateVal]) of
                            {ok, _, ResData} ->
                                Points = [#{<<"time">> => T, <<"value">> => spo2_to_float(V)} || {T, V} <- ResData],
                                {json, #{<<"code">> => 200, <<"data">> => Points}};
                            {error, Reason} ->
                                lager:warning("血氧明细查询错误：~p", [Reason]),
                                {json, #{<<"code">> => 200, <<"data">> => []}}
                        end
                end
            catch
                _:Error ->
                    lager:error("血氧明细查询失败：~p", [Error]),
                    {json, #{<<"code">> => 500,
                             <<"message">> => unicode:characters_to_binary("查询失败", utf8)}}
            end;
        {error, Reason} ->
            {json, #{<<"code">> => 401,
                     <<"message">> => unicode:characters_to_binary(Reason, utf8)}}
    end.

%% @doc
%% 呼吸时序明细
%% GET /api/v1/health/respiration?date=YYYY-MM-DD
%% @end
respiration(Req) ->
    case api_auth:verify_token_from_req(Req) of
        {ok, _LoginName} ->
            try
                ParsedQs = maps:get(parsed_qs, Req, #{}),
                DateStr = maps:get(<<"date">>, ParsedQs, <<>>),
                case validate_date(DateStr) of
                    false ->
                        {json, #{<<"code">> => 400,
                                 <<"message">> => unicode:characters_to_binary("日期格式错误", utf8)}};
                    DateVal ->
                        case eadm_pgpool:equery(pool_pg,
                            "SELECT to_char(pointtime, 'HH24:MI') as time, respvalue
                             FROM garmin_respiration_detail
                             WHERE respdate = $1
                             ORDER BY pointtime ASC",
                            [DateVal]) of
                            {ok, _, ResData} ->
                                Points = [#{<<"time">> => T, <<"value">> => resp_to_float(V)} || {T, V} <- ResData],
                                {json, #{<<"code">> => 200, <<"data">> => Points}};
                            {error, Reason} ->
                                lager:warning("呼吸明细查询错误：~p", [Reason]),
                                {json, #{<<"code">> => 200, <<"data">> => []}}
                        end
                end
            catch
                _:Error ->
                    lager:error("呼吸明细查询失败：~p", [Error]),
                    {json, #{<<"code">> => 500,
                             <<"message">> => unicode:characters_to_binary("查询失败", utf8)}}
            end;
        {error, Reason} ->
            {json, #{<<"code">> => 401,
                     <<"message">> => unicode:characters_to_binary(Reason, utf8)}}
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @doc
%% 验证日期格式 YYYY-MM-DD，返回 {Year, Month, Day} 或 false
%% @end
validate_date(<<Y1, Y2, Y3, Y4, $-, M1, M2, $-, D1, D2>>) ->
    try
        Year = list_to_integer([Y1, Y2, Y3, Y4]),
        Month = list_to_integer([M1, M2]),
        Day = list_to_integer([D1, D2]),
        case calendar:valid_date(Year, Month, Day) of
            true -> {Year, Month, Day};
            false -> false
        end
    catch
        _:_ -> false
    end;
validate_date(_) -> false.

%% @doc
%% null 值处理
%% @end
null_to_zero(null) -> 0;
null_to_zero(Val) when is_integer(Val) -> Val;
null_to_zero(_) -> 0.

null_to_zero_float(null) -> 0.0;
null_to_zero_float({decimal, Val}) -> binary_to_float(Val);
null_to_zero_float(Val) when is_float(Val) -> Val;
null_to_zero_float(Val) when is_integer(Val) -> Val * 1.0;
null_to_zero_float(Val) ->
    try
        case is_binary(Val) of
            true -> binary_to_float(Val);
            false -> float(Val)
        end
    catch
        _:_ -> 0.0
    end.

null_to_empty(null) -> <<>>;
null_to_empty(Val) -> Val.

level_to_float(Val) ->
    try
        case is_float(Val) of
            true -> round(Val);
            false ->
                case is_integer(Val) of
                    true -> Val;
                    false ->
                        case is_binary(Val) of
                            true -> round(binary_to_float(Val));
                            false -> round(float(Val))
                        end
                end
        end
    catch
        _:_ -> 0
    end.

spo2_to_float(Val) -> null_to_zero_float(Val).
resp_to_float(Val) -> null_to_zero_float(Val).
