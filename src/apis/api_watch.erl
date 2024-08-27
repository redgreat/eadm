%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%
%%% 手表 API
%%%
%%% @end
%%% Created : 2024-06-29 下午5:29
%%%-------------------------------------------------------------------
-module(api_watch).
-author("wangcw").

%%%===================================================================
%%% 函数导出
%%%===================================================================
-export([index/1]).

%%====================================================================
%% API 函数
%%====================================================================
%% @doc
%% 主函数
%% @end
index(#{params := Params}) ->
    lager:info("传入参数：~p~n", [Params]),
    MsgType = maps:get(<<"type">>, Params, <<"0">>),
    case MsgType of
        %% 数据
        <<"3">> ->
            % 基站数据
            try
                Lac = erlang:binary_to_integer(maps:get(<<"Lac">>, Params, <<"0">>)),
                Cid = erlang:binary_to_integer(maps:get(<<"cid">>, Params, <<"0">>)),
                Db = erlang:binary_to_integer(maps:get(<<"Db">>, Params, <<"0">>)),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, <<"1970/1/1 00:00:00">>)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchcell(ptime, lac, cid, db)
                  values($1, $2, $3, $4) on conflict (ptime)
                  do update set lac=excluded.lac, cid=excluded.cid, db=excluded.db;",
                    [BTUtcTime, Lac, Cid, Db]),
                #{<<"success">> => true}
            catch
                _:Error ->
                    lager:error("基站数据写入失败：~p~n", [Error])
            end;
        <<"4">> ->
            % 每日累计步数
            try
                Steps = erlang:binary_to_integer(maps:get(<<"steps">>, Params, <<"0">>)),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, <<"1970/1/1 00:00:00">>)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchstep(ptime, steps)
                  values($1, $2) on conflict (ptime)
                  do update set steps=excluded.steps;", [BTUtcTime, Steps]),
                #{<<"success">> => true}
            catch
                _:Error ->
                    lager:error("每日累计步数写入失败：~p~n", [Error])
            end;
        <<"5">> ->
            % WIFI 定位
            try
                Latitude = erlang:binary_to_float(maps:get(<<"Latitude">>, Params, <<"0">>)),
                Longitude = erlang:binary_to_float(maps:get(<<"Longitude">>, Params, <<"0">>)),
                TimeStr = eadm_utils:parse_date_time(maps:get(<<"timeStr">>, Params, <<"1970/1/1 00:00:00">>)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchlocation(ptime, lat, lng)
                  values($1, $2, $3) on conflict (ptime)
                  do update set lat=excluded.lat, lng=excluded.lng;", [TimeStr, Latitude, Longitude]),
                #{<<"success">> => true}
            catch
                _:Error ->
                    lager:error("WIFI定位写入失败：~p~n", [Error])
            end;
        <<"6">> ->
            % 心率数据
            try
                Heartbeat = erlang:binary_to_integer(maps:get(<<"heartbeat">>, Params, <<"0">>)),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, <<"1970/1/1 00:00:00">>)),
                lager:info("心率数据：~p~n", [Heartbeat]),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchhb(ptime, heartbeat)
                  values($1, $2) on conflict (ptime)
                  do update set heartbeat=excluded.heartbeat;", [BTUtcTime, Heartbeat]),
                #{<<"success">> => true}
            catch
                _:Error ->
                    lager:error("心率数据写入失败：~p~n", [Error])
            end;
        <<"8">> ->
            % 血压
            try
                Diastolic = erlang:binary_to_integer(maps:get(<<"diastolic">>, Params, <<"0">>)),
                Shrink = erlang:binary_to_integer(maps:get(<<"shrink">>, Params, <<"0">>)),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, <<"1970/1/1 00:00:00">>)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchbp(ptime, diastolic, shrink)
                  values($1, $2, $3) on conflict (ptime)
                  do update set diastolic=excluded.diastolic, shrink=excluded.shrink;",
                    [BTUtcTime, Diastolic, Shrink]),
                #{<<"success">> => true}
            catch
                _:Error ->
                    lager:error("血压数据写入失败：~p~n", [Error])
            end;
        <<"10">> ->
            % 血糖
            try
                BloodSugar = erlang:list_to_float(erlang:binary_to_list(
                    maps:get(<<"bloodSugar">>, Params, <<"0">>))),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, <<"1970/1/1 00:00:00">>)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchbs(ptime, bloodsugar)
                  values($1, $2) on conflict (ptime)
                  do update set bloodsugar=excluded.bloodsugar;", [BTUtcTime, BloodSugar]),
                #{<<"success">> => true}
            catch
                _:Error ->
                    lager:error("血糖数据写入失败：~p~n", [Error])
            end;
        <<"11">> ->
            % 翻转数据
            try
                Roll = erlang:binary_to_integer(maps:get(<<"roll">>, Params, <<"0">>)),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, <<"1970/1/1 00:00:00">>)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchroll(ptime, roll)
                  values($1, $2) on conflict (ptime)
                  do update set roll=excluded.roll;", [BTUtcTime, Roll]),
                #{<<"success">> => true}
            catch
                _:Error ->
                    lager:error("翻转数据写入失败：~p~n", [Error])
            end;
        <<"12">> ->
            % 体温数据
            try
                BodyTemperature = maps:get(<<"bodyTemperature">>, Params, <<"0">>),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, <<"1970/1/1 00:00:00">>)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchbt(ptime, bodytemperature)
                  values($1, $2::real) on conflict (ptime)
                  do update set bodytemperature=excluded.bodytemperature;", [BTUtcTime, BodyTemperature]),
                #{<<"success">> => true}
            catch
                _:Error ->
                    lager:error("体温数据写入失败：~p~n", [Error])
            end;
        <<"14">> ->
            % 体温数据
            try
                BodyTemperature = maps:get(<<"bodyTemperature">>, Params, <<"0">>),
                WristTemperature = maps:get(<<"wristTemperature">>, Params, <<"0">>),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, <<"1970/1/1 00:00:00">>)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchbt(ptime, bodytemperature, wristtemperature)
                  values($1, $2::real, $3::real) on conflict (ptime)
                  do update set bodytemperature=excluded.bodytemperature,
                  wristtemperature=excluded.wristtemperature;",
                    [BTUtcTime, BodyTemperature, WristTemperature]),
                #{<<"success">> => true}
            catch
                _:Error ->
                    lager:error("体温数据写入失败：~p~n", [Error])
            end;
        <<"16">> ->
            % 定位数据
            try
                LatStr = erlang:binary_to_float(maps:get(<<"latStr">>, Params, <<"0">>)),
                LngStr = erlang:binary_to_float(maps:get(<<"lngStr">>, Params, <<"0">>)),
                SpeedStr = erlang:binary_to_integer(maps:get(<<"speedStr">>, Params, <<"0">>)),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, <<"1970/1/1 00:00:00">>)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchlocation(ptime, lat, lng, speed)
                  values($1, $2, $3, $4) on conflict (ptime)
                  do update set lat=excluded.lat, lng=excluded.lng, speed=excluded.speed;",
                    [BTUtcTime, LatStr, LngStr, SpeedStr]),
                #{<<"success">> => true}
            catch
                _:Error ->
                    lager:error("定位数据写入失败：~p~n", [Error])
            end;
        <<"30">> ->
            % 信号/电量
            try
                Signal = erlang:binary_to_integer(maps:get(<<"singal">>, Params, <<"0">>)),
                Battery = erlang:binary_to_integer(maps:get(<<"battery">>, Params, <<"0">>)),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, <<"1970/1/1 00:00:00">>)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchsb(ptime, signal, battery)
                  values($1, $2, $3) on conflict (ptime)
                  do update set signal=excluded.signal, battery=excluded.battery;",
                    [BTUtcTime, Signal, Battery]),
                Steps = erlang:binary_to_integer(maps:get(<<"steps">>, Params, <<"0">>)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchstep(ptime, steps)
                  values($1, $2) on conflict (ptime)
                  do update set steps=excluded.steps;", [BTUtcTime, Steps]),
                #{<<"success">> => true}
            catch
                _:Error ->
                    lager:error("信号/电量数据写入失败：~p~n", [Error])
            end;
        <<"31">> ->
            % 血氧
            try
                BloodOxygen = erlang:binary_to_integer(maps:get(<<"BloodOxygen">>, Params, <<"0">>)),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, <<"1970/1/1 00:00:00">>)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchbo(ptime, bloodoxygen)
                  values($1, $2) on conflict (ptime)
                  do update set bloodoxygen=excluded.bloodoxygen;", [BTUtcTime, BloodOxygen]),
                #{<<"success">> => true}
            catch
                _:Error ->
                    lager:error("血氧数据写入失败：~p~n", [Error])
            end;
        <<"58">> ->
            % 睡眠
            try
                SleepType = erlang:binary_to_integer(maps:get(<<"sleepType">>, Params, <<"0">>)),
                Minute = erlang:binary_to_integer(maps:get(<<"minute">>, Params, <<"0">>)),
                StartTime = eadm_utils:parse_date_time(maps:get(<<"startTime">>, Params, <<"0">>)),
                EndTime = eadm_utils:parse_date_time(maps:get(<<"endTime">>, Params, <<"1970/1/1 00:00:00">>)),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, <<"1970/1/1 00:00:00">>)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchsleep(ptime, sleeptype, minute, starttime, endtime)
                  values($1, $2, $3, $4, $5) on conflict (ptime)
                  do update set sleeptype=excluded.sleeptype, minute=excluded.minute,
                  starttime=excluded.starttime, endtime=excluded.endtime;",
                    [BTUtcTime, SleepType, Minute, StartTime, EndTime]),
                #{<<"success">> => true}
            catch
                _:Error ->
                    lager:error("睡眠数据写入失败：~p，参数：~p~n", [Error, Params])
            end;
        <<"59">> ->
            % 蓝牙信息
            try
                BTInfo = maps:get(<<"BTInfo">>, Params, <<"0">>),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, <<"1970/1/1 00:00:00">>)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchbluet(ptime, btinfo)
                  values($1, $2) on conflict (ptime)
                  do update set btinfo=excluded.btinfo;", [BTUtcTime, BTInfo]),
                #{<<"success">> => true}
            catch
                _:Error ->
                    lager:error("蓝牙信息写入失败：~p~n", [Error])
            end;
        <<"100">> ->
            % 健康数据集合
            try
                Diastolic = erlang:binary_to_integer(maps:get(<<"diastolic">>, Params, <<"0">>)),
                Shrink = erlang:binary_to_integer(maps:get(<<"shrink">>, Params, <<"0">>)),
                Heartbeat = erlang:binary_to_integer(maps:get(<<"heartbeat">>, Params, <<"0">>)),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, <<"1970/1/1 00:00:00">>)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchbp(ptime, diastolic, shrink)
                                  values($1, $2, $3) on conflict (ptime)
                                  do update set diastolic=excluded.diastolic, shrink=excluded.shrink;",
                [BTUtcTime, Diastolic, Shrink]),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchhb(ptime, heartbeat)
                  values($1, $2) on conflict (ptime)
                  do update set heartbeat=excluded.heartbeat;", [BTUtcTime, Heartbeat]),
                #{<<"success">> => true}

            catch
                _:Error ->
                    lager:error("蓝牙信息写入失败：~p~n", [Error])
            end;
        %% 提醒
        <<"18">> ->
            try
                MsgContent = unicode:characters_to_binary("手表电量低！", utf8),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, <<"1970/1/1 00:00:00">>)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchalarm(alarmtime, alarmtype, alarminfo)
                  values($1, $2, $3);", [BTUtcTime, 18, MsgContent]),
                eadm_wechat:send_msg(MsgContent),
                #{<<"success">> => true}
            catch
                _:Error ->
                    lager:error("提醒类型18写入失败：~p~n", [Error])
            end;
        <<"19">> ->
            try
                MsgContent = unicode:characters_to_binary("紧急预警！", utf8),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, <<"1970/1/1 00:00:00">>)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchalarm(alarmtime, alarmtype, alarminfo)
                  values($1, $2, $3);", [BTUtcTime, 19, MsgContent]),
                eadm_wechat:send_msg(MsgContent),
                #{<<"success">> => true}
            catch
                _:Error ->
                    lager:error("提醒类型19写入失败：~p~n", [Error])
            end;
        <<"20">> ->
            try
                MsgContent = unicode:characters_to_binary("手表已关机！", utf8),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, <<"1970/1/1 00:00:00">>)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchalarm(alarmtime, alarmtype, alarminfo)
                  values($1, $2, $3);", [BTUtcTime, 20, MsgContent]),
                eadm_wechat:send_msg(MsgContent),
                #{<<"success">> => true}
            catch
                _:Error ->
                    lager:error("提醒类型120写入失败：~p~n", [Error])
            end;
        <<"21">> ->
            try
                MsgContent = unicode:characters_to_binary("手表已摘除！", utf8),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, <<"1970/1/1 00:00:00">>)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchalarm(alarmtime, alarmtype, alarminfo)
                  values($1, $2, $3);", [BTUtcTime, 21, MsgContent]),
                eadm_wechat:send_msg(MsgContent),
                #{<<"success">> => true}
            catch
                _:Error ->
                    lager:error("提醒类型21写入失败：~p~n", [Error])
            end;
        <<"24">> ->
            try
                MsgContent = unicode:characters_to_binary("签到！", utf8),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, <<"1970/1/1 00:00:00">>)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchalarm(alarmtime, alarmtype, alarminfo)
                  values($1, $2, $3);", [BTUtcTime, 24, MsgContent]),
                eadm_wechat:send_msg(MsgContent),
                #{<<"success">> => true}
            catch
                _:Error ->
                    lager:error("提醒类型24写入失败：~p~n", [Error])
            end;
        <<"25">> ->
            try
                MsgContent = unicode:characters_to_binary("签退！", utf8),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, <<"1970/1/1 00:00:00">>)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchalarm(alarmtime, alarmtype, alarminfo)
                  values($1, $2, $3);", [BTUtcTime, 25, MsgContent]),
                eadm_wechat:send_msg(MsgContent),
                #{<<"success">> => true}
            catch
                _:Error ->
                    lager:error("提醒类型25写入失败：~p~n", [Error])
            end;
        <<"36">> ->
            try
                MsgContent = unicode:characters_to_binary("久坐！", utf8),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, <<"1970/1/1 00:00:00">>)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchalarm(alarmtime, alarmtype, alarminfo)
                  values($1, $2, $3);", [BTUtcTime, 36, MsgContent]),
                eadm_wechat:send_msg(MsgContent),
                #{<<"success">> => true}
            catch
                _:Error ->
                    lager:error("提醒类型36写入失败：~p~n", [Error])
            end;
        <<"38">> ->
            try
                MsgContent = unicode:characters_to_binary("表带锁打开！", utf8),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, <<"1970/1/1 00:00:00">>)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchalarm(alarmtime, alarmtype, alarminfo)
                  values($1, $2, $3);", [BTUtcTime, 38, MsgContent]),
                eadm_wechat:send_msg(MsgContent),
                #{<<"success">> => true}
            catch
                _:Error ->
                    lager:error("提醒类型38写入失败：~p~n", [Error])
            end;
        <<"39">> ->
            try
                MsgContent = unicode:characters_to_binary("表带破坏！", utf8),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, <<"1970/1/1 00:00:00">>)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchalarm(alarmtime, alarmtype, alarminfo)
                  values($1, $2, $3);", [BTUtcTime, 39, MsgContent]),
                eadm_wechat:send_msg(MsgContent),
                #{<<"success">> => true}
            catch
                _:Error ->
                    lager:error("提醒类型39写入失败：~p~n", [Error])
            end;
        <<"51">> ->
            try
                MsgContent = unicode:characters_to_binary("进入睡眠！", utf8),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, <<"1970/1/1 00:00:00">>)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchalarm(alarmtime, alarmtype, alarminfo)
                  values($1, $2, $3);", [BTUtcTime, 51, MsgContent]),
                eadm_wechat:send_msg(MsgContent),
                #{<<"success">> => true}
            catch
                _:Error ->
                    lager:error("提醒类型40写入失败：~p~n", [Error])
            end;
        <<"52">> ->
            try
                MsgContent = unicode:characters_to_binary("退出睡眠！", utf8),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, <<"1970/1/1 00:00:00">>)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchalarm(alarmtime, alarmtype, alarminfo)
                  values($1, $2, $3);", [BTUtcTime, 52, MsgContent]),
                eadm_wechat:send_msg(MsgContent),
                #{<<"success">> => true}
            catch
                _:Error ->
                    lager:error("提醒类型52写入失败：~p~n", [Error])
            end;
        <<"57">> ->
            try
                MsgContent = unicode:characters_to_binary("手表已佩戴！", utf8),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, <<"1970/1/1 00:00:00">>)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchalarm(alarmtime, alarmtype, alarminfo)
                  values($1, $2, $3);", [BTUtcTime, 39, MsgContent]),
                eadm_wechat:send_msg(MsgContent),
                #{<<"success">> => true}
            catch
                _:Error ->
                    lager:error("提醒类型57写入失败：~p~n", [Error])
            end;
        <<"91">> ->
            try
                MsgContent = unicode:characters_to_binary("无信号！", utf8),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, <<"1970/1/1 00:00:00">>)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchalarm(alarmtime, alarmtype, alarminfo)
                  values($1, $2, $3);", [BTUtcTime, 91, MsgContent]),
                eadm_wechat:send_msg(MsgContent),
                #{<<"success">> => true}
            catch
                _:Error ->
                    lager:error("提醒类型91写入失败：~p~n", [Error])
            end;
        <<"110">> ->
            try
                MsgContent = unicode:characters_to_binary("手表跌落！", utf8),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, <<"1970/1/1 00:00:00">>)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchalarm(alarmtime, alarmtype, alarminfo)
                  values($1, $2, $3);", [BTUtcTime, 110, MsgContent]),
                eadm_wechat:send_msg(MsgContent),
                #{<<"success">> => true}
            catch
                _:Error ->
                    lager:error("提醒类型110写入失败：~p~n", [Error])
            end;
        <<"154">> ->
            try
                MsgContent = unicode:characters_to_binary("充电关机！", utf8),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, <<"1970/1/1 00:00:00">>)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchalarm(alarmtime, alarmtype, alarminfo)
                  values($1, $2, $3);", [BTUtcTime, 154, MsgContent]),
                eadm_wechat:send_msg(MsgContent),
                #{<<"success">> => true}
            catch
                _:Error ->
                    lager:error("提醒类型154写入失败：~p~n", [Error])
            end;
        <<"155">> ->
            try
                MsgContent = unicode:characters_to_binary("低电关机！", utf8),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, <<"1970/1/1 00:00:00">>)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchalarm(alarmtime, alarmtype, alarminfo)
                  values($1, $2, $3);", [BTUtcTime, 155, MsgContent]),
                eadm_wechat:send_msg(MsgContent),
                #{<<"success">> => true}
            catch
                _:Error ->
                    lager:error("提醒类型155写入失败：~p~n", [Error])
            end;
        <<"156">> ->
            try
                MsgContent = MsgContent = unicode:characters_to_binary("主动关机！", utf8),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, <<"1970/1/1 00:00:00">>)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchalarm(alarmtime, alarmtype, alarminfo)
                  values($1, $2, $3);", [BTUtcTime, 156, MsgContent]),
                eadm_wechat:send_msg(MsgContent),
                #{<<"success">> => true}
            catch
                _:Error ->
                    lager:error("提醒类型156写入失败：~p~n", [Error])
            end;
        _ ->
            lager:info("编码未定义: ~p~n", [Params]),
            #{<<"success">> => false}
    end.
