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
    % lager:info("传入参数：~p~n", [Params]),
    MsgType = maps:get(<<"type">>, Params, null),
    case MsgType of
        %% 数据
        <<"3">> ->
            % 基站数据
            try
                Lac = erlang:binary_to_integer(maps:get(<<"Lac">>, Params, null)),
                Cid = erlang:binary_to_integer(maps:get(<<"cid">>, Params, null)),
                Db = erlang:binary_to_integer(maps:get(<<"Db">>, Params, null)),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
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
                Steps = erlang:binary_to_integer(maps:get(<<"steps">>, Params, null)),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
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
                Latitude = erlang:binary_to_float(maps:get(<<"Latitude">>, Params, null)),
                Longitude = erlang:binary_to_float(maps:get(<<"Longitude">>, Params, null)),
                TimeStr = eadm_utils:parse_date_time(maps:get(<<"timeStr">>, Params, null)),
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
                Heartbeat = erlang:binary_to_integer(maps:get(<<"heartbeat">>, Params, null)),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
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
                Diastolic = erlang:binary_to_integer(maps:get(<<"diastolic">>, Params, null)),
                Shrink = erlang:binary_to_integer(maps:get(<<"shrink">>, Params, null)),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
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
                BloodSugar = erlang:binary_to_float(maps:get(<<"bloodSugar">>, Params, null)),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
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
                Roll = erlang:binary_to_integer(maps:get(<<"roll">>, Params, null)),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
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
                BodyTemperature = erlang:binary_to_float(maps:get(<<"bodyTemperature">>, Params, null)),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchbt(ptime, bodytemperature)
                  values($1, $2) on conflict (ptime)
                  do update set bodytemperature=excluded.bodytemperature;", [BTUtcTime, BodyTemperature]),
                #{<<"success">> => true}
            catch
                _:Error ->
                    lager:error("体温数据写入失败：~p~n", [Error])
            end;
        <<"14">> ->
            % 体温数据
            try
                BodyTemperature = erlang:binary_to_float(maps:get(<<"bodyTemperature">>, Params, null)),
                WristTemperature = erlang:binary_to_float(maps:get(<<"wristTemperature">>, Params, null)),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchbt(ptime, bodytemperature, wristtemperature)
                  values($1, $2, $3) on conflict (ptime)
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
                LatStr = erlang:binary_to_float(maps:get(<<"latStr">>, Params, null)),
                LngStr = erlang:binary_to_float(maps:get(<<"lngStr">>, Params, null)),
                SpeedStr = erlang:binary_to_integer(maps:get(<<"speedStr">>, Params, null)),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
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
                Signal = erlang:binary_to_integer(maps:get(<<"signal">>, Params, null)),
                Battery = erlang:binary_to_integer(maps:get(<<"battery">>, Params, null)),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchsb(ptime, signal, battery)
                  values($1, $2, $3) on conflict (ptime)
                  do update set signal=excluded.signal, battery=excluded.battery;",
                    [BTUtcTime, Signal, Battery]),
                Steps = maps:get(<<"steps">>, Params, null),
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
                BloodOxygen = erlang:binary_to_integer(maps:get(<<"BloodOxygen">>, Params, null)),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
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
                SleepType = erlang:binary_to_integer(maps:get(<<"sleepType">>, Params, null)),
                Minute = erlang:binary_to_integer(maps:get(<<"minute">>, Params, null)),
                StartTime = eadm_utils:parse_date_time(maps:get(<<"startTime">>, Params, null)),
                EndTime = eadm_utils:parse_date_time(maps:get(<<"endTime">>, Params, null)),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
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
                BTInfo = maps:get(<<"BTInfo">>, Params, null),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
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
                Diastolic = erlang:binary_to_integer(maps:get(<<"diastolic">>, Params, null)),
                Shrink = erlang:binary_to_integer(maps:get(<<"shrink">>, Params, null)),
                Heartbeat = erlang:binary_to_integer(maps:get(<<"heartbeat">>, Params, null)),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
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
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
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
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
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
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
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
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
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
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
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
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
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
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
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
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
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
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
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
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
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
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
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
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
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
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
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
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
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
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
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
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
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
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
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
