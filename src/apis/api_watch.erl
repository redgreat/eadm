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
    MsgType = maps:get(<<"type">>, Params, null),
    case MsgType of
        %% 数据
        <<"3">> ->
            % 基站数据
            try
                Lac = maps:get(<<"Lac">>, Params, null),
                Cid = maps:get(<<"cid">>, Params, null),
                Db = maps:get(<<"Db">>, Params, null),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchcell(ptime, lac, cid, db)
                  values($1, $2, $3, $4) on conflict (ptime)
                  do update set lac=excluded.lac, cid=excluded.cid, db=excluded.db;",
                    [BTUtcTime, Lac, Cid, Db]),
                #{<<"success">> => true}
            catch
                oops         -> lager:error("got_throw_oops！~n");
                throw:Other  -> lager:error("got_throw: ~p~n", [Other]);
                exit:Reason  -> lager:error("got_exit: ~p~n", [Reason]);
                error:Reason -> lager:error("got_error: ~p~n", [Reason])
            end;
        <<"4">> ->
            % 每日累计步数
            try
                Steps = maps:get(<<"steps">>, Params, null),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchstep(ptime, steps)
                  values($1, $2) on conflict (ptime)
                  do update set steps=excluded.steps;", [BTUtcTime, Steps]),
                #{<<"success">> => true}
            catch
                oops         -> lager:error("got_throw_oops！~n");
                throw:Other  -> lager:error("got_throw: ~p~n", [Other]);
                exit:Reason  -> lager:error("got_exit: ~p~n", [Reason]);
                error:Reason -> lager:error("got_error: ~p~n", [Reason])
            end;
        <<"5">> ->
            % WIFI 定位
            try
                Latitude = maps:get(<<"Latitude">>, Params, null),
                Longitude = maps:get(<<"Longitude">>, Params, null),
                TimeStr = eadm_utils:parse_date_time(maps:get(<<"timeStr">>, Params, null)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchlocation(ptime, lat, lng)
                  values($1, $2, $3) on conflict (ptime)
                  do update set lat=excluded.lat, lng=excluded.lng;", [TimeStr, Latitude, Longitude]),
                #{<<"success">> => true}
            catch
                oops         -> lager:error("got_throw_oops！~n");
                throw:Other  -> lager:error("got_throw: ~p~n", [Other]);
                exit:Reason  -> lager:error("got_exit: ~p~n", [Reason]);
                error:Reason -> lager:error("got_error: ~p~n", [Reason])
            end;
        <<"6">> ->
            % 心率数据
            try
                Heartbeat = maps:get(<<"heartbeat">>, Params, null),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
                lager:info("心率入参： Heartbeat: ~p,BTUtcTime: ~p~n", [Heartbeat, BTUtcTime]),
                ResData = eadm_pgpool:equery(pool_pg, "insert into lc_watchhb(ptime, heartbeat)
                  values($1, $2) on conflict (ptime)
                  do update set heartbeat=excluded.heartbeat;", [BTUtcTime, Heartbeat]),
                lager:info("心率写入结果: ~p~n", [ResData]),
                #{<<"success">> => true}
            catch
                oops         -> lager:error("got_throw_oops！~n");
                throw:Other  -> lager:error("got_throw: ~p~n", [Other]);
                exit:Reason  -> lager:error("got_exit: ~p~n", [Reason]);
                error:Reason -> lager:error("got_error: ~p~n", [Reason])
            end;
        <<"8">> ->
            % 血压
            try
                Diastolic = maps:get(<<"diastolic">>, Params, null),
                Shrink = maps:get(<<"shrink">>, Params, null),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
                lager:info("血压入参： Diastolic: ~p,Shrink: ~p,BTUtcTime: ~p~n", [Diastolic, Shrink, BTUtcTime]),
                ResData = eadm_pgpool:equery(pool_pg, "insert into lc_watchbp(ptime, diastolic, shrink)
                  values($1, $2, $3) on conflict (ptime)
                  do update set diastolic=excluded.diastolic, shrink=excluded.shrink;",
                    [BTUtcTime, Diastolic, Shrink]),
                lager:info("血压写入结果: ~p~n", [ResData]),
                #{<<"success">> => true}
            catch
                oops         -> lager:error("got_throw_oops！~n");
                throw:Other  -> lager:error("got_throw: ~p~n", [Other]);
                exit:Reason  -> lager:error("got_exit: ~p~n", [Reason]);
                error:Reason -> lager:error("got_error: ~p~n", [Reason])
            end;
        <<"10">> ->
            % 血糖
            try
                BloodSugar = maps:get(<<"bloodSugar">>, Params, null),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchbs(ptime, bloodsugar)
                  values($1, $2) on conflict (ptime)
                  do update set bloodsugar=excluded.bloodsugar;", [BTUtcTime, BloodSugar]),
                #{<<"success">> => true}
            catch
                oops         -> lager:error("got_throw_oops！~n");
                throw:Other  -> lager:error("got_throw: ~p~n", [Other]);
                exit:Reason  -> lager:error("got_exit: ~p~n", [Reason]);
                error:Reason -> lager:error("got_error: ~p~n", [Reason])
            end;
        <<"11">> ->
            % 翻转数据
            try
                Roll = maps:get(<<"roll">>, Params, null),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchroll(ptime, roll)
                  values($1, $2) on conflict (ptime)
                  do update set roll=excluded.roll;", [BTUtcTime, Roll]),
                #{<<"success">> => true}
            catch
                oops         -> lager:error("got_throw_oops！~n");
                throw:Other  -> lager:error("got_throw: ~p~n", [Other]);
                exit:Reason  -> lager:error("got_exit: ~p~n", [Reason]);
                error:Reason -> lager:error("got_error: ~p~n", [Reason])
            end;
        <<"12">> ->
            % 体温数据
            try
                BodyTemperature = maps:get(<<"bodyTemperature">>, Params, null),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchbt(ptime, bodytemperature)
                  values($1, $2) on conflict (ptime)
                  do update set bodytemperature=excluded.bodytemperature;", [BTUtcTime, BodyTemperature]),
                #{<<"success">> => true}
            catch
                oops         -> lager:error("got_throw_oops！~n");
                throw:Other  -> lager:error("got_throw: ~p~n", [Other]);
                exit:Reason  -> lager:error("got_exit: ~p~n", [Reason]);
                error:Reason -> lager:error("got_error: ~p~n", [Reason])
            end;
        <<"14">> ->
            % 体温数据
            try
                BodyTemperature = maps:get(<<"bodyTemperature">>, Params, null),
                WristTemperature = maps:get(<<"wristTemperature">>, Params, null),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchbt(ptime, bodytemperature, wristtemperature)
                  values($1, $2, $3) on conflict (ptime)
                  do update set bodytemperature=excluded.bodytemperature,
                  wristtemperature=excluded.wristtemperature;",
                    [BTUtcTime, BodyTemperature, WristTemperature]),
                #{<<"success">> => true}
            catch
                oops         -> lager:error("got_throw_oops！~n");
                throw:Other  -> lager:error("got_throw: ~p~n", [Other]);
                exit:Reason  -> lager:error("got_exit: ~p~n", [Reason]);
                error:Reason -> lager:error("got_error: ~p~n", [Reason])
            end;
        <<"16">> ->
            % 定位数据
            try
                LatStr = maps:get(<<"latStr">>, Params, null),
                LngStr = maps:get(<<"lngStr">>, Params, null),
                SpeedStr = maps:get(<<"speedStr">>, Params, null),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchlocation(ptime, lat, lng, speed)
                  values($1, $2, $3, $4) on conflict (ptime)
                  do update set lat=excluded.lat, lng=excluded.lng, speed=excluded.speed;",
                    [BTUtcTime, LatStr, LngStr, SpeedStr]),
                #{<<"success">> => true}
            catch
                oops         -> lager:error("got_throw_oops！~n");
                throw:Other  -> lager:error("got_throw: ~p~n", [Other]);
                exit:Reason  -> lager:error("got_exit: ~p~n", [Reason]);
                error:Reason -> lager:error("got_error: ~p~n", [Reason])
            end;
        <<"30">> ->
            % 信号/电量 (手表信号未传，只有一个电量字段)
            try
                Signal = maps:get(<<"signal">>, Params, null),
                Battery = maps:get(<<"battery">>, Params, null),
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
                oops         -> lager:error("got_throw_oops！~n");
                throw:Other  -> lager:error("got_throw: ~p~n", [Other]);
                exit:Reason  -> lager:error("got_exit: ~p~n", [Reason]);
                error:Reason -> lager:error("got_error: ~p~n", [Reason])
            end;
        <<"31">> ->
            % 血氧
            try
                BloodOxygen = maps:get(<<"BloodOxygen">>, Params, null),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchbo(ptime, bloodoxygen)
                  values($1, $2) on conflict (ptime)
                  do update set bloodoxygen=excluded.bloodoxygen;", [BTUtcTime, BloodOxygen]),
                #{<<"success">> => true}
            catch
                oops         -> lager:error("got_throw_oops！~n");
                throw:Other  -> lager:error("got_throw: ~p~n", [Other]);
                exit:Reason  -> lager:error("got_exit: ~p~n", [Reason]);
                error:Reason -> lager:error("got_error: ~p~n", [Reason])
            end;
        <<"58">> ->
            % 睡眠
            try
                SleepType = maps:get(<<"sleepType">>, Params, null),
                Minute = maps:get(<<"minute">>, Params, null),
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
                oops         -> lager:error("got_throw_oops！~n");
                throw:Other  -> lager:error("got_throw: ~p~n", [Other]);
                exit:Reason  -> lager:error("got_exit: ~p~n", [Reason]);
                error:Reason -> lager:error("got_error: ~p~n", [Reason])
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
                oops         -> lager:error("got_throw_oops！~n");
                throw:Other  -> lager:error("got_throw: ~p~n", [Other]);
                exit:Reason  -> lager:error("got_exit: ~p~n", [Reason]);
                error:Reason -> lager:error("got_error: ~p~n", [Reason])
            end;
        %% 提醒
        <<"18">> ->
            try
                MsgContent = unicode:characters_to_binary("手表电量低！"),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchalarm(alarmtime, alarmtype, alarminfo)
                  values($1, $2, $3);", [BTUtcTime, 18, MsgContent]),
                eadm_wechat:send_msg(MsgContent),
                #{<<"success">> => true}
            catch
                oops         -> lager:error("got_throw_oops！~n");
                throw:Other  -> lager:error("got_throw: ~p~n", [Other]);
                exit:Reason  -> lager:error("got_exit: ~p~n", [Reason]);
                error:Reason -> lager:error("got_error: ~p~n", [Reason])
            end;
        <<"19">> ->
            try
                MsgContent = unicode:characters_to_binary("紧急预警！"),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchalarm(alarmtime, alarmtype, alarminfo)
                  values($1, $2, $3);", [BTUtcTime, 19, MsgContent]),
                eadm_wechat:send_msg(MsgContent),
                #{<<"success">> => true}
            catch
                oops         -> lager:error("got_throw_oops！~n");
                throw:Other  -> lager:error("got_throw: ~p~n", [Other]);
                exit:Reason  -> lager:error("got_exit: ~p~n", [Reason]);
                error:Reason -> lager:error("got_error: ~p~n", [Reason])
            end;
        <<"20">> ->
            try
                MsgContent = unicode:characters_to_binary("手表已关机！"),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchalarm(alarmtime, alarmtype, alarminfo)
                  values($1, $2, $3);", [BTUtcTime, 20, MsgContent]),
                eadm_wechat:send_msg(MsgContent),
                #{<<"success">> => true}
            catch
                oops         -> lager:error("got_throw_oops！~n");
                throw:Other  -> lager:error("got_throw: ~p~n", [Other]);
                exit:Reason  -> lager:error("got_exit: ~p~n", [Reason]);
                error:Reason -> lager:error("got_error: ~p~n", [Reason])
            end;
        <<"21">> ->
            try
                MsgContent = unicode:characters_to_binary("手表已摘除！"),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchalarm(alarmtime, alarmtype, alarminfo)
                  values($1, $2, $3);", [BTUtcTime, 21, MsgContent]),
                eadm_wechat:send_msg(MsgContent),
                #{<<"success">> => true}
            catch
                oops         -> lager:error("got_throw_oops！~n");
                throw:Other  -> lager:error("got_throw: ~p~n", [Other]);
                exit:Reason  -> lager:error("got_exit: ~p~n", [Reason]);
                error:Reason -> lager:error("got_error: ~p~n", [Reason])
            end;
        <<"24">> ->
            try
                MsgContent = unicode:characters_to_binary("签到！"),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchalarm(alarmtime, alarmtype, alarminfo)
                  values($1, $2, $3);", [BTUtcTime, 24, MsgContent]),
                eadm_wechat:send_msg(MsgContent),
                #{<<"success">> => true}
            catch
                oops         -> lager:error("got_throw_oops！~n");
                throw:Other  -> lager:error("got_throw: ~p~n", [Other]);
                exit:Reason  -> lager:error("got_exit: ~p~n", [Reason]);
                error:Reason -> lager:error("got_error: ~p~n", [Reason])
            end;
        <<"25">> ->
            try
                MsgContent = unicode:characters_to_binary("签退！"),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchalarm(alarmtime, alarmtype, alarminfo)
                  values($1, $2, $3);", [BTUtcTime, 25, MsgContent]),
                eadm_wechat:send_msg(MsgContent),
                #{<<"success">> => true}
            catch
                oops         -> lager:error("got_throw_oops！~n");
                throw:Other  -> lager:error("got_throw: ~p~n", [Other]);
                exit:Reason  -> lager:error("got_exit: ~p~n", [Reason]);
                error:Reason -> lager:error("got_error: ~p~n", [Reason])
            end;
        <<"36">> ->
            try
                MsgContent = unicode:characters_to_binary("久坐！"),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchalarm(alarmtime, alarmtype, alarminfo)
                  values($1, $2, $3);", [BTUtcTime, 36, MsgContent]),
                eadm_wechat:send_msg(MsgContent),
                #{<<"success">> => true}
            catch
                oops         -> lager:error("got_throw_oops！~n");
                throw:Other  -> lager:error("got_throw: ~p~n", [Other]);
                exit:Reason  -> lager:error("got_exit: ~p~n", [Reason]);
                error:Reason -> lager:error("got_error: ~p~n", [Reason])
            end;
        <<"38">> ->
            try
                MsgContent = unicode:characters_to_binary("表带锁打开！"),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchalarm(alarmtime, alarmtype, alarminfo)
                  values($1, $2, $3);", [BTUtcTime, 38, MsgContent]),
                eadm_wechat:send_msg(MsgContent),
                #{<<"success">> => true}
            catch
                oops         -> lager:error("got_throw_oops！~n");
                throw:Other  -> lager:error("got_throw: ~p~n", [Other]);
                exit:Reason  -> lager:error("got_exit: ~p~n", [Reason]);
                error:Reason -> lager:error("got_error: ~p~n", [Reason])
            end;
        <<"39">> ->
            try
                MsgContent = unicode:characters_to_binary("表带破坏！"),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchalarm(alarmtime, alarmtype, alarminfo)
                  values($1, $2, $3);", [BTUtcTime, 39, MsgContent]),
                eadm_wechat:send_msg(MsgContent),
                #{<<"success">> => true}
            catch
                oops         -> lager:error("got_throw_oops！~n");
                throw:Other  -> lager:error("got_throw: ~p~n", [Other]);
                exit:Reason  -> lager:error("got_exit: ~p~n", [Reason]);
                error:Reason -> lager:error("got_error: ~p~n", [Reason])
            end;
        <<"51">> ->
            try
                MsgContent = unicode:characters_to_binary("进入睡眠！"),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchalarm(alarmtime, alarmtype, alarminfo)
                  values($1, $2, $3);", [BTUtcTime, 51, MsgContent]),
                eadm_wechat:send_msg(MsgContent),
                #{<<"success">> => true}
            catch
                oops         -> lager:error("got_throw_oops！~n");
                throw:Other  -> lager:error("got_throw: ~p~n", [Other]);
                exit:Reason  -> lager:error("got_exit: ~p~n", [Reason]);
                error:Reason -> lager:error("got_error: ~p~n", [Reason])
            end;
        <<"52">> ->
            try
                MsgContent = unicode:characters_to_binary("退出睡眠！"),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchalarm(alarmtime, alarmtype, alarminfo)
                  values($1, $2, $3);", [BTUtcTime, 52, MsgContent]),
                eadm_wechat:send_msg(MsgContent),
                #{<<"success">> => true}
            catch
                oops         -> lager:error("got_throw_oops！~n");
                throw:Other  -> lager:error("got_throw: ~p~n", [Other]);
                exit:Reason  -> lager:error("got_exit: ~p~n", [Reason]);
                error:Reason -> lager:error("got_error: ~p~n", [Reason])
            end;
        <<"57">> ->
            try
                MsgContent = unicode:characters_to_binary("手表已佩戴！"),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchalarm(alarmtime, alarmtype, alarminfo)
                  values($1, $2, $3);", [BTUtcTime, 39, MsgContent]),
                eadm_wechat:send_msg(MsgContent),
                #{<<"success">> => true}
            catch
                oops         -> lager:error("got_throw_oops！~n");
                throw:Other  -> lager:error("got_throw: ~p~n", [Other]);
                exit:Reason  -> lager:error("got_exit: ~p~n", [Reason]);
                error:Reason -> lager:error("got_error: ~p~n", [Reason])
            end;
        <<"91">> ->
            try
                MsgContent = unicode:characters_to_binary("无信号！"),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchalarm(alarmtime, alarmtype, alarminfo)
                  values($1, $2, $3);", [BTUtcTime, 91, MsgContent]),
                eadm_wechat:send_msg(MsgContent),
                #{<<"success">> => true}
            catch
                oops         -> lager:error("got_throw_oops！~n");
                throw:Other  -> lager:error("got_throw: ~p~n", [Other]);
                exit:Reason  -> lager:error("got_exit: ~p~n", [Reason]);
                error:Reason -> lager:error("got_error: ~p~n", [Reason])
            end;
        <<"110">> ->
            try
                MsgContent = unicode:characters_to_binary("手表跌落！"),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchalarm(alarmtime, alarmtype, alarminfo)
                  values($1, $2, $3);", [BTUtcTime, 110, MsgContent]),
                eadm_wechat:send_msg(MsgContent),
                #{<<"success">> => true}
            catch
                oops         -> lager:error("got_throw_oops！~n");
                throw:Other  -> lager:error("got_throw: ~p~n", [Other]);
                exit:Reason  -> lager:error("got_exit: ~p~n", [Reason]);
                error:Reason -> lager:error("got_error: ~p~n", [Reason])
            end;
        <<"154">> ->
            try
                MsgContent = unicode:characters_to_binary("充电关机！"),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchalarm(alarmtime, alarmtype, alarminfo)
                  values($1, $2, $3);", [BTUtcTime, 154, MsgContent]),
                eadm_wechat:send_msg(MsgContent),
                #{<<"success">> => true}
            catch
                oops         -> lager:error("got_throw_oops！~n");
                throw:Other  -> lager:error("got_throw: ~p~n", [Other]);
                exit:Reason  -> lager:error("got_exit: ~p~n", [Reason]);
                error:Reason -> lager:error("got_error: ~p~n", [Reason])
            end;
        <<"155">> ->
            try
                MsgContent = unicode:characters_to_binary("低电关机！"),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchalarm(alarmtime, alarmtype, alarminfo)
                  values($1, $2, $3);", [BTUtcTime, 155, MsgContent]),
                eadm_wechat:send_msg(MsgContent),
                #{<<"success">> => true}
            catch
                oops         -> lager:error("got_throw_oops！~n");
                throw:Other  -> lager:error("got_throw: ~p~n", [Other]);
                exit:Reason  -> lager:error("got_exit: ~p~n", [Reason]);
                error:Reason -> lager:error("got_error: ~p~n", [Reason])
            end;
        <<"156">> ->
            try
                MsgContent = MsgContent = unicode:characters_to_binary("主动关机！"),
                BTUtcTime = eadm_utils:parse_date_time(maps:get(<<"BTUtcTime">>, Params, null)),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchalarm(alarmtime, alarmtype, alarminfo)
                  values($1, $2, $3);", [BTUtcTime, 156, MsgContent]),
                eadm_wechat:send_msg(MsgContent),
                #{<<"success">> => true}
            catch
                oops         -> lager:error("got_throw_oops！~n");
                throw:Other  -> lager:error("got_throw: ~p~n", [Other]);
                exit:Reason  -> lager:error("got_exit: ~p~n", [Reason]);
                error:Reason -> lager:error("got_error: ~p~n", [Reason])
            end;
        _ ->
            lager:info("编码未定义: ~p~n", [Params]),
            #{<<"success">> => false}
    end.
