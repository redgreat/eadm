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
% index(#{auth_data := #{<<"authed">> := true, <<"username">> := UserName,
%       <<"permission">> := #{<<"api">> := #{<<"watch">> := true}}}}) ->
%    {ok, [{username, UserName}]};
index(#{params := Params}) ->
    lager:info("请求内容: ~p~n", [Params]),
    MsgType = maps:get(<<"type">>, Params),
    case MsgType of
        %% 数据
        <<"3">> ->
            % 基站数据
            try
                Lac = maps:get(<<"Lac">>, Params),
                Cid = maps:get(<<"cid">>, Params),
                Db = maps:get(<<"Db">>, Params),
                BTUtcTime = maps:get(<<"BTUtcTime">>, Params),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchcell(ptime, lac, cid, db)
                  values('$1, $2, $3, $4);", [BTUtcTime, Lac, Cid, Db])
            catch
                _:_ -> {error}
            end;
        <<"4">> ->
            % 每日累计步数
            try
                Steps = maps:get(<<"steps">>, Params),
                BTUtcTime = maps:get(<<"BTUtcTime">>, Params),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchstep(ptime, steps)
                  values('$1, $2);", [BTUtcTime, Steps])
            catch
                _:_ -> {error}
            end;
        <<"5">> ->
            % WIFI 定位
            try
                Latitude = maps:get(<<"Latitude">>, Params),
                Longitude = maps:get(<<"Longitude">>, Params),
                TimeStr = maps:get(<<"timeStr">>, Params),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchlocation(ptime, lat, lng)
                  values('$1, $2, $3);", [TimeStr, Latitude, Longitude])
            catch
                _:_ -> {error}
            end;
        <<"6">> ->
            % 翻转数据
            try
                Roll = maps:get(<<"roll">>, Params),
                BTUtcTime = maps:get(<<"BTUtcTime">>, Params),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchroll(ptime, roll)
                  values('$1, $2);", [BTUtcTime, Roll])
            catch
                _:_ -> {error}
            end;
        <<"8">> ->
            % 血压
            try
                Diastolic = maps:get(<<"Diastolic">>, Params),
                Shrink = maps:get(<<"Shrink">>, Params),
                BTUtcTime = maps:get(<<"BTUtcTime">>, Params),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchbp(ptime, diastolic, shrink)
                  values('$1, $2, $3);", [BTUtcTime, Diastolic, Shrink])
            catch
                _:_ -> {error}
            end;
        <<"10">> ->
            % 血糖
            try
                BloodSugar = maps:get(<<"bloodSugar">>, Params),
                BTUtcTime = maps:get(<<"BTUtcTime">>, Params),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchbs(ptime, bloodsugar)
                  values('$1, $2);", [BTUtcTime, BloodSugar])
            catch
                _:_ -> {error}
            end;
        <<"11">> ->
            % 心率数据
            try
                Heartbeat = maps:get(<<"heartbeat">>, Params),
                BTUtcTime = maps:get(<<"BTUtcTime">>, Params),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchhb(ptime, heartbeat)
                  values('$1, $2);", [BTUtcTime, Heartbeat])
            catch
                _:_ -> {error}
            end;
        <<"12">> ->
            % 体温数据
            try
                BodyTemperature = maps:get(<<"bodyTemperature">>, Params),
                BTUtcTime = maps:get(<<"BTUtcTime">>, Params),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchbt(ptime, bodytemperature)
                  values('$1, $2);", [BTUtcTime, BodyTemperature])
            catch
                _:_ -> {error}
            end;
        <<"14">> ->
            % 体温数据
            try
                BodyTemperature = maps:get(<<"bodyTemperature">>, Params),
                WristTemperature = maps:get(<<"wristTemperature">>, Params),
                BTUtcTime = maps:get(<<"BTUtcTime">>, Params),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchbt(ptime, bodytemperature, wristtemperature)
                  values('$1, $2, $3);", [BTUtcTime, BodyTemperature, WristTemperature])
            catch
                _:_ -> {error}
            end;
        <<"16">> ->
            % 定位数据
            try
                LatStr = maps:get(<<"latStr">>, Params),
                LngStr = maps:get(<<"lngStr">>, Params),
                SpeedStr = maps:get(<<"speedStr">>, Params),
                BTUtcTime = maps:get(<<"BTUtcTime">>, Params),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchlocation(ptime, lat, lng, speed)
                  values('$1, $2, $3, $4);", [BTUtcTime, LatStr, LngStr, SpeedStr])
            catch
                _:_ -> {error}
            end;
        <<"30">> ->
            % 信号/电量 (手表信号未传，只有一个电量字段)
            try
                % Signal = maps:get(<<"Signal">>, Params),
                Battery = maps:get(<<"battery">>, Params),
                BTUtcTime = maps:get(<<"BTUtcTime">>, Params),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchsb(ptime, battery)
                  values('$1, $2, $3);", [BTUtcTime, Battery])
            catch
                _:_ -> {error}
            end;
        <<"31">> ->
            % 血氧
            try
                BloodOxygen = maps:get(<<"BloodOxygen">>, Params),
                BTUtcTime = maps:get(<<"BTUtcTime">>, Params),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchbo(ptime, bloodoxygen)
                  values('$1, $2);", [BTUtcTime, BloodOxygen])
            catch
                _:_ -> {error}
            end;
        <<"58">> ->
            % 睡眠
            try
                SleepType = maps:get(<<"sleepType">>, Params),
                Minute = maps:get(<<"minute">>, Params),
                StartTime = maps:get(<<"startTime">>, Params),
                EndTime = maps:get(<<"endTime">>, Params),
                BTUtcTime = maps:get(<<"BTUtcTime">>, Params),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchsleep(ptime, sleeptype, minute, starttime, endtime)
                  values('$1, $2, $3, $4, $5);", [BTUtcTime, SleepType, Minute, StartTime, EndTime])
            catch
                _:_ -> {error}
            end;
        <<"59">> ->
            % 蓝牙信息
            try
                BTInfo = maps:get(<<"BTInfo">>, Params),
                BTUtcTime = maps:get(<<"BTUtcTime">>, Params),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchbluet(ptime, btinfo)
                  values('$1, $2);", [BTUtcTime, BTInfo])
            catch
                _:_ -> {error}
            end;
        %% 提醒
        <<"18">> ->
            try
                MsgContent = <<"手表电量低！">>,
                BTUtcTime = maps:get(<<"BTUtcTime">>, Params),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchalarm(alarmtime, alarmtype, aarminfo)
                  values('$1, $2, $3);", [BTUtcTime, 18, MsgContent]),
                eadm_wechat:send_msg(MsgContent)
            catch
                _:_ -> {error}
            end;
        <<"19">> ->
            try
                MsgContent = <<"紧急预警！">>,
                BTUtcTime = maps:get(<<"BTUtcTime">>, Params),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchalarm(alarmtime, alarmtype, aarminfo)
                  values('$1, $2, $3);", [BTUtcTime, 19, MsgContent]),
                eadm_wechat:send_msg(MsgContent)
            catch
                _:_ -> {error}
            end;
        <<"20">> ->
            try
                MsgContent = <<"手表已关机！">>,
                BTUtcTime = maps:get(<<"BTUtcTime">>, Params),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchalarm(alarmtime, alarmtype, aarminfo)
                  values('$1, $2, $3);", [BTUtcTime, 20, MsgContent]),
                eadm_wechat:send_msg(MsgContent)
            catch
                _:_ -> {error}
            end;
        <<"21">> ->
            try
                MsgContent = <<"手表已摘除！">>,
                BTUtcTime = maps:get(<<"BTUtcTime">>, Params),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchalarm(alarmtime, alarmtype, aarminfo)
                  values('$1, $2, $3);", [BTUtcTime, 21, MsgContent]),
                eadm_wechat:send_msg(MsgContent)
            catch
                _:_ -> {error}
            end;
        <<"24">> ->
            try
                MsgContent = <<"签到！">>,
                BTUtcTime = maps:get(<<"BTUtcTime">>, Params),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchalarm(alarmtime, alarmtype, aarminfo)
                  values('$1, $2, $3);", [BTUtcTime, 24, MsgContent]),
                eadm_wechat:send_msg(MsgContent)
            catch
                _:_ -> {error}
            end;
        <<"25">> ->
            try
                MsgContent = <<"签退！">>,
                BTUtcTime = maps:get(<<"BTUtcTime">>, Params),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchalarm(alarmtime, alarmtype, aarminfo)
                  values('$1, $2, $3);", [BTUtcTime, 25, MsgContent]),
                eadm_wechat:send_msg(MsgContent)
            catch
                _:_ -> {error}
            end;
        <<"36">> ->
            try
                MsgContent = <<"久坐！">>,
                BTUtcTime = maps:get(<<"BTUtcTime">>, Params),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchalarm(alarmtime, alarmtype, aarminfo)
                  values('$1, $2, $3);", [BTUtcTime, 36, MsgContent]),
                eadm_wechat:send_msg(MsgContent)
            catch
                _:_ -> {error}
            end;
        <<"38">> ->
            try
                MsgContent = <<"表带锁打开！">>,
                BTUtcTime = maps:get(<<"BTUtcTime">>, Params),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchalarm(alarmtime, alarmtype, aarminfo)
                  values('$1, $2, $3);", [BTUtcTime, 38, MsgContent]),
                eadm_wechat:send_msg(MsgContent)
            catch
                _:_ -> {error}
            end;
        <<"39">> ->
            try
                MsgContent = <<"表带破坏！">>,
                BTUtcTime = maps:get(<<"BTUtcTime">>, Params),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchalarm(alarmtime, alarmtype, aarminfo)
                  values('$1, $2, $3);", [BTUtcTime, 39, MsgContent]),
                eadm_wechat:send_msg(MsgContent)
            catch
                _:_ -> {error}
            end;
        <<"51">> ->
            try
                MsgContent = <<"进入睡眠！">>,
                BTUtcTime = maps:get(<<"BTUtcTime">>, Params),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchalarm(alarmtime, alarmtype, aarminfo)
                  values('$1, $2, $3);", [BTUtcTime, 51, MsgContent]),
                eadm_wechat:send_msg(MsgContent)
            catch
                _:_ -> {error}
            end;
        <<"52">> ->
            try
                MsgContent = <<"退出睡眠！">>,
                BTUtcTime = maps:get(<<"BTUtcTime">>, Params),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchalarm(alarmtime, alarmtype, aarminfo)
                  values('$1, $2, $3);", [BTUtcTime, 52, MsgContent]),
                eadm_wechat:send_msg(MsgContent)
            catch
                _:_ -> {error}
            end;
        <<"57">> ->
            try
                MsgContent = <<"手表已佩戴！">>,
                BTUtcTime = maps:get(<<"BTUtcTime">>, Params),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchalarm(alarmtime, alarmtype, aarminfo)
                  values('$1, $2, $3);", [BTUtcTime, 39, MsgContent]),
                eadm_wechat:send_msg(MsgContent)
            catch
                _:_ -> {error}
            end;
        <<"91">> ->
            try
                MsgContent = <<"无信号！">>,
                BTUtcTime = maps:get(<<"BTUtcTime">>, Params),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchalarm(alarmtime, alarmtype, aarminfo)
                  values('$1, $2, $3);", [BTUtcTime, 91, MsgContent]),
                eadm_wechat:send_msg(MsgContent)
            catch
                _:_ -> {error}
            end;
        <<"154">> ->
            try
                MsgContent = <<"充电关机！">>,
                BTUtcTime = maps:get(<<"BTUtcTime">>, Params),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchalarm(alarmtime, alarmtype, aarminfo)
                  values('$1, $2, $3);", [BTUtcTime, 154, MsgContent]),
                eadm_wechat:send_msg(MsgContent)
            catch
                _:_ -> {error}
            end;
        <<"155">> ->
            try
                MsgContent = <<"低电关机！">>,
                BTUtcTime = maps:get(<<"BTUtcTime">>, Params),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchalarm(alarmtime, alarmtype, aarminfo)
                  values('$1, $2, $3);", [BTUtcTime, 155, MsgContent]),
                eadm_wechat:send_msg(MsgContent)
            catch
                _:_ -> {error}
            end;
        <<"156">> ->
            try
                MsgContent = <<"主动关机！">>,
                BTUtcTime = maps:get(<<"BTUtcTime">>, Params),
                eadm_pgpool:equery(pool_pg, "insert into lc_watchalarm(alarmtime, alarmtype, aarminfo)
                  values('$1, $2, $3);", [BTUtcTime, 156, MsgContent]),
                eadm_wechat:send_msg(MsgContent)
            catch
                _:_ -> {error}
            end
    end,
    {ok, success}.


