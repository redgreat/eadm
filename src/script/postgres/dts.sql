-- @author wangcw
-- @copyright (C) 2024, REDGREAT
-- Created : 2024-06-07 下午2:38
-- Comment : 数据迁移

TRUNCATE TABLE "public"."carlocdaily";
INSERT INTO "public"."carlocdaily" ("ptime", "deviceno", "lat", "lng", "dirct", "speed", "mileage", "hight", "gnssnum", "rssi", "receivetime", "inserttime")
select ("dev_upload" - INTERVAL '8 hours') AT TIME ZONE 'UTC', "device_id", "lat", "lng", "dirct", "speed", "mileage", "hight", "gnss_num", "rssi",
serv_receive AT TIME ZONE 'UTC', "data_insert" AT TIME ZONE 'UTC'
from carlocdaily_bak
order by dev_upload;

TRUNCATE TABLE "public"."watchalarm";
INSERT INTO "public"."watchalarm" ("alerttype", "alertinfo", "heartnum", "lasttemper", "inserttime")
select "AlertType", "AlertInfo", "HeartNum", "LastTemper", "InsertTime" + interval '8 hours'
from "public"."watchalarm_bak"
order by "InsertTime";

TRUNCATE TABLE "public"."watchdaily";
INSERT INTO "public"."watchdaily" ("ptime", "steps", "heartbeat", "roll", "bodytemperature", "wristtemperature", "bloodsugar", "diastolic", "shrink", "bloodoxygen", "sleeptype", "sleepstartTime", "sleependtime", "sleepminute", "signal", "battery", "lat", "lng", "speed", "inserttime")
select "UtcTime" AT TIME ZONE 'UTC', "Steps", "Heartbeat", "Roll", "BodyTemperature", "WristTemperature", "BloodSugar", "Diastolic", "Shrink", "BloodOxygen", "SleepType", "SleepStartTime", "SleepEndTime", "SleepMinute", "Signal", "Battery", "Lat", "Lng", "Speed", "InsertTime" AT TIME ZONE 'UTC'
from "public"."watchdaily_bak"
order by "UtcTime";

TRUNCATE TABLE "public"."fn_paybilldetail" ;
INSERT INTO "public"."fn_paybilldetail" ("id", "owner", "source", "inorout", "counterparty", "counterbank", "counteraccount", "goodscomment", "paymethod",
"amount", "balance", "currency", "paystatus", "tradetype", "tradeorderno", "counterorderno", "tradetime", "billcomment", "inserttime", "deleteduser", "deletedat", "deleted")
select "Id", "Owner", "Source", "InOrOut", "CounterParty", "CounterBank", "CounterAccount", "GoodsComment", "PayMethod", "Amount", "Balance", "Currency",
"PayStatus", "TradeType", "TradeOrderNo", "CounterOrderNo", ("TradeTime" - INTERVAL '8 hours') AT TIME ZONE 'UTC' as "TradeTime", "BillComment",
("InsertTime" - INTERVAL '8 hours') AT TIME ZONE 'UTC' AS "InsertTime", "DeletedUser", ("DeletedAt" - INTERVAL '8 hours') AT TIME ZONE 'UTC' AS "DeletedAt" ,
CASE "Deleted" WHEN 0 THEN FALSE ELSE TRUE END
FROM paybilldetail
 -- where "Deleted" = 1
ORDER BY "Id" DESC
limit 100;
