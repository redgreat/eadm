# @author wangcw
# @copyright (C) 2024, REDGREAT
# Created : 2024-04-03 15:17
DROP PROCEDURE IF EXISTS proc_DashBoard;
CREATE PROCEDURE proc_DashBoard(InDate VARCHAR(10))
    SQL SECURITY INVOKER
    COMMENT '首页报表生成'
BEGIN
#Author: wangcw
#Create: 2024-04-03 15:19:12
	#日志记录定义模块
	DECLARE sys_StartTime  DATETIME;
	DECLARE sys_ErrCode    VARCHAR(5) DEFAULT '00000';
	DECLARE sys_ErrMessage VARCHAR(200);
	DECLARE Result INT(1) DEFAULT 1;
	DECLARE CONTINUE HANDLER FOR SQLEXCEPTION
	BEGIN GET DIAGNOSTICS CONDITION 1
		sys_ErrCode=RETURNED_SQLSTATE,sys_ErrMessage=MESSAGE_TEXT;
	END;
 	SET sys_StartTime=CURRENT_TIMESTAMP();

    #开启事务
	START TRANSACTION;

	#修改隔离级别,防止对业务表过长的共享锁占用
	SET SESSION TRANSACTION ISOLATION LEVEL READ UNCOMMITTED;

    TRUNCATE TABLE eadm_dashboard;

	# 周平均心率
    INSERT INTO eadm_dashboard(DataType, DateType, LoginName, DataValue, CheckDate)
    SELECT 1, 2, 'wangcw', IFNULL(ROUND(AVG(Heartbeat)),0), InDate
	FROM eadm.watchdaily
	WHERE CONVERT_TZ(UtcTime,'+00:00','+08:00') >= DATE_ADD(InDate, INTERVAL -7 DAY)
	  AND CONVERT_TZ(UtcTime,'+00:00','+08:00') < InDate
	  AND Heartbeat IS NOT NULL;

    # 周平均步数
    INSERT INTO eadm_dashboard(DataType, DateType, LoginName, DataValue, CheckDate)
	SELECT 2, 2, 'wangcw', IFNULL(ROUND(AVG(S.MaxSteps)),0), InDate
	FROM (
	SELECT MAX(Steps) AS MaxSteps
    FROM eadm.watchdaily
	WHERE CONVERT_TZ(UtcTime,'+00:00','+08:00') >= DATE_ADD(InDate, INTERVAL -7 DAY)
	  AND CONVERT_TZ(UtcTime,'+00:00','+08:00') < InDate
	  AND Steps IS NOT NULL
	GROUP BY DATE_FORMAT(CONVERT_TZ(UtcTime,'+00:00','+08:00'), '%Y-%m-%d')) AS S
	WHERE S.MaxSteps IS NOT NULL;

    # 周平均睡眠
    INSERT INTO eadm_dashboard(DataType, DateType, LoginName, DataValue, CheckDate)
	SELECT 3, 2, 'wangcw', IFNULL(ROUND(AVG(S.SumSleepMinute/60)),0), InDate
	FROM (
	SELECT SUM(SleepMinute) AS SumSleepMinute
    FROM eadm.watchdaily
	WHERE CONVERT_TZ(UtcTime,'+00:00','+08:00') >= DATE_ADD(InDate, INTERVAL -7 DAY)
	  AND CONVERT_TZ(UtcTime,'+00:00','+08:00') < InDate
	  AND SleepMinute IS NOT NULL
	GROUP BY DATE_FORMAT(CONVERT_TZ(UtcTime,'+00:00','+08:00'), '%Y-%m-%d')) AS S
	WHERE S.SumSleepMinute IS NOT NULL;

    # 周平均里程
    INSERT INTO eadm_dashboard(DataType, DateType, LoginName, DataValue, CheckDate)
    SELECT 4, 2, 'wangcw', IFNULL(ROUND(AVG(DuaMileAge)),0), InDate
	FROM (
	SELECT MAX(mileage) - MIN(mileage) AS DuaMileAge
    FROM eadm.carlocdaily
	WHERE CONVERT_TZ(dev_upload,'+00:00','+08:00') >= DATE_ADD(InDate, INTERVAL -7 DAY)
	  AND CONVERT_TZ(dev_upload,'+00:00','+08:00') < InDate
	  AND mileage IS NOT NULL
	GROUP BY DATE_FORMAT(CONVERT_TZ(dev_upload,'+00:00','+08:00'), '%Y-%m-%d')) AS S
	WHERE S.DuaMileAge IS NOT NULL
	  AND S.DuaMileAge !=0;

    # 年度平均心率
    INSERT INTO eadm_dashboard(DataType, DateType, LoginName, DataValue, CheckDate)
    SELECT 1, 4, 'wangcw', IFNULL(ROUND(AVG(Heartbeat)),0), InDate
	FROM eadm.watchdaily
	WHERE CONVERT_TZ(UtcTime,'+00:00','+08:00') >= DATE_ADD(InDate, INTERVAL -1 YEAR)
	  AND CONVERT_TZ(UtcTime,'+00:00','+08:00') < InDate
	  AND Heartbeat IS NOT NULL;

    # 年度平均步数
    INSERT INTO eadm_dashboard(DataType, DateType, LoginName, DataValue, CheckDate)
	SELECT 2, 4, 'wangcw', IFNULL(ROUND(AVG(S.MaxSteps)),0), InDate
	FROM (
	SELECT MAX(Steps) AS MaxSteps
    FROM eadm.watchdaily
	WHERE CONVERT_TZ(UtcTime,'+00:00','+08:00') >= DATE_ADD(InDate, INTERVAL -1 YEAR)
	  AND CONVERT_TZ(UtcTime,'+00:00','+08:00') < InDate
	  AND Steps IS NOT NULL
	GROUP BY DATE_FORMAT(CONVERT_TZ(UtcTime,'+00:00','+08:00'), '%Y-%m-%d')) AS S
	WHERE S.MaxSteps IS NOT NULL;

    # 年度平均睡眠
    INSERT INTO eadm_dashboard(DataType, DateType, LoginName, DataValue, CheckDate)
	SELECT 3, 4, 'wangcw', IFNULL(ROUND(AVG(S.SumSleepMinute/60)),0), InDate
	FROM (
	SELECT SUM(SleepMinute) AS SumSleepMinute
    FROM eadm.watchdaily
	WHERE CONVERT_TZ(UtcTime,'+00:00','+08:00') >= DATE_ADD(InDate, INTERVAL -1 YEAR)
	  AND CONVERT_TZ(UtcTime,'+00:00','+08:00') < InDate
	  AND SleepMinute IS NOT NULL
	GROUP BY DATE_FORMAT(CONVERT_TZ(UtcTime,'+00:00','+08:00'), '%Y-%m-%d')) AS S
	WHERE S.SumSleepMinute IS NOT NULL;

    # 年度总里程
	INSERT INTO eadm_dashboard(DataType, DateType, LoginName, DataValue, CheckDate)
	SELECT 4, 4, 'wangcw', IFNULL(MAX(mileage) - MIN(mileage),0), InDate
    FROM eadm.carlocdaily
	WHERE CONVERT_TZ(dev_upload,'+00:00','+08:00') >= CONCAT(YEAR(InDate), '-01-01')
	  AND CONVERT_TZ(dev_upload,'+00:00','+08:00') < InDate
	  AND mileage IS NOT NULL;

	# 每月里程
    INSERT INTO eadm_dashboard(DataType, DateType, DataValue, LoginName, CheckDate)
    VALUES (5, 3, 0, 'wangcw', DATE_FORMAT(InDate,'%Y-%m')),
           (5, 3, 0, 'wangcw', DATE_FORMAT(DATE_ADD(InDate, INTERVAL -1 MONTH),'%Y-%m')),
           (5, 3, 0, 'wangcw', DATE_FORMAT(DATE_ADD(InDate, INTERVAL -2 MONTH),'%Y-%m')),
           (5, 3, 0, 'wangcw', DATE_FORMAT(DATE_ADD(InDate, INTERVAL -3 MONTH),'%Y-%m')),
           (5, 3, 0, 'wangcw', DATE_FORMAT(DATE_ADD(InDate, INTERVAL -4 MONTH),'%Y-%m')),
           (5, 3, 0, 'wangcw', DATE_FORMAT(DATE_ADD(InDate, INTERVAL -5 MONTH),'%Y-%m')),
           (5, 3, 0, 'wangcw', DATE_FORMAT(DATE_ADD(InDate, INTERVAL -6 MONTH),'%Y-%m')),
           (5, 3, 0, 'wangcw', DATE_FORMAT(DATE_ADD(InDate, INTERVAL -7 MONTH),'%Y-%m')),
           (5, 3, 0, 'wangcw', DATE_FORMAT(DATE_ADD(InDate, INTERVAL -8 MONTH),'%Y-%m')),
           (5, 3, 0, 'wangcw', DATE_FORMAT(DATE_ADD(InDate, INTERVAL -9 MONTH),'%Y-%m')),
           (5, 3, 0, 'wangcw', DATE_FORMAT(DATE_ADD(InDate, INTERVAL -10 MONTH),'%Y-%m')),
           (5, 3, 0, 'wangcw', DATE_FORMAT(DATE_ADD(InDate, INTERVAL -11 MONTH),'%Y-%m'));

	UPDATE eadm_dashboard A,
	    (SELECT DATE_FORMAT(CONVERT_TZ(B.dev_upload,'+00:00','+08:00'), '%Y-%m') AS CheckMonth,
            MAX(B.mileage)-MIN(B.mileage) AS mileage
        FROM eadm.carlocdaily B,
             eadm_userdevice C
        WHERE B.device_id=C.DeviceNo
          AND C.LoginName='wangcw'
        GROUP BY DATE_FORMAT(CONVERT_TZ(B.dev_upload,'+00:00','+08:00'), '%Y-%m')
        ORDER BY CheckMonth) D
	SET A.DataValue=D.mileage
	WHERE A.DateType=3
	  AND A.DataType=5
	  AND A.CheckDate=D.CheckMonth;

    # 每月收入
    INSERT INTO eadm_dashboard(DataType, DateType, DataValue, LoginName, CheckDate)
    VALUES (6, 3, 0, 'wangcw', DATE_FORMAT(InDate,'%Y-%m')),
           (6, 3, 0, 'wangcw', DATE_FORMAT(DATE_ADD(InDate, INTERVAL -1 MONTH),'%Y-%m')),
           (6, 3, 0, 'wangcw', DATE_FORMAT(DATE_ADD(InDate, INTERVAL -2 MONTH),'%Y-%m')),
           (6, 3, 0, 'wangcw', DATE_FORMAT(DATE_ADD(InDate, INTERVAL -3 MONTH),'%Y-%m')),
           (6, 3, 0, 'wangcw', DATE_FORMAT(DATE_ADD(InDate, INTERVAL -4 MONTH),'%Y-%m')),
           (6, 3, 0, 'wangcw', DATE_FORMAT(DATE_ADD(InDate, INTERVAL -5 MONTH),'%Y-%m')),
           (6, 3, 0, 'wangcw', DATE_FORMAT(DATE_ADD(InDate, INTERVAL -6 MONTH),'%Y-%m')),
           (6, 3, 0, 'wangcw', DATE_FORMAT(DATE_ADD(InDate, INTERVAL -7 MONTH),'%Y-%m')),
           (6, 3, 0, 'wangcw', DATE_FORMAT(DATE_ADD(InDate, INTERVAL -8 MONTH),'%Y-%m')),
           (6, 3, 0, 'wangcw', DATE_FORMAT(DATE_ADD(InDate, INTERVAL -9 MONTH),'%Y-%m')),
           (6, 3, 0, 'wangcw', DATE_FORMAT(DATE_ADD(InDate, INTERVAL -10 MONTH),'%Y-%m')),
           (6, 3, 0, 'wangcw', DATE_FORMAT(DATE_ADD(InDate, INTERVAL -11 MONTH),'%Y-%m'));

	UPDATE eadm_dashboard A,
	    (SELECT DATE_FORMAT(TradeTime, '%Y-%m') AS CheckMonth,
            SUM(IFNULL(Amount,0)) AS Amount
        FROM paybilldetail B
        WHERE B.InOrOut='收入'
        GROUP BY DATE_FORMAT(B.TradeTime, '%Y-%m')) C
	SET A.DataValue=C.Amount
	WHERE A.DateType=3
	  AND A.DataType=6
	  AND A.CheckDate=C.CheckMonth;

    # 每月支出
    INSERT INTO eadm_dashboard(DataType, DateType, DataValue, LoginName, CheckDate)
    VALUES (7, 3, 0, 'wangcw', DATE_FORMAT(InDate,'%Y-%m')),
           (7, 3, 0, 'wangcw', DATE_FORMAT(DATE_ADD(InDate, INTERVAL -1 MONTH),'%Y-%m')),
           (7, 3, 0, 'wangcw', DATE_FORMAT(DATE_ADD(InDate, INTERVAL -2 MONTH),'%Y-%m')),
           (7, 3, 0, 'wangcw', DATE_FORMAT(DATE_ADD(InDate, INTERVAL -3 MONTH),'%Y-%m')),
           (7, 3, 0, 'wangcw', DATE_FORMAT(DATE_ADD(InDate, INTERVAL -4 MONTH),'%Y-%m')),
           (7, 3, 0, 'wangcw', DATE_FORMAT(DATE_ADD(InDate, INTERVAL -5 MONTH),'%Y-%m')),
           (7, 3, 0, 'wangcw', DATE_FORMAT(DATE_ADD(InDate, INTERVAL -6 MONTH),'%Y-%m')),
           (7, 3, 0, 'wangcw', DATE_FORMAT(DATE_ADD(InDate, INTERVAL -7 MONTH),'%Y-%m')),
           (7, 3, 0, 'wangcw', DATE_FORMAT(DATE_ADD(InDate, INTERVAL -8 MONTH),'%Y-%m')),
           (7, 3, 0, 'wangcw', DATE_FORMAT(DATE_ADD(InDate, INTERVAL -9 MONTH),'%Y-%m')),
           (7, 3, 0, 'wangcw', DATE_FORMAT(DATE_ADD(InDate, INTERVAL -10 MONTH),'%Y-%m')),
           (7, 3, 0, 'wangcw', DATE_FORMAT(DATE_ADD(InDate, INTERVAL -11 MONTH),'%Y-%m'));

	UPDATE eadm_dashboard A,
	    (SELECT DATE_FORMAT(TradeTime, '%Y-%m') AS CheckMonth,
            SUM(IFNULL(Amount,0)) AS Amount
        FROM paybilldetail B
        WHERE B.InOrOut <> '收入'
        GROUP BY DATE_FORMAT(B.TradeTime, '%Y-%m')) C
	SET A.DataValue=C.Amount
	WHERE A.DateType=3
	  AND A.DataType=7
	  AND A.CheckDate=C.CheckMonth;

    #恢复隔离级别
	SET SESSION TRANSACTION ISOLATION LEVEL READ COMMITTED;

	#事务提交/回滚模块
	IF sys_ErrCode <> '00000' THEN
    SET Result = 0;
    ROLLBACK;
	ELSE
    SET Result = 1;
	COMMIT;
	END IF;

    #日志记录生成模块
	INSERT INTO sys_proclog(ProcName, TimeSpan, Result, ErrCode, ErrMessage)
	VALUES ('proc_DashBoard', TIMESTAMPDIFF(SECOND, sys_StartTime, NOW()), Result, sys_ErrCode, sys_ErrMessage);

END
