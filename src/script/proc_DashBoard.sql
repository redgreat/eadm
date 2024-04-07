# @author wangcw
# @copyright (C) 2024, REDGREAT
# Created : 2024-04-03 15:17

CREATE PROCEDURE `proc_DashBoard`()
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

	# 周平均心率
    INSERT INTO eadm_dashboard(DataType, DateType, LoginName, DataValue, CheckDate)
    SELECT 1, 2, 'wangcw', IFNULL(ROUND(AVG(Heartbeat)),0), CURDATE()
	FROM eadm.watchdaily
	WHERE UtcTime >= DATE_ADD(NOW(), INTERVAL -7 DAY)
	  AND UtcTime < CURDATE()
	  AND Heartbeat IS NOT NULL;

    # 周平均步数
    INSERT INTO eadm_dashboard(DataType, DateType, LoginName, DataValue, CheckDate)
	SELECT 2, 2, 'wangcw', IFNULL(ROUND(AVG(S.MaxSteps)),0), CURDATE()
	FROM (
	SELECT MAX(Steps) AS MaxSteps
    FROM eadm.watchdaily
	WHERE UtcTime >= DATE_ADD(NOW(), INTERVAL -7 DAY)
	  AND UtcTime < CURDATE()
	  AND Steps IS NOT NULL
	GROUP BY DATE_FORMAT(UtcTime, '%Y-%m-%d')) AS S
	WHERE S.MaxSteps IS NOT NULL;

    # 周平均睡眠
    INSERT INTO eadm_dashboard(DataType, DateType, LoginName, DataValue, CheckDate)
	SELECT 3, 2, 'wangcw', IFNULL(ROUND(AVG(S.SumSleepMinute/60)),0), CURDATE()
	FROM (
	SELECT SUM(SleepMinute) AS SumSleepMinute
    FROM eadm.watchdaily
	WHERE UtcTime >= DATE_ADD(NOW(), INTERVAL -7 DAY)
	  AND UtcTime < CURDATE()
	  AND SleepMinute IS NOT NULL
	GROUP BY DATE_FORMAT(UtcTime, '%Y-%m-%d')) AS S
	WHERE S.SumSleepMinute IS NOT NULL;

    # 周平均里程
    INSERT INTO eadm_dashboard(DataType, DateType, LoginName, DataValue, CheckDate)
    SELECT 4, 2, 'wangcw', IFNULL(ROUND(AVG(DuaMileAge)),0), CURDATE()
	FROM (
	SELECT MAX(mileage) - MIN(mileage) AS DuaMileAge
    FROM eadm.carlocdaily
	WHERE dev_upload >= DATE_ADD(NOW(), INTERVAL -7 DAY)
	  AND dev_upload < CURDATE()
	  AND mileage IS NOT NULL
	GROUP BY DATE_FORMAT(dev_upload, '%Y-%m-%d')) AS S
	WHERE S.DuaMileAge IS NOT NULL
	  AND S.DuaMileAge !=0;

    # 年度里程


    # 年度财务

    # 年度平均心率
    INSERT INTO eadm_dashboard(DataType, DateType, LoginName, DataValue, CheckDate)
    SELECT 1, 4, 'wangcw', IFNULL(ROUND(AVG(Heartbeat)),0), CURDATE()
	FROM eadm.watchdaily
	WHERE UtcTime >= DATE_ADD(NOW(), INTERVAL -1 YEAR)
	  AND UtcTime < CURDATE()
	  AND Heartbeat IS NOT NULL;

    # 年度平均步数
    INSERT INTO eadm_dashboard(DataType, DateType, LoginName, DataValue, CheckDate)
	SELECT 2, 4, 'wangcw', IFNULL(ROUND(AVG(S.MaxSteps)),0), CURDATE()
	FROM (
	SELECT MAX(Steps) AS MaxSteps
    FROM eadm.watchdaily
	WHERE UtcTime >= DATE_ADD(NOW(), INTERVAL -1 YEAR)
	  AND UtcTime < CURDATE()
	  AND Steps IS NOT NULL
	GROUP BY DATE_FORMAT(UtcTime, '%Y-%m-%d')) AS S
	WHERE S.MaxSteps IS NOT NULL;

    # 年度平均睡眠
    INSERT INTO eadm_dashboard(DataType, DateType, LoginName, DataValue, CheckDate)
	SELECT 3, 4, 'wangcw', IFNULL(ROUND(AVG(S.SumSleepMinute/60)),0), CURDATE()
	FROM (
	SELECT SUM(SleepMinute) AS SumSleepMinute
    FROM eadm.watchdaily
	WHERE UtcTime >= DATE_ADD(NOW(), INTERVAL -1 YEAR)
	  AND UtcTime < CURDATE()
	  AND SleepMinute IS NOT NULL
	GROUP BY DATE_FORMAT(UtcTime, '%Y-%m-%d')) AS S
	WHERE S.SumSleepMinute IS NOT NULL;

    # 年度总里程
	INSERT INTO eadm_dashboard(DataType, DateType, LoginName, DataValue, CheckDate)
	SELECT 4, 4, 'wangcw', IFNULL(MAX(mileage) - MIN(mileage),0), CURDATE()
    FROM eadm.carlocdaily
	WHERE dev_upload >= CONCAT(YEAR(NOW()), '-01-01')
	  AND dev_upload < CURDATE()
	  AND mileage IS NOT NULL;

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
