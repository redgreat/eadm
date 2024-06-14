# @author wangcw
# @copyright (C) 2024, REDGREAT
# Created : 2024-04-07 16:33

SELECT X.CheckDate,X.mileage-IFNULL(Y.mileage,0) AS Mileage
FROM (SELECT DATE_FORMAT(A.dev_upload, '%Y-%m') AS CheckDate, MAX(A.mileage) AS mileage
  FROM eadm.carlocdaily A
  WHERE A.dev_upload >= '2023-02-01'
  AND A.dev_upload < '2024-04-01'
  GROUP BY DATE_FORMAT(A.dev_upload, '%Y-%m')) X
LEFT JOIN (SELECT DATE_FORMAT(DATE_ADD(B.dev_upload, INTERVAL 1 MONTH), '%Y-%m') AS CheckDate,
                  MAX(B.mileage) AS mileage
  FROM eadm.carlocdaily B
  WHERE B.dev_upload >= '2023-02-01'
  AND B.dev_upload < '2024-04-01'
  GROUP BY DATE_FORMAT(DATE_ADD(B.dev_upload, INTERVAL -1 MONTH), '%Y-%m')) Y
  ON X.CheckDate=Y.CheckDate
ORDER BY 1;

SELECT JSON_ARRAYAGG(DataValue) as data
FROM eadm_dashboard
WHERE LoginName = 'wangcw'
ORDER BY DateType, DataType;

SELECT JSON_ARRAYAGG(S.mileage) AS data
FROM (
SELECT DATE_FORMAT(A.dev_upload, '%Y-%m') AS CheckMonth,
       MAX(A.mileage)-MIN(A.mileage) AS mileage
FROM eadm.carlocdaily A,
     eadm_userdevice B
WHERE A.dev_upload >= '2023-02-01'
  AND A.dev_upload < '2024-04-01'
  AND A.device_id=B.DeviceNo
  AND B.LoginName='wangcw'
GROUP BY DATE_FORMAT(A.dev_upload, '%Y-%m')
ORDER BY CheckMonth) AS S;

SELECT JSON_ARRAYAGG(S.Amount) AS data
FROM (
SELECT DATE_FORMAT(TradeTime, '%Y-%m') AS CheckMonth,
       SUM(Amount) AS Amount
FROM paybilldetail
WHERE InOrOut=1
  AND TradeTime >= '2022-02-01'
  AND TradeTime < '2023-04-01'
GROUP BY DATE_FORMAT(TradeTime, '%Y-%m')
ORDER BY CheckMonth) AS S;

SELECT JSON_ARRAYAGG(S.Amount) AS data
FROM (
SELECT DATE_FORMAT(TradeTime, '%Y-%m') AS CheckMonth,
       IFNULL(SUM(Amount),0) AS Amount
FROM paybilldetail
WHERE InOrOut IN (0, 2)
  AND TradeTime >= '2023-04-01'
  AND TradeTime < '2024-04-01'
GROUP BY DATE_FORMAT(TradeTime, '%Y-%m')
ORDER BY CheckMonth) AS S;

CALL proc_DashBoard('2022-11-08');

CALL proc_DashBoard('2023-11-08');

CALL proc_DashBoard('2024-02-08');

UPDATE paybilldetail
SET InOrOut=CASE InOrOut WHEN '收入' THEN 1 WHEN '支出' THEN 2 ELSE 0 END
WHERE 1=1;

SELECT *
FROM eadm_dashboard;

SELECT * FROM sys_proclog ORDER BY InsertTime DESC;
