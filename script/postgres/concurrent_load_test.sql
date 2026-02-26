-- ============================================================================
-- 并发负载测试脚本：iOS位置追踪API
-- 用途：测试100个并发查询的性能表现
-- 使用方法：使用pgbench或自定义脚本执行
-- ============================================================================

-- 注意：此文件包含用于pgbench的测试查询
-- 执行方式：pgbench -c 100 -j 10 -t 10 -f concurrent_load_test.sql eadm

-- ============================================================================
-- 测试查询1：佳明活动轨迹查询
-- ============================================================================
\set start_time '2024-01-01 00:00:00'
\set end_time '2024-01-07 23:59:59'

SELECT
    a.activityid,
    a.starttime,
    a.endtime,
    d.pointtime,
    d.latitude,
    d.longitude,
    d.elevation,
    d.speed,
    d.heartrate
FROM garmin_activity a
JOIN garmin_activity_detail d ON a.activityid = d.activityid
WHERE a.starttime >= :'start_time'::timestamptz
  AND a.starttime <= :'end_time'::timestamptz
ORDER BY d.pointtime ASC
LIMIT 10000;
