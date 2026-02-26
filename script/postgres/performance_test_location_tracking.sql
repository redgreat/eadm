-- ============================================================================
-- 性能测试脚本：iOS位置追踪和轨迹回放功能
-- 用途：使用EXPLAIN ANALYZE分析查询性能，验证索引使用情况
-- ============================================================================

-- 设置查询超时时间为10秒
SET statement_timeout = '10s';

-- ============================================================================
-- 1. 佳明活动轨迹查询性能测试
-- ============================================================================

-- 测试1.1: 查询佳明活动记录（按时间范围）
EXPLAIN (ANALYZE, BUFFERS, VERBOSE)
SELECT activityid, starttime, endtime, startlat, startlng, endlat, endlng
FROM garmin_activity
WHERE starttime >= '2024-01-01 00:00:00'::timestamptz
  AND starttime <= '2024-01-07 23:59:59'::timestamptz
ORDER BY starttime ASC
LIMIT 10000;

-- 测试1.2: 查询佳明活动详细轨迹点
EXPLAIN (ANALYZE, BUFFERS, VERBOSE)
SELECT pointtime, latitude, longitude, elevation, speed, heartrate
FROM garmin_activity_detail
WHERE activityid = 1
ORDER BY pointtime ASC
LIMIT 10000;

-- ============================================================================
-- 2. 手表定位轨迹查询性能测试
-- ============================================================================

-- 测试2.1: 查询手表定位数据（按时间范围）
EXPLAIN (ANALYZE, BUFFERS, VERBOSE)
SELECT ptime, latitude, longitude, altitude, speed
FROM lc_watchlocation
WHERE ptime >= '2024-01-01 00:00:00'::timestamptz
  AND ptime <= '2024-01-07 23:59:59'::timestamptz
ORDER BY ptime ASC
LIMIT 10000;

-- ============================================================================
-- 3. 车辆定位轨迹查询性能测试
-- ============================================================================

-- 测试3.1: 查询车辆定位数据（按时间范围）
EXPLAIN (ANALYZE, BUFFERS, VERBOSE)
SELECT ptime, latitude, longitude, altitude, speed
FROM lc_carlocdaily
WHERE ptime >= '2024-01-01 00:00:00'::timestamptz
  AND ptime <= '2024-01-07 23:59:59'::timestamptz
ORDER BY ptime ASC
LIMIT 10000;

-- ============================================================================
-- 4. 设备数据轨迹查询性能测试
-- ============================================================================

-- 测试4.1: 查询EMQX设备数据（按时间范围）
EXPLAIN (ANALYZE, BUFFERS, VERBOSE)
SELECT ptime, latitude, longitude, altitude, speed
FROM emqx_device_data
WHERE ptime >= '2024-01-01 00:00:00'::timestamptz
  AND ptime <= '2024-01-07 23:59:59'::timestamptz
ORDER BY ptime ASC
LIMIT 10000;

-- ============================================================================
-- 5. 野点GNS轨迹查询性能测试
-- ============================================================================

-- 测试5.1: 查询野点GNS数据（按时间范围）
EXPLAIN (ANALYZE, BUFFERS, VERBOSE)
SELECT ptime, latitude, longitude, altitude, speed
FROM lc_yedgns
WHERE ptime >= '2024-01-01 00:00:00'::timestamptz
  AND ptime <= '2024-01-07 23:59:59'::timestamptz
ORDER BY ptime ASC
LIMIT 10000;

-- ============================================================================
-- 6. 赛车盒子轨迹查询性能测试
-- ============================================================================

-- 测试6.1: 查询赛车盒子数据（按时间范围）
EXPLAIN (ANALYZE, BUFFERS, VERBOSE)
SELECT ptime, latitude, longitude, altitude, speed
FROM lc_racebox
WHERE ptime >= '2024-01-01 00:00:00'::timestamptz
  AND ptime <= '2024-01-07 23:59:59'::timestamptz
ORDER BY ptime ASC
LIMIT 10000;

-- ============================================================================
-- 7. 索引使用情况验证
-- ============================================================================

-- 验证索引是否被正确创建
SELECT
    schemaname,
    tablename,
    indexname,
    indexdef
FROM pg_indexes
WHERE tablename IN (
    'garmin_activity',
    'garmin_activity_detail',
    'lc_watchlocation',
    'lc_carlocdaily',
    'emqx_device_data',
    'lc_yedgns',
    'lc_racebox'
)
ORDER BY tablename, indexname;

-- ============================================================================
-- 8. 表统计信息
-- ============================================================================

-- 查看各表的行数和大小
SELECT
    schemaname,
    tablename,
    pg_size_pretty(pg_total_relation_size(schemaname||'.'||tablename)) AS total_size,
    pg_size_pretty(pg_relation_size(schemaname||'.'||tablename)) AS table_size,
    pg_size_pretty(pg_total_relation_size(schemaname||'.'||tablename) - pg_relation_size(schemaname||'.'||tablename)) AS index_size,
    n_live_tup AS row_count
FROM pg_stat_user_tables
WHERE tablename IN (
    'garmin_activity',
    'garmin_activity_detail',
    'lc_watchlocation',
    'lc_carlocdaily',
    'emqx_device_data',
    'lc_yedgns',
    'lc_racebox'
)
ORDER BY tablename;

-- ============================================================================
-- 9. 查询性能分析说明
-- ============================================================================

/*
性能分析要点：

1. 执行计划分析：
   - 查看是否使用了索引扫描（Index Scan）而非全表扫描（Seq Scan）
   - 索引扫描表示索引被正确使用
   - 全表扫描可能表示索引未生效或数据量太小

2. 执行时间分析：
   - Planning Time: 查询规划时间，应该很短（< 1ms）
   - Execution Time: 实际执行时间，目标 < 2000ms（P95）
   - 如果执行时间过长，考虑优化索引或查询条件

3. 缓冲区分析（BUFFERS）：
   - Shared Hit: 从共享缓冲区命中的块数（缓存命中）
   - Shared Read: 从磁盘读取的块数（缓存未命中）
   - 高缓存命中率表示性能良好

4. 索引验证：
   - 确认所有时间字段都有降序索引
   - 索引命名格式：idx_<表名>_<字段名>_desc
   - 如果索引不存在，需要创建

5. 性能优化建议：
   - 如果查询时间 > 2秒，考虑：
     * 增加数据库连接池大小
     * 优化查询条件
     * 添加更多索引
     * 使用分区表
   - 如果缓存命中率低，考虑：
     * 增加shared_buffers配置
     * 预热缓存
*/

-- ============================================================================
-- 10. 连接池状态查询
-- ============================================================================

-- 查看当前活动连接数
SELECT
    count(*) AS total_connections,
    count(*) FILTER (WHERE state = 'active') AS active_connections,
    count(*) FILTER (WHERE state = 'idle') AS idle_connections,
    count(*) FILTER (WHERE state = 'idle in transaction') AS idle_in_transaction
FROM pg_stat_activity
WHERE datname = current_database();

-- 查看长时间运行的查询
SELECT
    pid,
    now() - query_start AS duration,
    state,
    query
FROM pg_stat_activity
WHERE state != 'idle'
  AND query NOT LIKE '%pg_stat_activity%'
ORDER BY duration DESC
LIMIT 10;
