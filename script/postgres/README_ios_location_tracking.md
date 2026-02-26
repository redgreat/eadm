# iOS位置追踪功能数据库迁移说明

## 概述

此迁移脚本为iOS位置追踪和轨迹回放功能创建必要的数据库表和索引。

## 迁移内容

### 新增表

1. **emqx_device_data** - MQTT设备定位数据表
   - 存储通过MQTT协议上报的设备GPS数据
   - 字段：device_id, ptime, latitude, longitude, altitude, speed

2. **lc_yedgns** - 野点GNS设备数据表
   - 存储野点GNS设备的GPS轨迹数据
   - 字段：device_id, ptime, latitude, longitude, altitude, speed

3. **lc_racebox** - 赛车盒子设备数据表
   - 存储赛车盒子设备的GPS和加速度数据
   - 字段：device_id, ptime, latitude, longitude, altitude, speed, acceleration

4. **audit_log** - 审计日志表
   - 记录所有位置数据访问操作
   - 字段：user_id, action, resource, timestamp, result, details

### 索引优化

所有轨迹表的时间字段均创建降序索引（DESC），优化最近数据查询性能：
- `emqx_device_data.ptime` (DESC)
- `lc_yedgns.ptime` (DESC)
- `lc_racebox.ptime` (DESC)
- `garmin_activity_detail.pointtime` (DESC) - 验证已存在
- `lc_watchlocation.ptime` (DESC) - 验证已存在
- `lc_carlocdaily.ptime` (DESC) - 验证已存在

所有设备ID字段创建索引以优化设备过滤查询。

## 执行方式

### 方式1：使用psql命令行

```bash
# 连接到数据库
psql -U user_eadm -d eadm

# 执行迁移脚本
\i script/postgres/ios_location_tracking_migration.sql
```

### 方式2：使用psql单行命令

```bash
psql -U user_eadm -d eadm -f script/postgres/ios_location_tracking_migration.sql
```

### 方式3：在应用中执行

如果需要在应用启动时自动执行迁移，可以在Erlang代码中调用：

```erlang
%% 读取并执行SQL文件
{ok, SQL} = file:read_file("script/postgres/ios_location_tracking_migration.sql"),
eadm_pgpool:squery(pool_pg, binary_to_list(SQL)).
```

## 验证迁移

执行以下SQL验证表和索引是否正确创建：

```sql
-- 检查表是否存在
SELECT table_name 
FROM information_schema.tables 
WHERE table_schema = 'public' 
  AND table_name IN ('emqx_device_data', 'lc_yedgns', 'lc_racebox', 'audit_log');

-- 检查索引是否存在
SELECT tablename, indexname 
FROM pg_indexes 
WHERE tablename IN ('emqx_device_data', 'lc_yedgns', 'lc_racebox', 'audit_log',
                    'garmin_activity_detail', 'lc_watchlocation', 'lc_carlocdaily')
ORDER BY tablename, indexname;

-- 检查表结构
\d emqx_device_data
\d lc_yedgns
\d lc_racebox
\d audit_log
```

## 回滚

如果需要回滚此迁移，执行以下SQL：

```sql
-- 删除新创建的表
DROP TABLE IF EXISTS emqx_device_data CASCADE;
DROP TABLE IF EXISTS lc_yedgns CASCADE;
DROP TABLE IF EXISTS lc_racebox CASCADE;
DROP TABLE IF EXISTS audit_log CASCADE;

-- 删除补充的索引（如果是新创建的）
DROP INDEX IF EXISTS idx_watchlocation_ptime;
DROP INDEX IF EXISTS idx_carlocdaily_ptime;
```

## 注意事项

1. **权限要求**：执行此脚本需要数据库用户具有CREATE TABLE和CREATE INDEX权限
2. **已存在表**：脚本使用`DROP TABLE IF EXISTS`，如果表已存在会先删除
3. **数据保留**：如果表中已有数据，删除表会导致数据丢失，请先备份
4. **索引验证**：脚本会检查已存在表的索引，如果不存在才创建
5. **时区设置**：脚本设置时区为'asia/shanghai'，所有TIMESTAMPTZ字段使用此时区

## 性能考虑

- 所有时间字段索引使用DESC排序，优化查询最近数据的场景
- 设备ID字段索引使用ASC排序，优化设备过滤查询
- 建议在数据量较大时（>100万行）考虑表分区策略
- audit_log表建议定期归档（保留90天）以控制表大小

## 相关文档

- 需求文档：`eadm/.kiro/specs/ios-location-tracking-replay/requirements.md`
- 设计文档：`eadm/.kiro/specs/ios-location-tracking-replay/design.md`
- 任务列表：`eadm/.kiro/specs/ios-location-tracking-replay/tasks.md`
