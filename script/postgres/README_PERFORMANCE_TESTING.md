# 性能测试指南

## 概述

本目录包含iOS位置追踪和轨迹回放功能的数据库性能测试脚本和工具。

## 测试目标

根据需求7（数据库查询优化），性能测试验证以下指标：

- ✓ 初始连接数：10个连接
- ✓ 最大溢出连接数：10个
- ✓ FIFO策略配置
- ✓ 查询超时时间：10秒
- ✓ API响应时间P95 < 2秒
- ✓ 支持100个并发查询

## 文件说明

### 测试脚本

1. **performance_test_location_tracking.sql**
   - SQL性能分析脚本
   - 使用EXPLAIN ANALYZE分析查询性能
   - 验证索引使用情况
   - 检查表统计信息

2. **concurrent_load_test.sql**
   - 并发负载测试脚本
   - 配合pgbench使用
   - 测试100个并发查询性能

3. **run_performance_tests.sh**
   - 自动化测试脚本
   - 执行所有性能测试
   - 生成测试报告

### Erlang测试模块

4. **src/test/performance_test_location.erl**
   - Erlang性能测试模块
   - 测试单次查询和并发查询
   - 监控连接池状态
   - 计算P50/P95/P99响应时间

## 快速开始

### 方法1: 使用自动化脚本（推荐）

```bash
# 进入脚本目录
cd eadm/script/postgres

# 执行自动化测试
./run_performance_tests.sh
```

脚本会自动执行：
- 索引验证
- EXPLAIN ANALYZE性能分析
- 表统计信息收集
- 并发负载测试（如果安装了pgbench）
- 连接池状态检查

测试结果保存在：`.kiro/specs/ios-location-tracking-replay/`

### 方法2: 手动执行SQL测试

```bash
# 连接到数据库
psql -h 23.95.68.200 -p 8432 -U user_eadm -d eadm

# 执行性能测试脚本
\i script/postgres/performance_test_location_tracking.sql
```

### 方法3: 使用Erlang测试模块

```bash
# 启动Erlang shell
cd eadm
rebar3 shell

# 编译测试模块
c("src/test/performance_test_location.erl").

# 运行所有测试
performance_test_location:run_all_tests().

# 或单独运行特定测试
performance_test_location:test_single_query().
performance_test_location:test_concurrent_queries(100).
performance_test_location:test_connection_pool().
```

## 详细测试步骤

### 1. 索引验证

验证所有轨迹表的时间字段已创建索引：

```sql
SELECT tablename, indexname, indexdef 
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
```

**预期结果**：每个表的时间字段都有降序索引（idx_*_desc）

### 2. 查询性能分析

使用EXPLAIN ANALYZE分析查询执行计划：

```sql
EXPLAIN (ANALYZE, BUFFERS, VERBOSE)
SELECT pointtime, latitude, longitude, elevation, speed, heartrate
FROM garmin_activity_detail
WHERE activityid = 1
ORDER BY pointtime ASC
LIMIT 10000;
```

**关键指标**：
- 执行计划：应使用Index Scan而非Seq Scan
- Execution Time：应 < 2000ms
- Buffers：高缓存命中率（Shared Hit）

### 3. 并发负载测试

使用pgbench测试100个并发连接：

```bash
pgbench -h 23.95.68.200 -p 8432 -U user_eadm -d eadm \
  -c 100 -j 10 -t 10 \
  -f script/postgres/concurrent_load_test.sql
```

**参数说明**：
- `-c 100`: 100个并发客户端
- `-j 10`: 10个工作线程
- `-t 10`: 每个客户端执行10次事务

**预期结果**：
- 平均延迟 < 2000ms
- 无连接超时错误
- TPS（每秒事务数）稳定

### 4. 连接池监控

检查连接池使用情况：

```sql
SELECT
    count(*) AS total_connections,
    count(*) FILTER (WHERE state = 'active') AS active_connections,
    count(*) FILTER (WHERE state = 'idle') AS idle_connections
FROM pg_stat_activity
WHERE datname = current_database();
```

**健康指标**：
- 总连接数 ≤ 20（size + max_overflow）
- 无长时间等待的连接
- 活跃连接数在合理范围

## 性能分析要点

### EXPLAIN ANALYZE输出解读

1. **执行计划类型**
   ```
   Index Scan using idx_xxx_desc  ✓ 好（使用索引）
   Seq Scan on table_name         ✗ 差（全表扫描）
   ```

2. **执行时间**
   ```
   Planning Time: 0.123 ms        ✓ 规划时间短
   Execution Time: 1234.567 ms    ✓ 执行时间 < 2000ms
   ```

3. **缓冲区统计**
   ```
   Buffers: shared hit=1000       ✓ 缓存命中
   Buffers: shared read=100       ⚠ 磁盘读取
   ```

### 性能优化建议

如果测试未达标，考虑以下优化：

1. **数据库层面**
   - 增加shared_buffers配置
   - 运行VACUUM ANALYZE更新统计信息
   - 考虑表分区（按时间分区）
   - 优化PostgreSQL配置参数

2. **连接池层面**
   - 调整size和max_overflow参数
   - 监控连接池使用率
   - 实现连接预热

3. **查询优化**
   - 添加复合索引（如需要）
   - 优化JOIN查询
   - 减少返回数据量

4. **应用层面**
   - 实现查询结果缓存
   - 异步查询处理
   - 请求限流

## 测试结果文档

测试完成后，查看以下文档：

- **performance_test_results.md**: 完整的性能测试结果文档
- **index_verification_*.txt**: 索引验证结果
- **explain_analyze_*.txt**: 查询性能分析结果
- **pgbench_results_*.txt**: 并发负载测试结果
- **connection_stats_*.txt**: 连接池状态
- **test_summary_*.txt**: 测试摘要报告

## 常见问题

### Q1: pgbench未安装怎么办？

A: pgbench通常随PostgreSQL一起安装。如果没有：

```bash
# Ubuntu/Debian
sudo apt-get install postgresql-contrib

# macOS
brew install postgresql
```

或者跳过pgbench测试，使用Erlang测试模块。

### Q2: 如何解读P95响应时间？

A: P95表示95%的请求响应时间低于该值。例如P95=1500ms表示95%的请求在1.5秒内完成。

### Q3: 连接池使用率过高怎么办？

A: 如果经常出现等待队列，考虑：
- 增加size和max_overflow
- 优化慢查询
- 实现查询缓存

### Q4: 索引未被使用怎么办？

A: 可能原因：
- 数据量太小（优化器选择全表扫描）
- 统计信息过期（运行ANALYZE）
- 查询条件不匹配索引
- 索引选择性不好

### Q5: 如何在生产环境测试？

A: 建议：
- 在低峰期执行测试
- 使用只读副本测试
- 逐步增加并发数
- 监控系统资源使用

## 性能监控

### 实时监控指标

建议监控以下指标：

1. **API响应时间**
   - P50, P95, P99百分位数
   - 平均响应时间
   - 最大响应时间

2. **数据库性能**
   - 查询执行时间
   - 慢查询数量
   - 连接数使用率

3. **连接池状态**
   - 可用连接数
   - 溢出连接数
   - 等待队列长度

### 告警阈值

| 指标 | 警告 | 严重 |
|------|------|------|
| P95响应时间 | > 1500ms | > 2000ms |
| 连接池使用率 | > 80% | > 95% |
| 等待队列长度 | > 5 | > 10 |
| 错误率 | > 1% | > 5% |

## 相关文档

- [需求文档](../../.kiro/specs/ios-location-tracking-replay/requirements.md)
- [设计文档](../../.kiro/specs/ios-location-tracking-replay/design.md)
- [任务列表](../../.kiro/specs/ios-location-tracking-replay/tasks.md)
- [性能测试结果](../../.kiro/specs/ios-location-tracking-replay/performance_test_results.md)

## 联系方式

如有问题，请联系开发团队或查看项目文档。
