#!/bin/bash
# ============================================================================
# 性能测试自动化脚本
# 用途：自动执行所有性能测试并生成报告
# ============================================================================

set -e  # 遇到错误立即退出

# 配置变量
DB_HOST="23.95.68.200"
DB_PORT="8432"
DB_NAME="eadm"
DB_USER="user_eadm"
export PGPASSWORD="Mm19890425"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
OUTPUT_DIR="${SCRIPT_DIR}/../../.kiro/specs/ios-location-tracking-replay"
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")

# 颜色输出
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "============================================================================"
echo "iOS位置追踪API性能测试"
echo "============================================================================"
echo ""

# 检查psql是否安装
if ! command -v psql &> /dev/null; then
    echo -e "${RED}错误: psql未安装，请先安装PostgreSQL客户端${NC}"
    exit 1
fi

# 检查pgbench是否安装
if ! command -v pgbench &> /dev/null; then
    echo -e "${YELLOW}警告: pgbench未安装，将跳过并发负载测试${NC}"
    SKIP_PGBENCH=1
else
    SKIP_PGBENCH=0
fi

# 测试数据库连接
echo "测试数据库连接..."
if psql -h "$DB_HOST" -p "$DB_PORT" -U "$DB_USER" -d "$DB_NAME" -c "SELECT 1" > /dev/null 2>&1; then
    echo -e "${GREEN}✓ 数据库连接成功${NC}"
else
    echo -e "${RED}✗ 数据库连接失败${NC}"
    exit 1
fi
echo ""

# ============================================================================
# 测试1: 验证索引
# ============================================================================
echo "测试1: 验证索引..."
INDEX_OUTPUT="${OUTPUT_DIR}/index_verification_${TIMESTAMP}.txt"

psql -h "$DB_HOST" -p "$DB_PORT" -U "$DB_USER" -d "$DB_NAME" > "$INDEX_OUTPUT" <<EOF
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
EOF

# 检查索引数量
INDEX_COUNT=$(grep -c "idx_.*_desc" "$INDEX_OUTPUT" || true)
if [ "$INDEX_COUNT" -ge 7 ]; then
    echo -e "${GREEN}✓ 索引验证通过（找到 $INDEX_COUNT 个索引）${NC}"
else
    echo -e "${YELLOW}⚠ 警告：只找到 $INDEX_COUNT 个索引，预期至少7个${NC}"
fi
echo "  结果保存到: $INDEX_OUTPUT"
echo ""

# ============================================================================
# 测试2: EXPLAIN ANALYZE性能分析
# ============================================================================
echo "测试2: EXPLAIN ANALYZE性能分析..."
EXPLAIN_OUTPUT="${OUTPUT_DIR}/explain_analyze_${TIMESTAMP}.txt"

echo "执行性能分析查询（这可能需要几分钟）..."
psql -h "$DB_HOST" -p "$DB_PORT" -U "$DB_USER" -d "$DB_NAME" \
    -f "${SCRIPT_DIR}/performance_test_location_tracking.sql" \
    > "$EXPLAIN_OUTPUT" 2>&1

# 分析结果
if grep -q "Index Scan" "$EXPLAIN_OUTPUT"; then
    echo -e "${GREEN}✓ 查询使用了索引扫描${NC}"
else
    echo -e "${YELLOW}⚠ 警告：某些查询可能未使用索引${NC}"
fi

# 检查执行时间
SLOW_QUERIES=$(grep -c "Execution Time:.*[0-9]\{4,\}\." "$EXPLAIN_OUTPUT" || true)
if [ "$SLOW_QUERIES" -gt 0 ]; then
    echo -e "${YELLOW}⚠ 警告：发现 $SLOW_QUERIES 个慢查询（>1000ms）${NC}"
else
    echo -e "${GREEN}✓ 所有查询执行时间正常${NC}"
fi

echo "  结果保存到: $EXPLAIN_OUTPUT"
echo ""

# ============================================================================
# 测试3: 表统计信息
# ============================================================================
echo "测试3: 表统计信息..."
STATS_OUTPUT="${OUTPUT_DIR}/table_stats_${TIMESTAMP}.txt"

psql -h "$DB_HOST" -p "$DB_PORT" -U "$DB_USER" -d "$DB_NAME" > "$STATS_OUTPUT" <<EOF
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
EOF

echo -e "${GREEN}✓ 表统计信息已生成${NC}"
echo "  结果保存到: $STATS_OUTPUT"
cat "$STATS_OUTPUT"
echo ""

# ============================================================================
# 测试4: 并发负载测试（使用pgbench）
# ============================================================================
if [ "$SKIP_PGBENCH" -eq 0 ]; then
    echo "测试4: 并发负载测试..."
    PGBENCH_OUTPUT="${OUTPUT_DIR}/pgbench_results_${TIMESTAMP}.txt"
    
    echo "执行100个并发连接测试（这可能需要几分钟）..."
    
    # 创建临时测试脚本
    TEMP_SCRIPT="/tmp/pgbench_test_${TIMESTAMP}.sql"
    cat > "$TEMP_SCRIPT" <<'EOF'
SELECT
    a.activityid,
    a.starttime,
    a.endtime
FROM garmin_activity a
WHERE a.starttime >= '2024-01-01 00:00:00'::timestamptz
  AND a.starttime <= '2024-01-07 23:59:59'::timestamptz
ORDER BY a.starttime ASC
LIMIT 100;
EOF
    
    # 运行pgbench
    if pgbench -h "$DB_HOST" -p "$DB_PORT" -U "$DB_USER" -d "$DB_NAME" \
        -c 100 -j 10 -t 10 \
        -f "$TEMP_SCRIPT" \
        > "$PGBENCH_OUTPUT" 2>&1; then
        
        # 提取关键指标
        TPS=$(grep "tps =" "$PGBENCH_OUTPUT" | awk '{print $3}')
        LATENCY_AVG=$(grep "latency average" "$PGBENCH_OUTPUT" | awk '{print $4}')
        
        echo -e "${GREEN}✓ 并发测试完成${NC}"
        echo "  TPS: $TPS"
        echo "  平均延迟: $LATENCY_AVG ms"
        echo "  结果保存到: $PGBENCH_OUTPUT"
        
        # 检查性能是否达标
        if (( $(echo "$LATENCY_AVG < 2000" | bc -l) )); then
            echo -e "${GREEN}✓ 性能达标（平均延迟 < 2000ms）${NC}"
        else
            echo -e "${RED}✗ 性能未达标（平均延迟 >= 2000ms）${NC}"
        fi
    else
        echo -e "${RED}✗ 并发测试失败${NC}"
        echo "  错误信息保存到: $PGBENCH_OUTPUT"
    fi
    
    # 清理临时文件
    rm -f "$TEMP_SCRIPT"
    echo ""
else
    echo "测试4: 跳过并发负载测试（pgbench未安装）"
    echo ""
fi

# ============================================================================
# 测试5: 连接池状态
# ============================================================================
echo "测试5: 连接池状态..."
CONN_OUTPUT="${OUTPUT_DIR}/connection_stats_${TIMESTAMP}.txt"

psql -h "$DB_HOST" -p "$DB_PORT" -U "$DB_USER" -d "$DB_NAME" > "$CONN_OUTPUT" <<EOF
-- 当前连接数
SELECT
    count(*) AS total_connections,
    count(*) FILTER (WHERE state = 'active') AS active_connections,
    count(*) FILTER (WHERE state = 'idle') AS idle_connections,
    count(*) FILTER (WHERE state = 'idle in transaction') AS idle_in_transaction
FROM pg_stat_activity
WHERE datname = current_database();

-- 长时间运行的查询
SELECT
    pid,
    now() - query_start AS duration,
    state,
    left(query, 100) AS query_preview
FROM pg_stat_activity
WHERE state != 'idle'
  AND query NOT LIKE '%pg_stat_activity%'
ORDER BY duration DESC
LIMIT 10;
EOF

echo -e "${GREEN}✓ 连接池状态已检查${NC}"
echo "  结果保存到: $CONN_OUTPUT"
cat "$CONN_OUTPUT"
echo ""

# ============================================================================
# 生成测试报告摘要
# ============================================================================
echo "============================================================================"
echo "测试完成！"
echo "============================================================================"
echo ""
echo "测试结果文件："
echo "  - 索引验证: $INDEX_OUTPUT"
echo "  - 性能分析: $EXPLAIN_OUTPUT"
echo "  - 表统计: $STATS_OUTPUT"
if [ "$SKIP_PGBENCH" -eq 0 ]; then
    echo "  - 并发测试: $PGBENCH_OUTPUT"
fi
echo "  - 连接状态: $CONN_OUTPUT"
echo ""
echo "请查看以上文件了解详细测试结果。"
echo ""

# 生成简要报告
SUMMARY_OUTPUT="${OUTPUT_DIR}/test_summary_${TIMESTAMP}.txt"
cat > "$SUMMARY_OUTPUT" <<EOF
性能测试摘要报告
================

测试时间: $(date)
数据库: $DB_HOST:$DB_PORT/$DB_NAME

测试结果:
---------
1. 索引验证: 找到 $INDEX_COUNT 个索引
2. 性能分析: 完成
3. 表统计: 完成
4. 并发测试: $([ "$SKIP_PGBENCH" -eq 0 ] && echo "完成" || echo "跳过")
5. 连接状态: 完成

详细结果请查看各个输出文件。
EOF

echo "测试摘要已保存到: $SUMMARY_OUTPUT"
echo ""
echo -e "${GREEN}所有测试已完成！${NC}"
