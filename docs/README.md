# EMQX设备数据接收服务

## 功能概述

这个服务用于从EMQX消息队列实时接收设备数据，并将关键信息存储到TDengine时序数据库中。

## 文件结构

```
src/service/
├── emqx_device_service.erl      # 主要服务模块
├── emqx_device_sup.erl          # 服务监督者
├── tdengine_schema.sql          # TDengine数据库表结构
├── device_data_fields.md        # 字段说明文档
└── emqx_td_config.template      # 配置模板
```

## 配置说明

### 1. 添加配置到sys.config.src

将`emqx_td_config.template`中的配置添加到`config/sys.config.src`文件中：

```erlang
{eadm, [
    %% 现有配置...
    
    %% EMQX配置
    {emqx, #{
        host => "your_emqx_host",
        port => 1883,
        username => "your_username",
        password => "your_password",
        client_id_prefix => "eadm_",
        keepalive => 60,
        clean_start => true,
        topics => [
            <<"devices/+/data">>,
            <<"devices/+/status">>
        ]
    }},
    
    %% TDengine配置
    {tdengine, #{
        host => "your_tdengine_host",
        port => 6030,
        username => "root",
        password => "taosdata",
        database => "eadm_iot",
        keepalive => true,
        connect_timeout => 5000,
        pool_size => 2
    }}
]}.
```

### 2. TDengine数据库初始化

执行`tdengine_schema.sql`脚本创建超级表：

```sql
-- 连接TDengine
taos -h localhost -P 6030 -u root -p taosdata

-- 创建数据库
CREATE DATABASE IF NOT EXISTS eadm_iot;

-- 使用数据库
USE eadm_iot;

-- 执行建表脚本
SOURCE src/service/tdengine_schema.sql;
```

## 依赖库

需要在`rebar.config`中添加以下依赖：

```erlang
{deps, [
    %% 现有依赖...
    {emqttc, ".*", {git, "https://github.com/emqx/emqttc.git", {branch, "master"}}},
    {gun, ".*", {git, "https://github.com/ninenines/gun.git", {branch, "master"}}}
]}.
```

## 数据处理流程

1. **消息接收**: 从EMQX订阅`devices/+/data`主题
2. **数据解析**: 解析JSON格式的设备数据
3. **字段提取**: 提取关键字段（位置、信号、时间等）
4. **数据存储**: 插入TDengine超级表`device_data`
5. **错误处理**: 记录错误日志，确保服务稳定性

## 存储的数据字段

### 主表字段（TAGS）
- `imei`: 设备IMEI号
- `imsi`: 设备IMSI号

### 子表字段（FIELDS）
- `ts`: 时间戳（TDengine自动生成）
- `lat, lng`: GPS坐标
- `agps_lat, agps_lng`: A-GPS坐标
- `uptime`: 设备运行时间
- `rsrp, rssi, rsrq, snr, csq`: 信号质量指标
- `vbat`: 电池电压
- `agps_ts, gps_ts`: 时间戳信息

## 监控和日志

服务启动后会输出以下日志：
- 连接EMQX状态
- 连接TDengine状态
- 消息接收和处理状态
- 错误信息

## 性能优化建议

1. **连接池**: TDengine使用连接池管理连接
2. **批量插入**: 可考虑批量插入提高性能
3. **数据压缩**: TDengine自动压缩历史数据
4. **分区策略**: 按设备IMEI自动分区

## 故障处理

- **连接断开**: 服务会自动重连
- **数据格式错误**: 记录错误日志，跳过无效数据
- **数据库错误**: 重试机制，确保数据不丢失

## 查询示例

查看设备最新位置：
```sql
SELECT imei, lat, lng, ts 
FROM device_data 
WHERE imei = '864269060008576' 
ORDER BY ts DESC LIMIT 1;
```

查看设备最近轨迹：
```sql
SELECT ts, lat, lng, rsrp, csq
FROM device_data 
WHERE imei = '864269060008576' 
AND ts >= NOW - 1d 
ORDER BY ts ASC;
```

## 启动服务

服务会随应用自动启动，也可以手动控制：

```erlang
% 启动服务
emqx_device_service:start_link().

% 停止服务
emqx_device_service:stop().
```

## 注意事项

1. 确保EMQX和TDengine服务正常运行
2. 检查网络连接和防火墙设置
3. 监控日志文件，及时发现问题
4. 定期维护TDengine数据库
