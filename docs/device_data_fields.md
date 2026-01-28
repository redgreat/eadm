# 设备数据字段说明

## 设备数据JSON字段含义

### 主表字段（TAGS）
- **imei** (string): 设备唯一识别码，15位数字，用于标识具体设备
- **imsi** (string): 用户识别码，15位数字，用于标识用户SIM卡

### 子表字段（FIELDS）

#### 位置信息
- **lat** (double): GPS纬度，范围-90到90度，0表示无效
- **lng** (double): GPS经度，范围-180到180度，0表示无效
- **agps_lat** (double): A-GPS纬度，辅助GPS定位的纬度信息
- **agps_lng** (double): A-GPS经度，辅助GPS定位的经度信息

#### 时间信息
- **uptime** (bigint): 设备运行时间，Unix时间戳(秒)
- **agps_ts** (bigint): A-GPS时间戳，Unix时间戳(秒)
- **gps_ts** (bigint): GPS时间戳，Unix时间戳(秒)，0表示无GPS信号

#### 信号质量
- **rsrp** (int): 参考信号接收功率，单位dBm，范围-140到-44，值越大信号越好
- **rssi** (int): 接收信号强度指示，单位dBm，负值，越接近0信号越强
- **rsrq** (int): 参考信号接收质量，单位dB，负值，范围-20到-3，值越大质量越好
- **snr** (int): 信噪比，单位dB，正值，值越大信号质量越好
- **csq** (int): 信号质量指标，范围0-31，值越大信号越好

#### 设备状态
- **vbat** (int): 电池电压，单位毫伏(mV)，典型值3000-4200
- **up_vbat** (int): 上报时的电池电压，单位毫伏(mV)

#### 网络信息（原始数据中存在但未存储）
- **ip** (string): 设备IP地址
- **cellinfo** (array): 基站信息数组，包含周边基站信号参数
- **wifi** (object): WiFi信息，包含扫描到的WiFi热点信息

## TDengine超级表设计

### 超级表名：device_data

#### 主表标签（TAGS）
- **imei**: 设备IMEI号，用于分组查询
- **imsi**: 设备IMSI号，用于用户关联

#### 子表字段（FIELDS）
- **ts**: 数据时间戳，TDengine自动生成
- **lat, lng, agps_lat, agps_lng**: 位置信息
- **uptime, agps_ts, gps_ts**: 时间信息
- **rsrp, rssi, rsrq, snr, csq**: 信号质量
- **vbat**: 设备状态

## 数据查询示例

### 1. 查询特定设备的最新位置
```sql
SELECT imei, imsi, lat, lng, agps_lat, agps_lng, ts 
FROM device_data 
WHERE imei = '864269060008576' 
ORDER BY ts DESC LIMIT 1;
```

### 2. 查询设备最近24小时的轨迹
```sql
SELECT ts, lat, lng, agps_lat, agps_lng, rsrp, csq
FROM device_data 
WHERE imei = '864269060008576' 
AND ts >= NOW - 1d 
ORDER BY ts ASC;
```

### 3. 查询所有设备的最新状态
```sql
SELECT imei, lat, lng, rsrp, csq, vbat, ts
FROM device_data 
WHERE ts >= NOW - 1h 
GROUP BY imei 
ORDER BY ts DESC;
```

### 4. 统计设备信号质量分布
```sql
SELECT 
    CASE 
        WHEN csq >= 20 THEN '优秀'
        WHEN csq >= 15 THEN '良好' 
        WHEN csq >= 10 THEN '一般'
        ELSE '较差'
    END as signal_level,
    COUNT(*) as device_count
FROM device_data 
WHERE ts >= NOW - 1h
GROUP BY signal_level;
```

## 数据存储策略

### 分区策略
- 按设备IMEI进行数据分区
- 每个设备一个子表，以TAGS区分

### 保留策略
- 建议原始数据保留3个月
- 可配置数据压缩和归档策略

### 性能优化
- 为常用查询字段创建索引
- 定期清理过期数据
- 使用TDengine的连续查询进行数据聚合
