-- TDengine设备数据超级表创建脚本
-- 创建超级表：device_data

-- 创建超级表
CREATE STABLE IF NOT EXISTS device_data (
    ts TIMESTAMP,                    -- 时间戳
    lat DOUBLE,                     -- GPS纬度
    lng DOUBLE,                     -- GPS经度
    agps_lat DOUBLE,                -- A-GPS纬度
    agps_lng DOUBLE,                -- A-GPS经度
    uptime BIGINT,                  -- 设备运行时间(秒)
    rsrp INT,                       -- 参考信号接收功率(dBm)
    csq INT,                        -- 信号质量(0-31)
    vbat INT,                       -- 电池电压(mV)
    agps_ts BIGINT,                 -- A-GPS时间戳
    gps_ts BIGINT,                  -- GPS时间戳
    rssi INT,                       -- 接收信号强度指示(dBm)
    rsrq INT,                       -- 参考信号接收质量(dB)
    snr INT                         -- 信噪比(dB)
) TAGS (
    imei BINARY(15),                -- 设备IMEI号(主表标签)
    imsi BINARY(15)                 -- 设备IMSI号(主表标签)
);

-- 创建子表示例（系统会自动创建，这里只是示例）
-- CREATE TABLE IF NOT EXISTS device_data_864269060008576 USING device_data TAGS('864269060008576', '460240299002723');

-- 为常用查询创建索引
-- CREATE INDEX IF NOT EXISTS idx_device_imei ON device_data(imei);
-- CREATE INDEX IF NOT EXISTS idx_device_ts ON device_data(ts);

-- 查询示例：
-- 1. 查询特定设备的最新数据
-- SELECT * FROM device_data WHERE imei = '864269060008576' ORDER BY ts DESC LIMIT 1;

-- 2. 查询特定设备最近24小时的数据
-- SELECT * FROM device_data WHERE imei = '864269060008576' AND ts >= NOW - 1d;

-- 3. 查询所有设备的最新位置
-- SELECT imei, imsi, lat, lng, agps_lat, agps_lng, ts FROM device_data 
-- WHERE ts >= NOW - 1h 
-- GROUP BY imei 
-- ORDER BY ts DESC;
