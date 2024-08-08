-- @author wangcw
-- @copyright (c) 2024, redgreat
-- created : 2024-8-7 16:08:41
-- mysql数据迁移

-- mysql_fdw创建
CREATE EXTENSION mysql_fdw;

SELECT mysql_fdw_version();

DROP SERVER IF EXISTS ticloud CASCADE;
CREATE SERVER ticloud
FOREIGN DATA WRAPPER mysql_fdw
OPTIONS (host 'gateway01.ap-northeast-1.prod.aws.tidbcloud.com',
         port '4000',
         ssl_ca '/etc/ssl/certs/ca-certificates.crt');

GRANT USAGE ON FOREIGN SERVER ticloud TO postgres;

-- create user mapping
CREATE USER MAPPING FOR postgres
SERVER ticloud
OPTIONS (username 'root', password 'xxx');

-- create foreign table
CREATE FOREIGN TABLE carlocdaily (
    dev_upload timestamptz,
    device_id varchar(20),
    lat numeric(9,6),
    lng numeric(9,6),
    dirct int,
    speed int,
    mileage numeric(18,2),
    hight int,
    gnss_num int,
    rssi int,
    serv_receive timestamptz,
    data_insert timestamptz not null default current_timestamp
    ) SERVER ticloud
    OPTIONS (dbname 'dailywong', table_name 'carlocdaily');

comment on column carlocdaily.dev_upload is '设备上传时间(主键)';
comment on column carlocdaily.device_id is '设备编码(iccid)';
comment on column carlocdaily.lat is '经度';
comment on column carlocdaily.lng is '纬度';
comment on column carlocdaily.dirct is '方向角';
comment on column carlocdaily.speed is '速度';
comment on column carlocdaily.mileage is '里程';
comment on column carlocdaily.hight is '海拔';
comment on column carlocdaily.gnss_num is 'gps卫星数量';
comment on column carlocdaily.rssi is '4g信号值';
comment on column carlocdaily.serv_receive is 'gateway处理时间';
comment on column carlocdaily.data_insert is '数据写入时间';

SELECT * FROM carlocdaily LIMIT 10;

IMPORT FOREIGN SCHEMA postgres
    FROM SERVER ticloud
    INTO public;

