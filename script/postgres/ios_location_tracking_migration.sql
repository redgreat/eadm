-- @author wangcw
-- @copyright (c) 2024, redgreat
-- created : 2024-12-19
-- iOS位置追踪和轨迹回放功能 - 数据库迁移脚本
-- 
-- 说明：
-- 1. 此脚本创建iOS位置追踪功能所需的新表和索引
-- 2. 已存在的表（garmin_activity, garmin_activity_detail, lc_watchlocation, lc_carlocdaily）
--    在datastruct.sql中已定义，此脚本仅验证和补充索引
-- 3. 新增表：emqx_device_data, lc_yedgns, lc_racebox, audit_log

-- 设置查询路径
alter role user_eadm set search_path to eadm, public;

-- 设置本地时区
set time zone 'asia/shanghai';

-- =============================================
-- MQTT设备数据表
-- =============================================
drop table if exists emqx_device_data cascade;
create table emqx_device_data (
  id serial,
  device_id varchar(50) not null,
  ptime timestamptz not null,
  latitude numeric(12,8),
  longitude numeric(12,8),
  altitude numeric(10,2),
  speed numeric(10,4),
  createdat timestamptz default current_timestamp
);

alter table emqx_device_data owner to user_eadm;
alter table emqx_device_data drop constraint if exists pk_emqx_device_data_id cascade;
alter table emqx_device_data add constraint pk_emqx_device_data_id primary key (id);

drop index if exists idx_emqx_device_ptime;
create index idx_emqx_device_ptime on emqx_device_data using btree (ptime desc nulls last);
drop index if exists idx_emqx_device_id;
create index idx_emqx_device_id on emqx_device_data using btree (device_id asc nulls last);

comment on column emqx_device_data.id is '自增主键';
comment on column emqx_device_data.device_id is '设备ID';
comment on column emqx_device_data.ptime is '设备上传时间';
comment on column emqx_device_data.latitude is '纬度(WGS84)';
comment on column emqx_device_data.longitude is '经度(WGS84)';
comment on column emqx_device_data.altitude is '海拔(米)';
comment on column emqx_device_data.speed is '速度(m/s)';
comment on column emqx_device_data.createdat is '数据写入时间';
comment on table emqx_device_data is '业务数据_MQTT设备定位信息';

-- =============================================
-- 野点GNS设备数据表
-- =============================================
drop table if exists lc_yedgns cascade;
create table lc_yedgns (
  id serial,
  device_id varchar(50),
  ptime timestamptz not null,
  latitude numeric(12,8),
  longitude numeric(12,8),
  altitude numeric(10,2),
  speed numeric(10,4),
  createdat timestamptz default current_timestamp
);

alter table lc_yedgns owner to user_eadm;
alter table lc_yedgns drop constraint if exists pk_yedgns_id cascade;
alter table lc_yedgns add constraint pk_yedgns_id primary key (id);

drop index if exists idx_yedgns_ptime;
create index idx_yedgns_ptime on lc_yedgns using btree (ptime desc nulls last);
drop index if exists idx_yedgns_device_id;
create index idx_yedgns_device_id on lc_yedgns using btree (device_id asc nulls last);

comment on column lc_yedgns.id is '自增主键';
comment on column lc_yedgns.device_id is '设备ID';
comment on column lc_yedgns.ptime is '设备上传时间';
comment on column lc_yedgns.latitude is '纬度(WGS84)';
comment on column lc_yedgns.longitude is '经度(WGS84)';
comment on column lc_yedgns.altitude is '海拔(米)';
comment on column lc_yedgns.speed is '速度(m/s)';
comment on column lc_yedgns.createdat is '数据写入时间';
comment on table lc_yedgns is '业务数据_野点GNS定位信息';

-- =============================================
-- 赛车盒子设备数据表
-- =============================================
drop table if exists lc_racebox cascade;
create table lc_racebox (
  id serial,
  device_id varchar(50),
  ptime timestamptz not null,
  latitude numeric(12,8),
  longitude numeric(12,8),
  altitude numeric(10,2),
  speed numeric(10,4),
  acceleration numeric(10,4),
  createdat timestamptz default current_timestamp
);

alter table lc_racebox owner to user_eadm;
alter table lc_racebox drop constraint if exists pk_racebox_id cascade;
alter table lc_racebox add constraint pk_racebox_id primary key (id);

drop index if exists idx_racebox_ptime;
create index idx_racebox_ptime on lc_racebox using btree (ptime desc nulls last);
drop index if exists idx_racebox_device_id;
create index idx_racebox_device_id on lc_racebox using btree (device_id asc nulls last);

comment on column lc_racebox.id is '自增主键';
comment on column lc_racebox.device_id is '设备ID';
comment on column lc_racebox.ptime is '设备上传时间';
comment on column lc_racebox.latitude is '纬度(WGS84)';
comment on column lc_racebox.longitude is '经度(WGS84)';
comment on column lc_racebox.altitude is '海拔(米)';
comment on column lc_racebox.speed is '速度(m/s)';
comment on column lc_racebox.acceleration is '加速度(m/s²)';
comment on column lc_racebox.createdat is '数据写入时间';
comment on table lc_racebox is '业务数据_赛车盒子定位信息';

-- =============================================
-- 审计日志表
-- =============================================
drop table if exists audit_log cascade;
create table audit_log (
  id serial,
  user_id varchar(50) not null,
  action varchar(50) not null,
  resource varchar(100),
  timestamp timestamptz not null,
  result varchar(20),
  details jsonb
);

alter table audit_log owner to user_eadm;
alter table audit_log drop constraint if exists pk_audit_log_id cascade;
alter table audit_log add constraint pk_audit_log_id primary key (id);

drop index if exists idx_audit_log_user_id;
create index idx_audit_log_user_id on audit_log using btree (user_id asc nulls last);
drop index if exists idx_audit_log_timestamp;
create index idx_audit_log_timestamp on audit_log using btree (timestamp desc nulls last);
drop index if exists idx_audit_log_action;
create index idx_audit_log_action on audit_log using btree (action asc nulls last);

comment on column audit_log.id is '自增主键';
comment on column audit_log.user_id is '用户ID';
comment on column audit_log.action is '操作类型(location_access等)';
comment on column audit_log.resource is '资源标识(设备类型等)';
comment on column audit_log.timestamp is '操作时间戳';
comment on column audit_log.result is '操作结果(success/failure)';
comment on column audit_log.details is '详细信息(JSON格式)';
comment on table audit_log is '系统域_审计日志表';

-- =============================================
-- 验证和补充已存在表的索引
-- =============================================

-- 验证 garmin_activity_detail 表的索引
-- 注意：pointtime索引已在datastruct.sql中创建为DESC，此处仅确认
do $$
begin
  if not exists (
    select 1 from pg_indexes 
    where tablename = 'garmin_activity_detail' 
    and indexname = 'non_activity_detail_pointtime'
  ) then
    create index non_activity_detail_pointtime on garmin_activity_detail using btree (pointtime desc nulls last);
    raise notice '已创建索引: non_activity_detail_pointtime';
  else
    raise notice '索引已存在: non_activity_detail_pointtime';
  end if;
end $$;

-- 验证 lc_watchlocation 表的索引
do $$
begin
  if not exists (
    select 1 from pg_indexes 
    where tablename = 'lc_watchlocation' 
    and indexname = 'idx_watchlocation_ptime'
  ) then
    create index idx_watchlocation_ptime on lc_watchlocation using btree (ptime desc nulls last);
    raise notice '已创建索引: idx_watchlocation_ptime';
  else
    raise notice '索引已存在: idx_watchlocation_ptime';
  end if;
end $$;

-- 验证 lc_carlocdaily 表的索引
do $$
begin
  if not exists (
    select 1 from pg_indexes 
    where tablename = 'lc_carlocdaily' 
    and indexname = 'idx_carlocdaily_ptime'
  ) then
    create index idx_carlocdaily_ptime on lc_carlocdaily using btree (ptime desc nulls last);
    raise notice '已创建索引: idx_carlocdaily_ptime';
  else
    raise notice '索引已存在: idx_carlocdaily_ptime';
  end if;
end $$;

-- =============================================
-- 迁移完成提示
-- =============================================
do $$
begin
  raise notice '========================================';
  raise notice 'iOS位置追踪功能数据库迁移完成';
  raise notice '========================================';
  raise notice '已创建表:';
  raise notice '  - emqx_device_data (MQTT设备数据)';
  raise notice '  - lc_yedgns (野点GNS设备数据)';
  raise notice '  - lc_racebox (赛车盒子设备数据)';
  raise notice '  - audit_log (审计日志)';
  raise notice '';
  raise notice '已验证索引:';
  raise notice '  - garmin_activity_detail.pointtime (DESC)';
  raise notice '  - lc_watchlocation.ptime (DESC)';
  raise notice '  - lc_carlocdaily.ptime (DESC)';
  raise notice '  - 所有新表的时间和设备ID索引';
  raise notice '========================================';
end $$;
