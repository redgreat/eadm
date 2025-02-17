-- @author wangcw
-- @copyright (c) 2024, redgreat
-- created : 2024-6-26 15:52:13
-- postgres 数据库创建用户和数据库

CREATE USER user_eadm WITH PASSWORD 'xxx';

ALTER USER user_eadm CREATEDB;

CREATE DATABASE eadm WITH OWNER = user_eadm ENCODING = 'UTF8';
GRANT ALL ON DATABASE eadm TO user_eadm;

-- 授予目标 Schema 的表读取权限
GRANT SELECT ON ALL TABLES IN SCHEMA PUBLIC TO user_eadm;

-- 授予目标 Schema 的 USAGE 权限
GRANT USAGE ON SCHEMA PUBLIC TO user_eadm;
