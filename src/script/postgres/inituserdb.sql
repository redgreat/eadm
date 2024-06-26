-- @author wangcw
-- @copyright (c) 2024, redgreat
-- created : 2024-6-26 15:52:13
-- postgres 数据库创建用户和数据库

CREATE USER user_eadm WITH PASSWORD 'xxx';

ALTER USER user_eadm CREATEDB;

CREATE DATABASE eadm WITH OWNER = user_eadm ENCODING = 'UTF8';
GRANT ALL ON DATABASE eadm TO user_eadm;

