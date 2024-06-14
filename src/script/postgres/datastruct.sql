-- @author wangcw
-- @copyright (C) 2024, REDGREAT
-- Created : 2024-05-17 10:00:44
-- Postgres表结构设计

SET TIME ZONE 'Asia/Shanghai';

-- 表最后一次更新时间函数
DROP FUNCTION IF EXISTS "public"."lastupdate" CASCADE;
CREATE OR REPLACE FUNCTION "public"."lastupdate"()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updatedat := CURRENT_TIMESTAMP;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- 创建序列
DROP SEQUENCE IF EXISTS SD CASCADE;
CREATE SEQUENCE SD
START 1
INCREMENT BY 1
MAXVALUE 9999999999
CACHE 10;

-- 系统_字典信息表
DROP TABLE IF EXISTS "public"."sys_dict" CASCADE;
CREATE TABLE "public"."sys_dict" (
  id CHAR(12) DEFAULT ('SD' || LPAD((nextval('SD')::VARCHAR), 10, '0')),
  dictno VARCHAR(50) NOT NULL,
  dictname VARCHAR(100) NOT NULL,
  parentid CHAR(12),
  createduser VARCHAR(50),
  createdat TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  updateduser VARCHAR(50),
  updatedat TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  deleteduser VARCHAR(50),
  deletedat TIMESTAMPTZ,
  deleted BOOLEAN NOT NULL DEFAULT FALSE
);

ALTER TABLE "public"."sys_dict" OWNER TO "user_eadm";
ALTER TABLE "public"."sys_dict" DROP CONSTRAINT IF EXISTS "pk_sysdict_id" CASCADE;
ALTER TABLE "public"."sys_dict" ADD CONSTRAINT "pk_sysdict_id" PRIMARY KEY ("id");
ALTER TABLE "public"."sys_dict" DROP CONSTRAINT IF EXISTS "uni_sysdict_dictno" CASCADE;
ALTER TABLE "public"."sys_dict" ADD CONSTRAINT "uni_sysdict_dictno" UNIQUE ("dictno");
ALTER TABLE "public"."sys_dict" DROP CONSTRAINT IF EXISTS "fk_parentid_sysdict_id" CASCADE;
ALTER TABLE "public"."sys_dict" ADD CONSTRAINT "fk_parentid_sysdict_id" FOREIGN KEY ("parentid")
    REFERENCES "public"."sys_dict" ("id") ON DELETE RESTRICT ON UPDATE RESTRICT;

DROP INDEX IF EXISTS "non_sysdict_parentid";
CREATE INDEX "non_sysdict_parentid" ON "public"."sys_dict" USING BTREE ("parentid" ASC NULLS LAST);

COMMENT ON COLUMN "public"."sys_dict"."id" IS '自定义主键(SD)';
COMMENT ON COLUMN "public"."sys_dict"."dictno" IS '字典编码';
COMMENT ON COLUMN "public"."sys_dict"."dictname" IS '字典名称';
COMMENT ON COLUMN "public"."sys_dict"."parentid" IS '父级Id';
COMMENT ON COLUMN "public"."sys_dict"."createduser" IS '创建人';
COMMENT ON COLUMN "public"."sys_dict"."createdat" IS '创建时间';
COMMENT ON COLUMN "public"."sys_dict"."updateduser" IS '更新人';
COMMENT ON COLUMN "public"."sys_dict"."updatedat" IS '更新时间';
COMMENT ON COLUMN "public"."sys_dict"."deleteduser" IS '删除人';
COMMENT ON COLUMN "public"."sys_dict"."deletedat" IS '删除时间';
COMMENT ON COLUMN "public"."sys_dict"."deleted" IS '是否删除(0否1是)';
COMMENT ON TABLE "public"."sys_dict" IS '系统_字典信息表';

-- 表最后一次更新时间触发器
DROP TRIGGER IF EXISTS "dict_lastupdate" ON "public"."sys_dict" CASCADE;

CREATE OR REPLACE TRIGGER "dict_lastupdate"
BEFORE UPDATE ON "public"."sys_dict"
FOR EACH ROW
EXECUTE FUNCTION "public"."lastupdate"();

-- 基础信息_租户信息表
DROP SEQUENCE IF EXISTS ET CASCADE;
CREATE SEQUENCE ET
START 1
INCREMENT BY 1
MAXVALUE 9999999999
CACHE 10;

DROP TABLE IF EXISTS "public"."eadm_tenant" CASCADE;
CREATE TABLE "public"."eadm_tenant" (
  id CHAR(12) NOT NULL DEFAULT ('ET' || LPAD((nextval('ET')::VARCHAR), 10, '0')),
  tenantname VARCHAR(20) NOT NULL,
  remark VARCHAR(100),
  enable BOOLEAN NOT NULL DEFAULT TRUE,
  createduser VARCHAR(50),
  createdat TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  updateduser VARCHAR(50),
  updatedat TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  deleteduser VARCHAR(50),
  deletedat TIMESTAMPTZ,
  deleted BOOLEAN NOT NULL DEFAULT FALSE
);

ALTER TABLE "public"."eadm_tenant" OWNER TO "user_eadm";
ALTER TABLE "public"."eadm_tenant" DROP CONSTRAINT IF EXISTS "pk_tenant_id" CASCADE;
ALTER TABLE "public"."eadm_tenant" ADD CONSTRAINT "pk_tenant_id" PRIMARY KEY ("id");
ALTER TABLE "public"."eadm_tenant" DROP CONSTRAINT IF EXISTS "uni_tenant_tenantname" CASCADE;
ALTER TABLE "public"."eadm_tenant" ADD CONSTRAINT "uni_tenant_tenantname" UNIQUE ("tenantname");

DROP INDEX IF EXISTS "non_tenant_enable";
CREATE INDEX "non_tenant_enable" ON "public"."eadm_tenant" USING BTREE ("enable" ASC NULLS LAST);

COMMENT ON COLUMN "public"."eadm_tenant"."id" IS '自定义主键(ET)';
COMMENT ON COLUMN "public"."eadm_tenant"."tenantname" IS '租户名称';
COMMENT ON COLUMN "public"."eadm_tenant"."remark" IS '备注信息';
COMMENT ON COLUMN "public"."eadm_tenant"."enable" IS '是否可用(0否1是)';
COMMENT ON COLUMN "public"."eadm_tenant"."createduser" IS '创建人';
COMMENT ON COLUMN "public"."eadm_tenant"."createdat" IS '创建时间';
COMMENT ON COLUMN "public"."eadm_tenant"."updateduser" IS '更新人';
COMMENT ON COLUMN "public"."eadm_tenant"."updatedat" IS '更新时间';
COMMENT ON COLUMN "public"."eadm_tenant"."deleteduser" IS '删除人';
COMMENT ON COLUMN "public"."eadm_tenant"."deletedat" IS '删除时间';
COMMENT ON COLUMN "public"."eadm_tenant"."deleted" IS '是否删除(0否1是)';
COMMENT ON TABLE "public"."eadm_tenant" IS '基础信息_租户信息表';

-- 表最后一次更新时间触发器
DROP TRIGGER IF EXISTS "tenant_lastupdate" ON "public"."eadm_tenant" CASCADE;

CREATE OR REPLACE TRIGGER "tenant_lastupdate"
BEFORE UPDATE ON "public"."eadm_tenant"
FOR EACH ROW
EXECUTE FUNCTION "public"."lastupdate"();

--写入数据
TRUNCATE TABLE "public"."eadm_tenant";

INSERT INTO "public"."eadm_tenant"(tenantname, remark)
VALUES('REDGREAT', '主租户');

INSERT INTO "public"."eadm_tenant"(tenantname, remark)
VALUES('管理客户', '手动添加客户');

INSERT INTO "public"."eadm_tenant"(tenantname, remark)
VALUES('注册客户', '界面注册客户');

-- SELECT * FROM "public"."eadm_tenant";

-- 获取商户名称
DROP FUNCTION IF EXISTS "public"."gettenantname" CASCADE;
CREATE OR REPLACE FUNCTION "public"."gettenantname"(IN inid CHAR(12))
RETURNS VARCHAR(100) AS $$
BEGIN
    RETURN (SELECT tenantname FROM "public"."eadm_tenant" WHERE id=inid AND enable IS TRUE AND deleted IS FALSE LIMIT 1);
END
$$ LANGUAGE plpgsql;

-- SELECT "public"."gettenantname"('ET0000000001');

-- 用户表
DROP SEQUENCE IF EXISTS EU CASCADE;
CREATE SEQUENCE EU
START 1
INCREMENT BY 1
MAXVALUE 9999999999
CACHE 10;

DROP TABLE IF EXISTS "public"."eadm_user" CASCADE;
CREATE TABLE "public"."eadm_user" (
  id CHAR(12) NOT NULL DEFAULT ('EU' || LPAD((nextval('EU')::VARCHAR), 10, '0')),
  tenantid CHAR(12) NOT NULL,
  loginname VARCHAR(50) NOT NULL,
  username VARCHAR(50) NOT NULL,
  email VARCHAR(20),
  passwd VARCHAR(50) NOT NULL,
  userstatus SMALLINT NOT NULL DEFAULT 0,
  createduser VARCHAR(50),
  createdat TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  updateduser VARCHAR(50),
  updatedat TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  deleteduser VARCHAR(50),
  deletedat TIMESTAMPTZ,
  deleted BOOLEAN NOT NULL DEFAULT FALSE
);

ALTER TABLE "public"."eadm_user" OWNER TO "user_eadm";
ALTER TABLE "public"."eadm_user" DROP CONSTRAINT IF EXISTS "pk_user_id" CASCADE;
ALTER TABLE "public"."eadm_user" ADD CONSTRAINT "pk_user_id" PRIMARY KEY ("id");

ALTER TABLE "public"."eadm_user" DROP CONSTRAINT IF EXISTS "fk_tenantid_tenant_id" CASCADE;
ALTER TABLE "public"."eadm_user" ADD CONSTRAINT "fk_tenantid_tenant_id" FOREIGN KEY ("tenantid")
    REFERENCES "public"."eadm_tenant" ("id") ON DELETE RESTRICT ON UPDATE RESTRICT;

ALTER TABLE "public"."eadm_user" DROP CONSTRAINT IF EXISTS "uni_user_loginname" CASCADE;
ALTER TABLE "public"."eadm_user" ADD CONSTRAINT "uni_user_loginname" UNIQUE ("loginname");

DROP INDEX IF EXISTS "non_user_userstatus";
CREATE INDEX "non_user_userstatus" ON "public"."eadm_user" USING BTREE ("userstatus" ASC NULLS LAST);
DROP INDEX IF EXISTS "non_user_updatedat";
CREATE INDEX "non_user_updatedat" ON "public"."eadm_user" USING BTREE ("updatedat" DESC NULLS LAST);

COMMENT ON COLUMN "public"."eadm_user"."id" IS '自定义主键(EU)';
COMMENT ON COLUMN "public"."eadm_user"."tenantid" IS '租户Id';
COMMENT ON COLUMN "public"."eadm_user"."loginname" IS '用户登录名';
COMMENT ON COLUMN "public"."eadm_user"."username" IS '用户姓名';
COMMENT ON COLUMN "public"."eadm_user"."email" IS '用户邮件';
COMMENT ON COLUMN "public"."eadm_user"."passwd" IS '密码';
COMMENT ON COLUMN "public"."eadm_user"."userstatus" IS '用户状态(0启用1禁用)';
COMMENT ON COLUMN "public"."eadm_user"."createduser" IS '创建人';
COMMENT ON COLUMN "public"."eadm_user"."createdat" IS '创建时间';
COMMENT ON COLUMN "public"."eadm_user"."updateduser" IS '更新人';
COMMENT ON COLUMN "public"."eadm_user"."updatedat" IS '更新时间';
COMMENT ON COLUMN "public"."eadm_user"."deleteduser" IS '删除人';
COMMENT ON COLUMN "public"."eadm_user"."deletedat" IS '删除时间';
COMMENT ON COLUMN "public"."eadm_user"."deleted" IS '是否删除(0否1是)';
COMMENT ON TABLE "public"."eadm_user" IS '基础信息_用户信息表';

-- 最后一次更新时间
DROP TRIGGER IF EXISTS "user_lastupdate" ON "public"."eadm_user" CASCADE;

CREATE OR REPLACE TRIGGER "user_lastupdate"
BEFORE UPDATE ON "public"."eadm_user"
FOR EACH ROW
EXECUTE FUNCTION "public"."lastupdate"();

-- 写入用户数据

-- SELECT * FROM "public"."eadm_tenant";

TRUNCATE TABLE "public"."eadm_user";

INSERT INTO "public"."eadm_user"(tenantid, loginname, username, email, passwd)
VALUES('ET0000000001','wangcw', '王存伟', 'rubygreat@msn.com', 'q122/4GBpiCNq83AbPQN/+kYq0KwczLxiWfLaLKk4NY=');

INSERT INTO "public"."eadm_user"(tenantid, loginname, username, email, passwd)
VALUES('ET0000000001','wongcw', '王存偉', 'rubygreat@msn.com', 'q122/4GBpiCNq83AbPQN/+kYq0KwczLxiWfLaLKk4NY=');

INSERT INTO "public"."eadm_user"(tenantid, loginname, username, email, passwd)
VALUES('ET0000000001','jiangyf', '姜玉凤', '1234567@qq.com', 'q122/4GBpiCNq83AbPQN/+kYq0KwczLxiWfLaLKk4NY=');


-- 用户视图
CREATE OR REPLACE VIEW "public"."vi_user"
AS
SELECT Id, "gettenantname"(tenantid) AS tenantname, loginname, username, email,
       CASE userstatus WHEN 1 THEN '禁用' WHEN 0 THEN '启用' END AS userstatus, createdat
FROM "public"."eadm_user"
  WHERE deleted IS FALSE;

-- SELECT * FROM vi_user;

-- 用户角色
DROP SEQUENCE IF EXISTS ER CASCADE;
CREATE SEQUENCE ER
START 1
INCREMENT BY 1
MAXVALUE 9999999999
CACHE 10;

DROP TABLE IF EXISTS "public"."eadm_role" CASCADE;
CREATE TABLE "public"."eadm_role" (
  id CHAR(12) NOT NULL DEFAULT ('ER' || LPAD((nextval('ER')::VARCHAR), 10, '0')),
  rolename VARCHAR(50) NOT NULL,
  rolepermission JSON,
  rolestatus SMALLINT NOT NULL DEFAULT 0,
  createduser VARCHAR(50),
  createdat TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  updateduser VARCHAR(50),
  updatedat TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  deleteduser VARCHAR(50),
  deletedat TIMESTAMPTZ,
  deleted BOOLEAN NOT NULL DEFAULT FALSE
);

ALTER TABLE "public"."eadm_role" OWNER TO "user_eadm";
ALTER TABLE "public"."eadm_role" DROP CONSTRAINT IF EXISTS "pk_role_id" CASCADE;
ALTER TABLE "public"."eadm_role" ADD CONSTRAINT "pk_role_id" PRIMARY KEY ("id");

DROP INDEX IF EXISTS "non_role_rolename";
CREATE INDEX "non_role_rolename" ON "public"."eadm_role" USING BTREE ("rolename" ASC NULLS LAST);
DROP INDEX IF EXISTS "non_user_rolestatus";
CREATE INDEX "non_user_rolestatus" ON "public"."eadm_role" USING BTREE ("rolestatus" ASC NULLS LAST);

COMMENT ON COLUMN "public"."eadm_role"."id" IS '自定义主键(ER)';
COMMENT ON COLUMN "public"."eadm_role"."rolename" IS '角色名称';
COMMENT ON COLUMN "public"."eadm_role"."rolepermission" IS '角色权限';
COMMENT ON COLUMN "public"."eadm_role"."rolestatus" IS '角色状态(0启用1禁用)';
COMMENT ON COLUMN "public"."eadm_role"."createduser" IS '创建人';
COMMENT ON COLUMN "public"."eadm_role"."createdat" IS '创建时间';
COMMENT ON COLUMN "public"."eadm_role"."updateduser" IS '更新人';
COMMENT ON COLUMN "public"."eadm_role"."updatedat" IS '更新时间';
COMMENT ON COLUMN "public"."eadm_role"."deleteduser" IS '删除人';
COMMENT ON COLUMN "public"."eadm_role"."deletedat" IS '删除时间';
COMMENT ON COLUMN "public"."eadm_role"."deleted" IS '是否删除(0否1是)';
COMMENT ON TABLE "public"."eadm_role" IS '基础信息_角色信息表';

-- 最后一次更新时间
DROP TRIGGER IF EXISTS "role_lastupdate" ON "public"."eadm_role" CASCADE;

CREATE OR REPLACE TRIGGER "role_lastupdate"
BEFORE UPDATE ON "public"."eadm_role"
FOR EACH ROW
EXECUTE FUNCTION "public"."lastupdate"();

-- 写入数据
TRUNCATE TABLE "public"."eadm_role";

INSERT INTO "public"."eadm_role"(rolename, rolepermission)
VALUES('超级管理员', '{}');

INSERT INTO "public"."eadm_role"(rolename, rolepermission)
VALUES('注册租户', '{}');

INSERT INTO "public"."eadm_role"(rolename, rolepermission)
VALUES('分配租户', '{}');

-- 角色视图
CREATE OR REPLACE VIEW "public"."vi_role"
AS
SELECT id, rolename, rolepermission, CASE rolestatus WHEN 1 THEN '禁用' WHEN 0 THEN '启用' END AS rolestatus, createdat
FROM "public"."eadm_role"
  WHERE deleted IS FALSE;

-- SELECT * FROM "public"."eadm_role";

-- 用户角色对应关系
DROP TABLE IF EXISTS "public"."eadm_userrole" CASCADE;
CREATE TABLE "public"."eadm_userrole" (
  id SERIAL,
  userid CHAR(12) NOT NULL,
  roleid CHAR(12) NOT NULL,
  createduser VARCHAR(50),
  createdat TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  updateduser VARCHAR(50),
  updatedat TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  deleteduser VARCHAR(50),
  deletedat TIMESTAMPTZ,
  deleted BOOLEAN NOT NULL DEFAULT FALSE
);

ALTER TABLE "public"."eadm_userrole" OWNER TO "user_eadm";
ALTER TABLE "public"."eadm_userrole" DROP CONSTRAINT IF EXISTS "pk_userrole_id" CASCADE;
ALTER TABLE "public"."eadm_userrole" ADD CONSTRAINT "pk_userrole_id" PRIMARY KEY ("id");

ALTER TABLE "public"."eadm_userrole" DROP CONSTRAINT IF EXISTS "fk_userid_user_id" CASCADE;
ALTER TABLE "public"."eadm_userrole" ADD CONSTRAINT "fk_userid_user_id" FOREIGN KEY ("userid")
    REFERENCES "public"."eadm_user" ("id") ON DELETE RESTRICT ON UPDATE RESTRICT;

ALTER TABLE "public"."eadm_userrole" DROP CONSTRAINT IF EXISTS "fk_roleid_role_id" CASCADE;
ALTER TABLE "public"."eadm_userrole" ADD CONSTRAINT "fk_roleid_role_id" FOREIGN KEY ("roleid")
    REFERENCES "public"."eadm_role" ("id") ON DELETE RESTRICT ON UPDATE RESTRICT;

COMMENT ON COLUMN "public"."eadm_userrole"."id" IS '自增主键';
COMMENT ON COLUMN "public"."eadm_userrole"."userid" IS '用户Id(eadm_user.Id)';
COMMENT ON COLUMN "public"."eadm_userrole"."roleid" IS '角色Id(eadm_role.Id)';
COMMENT ON COLUMN "public"."eadm_userrole"."createduser" IS '创建人';
COMMENT ON COLUMN "public"."eadm_userrole"."createdat" IS '创建时间';
COMMENT ON COLUMN "public"."eadm_userrole"."updateduser" IS '更新人';
COMMENT ON COLUMN "public"."eadm_userrole"."updatedat" IS '更新时间';
COMMENT ON COLUMN "public"."eadm_userrole"."deleteduser" IS '删除人';
COMMENT ON COLUMN "public"."eadm_userrole"."deletedat" IS '删除时间';
COMMENT ON COLUMN "public"."eadm_userrole"."deleted" IS '是否删除(0否1是)';
COMMENT ON TABLE "public"."eadm_userrole" IS '基础信息_用户角色对应关系表';

-- 最后一次更新时间
DROP TRIGGER IF EXISTS "roleuser_lastupdate" ON "public"."eadm_userrole" CASCADE;

CREATE OR REPLACE TRIGGER "roleuser_lastupdate"
BEFORE UPDATE ON "public"."eadm_userrole"
FOR EACH ROW
EXECUTE FUNCTION "public"."lastupdate"();

-- 写入数据
-- SELECT * FROM "public"."eadm_user";
-- SELECT * FROM "public"."eadm_role";

INSERT INTO "public"."eadm_userrole"(userid, roleid)
VALUES('EU0000000001', 'ER0000000001');

-- 用户角色信息视图
CREATE OR REPLACE VIEW "public"."vi_userrole"
AS
SELECT B.id, A.id AS userid, C.id AS roleid, C.rolename, B.updatedat
FROM "public"."eadm_user" A
INNER JOIN "public"."eadm_userrole" B
  ON B.userid=A.id
  AND B.deleted IS FALSE
INNER JOIN "public"."eadm_role" C
  ON C.id=B.roleid
  AND C.rolestatus=0
  AND C.deleted IS FALSE
WHERE A.deleted IS FALSE;

-- 用户权限信息视图
CREATE OR REPLACE VIEW "public"."vi_userpermission"
AS
SELECT B.id, A.loginname, C.rolepermission
FROM "public"."eadm_user" A
INNER JOIN "public"."eadm_userrole" B
  ON B.userid=A.id
  AND B.deleted IS FALSE
INNER JOIN "public"."eadm_role" C
  ON C.id=B.roleid
  AND C.rolestatus=0
  AND C.deleted IS FALSE
WHERE A.deleted IS FALSE;

-- 定时任务信息
DROP SEQUENCE IF EXISTS CR CASCADE;
CREATE SEQUENCE CR
START 1
INCREMENT BY 1
MAXVALUE 9999999999
CACHE 10;

DROP TABLE IF EXISTS "public"."eadm_crontab" CASCADE;
CREATE TABLE "public"."eadm_crontab" (
  id CHAR(12) NOT NULL DEFAULT ('CR' || LPAD((nextval('CR')::VARCHAR), 10, '0')),
  cronname VARCHAR(50) NOT NULL,
  cronexp VARCHAR(50),
  cronmfa VARCHAR(50),
  starttime TIMESTAMPTZ,
  endtime TIMESTAMPTZ,
  cronstatus SMALLINT NOT NULL DEFAULT 0,
  createduser VARCHAR(50),
  createdat TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  updateduser VARCHAR(50),
  updatedat TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  deleteduser VARCHAR(50),
  deletedat TIMESTAMPTZ,
  deleted BOOLEAN NOT NULL DEFAULT FALSE
);

ALTER TABLE "public"."eadm_crontab" OWNER TO "user_eadm";
ALTER TABLE "public"."eadm_crontab" DROP CONSTRAINT IF EXISTS "pk_crontab_id" CASCADE;
ALTER TABLE "public"."eadm_crontab" ADD CONSTRAINT "pk_crontab_id" PRIMARY KEY ("id");

DROP INDEX IF EXISTS "non_crontab_cronname";
CREATE INDEX "non_crontab_cronname" ON "public"."eadm_crontab" USING BTREE ("cronname" ASC NULLS LAST);

COMMENT ON COLUMN "public"."eadm_crontab"."id" IS '自定义主键(CR)';
COMMENT ON COLUMN "public"."eadm_crontab"."cronname" IS '任务名称';
COMMENT ON COLUMN "public"."eadm_crontab"."cronexp" IS '定时表达式';
COMMENT ON COLUMN "public"."eadm_crontab"."cronmfa" IS '任务备注';
COMMENT ON COLUMN "public"."eadm_crontab"."starttime" IS '任务开始时间';
COMMENT ON COLUMN "public"."eadm_crontab"."endtime" IS '任务结束时间';
COMMENT ON COLUMN "public"."eadm_crontab"."cronstatus" IS '任务状态(0启用1禁用)';
COMMENT ON COLUMN "public"."eadm_crontab"."createduser" IS '创建人';
COMMENT ON COLUMN "public"."eadm_crontab"."createdat" IS '创建时间';
COMMENT ON COLUMN "public"."eadm_crontab"."updateduser" IS '更新人';
COMMENT ON COLUMN "public"."eadm_crontab"."updatedat" IS '更新时间';
COMMENT ON COLUMN "public"."eadm_crontab"."deleteduser" IS '删除人';
COMMENT ON COLUMN "public"."eadm_crontab"."deletedat" IS '删除时间';
COMMENT ON COLUMN "public"."eadm_crontab"."deleted" IS '是否删除(0否1是)';
COMMENT ON TABLE "public"."eadm_crontab" IS '基础信息_定时任务信息表';

-- 最后一次更新时间
DROP TRIGGER IF EXISTS "crontab_lastupdate" ON "public"."eadm_crontab" CASCADE;

CREATE OR REPLACE TRIGGER "crontab_lastupdate"
BEFORE UPDATE ON "public"."eadm_crontab"
FOR EACH ROW
EXECUTE FUNCTION "public"."lastupdate"();

-- 首页报表
DROP TABLE IF EXISTS "public"."eadm_dashboard" CASCADE;
CREATE TABLE "public"."eadm_dashboard"(
    id SERIAL,
    datatype SMALLINT NOT NULL,
    datetype SMALLINT NOT NULL,
    loginname VARCHAR(50),
    datavalue VARCHAR(500),
    datajson JSON,
    checkdate VARCHAR(20) NOT NULL,
    updatedat TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    instertime TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

ALTER TABLE "public"."eadm_dashboard" OWNER TO "user_eadm";
ALTER TABLE "public"."eadm_dashboard" DROP CONSTRAINT IF EXISTS "pk_dashboard_id" CASCADE;
ALTER TABLE "public"."eadm_dashboard" ADD CONSTRAINT "pk_dashboard_id" PRIMARY KEY ("id");

ALTER TABLE "public"."eadm_dashboard" DROP CONSTRAINT IF EXISTS "uni_dashboard_ddlc" CASCADE;
ALTER TABLE "public"."eadm_dashboard" ADD CONSTRAINT "uni_dashboard_ddlc"
    UNIQUE ("datatype", "datetype", "loginname", "checkdate");

DROP INDEX IF EXISTS "non_dashboard_datatype";
CREATE INDEX "non_dashboard_datatype" ON "public"."eadm_dashboard" USING BTREE ("datatype" ASC NULLS LAST);
DROP INDEX IF EXISTS "non_dashboard_datetype";
CREATE INDEX "non_dashboard_datetype" ON "public"."eadm_dashboard" USING BTREE ("datetype" ASC NULLS LAST);
DROP INDEX IF EXISTS "non_dashboard_loginname";
CREATE INDEX "non_dashboard_loginname" ON "public"."eadm_dashboard" USING BTREE ("loginname" ASC NULLS LAST);
DROP INDEX IF EXISTS "non_dashboard_checkdate";
CREATE INDEX "non_dashboard_checkdate" ON "public"."eadm_dashboard" USING BTREE ("checkdate" ASC NULLS LAST);

COMMENT ON COLUMN "public"."eadm_dashboard"."id" IS '自增主键';
COMMENT ON COLUMN "public"."eadm_dashboard"."datatype" IS '数据类型(1心率2步数3睡眠4里程5每月里程6每月收入7每月支出)';
COMMENT ON COLUMN "public"."eadm_dashboard"."datetype" IS '统计周期类型(1日2周3月4年)';
COMMENT ON COLUMN "public"."eadm_dashboard"."loginname" IS '登录名';
COMMENT ON COLUMN "public"."eadm_dashboard"."datavalue" IS '数据值';
COMMENT ON COLUMN "public"."eadm_dashboard"."datajson" IS '数据JSON';
COMMENT ON COLUMN "public"."eadm_dashboard"."checkdate" IS '数据日期';
COMMENT ON COLUMN "public"."eadm_dashboard"."updatedat" IS '更新时间';
COMMENT ON COLUMN "public"."eadm_dashboard"."instertime" IS '插入时间';
COMMENT ON TABLE "public"."eadm_dashboard" IS '首页_看板报表';

-- 最后一次更新时间
DROP TRIGGER IF EXISTS "dashboard_lastupdate" ON "public"."eadm_dashboard" CASCADE;

CREATE OR REPLACE TRIGGER "dashboard_lastupdate"
BEFORE UPDATE ON "public"."eadm_dashboard"
FOR EACH ROW
EXECUTE FUNCTION "public"."lastupdate"();

-- 过程运行日志
DROP TABLE IF EXISTS "public"."sys_proclog" CASCADE;
CREATE TABLE "public"."sys_proclog" (
  id SERIAL,
  procname VARCHAR(50),
  timespan INT,
  result BOOLEAN NOT NULL DEFAULT TRUE,
  errcode VARCHAR(5),
  errmessage VARCHAR(5000),
  inserttime TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

ALTER TABLE "public"."sys_proclog" OWNER TO "user_eadm";
ALTER TABLE "public"."sys_proclog" DROP CONSTRAINT IF EXISTS "pk_proclog_id" CASCADE;
ALTER TABLE "public"."sys_proclog" ADD CONSTRAINT "pk_proclog_id" PRIMARY KEY ("id");

DROP INDEX IF EXISTS "non_proclog_procname";
CREATE INDEX "non_proclog_procname" ON "public"."sys_proclog" USING BTREE ("procname" ASC NULLS LAST);
DROP INDEX IF EXISTS "non_proclog_inserttime";
CREATE INDEX "non_proclog_inserttime" ON "public"."sys_proclog" USING BTREE ("inserttime" ASC NULLS LAST);

COMMENT ON COLUMN "public"."sys_proclog"."id" IS '自增主键';
COMMENT ON COLUMN "public"."sys_proclog"."procname" IS '过程名';
COMMENT ON COLUMN "public"."sys_proclog"."timespan" IS '耗时时长(秒)';
COMMENT ON COLUMN "public"."sys_proclog"."result" IS '是否成功(0否1是)';
COMMENT ON COLUMN "public"."sys_proclog"."errcode" IS '错误代码';
COMMENT ON COLUMN "public"."sys_proclog"."errmessage" IS '错误详细信息';
COMMENT ON COLUMN "public"."sys_proclog"."inserttime" IS '日志记录时间';
COMMENT ON TABLE "public"."sys_proclog" IS '系统域_过程执行日志';

-- 设备信息
DROP TABLE IF EXISTS "public"."eadm_device" CASCADE;
CREATE TABLE "public"."eadm_device" (
  deviceno VARCHAR(50),
  imei VARCHAR(50),
  simno VARCHAR(50),
  enable BOOLEAN NOT NULL DEFAULT TRUE,
  remark VARCHAR(200),
  createduser VARCHAR(50),
  createdat TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  updateduser VARCHAR(50),
  updatedat TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  deleteduser VARCHAR(50),
  deletedat TIMESTAMPTZ,
  deleted BOOLEAN NOT NULL DEFAULT FALSE
);

ALTER TABLE "public"."eadm_device" OWNER TO "user_eadm";
ALTER TABLE "public"."eadm_device" DROP CONSTRAINT IF EXISTS "pk_device_deviceno" CASCADE;
ALTER TABLE "public"."eadm_device" ADD CONSTRAINT "pk_device_deviceno" PRIMARY KEY ("deviceno");

DROP INDEX IF EXISTS "non_device_simno";
CREATE INDEX "non_device_simno" ON "public"."eadm_device" USING BTREE ("simno" ASC NULLS LAST);

COMMENT ON COLUMN "public"."eadm_device"."deviceno" IS '设备号(主键)';
COMMENT ON COLUMN "public"."eadm_device"."imei" IS '设备IMEI';
COMMENT ON COLUMN "public"."eadm_device"."simno" IS 'SIM卡号';
COMMENT ON COLUMN "public"."eadm_device"."enable" IS '设备状态(1启用0禁用)';
COMMENT ON COLUMN "public"."eadm_device"."remark" IS '设备描述';
COMMENT ON COLUMN "public"."eadm_device"."createduser" IS '创建人';
COMMENT ON COLUMN "public"."eadm_device"."createdat" IS '创建时间';
COMMENT ON COLUMN "public"."eadm_device"."updateduser" IS '更新人';
COMMENT ON COLUMN "public"."eadm_device"."updatedat" IS '更新时间';
COMMENT ON COLUMN "public"."eadm_device"."deleteduser" IS '删除人';
COMMENT ON COLUMN "public"."eadm_device"."deletedat" IS '删除时间';
COMMENT ON COLUMN "public"."eadm_device"."deleted" IS '是否删除(0否1是)';
COMMENT ON TABLE "public"."eadm_device" IS '业务域_设备信息';

-- 最后一次更新时间
DROP TRIGGER IF EXISTS "device_lastupdate" ON "public"."eadm_device" CASCADE;

CREATE OR REPLACE TRIGGER "device_lastupdate"
BEFORE UPDATE ON "public"."eadm_device"
FOR EACH ROW
EXECUTE FUNCTION "public"."lastupdate"();

-- 写入数据
INSERT INTO "public"."eadm_device"(deviceno, remark, createduser)
VALUES('16053489111', '充电宝', 'wangcw'),
      ('868977061978771', '手表', 'wangcw');

-- 人员设备对应关系
DROP TABLE IF EXISTS "public"."eadm_userdevice" CASCADE;
CREATE TABLE "public"."eadm_userdevice" (
  id SERIAL,
  userid CHAR(12),
  loginname VARCHAR(50),
  deviceno VARCHAR(50) NOT NULL,
  createduser VARCHAR(50),
  createdat TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  updateduser VARCHAR(50),
  updatedat TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  deleteduser VARCHAR(50),
  deletedat TIMESTAMPTZ,
  deleted BOOLEAN NOT NULL DEFAULT FALSE
);

ALTER TABLE "public"."eadm_userdevice" OWNER TO "user_eadm";
ALTER TABLE "public"."eadm_userdevice" DROP CONSTRAINT IF EXISTS "pk_userdevice_id" CASCADE;
ALTER TABLE "public"."eadm_userdevice" ADD CONSTRAINT "pk_userdevice_id" PRIMARY KEY ("id");

ALTER TABLE "public"."eadm_userdevice" DROP CONSTRAINT IF EXISTS "fk_userid_user_id" CASCADE;
ALTER TABLE "public"."eadm_userdevice" ADD CONSTRAINT "fk_userid_user_id" FOREIGN KEY ("userid")
    REFERENCES "public"."eadm_user" ("id") ON DELETE RESTRICT ON UPDATE RESTRICT;
ALTER TABLE "public"."eadm_userdevice" DROP CONSTRAINT IF EXISTS "fk_loginname_user_loginname" CASCADE;
ALTER TABLE "public"."eadm_userdevice" ADD CONSTRAINT "fk_loginname_user_loginname" FOREIGN KEY ("loginname")
    REFERENCES "public"."eadm_user" ("loginname") ON DELETE RESTRICT ON UPDATE RESTRICT;
ALTER TABLE "public"."eadm_userdevice" DROP CONSTRAINT IF EXISTS "fk_deviceno_device_deviceno" CASCADE;
ALTER TABLE "public"."eadm_userdevice" ADD CONSTRAINT "fk_deviceno_device_deviceno" FOREIGN KEY ("deviceno")
    REFERENCES "public"."eadm_device" ("deviceno") ON DELETE RESTRICT ON UPDATE RESTRICT;

COMMENT ON COLUMN "public"."eadm_userdevice"."id" IS '自增主键';
COMMENT ON COLUMN "public"."eadm_userdevice"."userid" IS '用户Id(eadm_user.Id)';
COMMENT ON COLUMN "public"."eadm_userdevice"."loginname" IS '用户登录名(eadm_user.LoginName)';
COMMENT ON COLUMN "public"."eadm_userdevice"."deviceno" IS '设备Id(eadm_device.DeviceNo)';
COMMENT ON COLUMN "public"."eadm_userdevice"."createduser" IS '创建人';
COMMENT ON COLUMN "public"."eadm_userdevice"."createdat" IS '创建时间';
COMMENT ON COLUMN "public"."eadm_userdevice"."updateduser" IS '更新人';
COMMENT ON COLUMN "public"."eadm_userdevice"."updatedat" IS '更新时间';
COMMENT ON COLUMN "public"."eadm_userdevice"."deleteduser" IS '删除人';
COMMENT ON COLUMN "public"."eadm_userdevice"."deletedat" IS '删除时间';
COMMENT ON COLUMN "public"."eadm_userdevice"."deleted" IS '是否删除(0否1是)';
COMMENT ON TABLE "public"."eadm_userdevice" IS '业务域_人员设备对应关系';

-- 最后一次更新时间
DROP TRIGGER IF EXISTS "userdevice_lastupdate" ON "public"."eadm_userdevice" CASCADE;

CREATE OR REPLACE TRIGGER "userdevice_lastupdate"
BEFORE UPDATE ON "public"."eadm_userdevice"
FOR EACH ROW
EXECUTE FUNCTION "public"."lastupdate"();

-- 写入数据
INSERT INTO "public"."eadm_userdevice"(userid, loginname, deviceno, createduser)
SELECT 'EU0000000001', 'wangcw', DeviceNo, 'wangcw'
FROM "public"."eadm_device";

-- SELECT * FROM "public"."eadm_userdevice";

-- 业务数据_车辆定位信息
DROP TABLE IF EXISTS "public"."lc_carlocdaily";
CREATE TABLE "public"."lc_carlocdaily" (
  "ptime" TIMESTAMPTZ,
  "deviceno" VARCHAR(20),
  "lat" NUMERIC(9,6),
  "lng" NUMERIC(9,6),
  "dirct" INT,
  "speed" INT,
  "mileage" NUMERIC(18,2),
  "hight" INT,
  "gnssnum" INT,
  "rssi" INT,
  "receivetime" TIMESTAMPTZ,
  "inserttime" TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP
);

ALTER TABLE "public"."lc_carlocdaily" OWNER TO "user_eadm";
ALTER TABLE "public"."lc_carlocdaily" DROP CONSTRAINT IF EXISTS "pk_carlocdaily_ptime" CASCADE;
ALTER TABLE "public"."lc_carlocdaily" ADD CONSTRAINT "pk_carlocdaily_ptime" PRIMARY KEY ("ptime");

DROP INDEX IF EXISTS "non_carlocdaily_deviceno";
CREATE INDEX "non_carlocdaily_deviceno" ON "public"."lc_carlocdaily" USING BTREE ("deviceno" ASC NULLS LAST);

COMMENT ON COLUMN "public"."lc_carlocdaily"."ptime" IS '设备上传时间(主键)';
COMMENT ON COLUMN "public"."lc_carlocdaily"."deviceno" IS '设备编码(ICCID)';
COMMENT ON COLUMN "public"."lc_carlocdaily"."lat" IS '经度';
COMMENT ON COLUMN "public"."lc_carlocdaily"."lng" IS '纬度';
COMMENT ON COLUMN "public"."lc_carlocdaily"."dirct" IS '方向角';
COMMENT ON COLUMN "public"."lc_carlocdaily"."speed" IS '速度';
COMMENT ON COLUMN "public"."lc_carlocdaily"."mileage" IS '里程';
COMMENT ON COLUMN "public"."lc_carlocdaily"."hight" IS '海拔';
COMMENT ON COLUMN "public"."lc_carlocdaily"."gnssnum" IS 'GPS卫星数量';
COMMENT ON COLUMN "public"."lc_carlocdaily"."rssi" IS '4G信号值';
COMMENT ON COLUMN "public"."lc_carlocdaily"."receivetime" IS 'GateWay处理时间';
COMMENT ON COLUMN "public"."lc_carlocdaily"."inserttime" IS '数据写入时间';
COMMENT ON TABLE "public"."lc_carlocdaily" IS '车辆日常定位信息';

-- 业务数据_手表信息
DROP TABLE IF EXISTS "public"."lc_watchdaily";
CREATE TABLE "public"."lc_watchdaily" (
  "ptime" TIMESTAMPTZ NOT NULL,
  "steps" VARCHAR(50),
  "heartbeat" VARCHAR(50),
  "roll" VARCHAR(50),
  "bodytemperature" VARCHAR(50),
  "wristtemperature" VARCHAR(50),
  "bloodsugar" VARCHAR(50),
  "diastolic" VARCHAR(50),
  "shrink" VARCHAR(50),
  "bloodoxygen" VARCHAR(50),
  "sleeptype" VARCHAR(50),
  "sleepstartTime" VARCHAR(50),
  "sleependtime" VARCHAR(50),
  "sleepminute" VARCHAR(50),
  "signal" VARCHAR(50),
  "battery" VARCHAR(50),
  "lat" VARCHAR(50),
  "lng" VARCHAR(50),
  "speed" VARCHAR(50),
  "inserttime" TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP
);

ALTER TABLE "public"."lc_watchdaily" OWNER TO "user_eadm";
ALTER TABLE "public"."lc_watchdaily" DROP CONSTRAINT IF EXISTS "pk_watchdaily_ptime" CASCADE;
ALTER TABLE "public"."lc_watchdaily" ADD CONSTRAINT "pk_watchdaily_ptime" PRIMARY KEY ("ptime");

COMMENT ON COLUMN "public"."lc_watchdaily"."ptime" IS '数据获取时间';
COMMENT ON COLUMN "public"."lc_watchdaily"."steps" IS '步数';
COMMENT ON COLUMN "public"."lc_watchdaily"."heartbeat" IS '心率';
COMMENT ON COLUMN "public"."lc_watchdaily"."roll" IS '翻转数';
COMMENT ON COLUMN "public"."lc_watchdaily"."bodytemperature" IS '体温';
COMMENT ON COLUMN "public"."lc_watchdaily"."wristtemperature" IS '腕温';
COMMENT ON COLUMN "public"."lc_watchdaily"."bloodsugar" IS '血糖';
COMMENT ON COLUMN "public"."lc_watchdaily"."diastolic" IS '舒张压';
COMMENT ON COLUMN "public"."lc_watchdaily"."shrink" IS '收缩压';
COMMENT ON COLUMN "public"."lc_watchdaily"."bloodoxygen" IS '血氧';
COMMENT ON COLUMN "public"."lc_watchdaily"."sleeptype" IS '睡眠类型(1深度睡眠2浅度睡眠3醒来时长)';
COMMENT ON COLUMN "public"."lc_watchdaily"."sleepstartTime" IS '睡眠开始时间';
COMMENT ON COLUMN "public"."lc_watchdaily"."sleependtime" IS '睡眠结束时间';
COMMENT ON COLUMN "public"."lc_watchdaily"."sleepminute" IS '睡眠时长(分钟)';
COMMENT ON COLUMN "public"."lc_watchdaily"."signal" IS '信号值';
COMMENT ON COLUMN "public"."lc_watchdaily"."battery" IS '电池电量';
COMMENT ON COLUMN "public"."lc_watchdaily"."lat" IS '定位纬度(GPS)';
COMMENT ON COLUMN "public"."lc_watchdaily"."lng" IS '定位经度(GPS)';
COMMENT ON COLUMN "public"."lc_watchdaily"."speed" IS '速度';
COMMENT ON COLUMN "public"."lc_watchdaily"."inserttime" IS '数据写入时间';
COMMENT ON TABLE "public"."lc_watchdaily" IS '手表日常数据';

DROP TABLE IF EXISTS "public"."lc_watchalarm";
CREATE TABLE "public"."lc_watchalarm" (
  "id" SERIAL,
  "alerttype" VARCHAR(10),
  "alertinfo" VARCHAR(1000),
  "heartnum" VARCHAR(50),
  "lasttemper" VARCHAR(50),
  "inserttime" TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP
);

ALTER TABLE "public"."lc_watchalarm" OWNER TO "user_eadm";
ALTER TABLE "public"."lc_watchalarm" DROP CONSTRAINT IF EXISTS "pk_watchalarm_id" CASCADE;
ALTER TABLE "public"."lc_watchalarm" ADD CONSTRAINT "pk_watchalarm_id" PRIMARY KEY ("id");

DROP INDEX IF EXISTS "non_watchalarm_alerttype";
CREATE INDEX "non_watchalarm_alerttype" ON "public"."lc_watchalarm" USING BTREE ("alerttype" ASC NULLS LAST);

COMMENT ON COLUMN "public"."lc_watchalarm"."id" IS '自增主键';
COMMENT ON COLUMN "public"."lc_watchalarm"."alerttype" IS '预警类型';
COMMENT ON COLUMN "public"."lc_watchalarm"."alertinfo" IS '报警信息内容';
COMMENT ON COLUMN "public"."lc_watchalarm"."heartnum" IS '当前心率';
COMMENT ON COLUMN "public"."lc_watchalarm"."lasttemper" IS '当前体温';
COMMENT ON TABLE "public"."lc_watchalarm" IS '手表日常报警信息';

-- 财务数据
DROP TABLE IF EXISTS "public"."fn_paybilldetail";
CREATE TABLE "public"."fn_paybilldetail" (
  id SERIAL,
  owner VARCHAR(50),
  source int2,
  inorout VARCHAR(10),
  counterparty VARCHAR(100),
  counterbank VARCHAR(100),
  counteraccount VARCHAR(50),
  goodscomment VARCHAR(200),
  paymethod VARCHAR(50),
  amount NUMERIC(18,2),
  balance NUMERIC(18,2),
  currency VARCHAR(50),
  paystatus VARCHAR(50),
  tradetype VARCHAR(50),
  tradeorderno VARCHAR(100),
  counterorderno VARCHAR(100),
  tradetime TIMESTAMPTZ,
  billcomment VARCHAR(500),
  inserttime TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
  deleteduser VARCHAR(50),
  deletedat TIMESTAMPTZ,
  deleted BOOLEAN NOT NULL DEFAULT FALSE
);

ALTER TABLE "public"."fn_paybilldetail" OWNER TO "user_eadm";

ALTER TABLE "public"."fn_paybilldetail" DROP CONSTRAINT IF EXISTS "pk_paybilldetail_id" CASCADE;
ALTER TABLE "public"."fn_paybilldetail" ADD CONSTRAINT "pk_paybilldetail_id" PRIMARY KEY ("id");

DROP INDEX IF EXISTS "non_paybilldetail_source";
CREATE INDEX "non_paybilldetail_source" ON "public"."fn_paybilldetail" USING BTREE ("source" ASC NULLS LAST);
DROP INDEX IF EXISTS "non_paybilldetail_inorout";
CREATE INDEX "non_paybilldetail_inorout" ON "public"."fn_paybilldetail" USING BTREE ("inorout" ASC NULLS LAST);
DROP INDEX IF EXISTS "non_paybilldetail_paymethod";
CREATE INDEX "non_paybilldetail_paymethod" ON "public"."fn_paybilldetail" USING BTREE ("paymethod" ASC NULLS LAST);
DROP INDEX IF EXISTS "non_paybilldetail_paystatus";
CREATE INDEX "non_paybilldetail_paystatus" ON "public"."fn_paybilldetail" USING BTREE ("paystatus" ASC NULLS LAST);
DROP INDEX IF EXISTS "non_paybilldetail_tradetype";
CREATE INDEX "non_paybilldetail_tradetype" ON "public"."fn_paybilldetail" USING BTREE ("tradetype" ASC NULLS LAST);
DROP INDEX IF EXISTS "non_paybilldetail_tradetime";
CREATE INDEX "non_paybilldetail_tradetime" ON "public"."fn_paybilldetail" USING BTREE ("tradetime" ASC NULLS LAST);

COMMENT ON COLUMN "public"."fn_paybilldetail"."id" IS '自增主键';
COMMENT ON COLUMN "public"."fn_paybilldetail"."owner" IS '来源人(姓名)';
COMMENT ON COLUMN "public"."fn_paybilldetail"."source" IS '来源:1支付宝2微信3青岛银行4中国银行';
COMMENT ON COLUMN "public"."fn_paybilldetail"."inorout" IS '收/支';
COMMENT ON COLUMN "public"."fn_paybilldetail"."counterparty" IS '交易对方';
COMMENT ON COLUMN "public"."fn_paybilldetail"."counterbank" IS '对方开户行';
COMMENT ON COLUMN "public"."fn_paybilldetail"."counteraccount" IS '对方账号';
COMMENT ON COLUMN "public"."fn_paybilldetail"."goodscomment" IS '商品说明';
COMMENT ON COLUMN "public"."fn_paybilldetail"."paymethod" IS '收/付款方式';
COMMENT ON COLUMN "public"."fn_paybilldetail"."amount" IS '金额';
COMMENT ON COLUMN "public"."fn_paybilldetail"."balance" IS '余额';
COMMENT ON COLUMN "public"."fn_paybilldetail"."currency" IS '币种';
COMMENT ON COLUMN "public"."fn_paybilldetail"."paystatus" IS '交易状态';
COMMENT ON COLUMN "public"."fn_paybilldetail"."tradetype" IS '交易分类';
COMMENT ON COLUMN "public"."fn_paybilldetail"."tradeorderno" IS '交易订单号';
COMMENT ON COLUMN "public"."fn_paybilldetail"."counterorderno" IS '商家订单号';
COMMENT ON COLUMN "public"."fn_paybilldetail"."tradetime" IS '交易时间';
COMMENT ON COLUMN "public"."fn_paybilldetail"."billcomment" IS '交易备注';
COMMENT ON COLUMN "public"."fn_paybilldetail"."inserttime" IS '数据写入时间';
COMMENT ON COLUMN "public"."fn_paybilldetail"."deleteduser" IS '删除人账号';
COMMENT ON COLUMN "public"."fn_paybilldetail"."deletedat" IS '删除时间';
COMMENT ON COLUMN "public"."fn_paybilldetail"."deleted" IS '是否删除(0否1是)';
COMMENT ON TABLE "public"."fn_paybilldetail" IS '账单明细';
