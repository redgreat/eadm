-- @author wangcw
-- @copyright (C) 2024, REDGREAT
-- Created : 2024-06-14 下午1:36
-- DB2表结构设计

-- SELECT SERVICE_LEVEL, BLD_LEVEL, FIXPACK_NUM FROM SYSIBMADM.ENV_INST_INFO;
-- 创建序列
CREATE OR REPLACE SEQUENCE SEQ_SD
START WITH 1
INCREMENT BY 1
NO MAXVALUE
NO CYCLE
CACHE 10;

-- 系统_字典信息表
DROP TABLE IF EXISTS SYS_DICT;
CREATE TABLE SYS_DICT (
  ID CHAR(12) NOT NULL DEFAULT ,
  DICTNO VARCHAR(50) NOT NULL,
  DICTNAME VARCHAR(100) NOT NULL,
  PARENTID CHAR(12),
  CREATEDUSER VARCHAR(50),
  CREATEDAT TIMESTAMP NOT NULL WITH DEFAULT CURRENT TIMESTAMP,
  UPDATEDUSER VARCHAR(50),
  UPDATEDAT TIMESTAMP NOT NULL WITH DEFAULT CURRENT TIMESTAMP,
  DELETEDUSER VARCHAR(50),
  DELETEDAT TIMESTAMP,
  DELETED BOOLEAN NOT NULL DEFAULT FALSE
);

ALTER TABLE SYS_DICT ADD CONSTRAINT PK_SYSDICT_ID PRIMARY KEY (ID);
ALTER TABLE SYS_DICT ADD CONSTRAINT UNI_SYSDICT_DICTNO UNIQUE (DICTNO);
ALTER TABLE SYS_DICT ADD CONSTRAINT FK_PARENTID_SYSDICT_ID FOREIGN KEY (PARENTID)
    REFERENCES SYS_DICT (ID) ON DELETE RESTRICT ON UPDATE RESTRICT;
CREATE INDEX NON_SYSDICT_PARENTID ON SYS_DICT (PARENTID ASC);

COMMENT ON COLUMN SYS_DICT.ID IS '自定义主键(SD)';
COMMENT ON COLUMN SYS_DICT.DICTNO IS '字典编码';
COMMENT ON COLUMN SYS_DICT.DICTNAME IS '字典名称';
COMMENT ON COLUMN SYS_DICT.PARENTID IS '父级ID';
COMMENT ON COLUMN SYS_DICT.CREATEDUSER IS '创建人';
COMMENT ON COLUMN SYS_DICT.CREATEDAT IS '创建时间';
COMMENT ON COLUMN SYS_DICT.UPDATEDUSER IS '更新人';
COMMENT ON COLUMN SYS_DICT.UPDATEDAT IS '更新时间';
COMMENT ON COLUMN SYS_DICT.DELETEDUSER IS '删除人';
COMMENT ON COLUMN SYS_DICT.DELETEDAT IS '删除时间';
COMMENT ON COLUMN SYS_DICT.DELETED IS '是否删除(0否1是)';
COMMENT ON CONSTRAINT SYS_DICT.PK_SYSDICT_ID IS '主键约束';
COMMENT ON CONSTRAINT SYS_DICT.UNI_SYSDICT_DICTNO IS 'DICTNO唯一约束';
COMMENT ON CONSTRAINT SYS_DICT.FK_PARENTID_SYSDICT_ID IS 'PARENTID-SYS_DICT.ID外键约束';
COMMENT ON INDEX NON_SYSDICT_PARENTID IS 'PARENTID升序索引';
COMMENT ON TABLE SYS_DICT IS '系统_字典信息表';

-- 更新时间
CREATE OR REPLACE TRIGGER TR_SYS_DICT_UPDATEAT
AFTER UPDATE ON SYS_DICT
REFERENCING OLD AS OLDROW NEW AS NEWROW
FOR EACH ROW
BEGIN ATOMIC
  IF(OLDROW.UPDATEDAT = NEWROW.UPDATEDAT) THEN
    UPDATE SYS_DICT
    SET UPDATEDAT = CURRENT TIMESTAMP
    WHERE ID = NEWROW.ID;
  END IF;
END;

--自定义主键
CREATE OR REPLACE TRIGGER TR_SYS_DICT_ID
BEFORE INSERT ON SYS_DICT
REFERENCING NEW AS NEWROW
FOR EACH ROW
BEGIN ATOMIC
  IF (NEWROW.ID IS NULL) THEN
    SET NEWROW.ID = 'SD' || DIGITS(SEQ_SD.NEXTVAL);
  END IF;
END;

SELECT 'SD' || DIGITS(SEQ_SD.NEXTVAL) FROM SYSIBM.SYSDUMMY1;

-- 测试数据写入
INSERT INTO SYS_DICT(DICTNO, DICTNAME, PARENTID, CREATEDUSER, UPDATEDUSER)
VALUES('TEST', '测试字典', NULL, 'WANGCW','WANGCW');

SELECT * FROM SYS_DICT;
DELETE FROM SYS_DICT;

--
-- INSERT INTO SYS_DICT(DICTNO, DICTNAME, PARENTID, CREATEDUSER, UPDATEDUSER)
-- VALUES('TESTSUB', '测试字典子选项', 'SD0000000001', 'WANGCW','WANGCW');
--
-- UPDATE SYS_DICT SET DICTNO='TEST' WHERE ID='SD0000000001';
--
-- SELECT UPDATEDAT,* FROM SYS_DICT;

-- 基础信息_租户信息表
CREATE OR REPLACE SEQUENCE SEQ_ET
START WITH 1
INCREMENT BY 1
NO MAXVALUE
NO CYCLE
CACHE 10;

DROP TABLE IF EXISTS EADM_TENANT;
CREATE TABLE EADM_TENANT (
  ID CHAR(12) NOT NULL,
  TENANTNAME VARCHAR(20) NOT NULL,
  REMARK VARCHAR(100),
  ENABLE BOOLEAN NOT NULL DEFAULT TRUE,
  CREATEDUSER VARCHAR(50),
  CREATEDAT TIMESTAMP NOT NULL WITH DEFAULT CURRENT TIMESTAMP,
  UPDATEDUSER VARCHAR(50),
  UPDATEDAT TIMESTAMP NOT NULL WITH DEFAULT CURRENT TIMESTAMP,
  DELETEDUSER VARCHAR(50),
  DELETEDAT TIMESTAMP,
  DELETED BOOLEAN NOT NULL DEFAULT FALSE
);

ALTER TABLE EADM_TENANT ADD CONSTRAINT PK_TENANT_ID PRIMARY KEY (ID);
ALTER TABLE EADM_TENANT ADD CONSTRAINT UNI_TENANT_TENANTNAME UNIQUE (TENANTNAME);

CREATE INDEX NON_TENANT_ENABLE ON EADM_TENANT (ENABLE ASC);

COMMENT ON CONSTRAINT EADM_TENANT.PK_TENANT_ID IS '主键约束';
COMMENT ON CONSTRAINT EADM_TENANT.UNI_TENANT_TENANTNAME IS 'TENANTNAME唯一约束';
COMMENT ON INDEX NON_TENANT_ENABLE IS 'ENABLE升序索引';

COMMENT ON COLUMN EADM_TENANT.ID IS '自定义主键(ET)';
COMMENT ON COLUMN EADM_TENANT.TENANTNAME IS '租户名称';
COMMENT ON COLUMN EADM_TENANT.REMARK IS '备注信息';
COMMENT ON COLUMN EADM_TENANT.ENABLE IS '是否可用(0否1是)';
COMMENT ON COLUMN EADM_TENANT.CREATEDUSER IS '创建人';
COMMENT ON COLUMN EADM_TENANT.CREATEDAT IS '创建时间';
COMMENT ON COLUMN EADM_TENANT.UPDATEDUSER IS '更新人';
COMMENT ON COLUMN EADM_TENANT.UPDATEDAT IS '更新时间';
COMMENT ON COLUMN EADM_TENANT.DELETEDUSER IS '删除人';
COMMENT ON COLUMN EADM_TENANT.DELETEDAT IS '删除时间';
COMMENT ON COLUMN EADM_TENANT.DELETED IS '是否删除(0否1是)';
COMMENT ON TABLE EADM_TENANT IS '基础信息_租户信息表';

-- 更新时间
CREATE OR REPLACE TRIGGER TR_EADM_TENANT_UPDATEAT
AFTER UPDATE ON EADM_TENANT
REFERENCING OLD AS OLDROW NEW AS NEWROW
FOR EACH ROW
BEGIN ATOMIC
  IF(OLDROW.UPDATEDAT = NEWROW.UPDATEDAT) THEN
    UPDATE EADM_TENANT
    SET UPDATEDAT = CURRENT TIMESTAMP
    WHERE ID = NEWROW.ID;
  END IF;
END;

-- 自定义主键
CREATE OR REPLACE TRIGGER TR_EADM_TENANT_ID
BEFORE INSERT ON EADM_TENANT
REFERENCING NEW AS NEWROW
FOR EACH ROW
BEGIN ATOMIC
  IF (NEWROW.ID IS NULL) THEN
    SET NEWROW.ID = ('ET' || DIGITS(SEQ_ET.NEXTVAL));
  END IF;
END;

--写入数据
TRUNCATE TABLE EADM_TENANT IMMEDIATE;

INSERT INTO EADM_TENANT(TENANTNAME, REMARK, CREATEDUSER)
VALUES('REDGREAT', '主租户', 'WANGCW');

INSERT INTO EADM_TENANT(TENANTNAME, REMARK, CREATEDUSER)
VALUES('管理客户', '手动添加客户', 'WANGCW');

INSERT INTO EADM_TENANT(TENANTNAME, REMARK, CREATEDUSER)
VALUES('注册客户', '界面注册客户', 'WANGCW');

-- SELECT * FROM EADM_TENANT;

-- 获取商户名称
CREATE OR REPLACE FUNCTION FN_GETTENANTNAME(INID CHAR(12))
LANGUAGE SQL
RETURNS VARCHAR(100)
DETERMINISTIC NO EXTERNAL ACTION READS SQL DATA
BEGIN
    RETURN (SELECT TENANTNAME FROM EADM_TENANT WHERE ID=INID AND ENABLE IS TRUE AND DELETED IS FALSE LIMIT 1);
END;

-- SELECT FN_GETTENANTNAME('ET0000000005') FROM sysibm.sysdummy1;

-- 用户表
-- 创建序列
CREATE OR REPLACE SEQUENCE SEQ_EU
START WITH 1
INCREMENT BY 1
NO MAXVALUE
NO CYCLE
CACHE 10;

DROP TABLE IF EXISTS EADM_USER;
CREATE TABLE EADM_USER (
  ID CHAR(12) NOT NULL,
  TENANTID CHAR(12) NOT NULL,
  LOGINNAME VARCHAR(50) NOT NULL,
  USERNAME VARCHAR(50) NOT NULL,
  EMAIL VARCHAR(20),
  PASSWD VARCHAR(50) NOT NULL,
  USERSTATUS SMALLINT NOT NULL DEFAULT 0,
  CREATEDUSER VARCHAR(50),
  CREATEDAT TIMESTAMP NOT NULL WITH DEFAULT CURRENT TIMESTAMP,
  UPDATEDUSER VARCHAR(50),
  UPDATEDAT TIMESTAMP NOT NULL WITH DEFAULT CURRENT TIMESTAMP,
  DELETEDUSER VARCHAR(50),
  DELETEDAT TIMESTAMP,
  DELETED BOOLEAN NOT NULL DEFAULT FALSE
);

ALTER TABLE EADM_USER ADD CONSTRAINT PK_USER_ID PRIMARY KEY (ID);

ALTER TABLE EADM_USER ADD CONSTRAINT FK_TENANTID_TENANT_ID FOREIGN KEY (TENANTID)
    REFERENCES EADM_TENANT (ID) ON DELETE RESTRICT ON UPDATE RESTRICT;

ALTER TABLE EADM_USER ADD CONSTRAINT UNI_USER_LOGINNAME UNIQUE (LOGINNAME);

CREATE INDEX NON_USER_USERSTATUS ON EADM_USER (USERSTATUS ASC);
CREATE INDEX NON_USER_UPDATEDAT ON EADM_USER (UPDATEDAT DESC);

COMMENT ON CONSTRAINT EADM_USER.PK_USER_ID IS '主键约束';
COMMENT ON CONSTRAINT EADM_USER.FK_TENANTID_TENANT_ID IS 'TENANTID_TENANT_ID外键约束';
COMMENT ON CONSTRAINT EADM_USER.UNI_USER_LOGINNAME IS 'LOGINNAME唯一约束';
COMMENT ON INDEX NON_USER_USERSTATUS IS 'USERSTATUS升序索引';
COMMENT ON INDEX NON_USER_UPDATEDAT IS 'UPDATEDAT升序索引';

COMMENT ON COLUMN EADM_USER.ID IS '自定义主键(EU)';
COMMENT ON COLUMN EADM_USER.TENANTID IS '租户ID';
COMMENT ON COLUMN EADM_USER.LOGINNAME IS '用户登录名';
COMMENT ON COLUMN EADM_USER.USERNAME IS '用户姓名';
COMMENT ON COLUMN EADM_USER.EMAIL IS '用户邮件';
COMMENT ON COLUMN EADM_USER.PASSWD IS '密码';
COMMENT ON COLUMN EADM_USER.USERSTATUS IS '用户状态(0启用1禁用)';
COMMENT ON COLUMN EADM_USER.CREATEDUSER IS '创建人';
COMMENT ON COLUMN EADM_USER.CREATEDAT IS '创建时间';
COMMENT ON COLUMN EADM_USER.UPDATEDUSER IS '更新人';
COMMENT ON COLUMN EADM_USER.UPDATEDAT IS '更新时间';
COMMENT ON COLUMN EADM_USER.DELETEDUSER IS '删除人';
COMMENT ON COLUMN EADM_USER.DELETEDAT IS '删除时间';
COMMENT ON COLUMN EADM_USER.DELETED IS '是否删除(0否1是)';
COMMENT ON TABLE EADM_USER IS '基础信息_用户信息表';

-- 更新时间
CREATE OR REPLACE TRIGGER TR_EADM_USER_UPDATEAT
AFTER UPDATE ON EADM_USER
REFERENCING OLD AS OLDROW NEW AS NEWROW
FOR EACH ROW
BEGIN ATOMIC
  IF(OLDROW.UPDATEDAT = NEWROW.UPDATEDAT) THEN
    UPDATE EADM_USER
    SET UPDATEDAT = CURRENT TIMESTAMP
    WHERE ID = NEWROW.ID;
  END IF;
END;

-- 自定义主键
CREATE OR REPLACE TRIGGER TR_EADM_USER_ID
BEFORE INSERT ON EADM_USER
REFERENCING NEW AS NEWROW
FOR EACH ROW
BEGIN ATOMIC
  IF (NEWROW.ID IS NULL) THEN
    SET NEWROW.ID = ('EU' || DIGITS(SEQ_EU.NEXTVAL));
  END IF;
END;

-- 写入用户数据

TRUNCATE TABLE EADM_USER IMMEDIATE;

-- SELECT * FROM EADM_TENANT;
-- SELECT * FROM EADM_USER;

INSERT INTO EADM_USER(TENANTID, LOGINNAME, USERNAME, EMAIL, PASSWD, CREATEDUSER)
VALUES('ET0000000001','WANGCW', '王存伟', 'RUBYGREAT@MSN.COM', 'Q122/4GBPICNQ83ABPQN/+KYQ0KWCZLXIWFLALKK4NY=', 'WANGCW');

INSERT INTO EADM_USER(TENANTID, LOGINNAME, USERNAME, EMAIL, PASSWD, CREATEDUSER)
VALUES('ET0000000001','WONGCW', '王存偉', 'RUBYGREAT@MSN.COM', 'Q122/4GBPICNQ83ABPQN/+KYQ0KWCZLXIWFLALKK4NY=', 'WANGCW');

INSERT INTO EADM_USER(TENANTID, LOGINNAME, USERNAME, EMAIL, PASSWD, CREATEDUSER)
VALUES('ET0000000001','JIANGYF', '姜玉凤', '1234567@QQ.COM', 'Q122/4GBPICNQ83ABPQN/+KYQ0KWCZLXIWFLALKK4NY=', 'WANGCW');

-- 用户视图
CREATE OR REPLACE VIEW VI_USER
AS
SELECT ID, FN_GETTENANTNAME(TENANTID) AS TENANTNAME, LOGINNAME, USERNAME, EMAIL,
       CASE USERSTATUS WHEN 1 THEN '禁用' WHEN 0 THEN '启用' END AS USERSTATUS, CREATEDAT
FROM EADM_USER
  WHERE DELETED IS FALSE;

-- SELECT * FROM VI_USER;

-- 用户角色
-- 创建序列
CREATE OR REPLACE SEQUENCE SEQ_ER
START WITH 1
INCREMENT BY 1
NO MAXVALUE
NO CYCLE
CACHE 10;

DROP TABLE IF EXISTS EADM_ROLE;
CREATE TABLE EADM_ROLE (
  ID CHAR(12) NOT NULL,
  ROLENAME VARCHAR(50) NOT NULL,
  ROLEPERMISSION CLOB,
  ROLESTATUS SMALLINT NOT NULL DEFAULT 0,
  CREATEDUSER VARCHAR(50),
  CREATEDAT TIMESTAMP NOT NULL WITH DEFAULT CURRENT TIMESTAMP,
  UPDATEDUSER VARCHAR(50),
  UPDATEDAT TIMESTAMP NOT NULL WITH DEFAULT CURRENT TIMESTAMP,
  DELETEDUSER VARCHAR(50),
  DELETEDAT TIMESTAMP,
  DELETED BOOLEAN NOT NULL DEFAULT FALSE
);

ALTER TABLE EADM_ROLE ADD CONSTRAINT PK_ROLE_ID PRIMARY KEY (ID);

CREATE INDEX NON_ROLE_ROLENAME ON EADM_ROLE (ROLENAME ASC);
CREATE INDEX NON_USER_ROLESTATUS ON EADM_ROLE (ROLESTATUS ASC);

COMMENT ON CONSTRAINT EADM_ROLE.PK_ROLE_ID IS '主键约束';
COMMENT ON INDEX NON_ROLE_ROLENAME IS 'ROLENAME升序索引';
COMMENT ON INDEX NON_USER_ROLESTATUS IS 'ROLESTATUS升序索引';

COMMENT ON COLUMN EADM_ROLE.ID IS '自定义主键(ER)';
COMMENT ON COLUMN EADM_ROLE.ROLENAME IS '角色名称';
COMMENT ON COLUMN EADM_ROLE.ROLEPERMISSION IS '角色权限';
COMMENT ON COLUMN EADM_ROLE.ROLESTATUS IS '角色状态(0启用1禁用)';
COMMENT ON COLUMN EADM_ROLE.CREATEDUSER IS '创建人';
COMMENT ON COLUMN EADM_ROLE.CREATEDAT IS '创建时间';
COMMENT ON COLUMN EADM_ROLE.UPDATEDUSER IS '更新人';
COMMENT ON COLUMN EADM_ROLE.UPDATEDAT IS '更新时间';
COMMENT ON COLUMN EADM_ROLE.DELETEDUSER IS '删除人';
COMMENT ON COLUMN EADM_ROLE.DELETEDAT IS '删除时间';
COMMENT ON COLUMN EADM_ROLE.DELETED IS '是否删除(0否1是)';
COMMENT ON TABLE EADM_ROLE IS '基础信息_角色信息表';

-- 更新时间
CREATE OR REPLACE TRIGGER TR_EADM_ROLE_UPDATEAT
AFTER UPDATE ON EADM_ROLE
REFERENCING OLD AS OLDROW NEW AS NEWROW
FOR EACH ROW
BEGIN ATOMIC
  IF(OLDROW.UPDATEDAT = NEWROW.UPDATEDAT) THEN
    UPDATE EADM_ROLE
    SET UPDATEDAT = CURRENT TIMESTAMP
    WHERE ID = NEWROW.ID;
  END IF;
END;

-- 自定义主键
CREATE OR REPLACE TRIGGER TR_EADM_ROLE_ID
BEFORE INSERT ON EADM_ROLE
REFERENCING NEW AS NEWROW
FOR EACH ROW
BEGIN ATOMIC
  IF (NEWROW.ID IS NULL) THEN
    SET NEWROW.ID = ('ER' || DIGITS(SEQ_ER.NEXTVAL));
  END IF;
END;

-- 写入数据
TRUNCATE TABLE EADM_ROLE IMMEDIATE;

INSERT INTO EADM_ROLE(ROLENAME, ROLEPERMISSION, CREATEDUSER)
VALUES('超级管理员', '{}', 'WANGCW');

INSERT INTO EADM_ROLE(ROLENAME, ROLEPERMISSION, CREATEDUSER)
VALUES('注册租户', '{}', 'WANGCW');

INSERT INTO EADM_ROLE(ROLENAME, ROLEPERMISSION, CREATEDUSER)
VALUES('分配租户', '{}', 'WANGCW');

-- 角色视图
CREATE OR REPLACE VIEW VI_ROLE
AS
SELECT ID, ROLENAME, ROLEPERMISSION, CASE ROLESTATUS WHEN 1 THEN '禁用' WHEN 0 THEN '启用' END AS ROLESTATUS, CREATEDAT
FROM EADM_ROLE
  WHERE DELETED IS FALSE;

-- SELECT * FROM EADM_ROLE;
-- SELECT * FROM VI_ROLE;

-- 用户角色对应关系
DROP TABLE IF EXISTS EADM_USERROLE;
CREATE TABLE EADM_USERROLE (
  ID INT GENERATED ALWAYS AS IDENTITY,
  USERID CHAR(12) NOT NULL,
  ROLEID CHAR(12) NOT NULL,
  CREATEDUSER VARCHAR(50),
  CREATEDAT TIMESTAMP NOT NULL WITH DEFAULT CURRENT TIMESTAMP,
  UPDATEDUSER VARCHAR(50),
  UPDATEDAT TIMESTAMP NOT NULL WITH DEFAULT CURRENT TIMESTAMP,
  DELETEDUSER VARCHAR(50),
  DELETEDAT TIMESTAMP,
  DELETED BOOLEAN NOT NULL DEFAULT FALSE
);

ALTER TABLE EADM_USERROLE ADD CONSTRAINT PK_USERROLE_ID PRIMARY KEY (ID);

ALTER TABLE EADM_USERROLE ADD CONSTRAINT FK_USERID_USER_ID FOREIGN KEY (USERID)
    REFERENCES EADM_USER (ID) ON DELETE RESTRICT ON UPDATE RESTRICT;
ALTER TABLE EADM_USERROLE ADD CONSTRAINT FK_ROLEID_ROLE_ID FOREIGN KEY (ROLEID)
    REFERENCES EADM_ROLE (ID) ON DELETE RESTRICT ON UPDATE RESTRICT;

COMMENT ON CONSTRAINT EADM_USERROLE.PK_USERROLE_ID IS '主键约束';
COMMENT ON CONSTRAINT EADM_USERROLE.FK_USERID_USER_ID IS 'USERID_USER_ID外键约束';
COMMENT ON CONSTRAINT EADM_USERROLE.FK_ROLEID_ROLE_ID IS 'ROLEID_ROLE_ID外键约束';

COMMENT ON COLUMN EADM_USERROLE.ID IS '自增主键';
COMMENT ON COLUMN EADM_USERROLE.USERID IS '用户ID(EADM_USER.ID)';
COMMENT ON COLUMN EADM_USERROLE.ROLEID IS '角色ID(EADM_ROLE.ID)';
COMMENT ON COLUMN EADM_USERROLE.CREATEDUSER IS '创建人';
COMMENT ON COLUMN EADM_USERROLE.CREATEDAT IS '创建时间';
COMMENT ON COLUMN EADM_USERROLE.UPDATEDUSER IS '更新人';
COMMENT ON COLUMN EADM_USERROLE.UPDATEDAT IS '更新时间';
COMMENT ON COLUMN EADM_USERROLE.DELETEDUSER IS '删除人';
COMMENT ON COLUMN EADM_USERROLE.DELETEDAT IS '删除时间';
COMMENT ON COLUMN EADM_USERROLE.DELETED IS '是否删除(0否1是)';
COMMENT ON TABLE EADM_USERROLE IS '基础信息_用户角色对应关系表';

-- 更新时间
CREATE OR REPLACE TRIGGER TR_EADM_USERROLE_UPDATEAT
AFTER UPDATE ON EADM_USERROLE
REFERENCING OLD AS OLDROW NEW AS NEWROW
FOR EACH ROW
BEGIN ATOMIC
  IF(OLDROW.UPDATEDAT = NEWROW.UPDATEDAT) THEN
    UPDATE EADM_USERROLE
    SET UPDATEDAT = CURRENT TIMESTAMP
    WHERE ID = NEWROW.ID;
  END IF;
END;

-- 写入数据
-- SELECT * FROM EADM_USER;
-- SELECT * FROM EADM_ROLE;

INSERT INTO EADM_USERROLE(USERID, ROLEID, CREATEDUSER)
VALUES('EU0000000001', 'ER0000000001', 'WANGCW');

-- 用户角色信息视图
CREATE OR REPLACE VIEW VI_USERROLE
AS
SELECT B.ID, A.ID AS USERID, C.ID AS ROLEID, C.ROLENAME, B.UPDATEDAT
FROM EADM_USER A
INNER JOIN EADM_USERROLE B
  ON B.USERID=A.ID
  AND B.DELETED IS FALSE
INNER JOIN EADM_ROLE C
  ON C.ID=B.ROLEID
  AND C.ROLESTATUS=0
  AND C.DELETED IS FALSE
WHERE A.DELETED IS FALSE;

-- 用户权限信息视图
CREATE OR REPLACE VIEW VI_USERPERMISSION
AS
SELECT B.ID, A.LOGINNAME, C.ROLEPERMISSION
FROM EADM_USER A
INNER JOIN EADM_USERROLE B
  ON B.USERID=A.ID
  AND B.DELETED IS FALSE
INNER JOIN EADM_ROLE C
  ON C.ID=B.ROLEID
  AND C.ROLESTATUS=0
  AND C.DELETED IS FALSE
WHERE A.DELETED IS FALSE;

-- 定时任务信息
-- 创建序列
CREATE OR REPLACE SEQUENCE SEQ_CR
START WITH 1
INCREMENT BY 1
NO MAXVALUE
NO CYCLE
CACHE 10;

DROP TABLE IF EXISTS EADM_CRONTAB;
CREATE TABLE EADM_CRONTAB (
  ID CHAR(12) NOT NULL,
  CRONNAME VARCHAR(50) NOT NULL,
  CRONEXP VARCHAR(50),
  CRONMFA VARCHAR(50),
  STARTTIME TIMESTAMP,
  ENDTIME TIMESTAMP,
  CRONSTATUS SMALLINT NOT NULL DEFAULT 0,
  CREATEDUSER VARCHAR(50),
  CREATEDAT TIMESTAMP NOT NULL WITH DEFAULT CURRENT TIMESTAMP,
  UPDATEDUSER VARCHAR(50),
  UPDATEDAT TIMESTAMP NOT NULL WITH DEFAULT CURRENT TIMESTAMP,
  DELETEDUSER VARCHAR(50),
  DELETEDAT TIMESTAMP,
  DELETED BOOLEAN NOT NULL DEFAULT FALSE
);

ALTER TABLE EADM_CRONTAB ADD CONSTRAINT PK_CRONTAB_ID PRIMARY KEY (ID);
CREATE INDEX NON_CRONTAB_CRONNAME ON EADM_CRONTAB (CRONNAME ASC);

COMMENT ON CONSTRAINT EADM_CRONTAB.PK_CRONTAB_ID IS '主键约束';
COMMENT ON INDEX NON_CRONTAB_CRONNAME IS 'CRONNAME升序索引';

COMMENT ON COLUMN EADM_CRONTAB.ID IS '自定义主键(CR)';
COMMENT ON COLUMN EADM_CRONTAB.CRONNAME IS '任务名称';
COMMENT ON COLUMN EADM_CRONTAB.CRONEXP IS '定时表达式';
COMMENT ON COLUMN EADM_CRONTAB.CRONMFA IS '任务备注';
COMMENT ON COLUMN EADM_CRONTAB.STARTTIME IS '任务开始时间';
COMMENT ON COLUMN EADM_CRONTAB.ENDTIME IS '任务结束时间';
COMMENT ON COLUMN EADM_CRONTAB.CRONSTATUS IS '任务状态(0启用1禁用)';
COMMENT ON COLUMN EADM_CRONTAB.CREATEDUSER IS '创建人';
COMMENT ON COLUMN EADM_CRONTAB.CREATEDAT IS '创建时间';
COMMENT ON COLUMN EADM_CRONTAB.UPDATEDUSER IS '更新人';
COMMENT ON COLUMN EADM_CRONTAB.UPDATEDAT IS '更新时间';
COMMENT ON COLUMN EADM_CRONTAB.DELETEDUSER IS '删除人';
COMMENT ON COLUMN EADM_CRONTAB.DELETEDAT IS '删除时间';
COMMENT ON COLUMN EADM_CRONTAB.DELETED IS '是否删除(0否1是)';
COMMENT ON TABLE EADM_CRONTAB IS '基础信息_定时任务信息表';

-- 更新时间
CREATE OR REPLACE TRIGGER TR_EADM_CRONTAB_UPDATEAT
AFTER UPDATE ON EADM_CRONTAB
REFERENCING OLD AS OLDROW NEW AS NEWROW
FOR EACH ROW
BEGIN ATOMIC
  IF(OLDROW.UPDATEDAT = NEWROW.UPDATEDAT) THEN
    UPDATE EADM_CRONTAB
    SET UPDATEDAT = CURRENT TIMESTAMP
    WHERE ID = NEWROW.ID;
  END IF;
END;

-- 自定义主键
CREATE OR REPLACE TRIGGER TR_EADM_CRONTAB_ID
BEFORE INSERT ON EADM_CRONTAB
REFERENCING NEW AS NEWROW
FOR EACH ROW
BEGIN ATOMIC
  IF (NEWROW.ID IS NULL) THEN
    SET NEWROW.ID = ('CR' || DIGITS(SEQ_CR.NEXTVAL));
  END IF;
END;

-- 首页报表
DROP TABLE IF EXISTS EADM_DASHBOARD;
CREATE TABLE EADM_DASHBOARD(
    ID INT GENERATED ALWAYS AS IDENTITY,
    DATATYPE SMALLINT NOT NULL,
    DATETYPE SMALLINT NOT NULL,
    LOGINNAME VARCHAR(50) NOT NULL,
    DATAVALUE VARCHAR(500),
    DATAJSON CLOB,
    CHECKDATE VARCHAR(20) NOT NULL,
    UPDATEDAT TIMESTAMP NOT NULL WITH DEFAULT CURRENT TIMESTAMP,
    INSTERTIME TIMESTAMP NOT NULL WITH DEFAULT CURRENT TIMESTAMP
);

ALTER TABLE EADM_DASHBOARD ADD CONSTRAINT PK_DASHBOARD_ID PRIMARY KEY (ID);

ALTER TABLE EADM_DASHBOARD ADD CONSTRAINT UNI_DASHBOARD_DDLC
    UNIQUE (DATATYPE, DATETYPE, LOGINNAME, CHECKDATE);

CREATE INDEX NON_DASHBOARD_DATATYPE ON EADM_DASHBOARD (DATATYPE ASC);
CREATE INDEX NON_DASHBOARD_DATETYPE ON EADM_DASHBOARD (DATETYPE ASC);
CREATE INDEX NON_DASHBOARD_LOGINNAME ON EADM_DASHBOARD (LOGINNAME ASC);
CREATE INDEX NON_DASHBOARD_CHECKDATE ON EADM_DASHBOARD (CHECKDATE ASC);

COMMENT ON CONSTRAINT EADM_DASHBOARD.PK_DASHBOARD_ID IS '主键约束';
COMMENT ON CONSTRAINT EADM_DASHBOARD.UNI_DASHBOARD_DDLC IS 'DDLC唯一约束';
COMMENT ON INDEX NON_DASHBOARD_DATATYPE IS 'DATATYPE升序索引';
COMMENT ON INDEX NON_DASHBOARD_DATETYPE IS 'DATETYPE升序索引';
COMMENT ON INDEX NON_DASHBOARD_LOGINNAME IS 'LOGINNAME升序索引';
COMMENT ON INDEX NON_DASHBOARD_CHECKDATE IS 'CHECKDATE升序索引';

COMMENT ON COLUMN EADM_DASHBOARD.ID IS '自增主键';
COMMENT ON COLUMN EADM_DASHBOARD.DATATYPE IS '数据类型(1心率2步数3睡眠4里程5每月里程6每月收入7每月支出)';
COMMENT ON COLUMN EADM_DASHBOARD.DATETYPE IS '统计周期类型(1日2周3月4年)';
COMMENT ON COLUMN EADM_DASHBOARD.LOGINNAME IS '登录名';
COMMENT ON COLUMN EADM_DASHBOARD.DATAVALUE IS '数据值';
COMMENT ON COLUMN EADM_DASHBOARD.DATAJSON IS '数据JSON';
COMMENT ON COLUMN EADM_DASHBOARD.CHECKDATE IS '数据日期';
COMMENT ON COLUMN EADM_DASHBOARD.UPDATEDAT IS '更新时间';
COMMENT ON COLUMN EADM_DASHBOARD.INSTERTIME IS '插入时间';
COMMENT ON TABLE EADM_DASHBOARD IS '首页_看板报表';

-- 更新时间
CREATE OR REPLACE TRIGGER TR_EADM_DASHBOARD_UPDATEAT
AFTER UPDATE ON EADM_DASHBOARD
REFERENCING OLD AS OLDROW NEW AS NEWROW
FOR EACH ROW
BEGIN ATOMIC
  IF(OLDROW.UPDATEDAT = NEWROW.UPDATEDAT) THEN
    UPDATE EADM_DASHBOARD
    SET UPDATEDAT = CURRENT TIMESTAMP
    WHERE ID = NEWROW.ID;
  END IF;
END;

-- 过程运行日志
DROP TABLE IF EXISTS SYS_PROCLOG;
CREATE TABLE SYS_PROCLOG (
  ID INT GENERATED ALWAYS AS IDENTITY,
  PROCNAME VARCHAR(50),
  TIMESPAN INT,
  RESULT BOOLEAN NOT NULL DEFAULT TRUE,
  ERRCODE VARCHAR(5),
  ERRMESSAGE VARCHAR(5000),
  INSERTTIME TIMESTAMP NOT NULL WITH DEFAULT CURRENT TIMESTAMP
);

ALTER TABLE SYS_PROCLOG ADD CONSTRAINT PK_PROCLOG_ID PRIMARY KEY (ID);

CREATE INDEX NON_PROCLOG_PROCNAME ON SYS_PROCLOG (PROCNAME ASC);
CREATE INDEX NON_PROCLOG_INSERTTIME ON SYS_PROCLOG (INSERTTIME ASC);

COMMENT ON CONSTRAINT SYS_PROCLOG.PK_PROCLOG_ID IS '主键约束';
COMMENT ON INDEX NON_PROCLOG_PROCNAME IS 'PROCNAME升序索引';
COMMENT ON INDEX NON_PROCLOG_INSERTTIME IS 'PROCNAME升序索引';

COMMENT ON COLUMN SYS_PROCLOG.ID IS '自增主键';
COMMENT ON COLUMN SYS_PROCLOG.PROCNAME IS '过程名';
COMMENT ON COLUMN SYS_PROCLOG.TIMESPAN IS '耗时时长(秒)';
COMMENT ON COLUMN SYS_PROCLOG.RESULT IS '是否成功(0否1是)';
COMMENT ON COLUMN SYS_PROCLOG.ERRCODE IS '错误代码';
COMMENT ON COLUMN SYS_PROCLOG.ERRMESSAGE IS '错误详细信息';
COMMENT ON COLUMN SYS_PROCLOG.INSERTTIME IS '日志记录时间';
COMMENT ON TABLE SYS_PROCLOG IS '系统域_过程执行日志';

-- 设备信息
DROP TABLE IF EXISTS EADM_DEVICE;
CREATE TABLE EADM_DEVICE (
  DEVICENO VARCHAR(50) NOT NULL,
  IMEI VARCHAR(50),
  SIMNO VARCHAR(50),
  ENABLE BOOLEAN NOT NULL DEFAULT TRUE,
  REMARK VARCHAR(200),
  CREATEDUSER VARCHAR(50),
  CREATEDAT TIMESTAMP NOT NULL WITH DEFAULT CURRENT TIMESTAMP,
  UPDATEDUSER VARCHAR(50),
  UPDATEDAT TIMESTAMP NOT NULL WITH DEFAULT CURRENT TIMESTAMP,
  DELETEDUSER VARCHAR(50),
  DELETEDAT TIMESTAMP,
  DELETED BOOLEAN NOT NULL DEFAULT FALSE
);

ALTER TABLE EADM_DEVICE ADD CONSTRAINT PK_DEVICE_DEVICENO PRIMARY KEY (DEVICENO);

CREATE INDEX NON_DEVICE_SIMNO ON EADM_DEVICE (SIMNO ASC);

COMMENT ON CONSTRAINT EADM_DEVICE.PK_DEVICE_DEVICENO IS '主键约束';
COMMENT ON INDEX NON_DEVICE_SIMNO IS 'SIMNO升序索引';

COMMENT ON COLUMN EADM_DEVICE.DEVICENO IS '设备号(主键)';
COMMENT ON COLUMN EADM_DEVICE.IMEI IS '设备IMEI';
COMMENT ON COLUMN EADM_DEVICE.SIMNO IS 'SIM卡号';
COMMENT ON COLUMN EADM_DEVICE.ENABLE IS '设备状态(1启用0禁用)';
COMMENT ON COLUMN EADM_DEVICE.REMARK IS '设备描述';
COMMENT ON COLUMN EADM_DEVICE.CREATEDUSER IS '创建人';
COMMENT ON COLUMN EADM_DEVICE.CREATEDAT IS '创建时间';
COMMENT ON COLUMN EADM_DEVICE.UPDATEDUSER IS '更新人';
COMMENT ON COLUMN EADM_DEVICE.UPDATEDAT IS '更新时间';
COMMENT ON COLUMN EADM_DEVICE.DELETEDUSER IS '删除人';
COMMENT ON COLUMN EADM_DEVICE.DELETEDAT IS '删除时间';
COMMENT ON COLUMN EADM_DEVICE.DELETED IS '是否删除(0否1是)';
COMMENT ON TABLE EADM_DEVICE IS '业务域_设备信息';

-- 更新时间
CREATE OR REPLACE TRIGGER TR_EADM_DEVICE_UPDATEAT
AFTER UPDATE ON EADM_DEVICE
REFERENCING OLD AS OLDROW NEW AS NEWROW
FOR EACH ROW
BEGIN ATOMIC
  IF(NEWROW.UPDATEDAT != NEWROW.UPDATEDAT) THEN
    UPDATE EADM_DEVICE
    SET UPDATEDAT = CURRENT TIMESTAMP
    WHERE DEVICENO = NEWROW.DEVICENO;
  END IF;
END;

-- 写入数据
INSERT INTO EADM_DEVICE(DEVICENO, REMARK, CREATEDUSER)
VALUES('16053489111', '充电宝', 'WANGCW'),
      ('868977061978771', '手表', 'WANGCW');

-- SELECT * FROM EADM_DEVICE;

-- 人员设备对应关系
DROP TABLE IF EXISTS EADM_USERDEVICE;
CREATE TABLE EADM_USERDEVICE (
  ID INT GENERATED ALWAYS AS IDENTITY,
  USERID CHAR(12) NOT NULL,
  LOGINNAME VARCHAR(50) NOT NULL,
  DEVICENO VARCHAR(50) NOT NULL,
  CREATEDUSER VARCHAR(50),
  CREATEDAT TIMESTAMP NOT NULL WITH DEFAULT CURRENT TIMESTAMP,
  UPDATEDUSER VARCHAR(50),
  UPDATEDAT TIMESTAMP NOT NULL WITH DEFAULT CURRENT TIMESTAMP,
  DELETEDUSER VARCHAR(50),
  DELETEDAT TIMESTAMP,
  DELETED BOOLEAN NOT NULL DEFAULT FALSE
);

ALTER TABLE EADM_USERDEVICE ADD CONSTRAINT PK_USERDEVICE_ID PRIMARY KEY (ID);

ALTER TABLE EADM_USERDEVICE ADD CONSTRAINT FK_USERID_USER_ID FOREIGN KEY (USERID)
    REFERENCES EADM_USER (ID) ON DELETE RESTRICT ON UPDATE RESTRICT;
ALTER TABLE EADM_USERDEVICE ADD CONSTRAINT FK_LOGINNAME_USER_LOGINNAME FOREIGN KEY (LOGINNAME)
    REFERENCES EADM_USER (LOGINNAME) ON DELETE RESTRICT ON UPDATE RESTRICT;
ALTER TABLE EADM_USERDEVICE ADD CONSTRAINT FK_DEVICENO_DEVICE_DEVICENO FOREIGN KEY (DEVICENO)
    REFERENCES EADM_DEVICE (DEVICENO) ON DELETE RESTRICT ON UPDATE RESTRICT;

COMMENT ON CONSTRAINT EADM_USERDEVICE.PK_USERDEVICE_ID IS '主键约束';
COMMENT ON CONSTRAINT EADM_USERDEVICE.FK_USERID_USER_ID IS 'USERID_USER_ID外键约束';
COMMENT ON CONSTRAINT EADM_USERDEVICE.FK_LOGINNAME_USER_LOGINNAME IS 'LOGINNAME_USER_LOGINNAME外键约束';
COMMENT ON CONSTRAINT EADM_USERDEVICE.FK_DEVICENO_DEVICE_DEVICENO IS 'DEVICENO_DEVICE_DEVICENO外键约束';

COMMENT ON COLUMN EADM_USERDEVICE.ID IS '自增主键';
COMMENT ON COLUMN EADM_USERDEVICE.USERID IS '用户ID(EADM_USER.ID)';
COMMENT ON COLUMN EADM_USERDEVICE.LOGINNAME IS '用户登录名(EADM_USER.LOGINNAME)';
COMMENT ON COLUMN EADM_USERDEVICE.DEVICENO IS '设备ID(EADM_DEVICE.DEVICENO)';
COMMENT ON COLUMN EADM_USERDEVICE.CREATEDUSER IS '创建人';
COMMENT ON COLUMN EADM_USERDEVICE.CREATEDAT IS '创建时间';
COMMENT ON COLUMN EADM_USERDEVICE.UPDATEDUSER IS '更新人';
COMMENT ON COLUMN EADM_USERDEVICE.UPDATEDAT IS '更新时间';
COMMENT ON COLUMN EADM_USERDEVICE.DELETEDUSER IS '删除人';
COMMENT ON COLUMN EADM_USERDEVICE.DELETEDAT IS '删除时间';
COMMENT ON COLUMN EADM_USERDEVICE.DELETED IS '是否删除(0否1是)';
COMMENT ON TABLE EADM_USERDEVICE IS '业务域_人员设备对应关系';

-- 更新时间
CREATE OR REPLACE TRIGGER TR_EADM_USERDEVICE_UPDATEAT
AFTER UPDATE ON EADM_USERDEVICE
REFERENCING OLD AS OLDROW NEW AS NEWROW
FOR EACH ROW
BEGIN ATOMIC
  IF(OLDROW.UPDATEDAT = NEWROW.UPDATEDAT) THEN
    UPDATE EADM_USERDEVICE
    SET UPDATEDAT = CURRENT TIMESTAMP
    WHERE ID = NEWROW.ID;
  END IF;
END;

-- 写入数据
INSERT INTO EADM_USERDEVICE(USERID, LOGINNAME, DEVICENO, CREATEDUSER)
SELECT 'EU0000000001', 'WANGCW', DEVICENO, 'WANGCW'
FROM EADM_DEVICE;

-- SELECT * FROM EADM_USERDEVICE;

-- 业务数据_车辆定位信息
DROP TABLE IF EXISTS LC_CARLOCDAILY;
CREATE TABLE LC_CARLOCDAILY (
  PTIME TIMESTAMP NOT NULL,
  DEVICENO VARCHAR(20),
  LAT NUMERIC(9,6),
  LNG NUMERIC(9,6),
  DIRCT INT,
  SPEED INT,
  MILEAGE NUMERIC(18,2),
  HIGHT INT,
  GNSSNUM INT,
  RSSI INT,
  RECEIVETIME TIMESTAMP,
  INSERTTIME TIMESTAMP NOT NULL WITH DEFAULT CURRENT TIMESTAMP
);
;
ALTER TABLE LC_CARLOCDAILY ADD CONSTRAINT PK_CARLOCDAILY_PTIME PRIMARY KEY (PTIME);

CREATE INDEX NON_CARLOCDAILY_DEVICENO ON LC_CARLOCDAILY (DEVICENO ASC);

COMMENT ON CONSTRAINT LC_CARLOCDAILY.PK_CARLOCDAILY_PTIME IS '主键约束';
COMMENT ON INDEX NON_CARLOCDAILY_DEVICENO IS 'DEVICENO升序索引';

COMMENT ON COLUMN LC_CARLOCDAILY.PTIME IS '设备上传时间(主键)';
COMMENT ON COLUMN LC_CARLOCDAILY.DEVICENO IS '设备编码(ICCID)';
COMMENT ON COLUMN LC_CARLOCDAILY.LAT IS '经度';
COMMENT ON COLUMN LC_CARLOCDAILY.LNG IS '纬度';
COMMENT ON COLUMN LC_CARLOCDAILY.DIRCT IS '方向角';
COMMENT ON COLUMN LC_CARLOCDAILY.SPEED IS '速度';
COMMENT ON COLUMN LC_CARLOCDAILY.MILEAGE IS '里程';
COMMENT ON COLUMN LC_CARLOCDAILY.HIGHT IS '海拔';
COMMENT ON COLUMN LC_CARLOCDAILY.GNSSNUM IS 'GPS卫星数量';
COMMENT ON COLUMN LC_CARLOCDAILY.RSSI IS '4G信号值';
COMMENT ON COLUMN LC_CARLOCDAILY.RECEIVETIME IS 'GATEWAY处理时间';
COMMENT ON COLUMN LC_CARLOCDAILY.INSERTTIME IS '数据写入时间';
COMMENT ON TABLE LC_CARLOCDAILY IS '车辆日常定位信息';

-- 业务数据_手表信息
DROP TABLE IF EXISTS LC_WATCHDAILY;
CREATE TABLE LC_WATCHDAILY (
  PTIME TIMESTAMP NOT NULL,
  STEPS VARCHAR(50),
  HEARTBEAT VARCHAR(50),
  ROLL VARCHAR(50),
  BODYTEMPERATURE VARCHAR(50),
  WRISTTEMPERATURE VARCHAR(50),
  BLOODSUGAR VARCHAR(50),
  DIASTOLIC VARCHAR(50),
  SHRINK VARCHAR(50),
  BLOODOXYGEN VARCHAR(50),
  SLEEPTYPE VARCHAR(50),
  SLEEPSTARTTIME VARCHAR(50),
  SLEEPENDTIME VARCHAR(50),
  SLEEPMINUTE VARCHAR(50),
  SIGNAL VARCHAR(50),
  BATTERY VARCHAR(50),
  LAT VARCHAR(50),
  LNG VARCHAR(50),
  SPEED VARCHAR(50),
  INSERTTIME TIMESTAMP NOT NULL WITH DEFAULT CURRENT TIMESTAMP
);

ALTER TABLE LC_WATCHDAILY ADD CONSTRAINT PK_WATCHDAILY_PTIME PRIMARY KEY (PTIME);

COMMENT ON CONSTRAINT LC_WATCHDAILY.PK_WATCHDAILY_PTIME IS '主键约束';

COMMENT ON COLUMN LC_WATCHDAILY.PTIME IS '数据获取时间';
COMMENT ON COLUMN LC_WATCHDAILY.STEPS IS '步数';
COMMENT ON COLUMN LC_WATCHDAILY.HEARTBEAT IS '心率';
COMMENT ON COLUMN LC_WATCHDAILY.ROLL IS '翻转数';
COMMENT ON COLUMN LC_WATCHDAILY.BODYTEMPERATURE IS '体温';
COMMENT ON COLUMN LC_WATCHDAILY.WRISTTEMPERATURE IS '腕温';
COMMENT ON COLUMN LC_WATCHDAILY.BLOODSUGAR IS '血糖';
COMMENT ON COLUMN LC_WATCHDAILY.DIASTOLIC IS '舒张压';
COMMENT ON COLUMN LC_WATCHDAILY.SHRINK IS '收缩压';
COMMENT ON COLUMN LC_WATCHDAILY.BLOODOXYGEN IS '血氧';
COMMENT ON COLUMN LC_WATCHDAILY.SLEEPTYPE IS '睡眠类型(1深度睡眠2浅度睡眠3醒来时长)';
COMMENT ON COLUMN LC_WATCHDAILY.SLEEPSTARTTIME IS '睡眠开始时间';
COMMENT ON COLUMN LC_WATCHDAILY.SLEEPENDTIME IS '睡眠结束时间';
COMMENT ON COLUMN LC_WATCHDAILY.SLEEPMINUTE IS '睡眠时长(分钟)';
COMMENT ON COLUMN LC_WATCHDAILY.SIGNAL IS '信号值';
COMMENT ON COLUMN LC_WATCHDAILY.BATTERY IS '电池电量';
COMMENT ON COLUMN LC_WATCHDAILY.LAT IS '定位纬度(GPS)';
COMMENT ON COLUMN LC_WATCHDAILY.LNG IS '定位经度(GPS)';
COMMENT ON COLUMN LC_WATCHDAILY.SPEED IS '速度';
COMMENT ON COLUMN LC_WATCHDAILY.INSERTTIME IS '数据写入时间';
COMMENT ON TABLE LC_WATCHDAILY IS '手表日常数据';

-- 手表预警
DROP TABLE IF EXISTS LC_WATCHALARM;
CREATE TABLE LC_WATCHALARM (
  ID INT GENERATED ALWAYS AS IDENTITY,
  ALERTTYPE VARCHAR(10),
  ALERTINFO VARCHAR(1000),
  HEARTNUM VARCHAR(50),
  LASTTEMPER VARCHAR(50),
  INSERTTIME TIMESTAMP NOT NULL WITH DEFAULT CURRENT TIMESTAMP
);

ALTER TABLE LC_WATCHALARM ADD CONSTRAINT PK_WATCHALARM_ID PRIMARY KEY (ID);

CREATE INDEX NON_WATCHALARM_ALERTTYPE ON LC_WATCHALARM (ALERTTYPE ASC);

COMMENT ON CONSTRAINT LC_WATCHALARM.PK_WATCHALARM_ID IS '主键约束';
COMMENT ON INDEX NON_WATCHALARM_ALERTTYPE IS 'USERSTATUS升序索引';

COMMENT ON COLUMN LC_WATCHALARM.ID IS '自增主键';
COMMENT ON COLUMN LC_WATCHALARM.ALERTTYPE IS '预警类型';
COMMENT ON COLUMN LC_WATCHALARM.ALERTINFO IS '报警信息内容';
COMMENT ON COLUMN LC_WATCHALARM.HEARTNUM IS '当前心率';
COMMENT ON COLUMN LC_WATCHALARM.LASTTEMPER IS '当前体温';
COMMENT ON TABLE LC_WATCHALARM IS '手表日常报警信息';

-- 财务数据
DROP TABLE IF EXISTS FN_PAYBILLDETAIL;
CREATE TABLE FN_PAYBILLDETAIL (
  ID INT GENERATED ALWAYS AS IDENTITY,
  OWNER VARCHAR(50),
  SOURCETYPE INT,
  INOROUT VARCHAR(10),
  COUNTERPARTY VARCHAR(100),
  COUNTERBANK VARCHAR(100),
  COUNTERACCOUNT VARCHAR(50),
  GOODSCOMMENT VARCHAR(200),
  PAYMETHOD VARCHAR(50),
  AMOUNT NUMERIC(18,2),
  BALANCE NUMERIC(18,2),
  CURRENCY VARCHAR(50),
  PAYSTATUS VARCHAR(50),
  TRADETYPE VARCHAR(50),
  TRADEORDERNO VARCHAR(100),
  COUNTERORDERNO VARCHAR(100),
  TRADETIME TIMESTAMP,
  BILLCOMMENT VARCHAR(500),
  INSERTTIME TIMESTAMP NOT NULL WITH DEFAULT CURRENT TIMESTAMP,
  DELETEDUSER VARCHAR(50),
  DELETEDAT TIMESTAMP,
  DELETED BOOLEAN NOT NULL DEFAULT FALSE
);

ALTER TABLE FN_PAYBILLDETAIL ADD CONSTRAINT PK_PAYBILLDETAIL_ID PRIMARY KEY (ID);

CREATE INDEX NON_PAYBILLDETAIL_SOURCE ON FN_PAYBILLDETAIL (SOURCETYPE ASC);
CREATE INDEX NON_PAYBILLDETAIL_INOROUT ON FN_PAYBILLDETAIL (INOROUT ASC);
CREATE INDEX NON_PAYBILLDETAIL_PAYMETHOD ON FN_PAYBILLDETAIL (PAYMETHOD ASC);
CREATE INDEX NON_PAYBILLDETAIL_PAYSTATUS ON FN_PAYBILLDETAIL (PAYSTATUS ASC);
CREATE INDEX NON_PAYBILLDETAIL_TRADETYPE ON FN_PAYBILLDETAIL (TRADETYPE ASC);
CREATE INDEX NON_PAYBILLDETAIL_TRADETIME ON FN_PAYBILLDETAIL (TRADETIME ASC);

COMMENT ON CONSTRAINT FN_PAYBILLDETAIL.PK_PAYBILLDETAIL_ID IS '主键约束';
COMMENT ON INDEX NON_PAYBILLDETAIL_SOURCE IS 'PAYBILLDETAIL_SOURCE升序索引';
COMMENT ON INDEX NON_PAYBILLDETAIL_INOROUT IS 'PAYBILLDETAIL_INOROUT升序索引';
COMMENT ON INDEX NON_PAYBILLDETAIL_PAYMETHOD IS 'PAYBILLDETAIL_PAYMETHOD升序索引';
COMMENT ON INDEX NON_PAYBILLDETAIL_PAYSTATUS IS 'PAYBILLDETAIL_PAYSTATUS升序索引';
COMMENT ON INDEX NON_PAYBILLDETAIL_TRADETYPE IS 'PAYBILLDETAIL_TRADETYPE升序索引';
COMMENT ON INDEX NON_PAYBILLDETAIL_TRADETIME IS 'PAYBILLDETAIL_TRADETIME升序索引';

COMMENT ON COLUMN FN_PAYBILLDETAIL.ID IS '自增主键';
COMMENT ON COLUMN FN_PAYBILLDETAIL.OWNER IS '来源人(姓名)';
COMMENT ON COLUMN FN_PAYBILLDETAIL.SOURCETYPE IS '来源:1支付宝2微信3青岛银行4中国银行';
COMMENT ON COLUMN FN_PAYBILLDETAIL.INOROUT IS '收/支';
COMMENT ON COLUMN FN_PAYBILLDETAIL.COUNTERPARTY IS '交易对方';
COMMENT ON COLUMN FN_PAYBILLDETAIL.COUNTERBANK IS '对方开户行';
COMMENT ON COLUMN FN_PAYBILLDETAIL.COUNTERACCOUNT IS '对方账号';
COMMENT ON COLUMN FN_PAYBILLDETAIL.GOODSCOMMENT IS '商品说明';
COMMENT ON COLUMN FN_PAYBILLDETAIL.PAYMETHOD IS '收/付款方式';
COMMENT ON COLUMN FN_PAYBILLDETAIL.AMOUNT IS '金额';
COMMENT ON COLUMN FN_PAYBILLDETAIL.BALANCE IS '余额';
COMMENT ON COLUMN FN_PAYBILLDETAIL.CURRENCY IS '币种';
COMMENT ON COLUMN FN_PAYBILLDETAIL.PAYSTATUS IS '交易状态';
COMMENT ON COLUMN FN_PAYBILLDETAIL.TRADETYPE IS '交易分类';
COMMENT ON COLUMN FN_PAYBILLDETAIL.TRADEORDERNO IS '交易订单号';
COMMENT ON COLUMN FN_PAYBILLDETAIL.COUNTERORDERNO IS '商家订单号';
COMMENT ON COLUMN FN_PAYBILLDETAIL.TRADETIME IS '交易时间';
COMMENT ON COLUMN FN_PAYBILLDETAIL.BILLCOMMENT IS '交易备注';
COMMENT ON COLUMN FN_PAYBILLDETAIL.INSERTTIME IS '数据写入时间';
COMMENT ON COLUMN FN_PAYBILLDETAIL.DELETEDUSER IS '删除人账号';
COMMENT ON COLUMN FN_PAYBILLDETAIL.DELETEDAT IS '删除时间';
COMMENT ON COLUMN FN_PAYBILLDETAIL.DELETED IS '是否删除(0否1是)';
COMMENT ON TABLE FN_PAYBILLDETAIL IS '账单明细';
