# @author wangcw
# @copyright (C) 2024, REDGREAT
# Created : 2024-03-26 09:37
# 表结构设计

USE eadm;

# 统一字符集、排序规则
# SHOW GLOBAL VARIABLES LIKE 'char%';
# SHOW GLOBAL VARIABLES LIKE 'collation%';
#
# ALTER DATABASE eadm COLLATE utf8mb4_0900_ai_ci;
#
# SELECT TABLE_SCHEMA, TABLE_NAME, TABLE_COLLATION,
#        CONCAT('ALTER TABLE ', TABLE_NAME,' CONVERT TO CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci;')
# FROM information_schema.TABLES
# WHERE TABLE_SCHEMA = 'eadm'
#   AND TABLE_COLLATION != 'utf8mb4_0900_ai_ci';
#
# SELECT TABLE_NAME,COLUMN_NAME,
#     CONCAT('ALTER TABLE ', TABLE_NAME, ' ALTER COLUMN ', COLUMN_NAME,
#            ' CONVERT TO CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci;')
# FROM information_schema.COLUMNS
# WHERE TABLE_SCHEMA = 'eadm'
#   AND COLUMNS.COLLATION_NAME != 'utf8mb4_0900_ai_ci';

# 系统_自定义序列信息表
DROP TABLE IF EXISTS sys_sequence;
CREATE TABLE `sys_sequence` (
  `SeqNo` CHAR(2) NOT NULL PRIMARY KEY COMMENT '序列前缀,两位字符',
  `DBName` VARCHAR(50) DEFAULT NULL COMMENT '数据库名',
  `TableName` VARCHAR(100) NOT NULL COMMENT '表名',
  `ColumnName` VARCHAR(50) NOT NULL COMMENT '字段名',
  `CurrentValue` BIGINT NOT NULL COMMENT '序列当前值',
  `Increment` SMALLINT NOT NULL COMMENT '步长',
  `InsertTime` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP() COMMENT '数据写入时间'
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci ROW_FORMAT=DYNAMIC COMMENT='系统_自定义序列信息表';

# 写入自增序列值
# TRUNCATE TABLE sys_sequence;
INSERT INTO sys_sequence(SeqNo, DBName, TableName, ColumnName, CurrentValue, Increment)
SELECT SUBSTR(B.COLUMN_COMMENT, INSTR(B.COLUMN_COMMENT,'(')+1, 2) AS SeqNo,
       A.TABLE_SCHEMA, A.TABLE_NAME, B.COLUMN_NAME, 9999999999, -1
FROM information_schema.TABLES A
INNER JOIN information_schema.COLUMNS B
  ON A.TABLE_SCHEMA=B.TABLE_SCHEMA
  AND A.TABLE_NAME=B.TABLE_NAME
  AND B.COLUMN_NAME='Id'
  AND B.COLUMN_COMMENT LIKE '自定义主键%'
WHERE A.TABLE_SCHEMA='eadm'
  AND A.TABLE_TYPE='BASE TABLE'
  AND NOT EXISTS(SELECT 1
                 FROM sys_sequence C
                 WHERE C.DBName=A.TABLE_SCHEMA
                   AND C.TableName=A.TABLE_NAME
                   AND C.ColumnName=B.COLUMN_NAME
                   AND C.SeqNo=SUBSTR(B.COLUMN_COMMENT, INSTR(B.COLUMN_COMMENT,'(')+1, 2));

# 自定义序列函数
CREATE FUNCTION `fn_nextval`(
InSeqNo char(2)) RETURNS char(12)
    SQL SECURITY INVOKER
    COMMENT '获取自定义序列'
BEGIN

   UPDATE sys_sequence
      SET CurrentValue = LAST_INSERT_ID(CurrentValue + Increment)
    WHERE SeqNo = InSeqNo;

   RETURN CONCAT(InSeqNo,LPAD(CAST(LAST_INSERT_ID() AS CHAR),10,0));

END;

# 系统_字典信息表
DROP TABLE IF EXISTS sys_dict;
CREATE TABLE `sys_dict` (
  `Id` CHAR(2) NOT NULL PRIMARY KEY COMMENT '自定义主键(SD)',
  `DictNo` VARCHAR(50) NOT NULL COMMENT '字典编码',
  `DictName` VARCHAR(100) NOT NULL COMMENT '字典名称',
  `ParentId` CHAR(12) DEFAULT NULL COMMENT '父级Id',
  `CreatedUser` VARCHAR(50) DEFAULT NULL COMMENT '创建人',
  `CreatedAt` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP() COMMENT '数据写入时间',
  `UpdatedUser` VARCHAR(50) DEFAULT NULL COMMENT '更新人',
  `UpdatedAt` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP()  ON UPDATE CURRENT_TIMESTAMP() COMMENT '更新时间',
  `DeletedUser` VARCHAR(50) DEFAULT NULL COMMENT '删除人',
  `DeletedAt` DATETIME DEFAULT NULL COMMENT '删除时间',
  `Deleted` TINYINT NOT NULL DEFAULT 0 COMMENT '是否删除(0否1是)',
  UNIQUE KEY `UNI_DictNo` (`DictNo`) USING BTREE,
  KEY `IDX` (`ParentId`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci ROW_FORMAT=DYNAMIC ROW_FORMAT=DYNAMIC COMMENT='系统_字典信息表';

# 基础信息_租户信息表
DROP TABLE IF EXISTS `eadm_tenant`;
CREATE TABLE `eadm_tenant` (
  `Id` CHAR(12) NOT NULL PRIMARY KEY COMMENT '自定义主键(ET)',
  `TenantName` VARCHAR(20) NOT NULL COMMENT '租户Id',
  `Remark` VARCHAR(100) NULL COMMENT '备注信息',
  `Enable` TINYINT NOT NULL DEFAULT 1 COMMENT '是否可用(0否1是)',
  `CreatedUser` VARCHAR(50) DEFAULT NULL COMMENT '创建人',
  `CreatedAt` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP() COMMENT '数据写入时间',
  `UpdatedUser` VARCHAR(50) DEFAULT NULL COMMENT '更新人',
  `UpdatedAt` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP()  ON UPDATE CURRENT_TIMESTAMP() COMMENT '更新时间',
  `DeletedUser` VARCHAR(50) DEFAULT NULL COMMENT '删除人',
  `DeletedAt` DATETIME DEFAULT NULL COMMENT '删除时间',
  `Deleted` TINYINT NOT NULL DEFAULT 0 COMMENT '是否删除(0否1是)'
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci ROW_FORMAT=DYNAMIC COMMENT '基础信息_租户信息表';

INSERT INTO eadm_tenant(Id, TenantName, Remark)
VALUES(fn_nextval('ET'),'REDGREAT', '主租户');

INSERT INTO eadm_tenant(Id, TenantName, Remark)
VALUES(fn_nextval('ET'),'管理客户', '手动添加客户');

INSERT INTO eadm_tenant(Id, TenantName, Remark)
VALUES(fn_nextval('ET'),'注册客户', '界面注册客户');

# 获取名称
DROP FUNCTION IF EXISTS fn_GetTenantNameById;
CREATE FUNCTION fn_GetTenantNameById(InId CHAR(12) CHARSET utf8mb4 COLLATE utf8mb4_0900_ai_ci)
    RETURNS VARCHAR(100) CHARSET utf8mb4 COLLATE utf8mb4_0900_ai_ci
    SQL SECURITY INVOKER
    COMMENT '根据Id获取租户名称'
BEGIN

    RETURN (SELECT TenantName FROM eadm_tenant WHERE Id=InId AND Deleted=0 LIMIT 1);

END;

# 用户表
DROP TABLE IF EXISTS `eadm_user`;
CREATE TABLE `eadm_user` (
  `Id` CHAR(12) NOT NULL PRIMARY KEY COMMENT '自定义主键(EU)',
  `TenantId` CHAR(12) NOT NULL COMMENT '租户Id',
  `LoginName` VARCHAR(50) NOT NULL COMMENT '用户登录名',
  `UserName` VARCHAR(50) NOT NULL COMMENT '用户姓名',
  `Email` VARCHAR(20) NOT NULL COMMENT '用户邮件',
  `CryptoGram` VARCHAR(50) NOT NULL COMMENT '密码',
  `UserStatus` TINYINT NOT NULL DEFAULT 0 COMMENT '用户状态(0启用1禁用)',
  `CreatedUser` VARCHAR(50) DEFAULT NULL COMMENT '创建人',
  `CreatedAt` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP() COMMENT '数据写入时间',
  `UpdatedUser` VARCHAR(50) DEFAULT NULL COMMENT '更新人',
  `UpdatedAt` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP()  ON UPDATE CURRENT_TIMESTAMP() COMMENT '更新时间',
  `DeletedUser` VARCHAR(50) DEFAULT NULL COMMENT '删除人',
  `DeletedAt` DATETIME DEFAULT NULL COMMENT '删除时间',
  `Deleted` TINYINT NOT NULL DEFAULT 0 COMMENT '是否删除(0否1是)',
  KEY `IDX-TenantId` (`TenantId`),
  KEY `IDX-LoginName` (`LoginName`),
  KEY `IDX-UpdatedAt` (`UpdatedAt`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci ROW_FORMAT=DYNAMIC COMMENT '基础信息_用户信息表';

INSERT INTO eadm_user(Id, TenantId, LoginName, UserName, Email, CryptoGram)
VALUES(fn_nextval('EU'), 'ET9999999998','wangcw', '王存伟', 'rubygreat@msn.com', 'q122/4GBpiCNq83AbPQN/+kYq0KwczLxiWfLaLKk4NY=');

INSERT INTO eadm_user(Id, TenantId, LoginName, UserName, Email, CryptoGram)
VALUES(fn_nextval('EU'), 'ET9999999998','wongcw', '王存偉', 'rubygreat@msn.com', 'q122/4GBpiCNq83AbPQN/+kYq0KwczLxiWfLaLKk4NY=');

INSERT INTO eadm_user(Id, TenantId, LoginName, UserName, Email, CryptoGram)
VALUES(fn_nextval('EU'), 'ET9999999998','jiangyf', '姜玉凤', '1234567@qq.com', 'q122/4GBpiCNq83AbPQN/+kYq0KwczLxiWfLaLKk4NY=');


# 用户视图
CREATE OR REPLACE VIEW vi_user
AS
SELECT Id, fn_GetTenantNameById(TenantId) AS TenantName, LoginName, UserName, Email,
       CASE UserStatus WHEN 1 THEN '禁用' WHEN 0 THEN '启用' END AS UserStatus, CreatedAt
FROM eadm_user
  WHERE Deleted=0;

# 用户角色
DROP TABLE IF EXISTS `eadm_role`;
CREATE TABLE `eadm_role` (
  `Id` CHAR(12) NOT NULL PRIMARY KEY COMMENT '自定义主键(ER)',
  `RoleName` VARCHAR(50) NOT NULL COMMENT '角色名称',
  `RolePermission` JSON NOT NULL COMMENT '角色权限',
  `RoleStatus` TINYINT NOT NULL DEFAULT 0 COMMENT '角色状态(0启用1禁用)',
  `CreatedUser` VARCHAR(50) DEFAULT NULL COMMENT '创建人',
  `CreatedAt` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP() COMMENT '数据写入时间',
  `UpdatedUser` VARCHAR(50) DEFAULT NULL COMMENT '更新人',
  `UpdatedAt` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP()  ON UPDATE CURRENT_TIMESTAMP() COMMENT '更新时间',
  `DeletedUser` VARCHAR(50) DEFAULT NULL COMMENT '删除人',
  `DeletedAt` DATETIME DEFAULT NULL COMMENT '删除时间',
  `Deleted` TINYINT NOT NULL DEFAULT 0 COMMENT '是否删除(0否1是)',
  KEY `IDX-RoleName` (`RoleName`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci ROW_FORMAT=DYNAMIC COMMENT '基础信息_角色信息表';

# 写入数据
INSERT INTO eadm_role(Id, RoleName, RolePermission)
# VALUES(fn_nextval('ER'), '超级管理员', '{}');
VALUES('ER9999999998', '超级管理员', '{}');

INSERT INTO eadm_role(Id, RoleName, RolePermission)
VALUES(fn_nextval('ER'), '注册租户', '{}');

INSERT INTO eadm_role(Id, RoleName, RolePermission)
VALUES(fn_nextval('ER'), '分配租户', '{}');

# 角色视图
CREATE OR REPLACE VIEW vi_role
AS
SELECT Id, RoleName, RolePermission, CASE RoleStatus WHEN 1 THEN '禁用' WHEN 0 THEN '启用' END AS RoleStatus, CreatedAt
FROM eadm_role
  WHERE Deleted=0;

# 用户角色对应关系
DROP TABLE IF EXISTS `eadm_userrole`;
CREATE TABLE `eadm_userrole` (
  `Id` BIGINT NOT NULL AUTO_INCREMENT PRIMARY KEY COMMENT '自增主键',
  `UserId` CHAR(12) NOT NULL COMMENT '用户Id(eadm_user.Id)',
  `RoleId` CHAR(12) NOT NULL COMMENT '角色Id(eadm_role.Id)',
  `CreatedUser` VARCHAR(50) DEFAULT NULL COMMENT '创建人',
  `CreatedAt` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP() COMMENT '数据写入时间',
  `UpdatedUser` VARCHAR(50) DEFAULT NULL COMMENT '更新人',
  `UpdatedAt` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP()  ON UPDATE CURRENT_TIMESTAMP() COMMENT '更新时间',
  `DeletedUser` VARCHAR(50) DEFAULT NULL COMMENT '删除人',
  `DeletedAt` DATETIME DEFAULT NULL COMMENT '删除时间',
  `Deleted` TINYINT NOT NULL DEFAULT 0 COMMENT '是否删除(0否1是)',
  KEY `IDX-UserId` (`UserId`),
  KEY `IDX-RoleId` (`RoleId`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci ROW_FORMAT=DYNAMIC COMMENT '基础信息_用户角色对应关系表';

INSERT INTO eadm_userrole(UserId, RoleId)
VALUES('EU9999999998', 'ER9999999998');

# 用户角色信息视图
CREATE OR REPLACE VIEW vi_userrole
AS
SELECT B.Id, A.Id AS UserId, C.Id AS RoleId, C.RoleName, B.UpdatedAt
FROM eadm_user A
INNER JOIN eadm_userrole B
  ON B.UserId=A.Id
  AND B.Deleted=0
INNER JOIN eadm_role C
  ON C.Id=B.RoleId
  AND C.RoleStatus=0
  AND C.Deleted=0
WHERE A.Deleted=0;

# 用户权限信息视图
CREATE OR REPLACE VIEW vi_userpermission
AS
SELECT B.Id, A.LoginName, C.RolePermission
FROM eadm_user A
INNER JOIN eadm_userrole B
  ON B.UserId=A.Id
  AND B.Deleted=0
INNER JOIN eadm_role C
  ON C.Id=B.RoleId
  AND C.RoleStatus=0
  AND C.Deleted=0
WHERE A.Deleted=0;

SELECT * FROM eadm_user;
SELECT * FROM eadm_role;
SELECT * FROM eadm_userrole;
SELECT * FROM vi_userrole;

# 定时任务信息
DROP TABLE IF EXISTS `eadm_crontab`;
CREATE TABLE `eadm_crontab` (
  `Id` BIGINT NOT NULL AUTO_INCREMENT PRIMARY KEY COMMENT '自增主键',
  `CronName` VARCHAR(50) NOT NULL COMMENT '任务名称',
  `CronExp` VARCHAR(50) NOT NULL COMMENT '定时表达式',
  `CronMFA` VARCHAR(50) NOT NULL COMMENT '任务备注',
  `StartDateTime` DATETIME COMMENT '任务开始时间',
  `EndDateTime` DATETIME COMMENT '任务结束时间',
  `CronStatus` TINYINT NOT NULL DEFAULT 0 COMMENT '任务状态(0启用1禁用)',
  `CreatedUser` VARCHAR(50) DEFAULT NULL COMMENT '创建人',
  `CreatedAt` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP() COMMENT '数据写入时间',
  `UpdatedUser` VARCHAR(50) DEFAULT NULL COMMENT '更新人',
  `UpdatedAt` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP()  ON UPDATE CURRENT_TIMESTAMP() COMMENT '更新时间',
  `DeletedUser` VARCHAR(50) DEFAULT NULL COMMENT '删除人',
  `DeletedAt` DATETIME DEFAULT NULL COMMENT '删除时间',
  `Deleted` TINYINT NOT NULL DEFAULT 0 COMMENT '是否删除(0否1是)',
  KEY `IDX-CronName` (`CronName`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci ROW_FORMAT=DYNAMIC COMMENT '基础信息_定时任务信息表';

# 首页报表
DROP TABLE IF EXISTS eadm.eadm_dashboard;
CREATE TABLE eadm.eadm_dashboard(
    Id BIGINT NOT NULL AUTO_INCREMENT PRIMARY KEY COMMENT '自增主键',
    DataType SMALLINT NOT NULL COMMENT '数据类型(1心率2步数3睡眠4里程5每月里程6每月收入7每月支出)',
    DateType SMALLINT NOT NULL COMMENT '统计周期类型(1日2周3月4年)',
    LoginName VARCHAR(50) NOT NULL COMMENT '登录名',
    DataValue VARCHAR(500) COMMENT '数据值',
    DataJson JSON COMMENT '数据JSON',
    CheckDate VARCHAR(20) NOT NULL COMMENT '数据日期',
    InsertTime DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '插入时间',
    UpdateTime DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
    CONSTRAINT UQ_DDCL UNIQUE (DataType, DateType, CheckDate, LoginName),
    KEY `NON-DataType` (DataType),
    KEY `NON-DateType` (DateType),
    KEY `NON-LoginName` (LoginName),
    KEY `NON-CheckDate` (CheckDate)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci ROW_FORMAT=DYNAMIC COMMENT='首页_看板报表';

# 过程运行日志
DROP TABLE IF EXISTS `sys_proclog`;
CREATE TABLE `sys_proclog` (
  `Id` INT NOT NULL AUTO_INCREMENT PRIMARY KEY COMMENT '自增主键',
  `ProcName` VARCHAR(50) COMMENT '过程名',
  `TimeSpan` INT COMMENT '耗时时长(秒)',
  `Result` TINYINT NOT NULL DEFAULT '1' COMMENT '是否成功(0否1是)',
  `ErrCode` VARCHAR(5) COMMENT '错误代码',
  `ErrMessage` VARCHAR(5000) COMMENT '错误详细信息',
  `InsertTime` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '日志记录时间',
  KEY `NON-ProcName` (`ProcName`),
  KEY `NON-InsertTime` (`InsertTime`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci ROW_FORMAT=DYNAMIC COMMENT='系统域_过程执行日志';

# 设备信息
DROP TABLE IF EXISTS `eadm_device`;
CREATE TABLE `eadm_device` (
  `Id` CHAR(12) NOT NULL PRIMARY KEY COMMENT '自定义主键(ED)',
  `DeviceNo` VARCHAR(50) COMMENT '设备号',
  `Imei` VARCHAR(50) COMMENT '设备IMEI',
  `SimNo` VARCHAR(50) COMMENT 'SIM卡号',
  `Remark` VARCHAR(200) COMMENT '设备描述',
  `CreatedUser` VARCHAR(50) DEFAULT NULL COMMENT '创建人',
  `CreatedAt` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP() COMMENT '数据写入时间',
  `UpdatedUser` VARCHAR(50) DEFAULT NULL COMMENT '更新人',
  `UpdatedAt` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP()  ON UPDATE CURRENT_TIMESTAMP() COMMENT '更新时间',
  `DeletedUser` VARCHAR(50) DEFAULT NULL COMMENT '删除人',
  `DeletedAt` DATETIME DEFAULT NULL COMMENT '删除时间',
  `Deleted` TINYINT NOT NULL DEFAULT 0 COMMENT '是否删除(0否1是)',
  KEY `NON-DeviceNo` (`DeviceNo`),
  KEY `NON-SimNo` (`SimNo`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci ROW_FORMAT=DYNAMIC COMMENT='业务域_设备信息';

INSERT INTO eadm_device(Id, DeviceNo, Remark, CreatedUser)
VALUES(fn_nextval('ED'), '16053489111', '充电宝', 'wangcw'),
      (fn_nextval('ED'), '868977061978771', '手表', 'wangcw');

# 人员设备对应关系
DROP TABLE IF EXISTS `eadm_userdevice`;
CREATE TABLE `eadm_userdevice` (
  `Id` BIGINT NOT NULL AUTO_INCREMENT PRIMARY KEY COMMENT '自增主键',
  `UserId` CHAR(12) COMMENT '用户Id(eadm_user.Id)',
  `LoginName` VARCHAR(50) COMMENT '用户登录名(eadm_user.LoginName)',
  `DeviceNo` VARCHAR(50) COMMENT '设备Id(eadm_device.DeviceNo)',
  `CreatedUser` VARCHAR(50) DEFAULT NULL COMMENT '创建人',
  `CreatedAt` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP() COMMENT '数据写入时间',
  `UpdatedUser` VARCHAR(50) DEFAULT NULL COMMENT '更新人',
  `UpdatedAt` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP()  ON UPDATE CURRENT_TIMESTAMP() COMMENT '更新时间',
  `DeletedUser` VARCHAR(50) DEFAULT NULL COMMENT '删除人',
  `DeletedAt` DATETIME DEFAULT NULL COMMENT '删除时间',
  `Deleted` TINYINT NOT NULL DEFAULT 0 COMMENT '是否删除(0否1是)',
  KEY `NON-UserId` (`UserId`),
  KEY `NON-LoginName` (`LoginName`),
  KEY `NON-DeviceNo` (`DeviceNo`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci ROW_FORMAT=DYNAMIC COMMENT='业务域_人员设备对应关系';

INSERT INTO eadm_userdevice(UserId, LoginName, DeviceNo)
SELECT 'EU9999999998', 'wangcw', DeviceNo
FROM eadm_device;

SELECT * FROM eadm_userdevice;
