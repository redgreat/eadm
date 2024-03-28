# @author wangcw
# @copyright (C) 2024, REDGREAT
# Created : 2024-03-26 09:37

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

# 角色视图
CREATE OR REPLACE VIEW vi_role
AS
SELECT Id, RoleName, RolePermission, CASE RoleStatus WHEN 1 THEN '禁用' WHEN 0 THEN '启用' END AS RoleStatus, CreatedAt
FROM eadm_role
  WHERE Deleted=0;

SELECT * FROM eadm_user;

SELECT * FROM eadm_role;

SELECT RolePermission
            FROM eadm_role
            WHERE Id = "ER9999999998"
              AND Deleted = 0;
