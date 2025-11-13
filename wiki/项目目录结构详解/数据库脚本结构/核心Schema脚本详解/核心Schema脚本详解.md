# 核心Schema脚本详解

<cite>
**本文档引用文件**   
- [DataStruct.sql](file://script/mysql/DataStruct.sql)
- [datastruct.sql](file://script/kingbase/datastruct.sql)
- [datastruct.sql](file://script/postgres/datastruct.sql)
- [DATASTRUCT.sql](file://script/oracle/DATASTRUCT.sql)
- [datastruct.sql](file://script/tidb/datastruct.sql)
- [proc_DashBoard.sql](file://script/mysql/proc_DashBoard.sql)
- [proc_dashboard.sql](file://script/kingbase/proc_dashboard.sql)
- [proc_dashboard.sql](file://script/postgres/proc_dashboard.sql)
- [PROC_DASHBOARD.sql](file://script/oracle/PROC_DASHBOARD.sql)
- [ev_DashBoard.sql](file://script/mysql/ev_DashBoard.sql)
- [ev_DashBoard.sql](file://script/kingbase/ev_DashBoard.sql)
- [pgcron.sql](file://script/postgres/pgcron.sql)
- [dts.sql](file://script/kingbase/dts.sql)
- [dts.sql](file://script/postgres/dts.sql)
</cite>

## 目录
1. [引言](#引言)
2. [核心表结构分析](#核心表结构分析)
3. [存储过程对比分析](#存储过程对比分析)
4. [事件调度器与触发器机制](#事件调度器与触发器机制)
5. [SQL脚本执行建议](#sql脚本执行建议)
6. [常见执行错误与解决方案](#常见执行错误与解决方案)
7. [结论](#结论)

## 引言
本文档旨在深入解析 `eadm` 项目中各数据库目录下的核心 Schema 脚本，涵盖表结构定义、存储过程、事件调度器和触发器等关键组件。通过对 `DataStruct.sql`、`proc_dashboard.sql`、`ev_DashBoard.sql` 和 `dts.sql` 等脚本的详细分析，全面阐述用户表、角色表、设备表、财务记录表和定时任务表等核心实体的设计细节。同时，对比不同数据库（MySQL、Kingbase、PostgreSQL、Oracle）中 `proc_dashboard` 存储过程的实现差异，并说明事件调度器与触发器如何实现自动化数据处理。最后，提供脚本执行顺序建议和常见问题解决方案，确保数据库结构能够正确初始化。

## 核心表结构分析

### 用户表 (eadm_user)
用户表是系统权限管理的基础，存储所有用户的核心信息。
- **字段定义**: `Id` (主键), `TenantId` (租户ID), `LoginName` (登录名), `UserName` (姓名), `Email` (邮箱), `CryptoGram` (密码), `UserStatus` (状态) 等。
- **约束条件**: 
  - 主键: `Id` 字段。
  - 外键: `TenantId` 关联 `eadm_tenant` 表的 `Id`。
  - 唯一性: `LoginName` 在 MySQL/TiDB 中通过 `IDX-LoginName` 索引保证唯一。
- **索引策略**: 
  - MySQL/TiDB: `IDX-TenantId`, `IDX-LoginName`, `IDX-UpdatedAt`。
  - Kingbase/PostgreSQL/Oracle: `non_user_userstatus`, `non_user_updatedat`。
- **业务含义**: 实现多租户下的用户管理，通过 `UserStatus` 控制用户启用/禁用状态。

### 角色表 (eadm_role)
角色表定义了系统中的权限角色。
- **字段定义**: `Id` (主键), `RoleName` (角色名), `RolePermission` (权限JSON), `RoleStatus` (状态)。
- **约束条件**: 
  - 主键: `Id` 字段。
  - 唯一性: `RoleName` 在 MySQL/TiDB 中通过 `IDX-RoleName` 索引保证唯一。
- **索引策略**: 
  - MySQL/TiDB: `IDX-RoleName`。
  - Kingbase/PostgreSQL/Oracle: `non_role_rolename`, `non_user_rolestatus`。
- **业务含义**: 通过 `RolePermission` JSON 字段灵活定义角色权限，如 `{"health": true, "finance": {"finlist": true}}`。

### 设备表 (eadm_device)
设备表管理所有硬件设备信息。
- **字段定义**: `Id` 或 `DeviceNo` (主键), `Imei`, `SimNo`, `Remark`。
- **约束条件**: 
  - 主键: MySQL/TiDB 使用 `Id`，Kingbase/PostgreSQL/Oracle 使用 `deviceno`。
  - 唯一性: `DeviceNo` 和 `SimNo` 在 MySQL/TiDB 中通过 `NON-DeviceNo` 和 `NON-SimNo` 索引保证唯一。
- **索引策略**: 
  - MySQL/TiDB: `NON-DeviceNo`, `NON-SimNo`。
  - Kingbase/PostgreSQL/Oracle: `non_device_simno`。
- **业务含义**: 作为设备信息的主数据表，`deviceno` 作为业务主键被其他表引用。

### 财务记录表 (fn_paybilldetail)
财务记录表存储详细的收支流水。
- **字段定义**: `Id` (主键), `Owner`, `InOrOut` (收支类型), `Amount` (金额), `TradeTime` (交易时间)。
- **约束条件**: 
  - 主键: `Id` 字段。
- **索引策略**: Kingbase/PostgreSQL/Oracle 中创建了 `non_paybilldetail_sourcetype`, `non_paybilldetail_inorout`, `non_paybilldetail_paymethod` 等索引，以优化按收支类型、支付方式等条件的查询性能。
- **业务含义**: 记录每一笔财务交易，`InOrOut` 字段区分收入与支出，为财务报表提供数据源。

### 定时任务表 (eadm_crontab)
定时任务表用于管理系统级的定时任务。
- **字段定义**: `Id` (主键), `CronName` (任务名), `CronExp` (表达式), `CronStatus` (状态)。
- **约束条件**: 
  - 主键: `Id` 字段。
  - 唯一性: `CronName` 在 MySQL/TiDB 中通过 `IDX-CronName` 索引保证唯一。
- **索引策略**: 
  - MySQL/TiDB: `IDX-CronName`。
  - Kingbase/PostgreSQL/Oracle: `non_crontab_cronname`。
- **业务含义**: 存储定时任务的配置信息，`CronStatus` 控制任务的启用/禁用。

**核心表结构分析**
- [用户表定义](file://script/mysql/DataStruct.sql#L130-L160)
- [角色表定义](file://script/mysql/DataStruct.sql#L162-L187)
- [设备表定义](file://script/mysql/DataStruct.sql#L280-L309)
- [财务记录表定义](file://script/kingbase/datastruct.sql#L770-L799)
- [定时任务表定义](file://script/mysql/DataStruct.sql#L311-L336)

## 存储过程对比分析

### proc_dashboard 存储过程实现逻辑差异
`proc_dashboard` 存储过程用于生成首页报表数据，不同数据库的实现存在显著差异。

#### 参数传递方式
- **MySQL**: 使用 `InDate VARCHAR(10)` 作为输入参数，类型为字符串。
- **Kingbase/PostgreSQL**: 使用 `indate character varying` 或 `in indate date`，Kingbase 为字符串，PostgreSQL 为日期类型。
- **Oracle**: 使用 `indate VARCHAR(10)`，与 MySQL 一致。

#### 异常处理机制
- **MySQL**: 使用 `DECLARE CONTINUE HANDLER FOR SQLEXCEPTION` 捕获异常，并通过 `GET DIAGNOSTICS` 获取错误码和消息。
- **Kingbase/PostgreSQL/Oracle**: 使用标准的 `BEGIN ... EXCEPTION WHEN others THEN ... END;` 块来捕获所有异常，并通过 `sqlstate` 和 `sqlerrm` 获取错误信息。

#### 事务控制
- **MySQL**: 显式使用 `START TRANSACTION` 开启事务，根据错误码 `sys_ErrCode` 决定 `COMMIT` 或 `ROLLBACK`。
- **Kingbase/PostgreSQL/Oracle**: 依赖于存储过程的自动事务管理。在 `EXCEPTION` 块中显式调用 `COMMIT` 或 `ROLLBACK`。Kingbase 和 Oracle 版本在成功时显式 `COMMIT`，而 PostgreSQL 版本依赖于默认提交。

#### 时区处理
- **MySQL**: 使用 `CONVERT_TZ(UtcTime,'+00:00','+08:00')` 进行时区转换。
- **Kingbase/PostgreSQL/Oracle**: 使用 `set time zone 'Asia/Shanghai';` 设置会话时区，后续所有时间操作均基于此时区。

#### 数据类型转换
- **MySQL**: 使用 `CAST(Steps AS UNSIGNED)` 进行类型转换。
- **Kingbase/PostgreSQL**: 使用 `cast(steps as int)` 或 `steps::int` 进行转换。
- **Oracle**: 未在提供的脚本中明确显示，但通常使用 `TO_NUMBER`。

#### 日志记录
所有版本均在 `sys_proclog` 表中记录执行日志，包含过程名、耗时、结果、错误码和错误消息。

**存储过程对比分析**
- [MySQL proc_DashBoard.sql](file://script/mysql/proc_DashBoard.sql#L1-L213)
- [Kingbase proc_dashboard.sql](file://script/kingbase/proc_dashboard.sql#L1-L210)
- [PostgreSQL proc_dashboard.sql](file://script/postgres/proc_dashboard.sql#L1-L207)
- [Oracle PROC_DASHBOARD.sql](file://script/oracle/PROC_DASHBOARD.sql#L1-L208)

## 事件调度器与触发器机制

### 事件调度器 (ev_DashBoard)
事件调度器负责在指定时间自动调用 `proc_dashboard` 存储过程，实现报表的每日自动生成。
- **MySQL**: 使用 `CREATE EVENT` 语法，`ON SCHEDULE EVERY 1 DAY` 定义每日执行，`DO CALL proc_DashBoard(CURDATE());` 调用存储过程。
- **Kingbase**: 语法与 MySQL 几乎完全相同，仅将 `CURDATE()` 替换为 `CURRENT_DATE`。
- **PostgreSQL**: 使用 `pg_cron` 扩展，通过 `cron.schedule_in_database` 函数进行调度，`'0 4 * * *'` 表示每天凌晨4点执行 `call proc_dashboard(current_date);`。

```mermaid
flowchart TD
A[事件调度器] --> B{数据库类型}
B --> C[MySQL]
B --> D[Kingbase]
B --> E[PostgreSQL]
C --> F[CREATE EVENT ... ON SCHEDULE EVERY 1 DAY DO CALL proc_DashBoard(CURDATE());]
D --> G[CREATE EVENT ... ON SCHEDULE EVERY 1 DAY DO CALL proc_DashBoard(CURRENT_DATE);]
E --> H[SELECT cron.schedule_in_database('...', '0 4 * * *', 'call proc_dashboard(current_date);', '...');]
```

**事件调度器与触发器机制**
- [MySQL ev_DashBoard.sql](file://script/mysql/ev_DashBoard.sql#L1-L14)
- [Kingbase ev_DashBoard.sql](file://script/kingbase/ev_DashBoard.sql#L1-L14)
- [PostgreSQL pgcron.sql](file://script/postgres/pgcron.sql#L1-L25)

### 触发器 (Triggers)
虽然 `dts.sql` 脚本本身不包含触发器，但 `datastruct.sql` 脚本在 Kingbase、PostgreSQL 和 Oracle 中定义了用于自动更新 `UpdatedAt` 字段的触发器。
- **功能**: 在 `UPDATE` 操作前，自动将 `updatedat` 字段的值设置为当前时间戳。
- **实现**: 通过创建一个函数 `lastupdate()`，该函数将 `NEW.updatedat` 设置为 `current_timestamp`，然后在每个需要此功能的表上创建一个 `BEFORE UPDATE` 触发器来调用此函数。
- **业务含义**: 确保每条记录的最后更新时间被准确记录，无需在应用层代码中手动维护。

### 数据迁移脚本 (dts.sql)
`dts.sql` 脚本展示了如何利用 SQL 语句实现复杂的数据迁移和转换，其作用类似于触发器，但是一次性或周期性执行。
- **功能**: 将数据从备份表（如 `carlocdaily_bak`）迁移到生产表（如 `carlocdaily`），并在此过程中进行数据清洗和转换。
- **数据转换示例**:
  - 时区调整: `("dev_upload" - INTERVAL '8 hours') AT TIME ZONE 'UTC'` 将时间从北京时间转换为UTC。
  - 布尔值转换: `CASE "Deleted" WHEN 0 THEN FALSE ELSE TRUE END` 将整数标志位转换为布尔值。
- **业务含义**: 实现不同系统或数据库之间的数据同步，确保数据的一致性和完整性。

**事件调度器与触发器机制**
- [Kingbase dts.sql](file://script/kingbase/dts.sql#L1-L36)
- [PostgreSQL dts.sql](file://script/postgres/dts.sql#L1-L36)

## SQL脚本执行建议
为确保数据库结构正确初始化，建议按照以下顺序执行脚本：
1.  **执行 `DataStruct.sql`**: 首先创建所有数据库表、视图、序列和函数。这是基础，所有后续操作都依赖于这些表结构的存在。
2.  **执行 `proc_dashboard.sql`**: 在表结构创建完成后，创建存储过程。存储过程依赖于 `eadm_dashboard` 等表，因此必须在表之后创建。
3.  **执行 `dts.sql`**: 在表和存储过程都就绪后，执行数据迁移脚本。此脚本可能依赖于已创建的表结构。
4.  **执行 `ev_DashBoard.sql` 或 `pgcron.sql`**: 最后创建事件调度器。调度器调用 `proc_dashboard`，因此必须在存储过程创建之后才能成功创建。

**SQL脚本执行建议**
- [脚本执行顺序](file://script/mysql/DataStruct.sql#L1, file://script/mysql/proc_DashBoard.sql#L1, file://script/mysql/ev_DashBoard.sql#L1)

## 常见执行错误与解决方案
在执行这些SQL脚本时，可能会遇到以下常见错误：

| 错误类型 | 可能原因 | 解决方案 |
| :--- | :--- | :--- |
| **权限不足** | 当前数据库用户没有 `CREATE`, `DROP`, `INSERT`, `UPDATE` 等权限。 | 确保使用具有足够权限的数据库管理员账户（如 `user_eadm`）执行脚本。在 Kingbase/PostgreSQL 中，可能需要 `ALTER ROLE user_eadm SET SEARCH_PATH` 来设置正确的搜索路径。 |
| **依赖缺失** | 脚本执行顺序错误。例如，在创建存储过程时，其依赖的表尚未创建。 | 严格按照建议的执行顺序：先 `DataStruct.sql`，再 `proc_dashboard.sql`，然后 `dts.sql`，最后是事件调度器脚本。 |
| **对象已存在** | 重复执行 `CREATE` 语句，而对象（如表、过程）已存在。 | 脚本中通常包含 `DROP IF EXISTS` 语句来避免此问题。如果仍报错，手动检查并删除冲突的对象，或确保脚本的幂等性。 |
| **语法错误** | SQL 语法不兼容目标数据库。例如，在 MySQL 中使用了 PostgreSQL 的 `::` 类型转换符。 | 使用与目标数据库匹配的脚本版本。例如，为 MySQL 使用 `script/mysql/` 目录下的脚本，为 PostgreSQL 使用 `script/postgres/` 目录下的脚本。 |
| **数据类型不匹配** | 在 `dts.sql` 迁移数据时，源表和目标表的字段类型不一致。 | 仔细检查 `dts.sql` 脚本中的 `INSERT` 语句，确保使用了正确的类型转换函数（如 `CAST`, `::`, `TO_NUMBER`）。 |

## 结论
通过对 `eadm` 项目中核心 Schema 脚本的详细解析，可以看出其设计具有良好的跨数据库兼容性和清晰的层次结构。表结构设计规范，约束和索引合理，为业务功能提供了坚实的数据基础。`proc_dashboard` 存储过程在不同数据库上的实现展示了如何根据数据库特性进行适配，而事件调度器和数据迁移脚本则实现了自动化运维。遵循正确的执行顺序并注意权限和依赖问题，可以确保数据库环境被正确、高效地初始化。