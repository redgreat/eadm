# Mnesia迁移 - README

## 概述

本次迁移将eadm项目中的8个框架表从PostgreSQL迁移到Erlang内置的mnesia数据库。

## 已完成工作

### 1. 核心模块创建

#### `src/eadm_mnesia.erl`
- 定义8个表的record结构
- 实现schema初始化
- 实现表创建(disc_copies类型,持久化到磁盘)
- 实现种子数据初始化

#### `src/eadm_mnesia_api.erl`
- 封装统一的CRUD操作API
- 提供事务和脏读/写支持
- 实现查询辅助函数

#### `src/eadm_mnesia_backup.erl`
- 完整备份功能
- 压缩备份功能
- 从备份恢复功能
- 备份文件管理(压缩、清理30天前)
- 定时备份调度(每日凌晨3点)

#### `src/eadm_mnesia.hrl`
- 所有表的record定义
- 供controller模块引用

### 2. 应用集成

#### `src/eadm_app.erl`
- 在启动时初始化mnesia
- 创建schema、表、等待表就绪
- 导入种子数据
- 启动备份调度器

### 3. Docker配置

####`docker/docker-entrypoint.sh`
- 创建mnesia数据目录 `/opt/eadm/mnesia`
- 创建备份目录 `/opt/eadm/backups`

#### `docker-compose.yml`
- 添加volume映射: `./mnesia_data:/opt/eadm/mnesia`
- 添加volume映射: `./backups:/opt/eadm/backups`

#### `config/sys.config`
- 配置mnesia数据目录
- 配置dump阈值参数

#### `config/vm.args`
- 设置mnesia目录为 `/opt/eadm/mnesia`

## 迁移的表

1. `eadm_tenant` - 租户信息表
2. `eadm_user` - 用户信息表
3. `eadm_role` - 角色信息表
4. `eadm_userrole` - 用户角色关联表
5. `eadm_crontab` - 定时任务表
6. `eadm_dashboard` - 仪表盘数据表
7. `eadm_device` - 设备信息表
8. `eadm_userdevice` - 用户设备关联表

## 下一步工作

### 需要迁移controller代码

以下controller需要从PostgreSQL API改为mnesia API:

1. `src/controllers/eadm_user_controller.erl` - 用户管理
2. `src/controllers/eadm_role_controller.erl` - 角色管理
3. `src/controllers/eadm_crontab_controller.erl` - 定时任务管理
4. `src/controllers/eadm_device_controller.erl` - 设备管理
5. `src/controllers/eadm_dashboard_controller.erl` - 仪表盘数据

### 典型迁移示例

**PostgreSQL版本**:
```erlang
{ok, _, Rows} = eadm_pgpool:equery(pool_pg,
    "SELECT id, loginname, username FROM eadm_user WHERE deleted = false", [])
```

**Mnesia版本**:
```erlang
-include("eadm_mnesia.hrl").

Users = eadm_mnesia_api:query_all(eadm_user),
%% Users是#eadm_user{}记录列表
```

## 备份恢复操作

### 手动备份
```erlang
%% 在erlang shell中执行
eadm_mnesia_backup:backup().
%% 或指定文件名
eadm_mnesia_backup:backup("/opt/eadm/backups/manual.bup").
```

### 压缩备份
```erlang
eadm_mnesia_backup:backup_compressed().
```

### 恢复数据
```erlang
eadm_mnesia_backup:restore("/opt/eadm/backups/mnesia_backup_20260124_030000.bup").
```

### 定时备份
已自动配置每日凌晨3点备份,保留最近30天。

## 测试验证

### 编译项目
```bash
cd /Users/wangcw/Documents/github/eadm
rebar3 compile
```

### 本地测试
```bash
rebar3 shell
```

### Docker测试
```bash
docker-compose build eadm
docker-compose up eadm
```

### 验证数据
```erlang
%% 查看所有表
mnesia:system_info(tables).

%% 查看租户数据
mnesia:dirty_all_keys(eadm_tenant).

%% 查看用户数据
mnesia:dirty_all_keys(eadm_user).
```

## 注意事项

1. **首次启动**: mnesia会自动创建schema和表,无需手动干预
2. **数据持久化**: 使用disc_copies类型,数据会写入磁盘
3. **Docker volume**: mnesia_data和backups目录会映射到宿主机,确保数据不丢失
4. **节点名称**: vm.args中设置了节点名,mnesia需要
5. **备份策略**: 每日3点自动备份,保留30天,支持手动备份

## 故障排查

### 如果mnesia启动失败
```bash
# 删除mnesia目录重新初始化
rm -rf ./mnesia_data/*
docker-compose restart eadm
```

### 如果数据损坏
```bash
# 从最新备份恢复
docker exec eadm /opt/eadm/bin/eadm eval "eadm_mnesia_backup:restore(<<\"/opt/eadm/backups/mnesia_backup_YYYYMMDD_HHMMSS.bup\">>)."
```

## 参考文档

- Erlang Mnesia文档: https://www.erlang.org/doc/man/mnesia.html
- 实施方案: `implementation_plan.md`
- 任务清单: `task.md`
