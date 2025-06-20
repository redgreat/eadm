# EADM Mnesia数据库组件使用说明

## 概述

本项目已集成Mnesia数据库组件，用于管理用户信息、角色信息及其对应关系。Mnesia是Erlang/OTP内置的分布式数据库管理系统，具有高可用性和容错性。

## 配置说明

### 1. 数据库文件位置

在 `/Users/wangcw/Documents/github/eadm/config/vm.args` 文件中已配置数据库文件存储位置：

```erlang
-mnesia dir '"/var/eadm_db"'
```

### 2. 应用依赖

已在以下文件中添加mnesia依赖：

- `rebar.config`: 在release配置中添加mnesia
- `src/eadm.app.src`: 在applications列表中添加mnesia
- `src/eadm_app.erl`: 在启动函数中初始化mnesia

## 数据模型

### 用户表 (eadm_user)

```erlang
-record(eadm_user, {
    id,           %% 用户ID
    username,     %% 用户名
    password,     %% 密码
    email,        %% 邮箱
    created_at,   %% 创建时间
    updated_at    %% 更新时间
}).
```

### 角色表 (eadm_role)

```erlang
-record(eadm_role, {
    id,           %% 角色ID
    name,         %% 角色名称
    description,  %% 角色描述
    created_at,   %% 创建时间
    updated_at    %% 更新时间
}).
```

### 用户角色关系表 (eadm_user_role)

```erlang
-record(eadm_user_role, {
    user_id,      %% 用户ID
    role_id,      %% 角色ID
    assigned_at   %% 分配时间
}).
```

## 核心模块

### 1. eadm_mnesia.erl

数据库操作核心模块，提供以下功能：

#### 用户管理
- `create_user/4`: 创建用户
- `get_user/1`: 根据ID获取用户
- `get_user_by_username/1`: 根据用户名获取用户
- `update_user/2`: 更新用户信息
- `delete_user/1`: 删除用户
- `list_users/0`: 获取所有用户

#### 角色管理
- `create_role/3`: 创建角色
- `get_role/1`: 根据ID获取角色
- `get_role_by_name/1`: 根据角色名获取角色
- `update_role/2`: 更新角色信息
- `delete_role/1`: 删除角色
- `list_roles/0`: 获取所有角色

#### 用户角色关系管理
- `assign_role_to_user/2`: 为用户分配角色
- `remove_role_from_user/2`: 移除用户角色
- `get_user_roles/1`: 获取用户的所有角色
- `get_role_users/1`: 获取角色下的所有用户

### 2. 控制器模块

#### eadm_user_mnesia_controller.erl
基于mnesia的用户管理控制器，提供Web API接口。

#### eadm_role_mnesia_controller.erl
基于mnesia的角色管理控制器，提供Web API接口。

### 3. 路由配置

#### eadm_mnesia_router.erl
提供基于mnesia的路由配置，可以替换原有的PostgreSQL路由。

## 使用示例

### 1. 基本操作示例

```erlang
%% 创建用户
{ok, User} = eadm_mnesia:create_user(<<"admin">>, <<"password123">>, <<"admin@example.com">>, 1).

%% 创建角色
{ok, Role} = eadm_mnesia:create_role(<<"admin">>, <<"系统管理员">>, 1).

%% 为用户分配角色
ok = eadm_mnesia:assign_role_to_user(1, 1).

%% 获取用户角色
{ok, UserRoles} = eadm_mnesia:get_user_roles(1).
```

### 2. 运行示例代码

```erlang
%% 在Erlang shell中运行
eadm_mnesia_example:init_sample_data().
eadm_mnesia_example:test_user_operations().
eadm_mnesia_example:test_role_operations().
eadm_mnesia_example:test_user_role_operations().
```

## 启动和初始化

### 1. 自动初始化

应用启动时会自动初始化mnesia数据库：

```erlang
%% 在eadm_app.erl的start函数中
eadm_mnesia:init(),
```

### 2. 手动初始化

如果需要手动初始化：

```erlang
%% 启动mnesia
mnesia:start().

%% 初始化数据库
eadm_mnesia:init().
```

## 路由配置

### 使用新的mnesia路由

在 `eadm_router.erl` 中可以添加mnesia路由：

```erlang
%% 添加到routes函数的返回列表中
eadm_mnesia_router:mnesia_routes()
```

### 路由端点

- `/menu/user_mnesia` - 用户管理页面
- `/menu/role_mnesia` - 角色管理页面
- `/user_mnesia` - 用户数据查询
- `/role_mnesia` - 角色数据查询
- `/user_mnesia/add` - 添加用户
- `/user_mnesia/edit` - 编辑用户
- `/user_mnesia/delete/:userId` - 删除用户
- `/userrole_mnesia/:userId` - 用户角色管理
- `/role_mnesia/add` - 添加角色
- `/permission_mnesia/user/:userId` - 用户权限查询

## 数据迁移

如果需要从PostgreSQL迁移到Mnesia：

1. 导出PostgreSQL中的用户和角色数据
2. 使用mnesia API导入数据
3. 更新路由配置使用新的控制器
4. 测试功能完整性

## 性能优化

### 1. 索引优化

已为常用查询字段创建索引：
- 用户表的username字段
- 角色表的name字段

### 2. 事务处理

所有数据库操作都使用mnesia事务确保数据一致性。

### 3. 错误处理

完善的错误处理机制，包括：
- 数据验证
- 重复检查
- 外键约束

## 监控和维护

### 1. 数据库状态检查

```erlang
%% 检查mnesia状态
mnesia:system_info(is_running).

%% 查看表信息
mnesia:table_info(eadm_user, all).
```

### 2. 备份和恢复

```erlang
%% 备份数据库
mnesia:backup("/path/to/backup.bak").

%% 恢复数据库
mnesia:restore("/path/to/backup.bak", []).
```

## 注意事项

1. 确保 `/var/eadm_db` 目录有适当的读写权限
2. 在生产环境中考虑配置mnesia集群以提高可用性
3. 定期备份数据库文件
4. 监控数据库性能和存储空间使用情况
5. 在更新用户密码时确保使用适当的加密方法

## 故障排除

### 常见问题

1. **mnesia启动失败**
   - 检查目录权限
   - 确认磁盘空间充足
   - 查看错误日志

2. **表创建失败**
   - 检查表是否已存在
   - 确认节点配置正确
   - 验证记录定义

3. **数据查询异常**
   - 检查事务是否正确提交
   - 验证索引是否正常
   - 确认数据类型匹配

### 调试命令

```erlang
%% 查看所有表
mnesia:system_info(tables).

%% 查看表结构
mnesia:table_info(eadm_user, attributes).

%% 查看表数据
mnesia:dirty_select(eadm_user, [{'_', [], ['$_']}]).
```