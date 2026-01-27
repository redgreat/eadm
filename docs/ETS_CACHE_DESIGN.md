# ETS缓存设计方案

## 一、概述

### 1.1 目标
为EADM项目的API接口添加基于ETS（Erlang Term Storage）的内存缓存机制，减少数据库查询压力，提升API响应速度。

### 1.2 技术选型
- **缓存实现**：ETS（Erlang Term Storage）
  - 优势：Erlang原生支持，性能极高，进程间共享，无需外部依赖
  - 类型：`set`（键值对，键唯一）
  - 访问模式：`public`（所有进程可访问）
  - 写并发：`protected`（单进程写，多进程读）

### 1.3 设计原则
1. **透明性**：缓存对业务代码透明，通过统一API访问
2. **可配置性**：支持不同缓存策略（TTL、失效策略等）
3. **一致性**：数据更新时自动失效相关缓存
4. **性能优先**：优先缓存高频查询、计算成本高的数据

## 二、缓存架构设计

### 2.1 模块结构

```
eadm_cache/
├── eadm_cache.erl              # 缓存核心模块（ETS操作封装）
├── eadm_cache_manager.erl      # 缓存管理器（启动、监控、清理）
├── eadm_cache_helper.erl       # 缓存辅助函数（键生成、TTL检查等）
└── eadm_cache_wrapper.erl      # 数据访问层缓存包装器（统一封装）
```

### 2.2 缓存层位置设计

为了确保缓存机制对所有数据源（Mnesia、PostgreSQL、未来其他数据库）统一生效，采用**数据访问层（DAL）缓存**设计：

```
┌─────────────────────────────────────────┐
│  控制器层 (Controller Layer)            │
│  eadm_*_controller.erl                  │
└──────────────┬──────────────────────────┘
               │
┌──────────────▼──────────────────────────┐
│  数据访问层 (Data Access Layer)         │
│  ┌────────────────────────────────────┐ │
│  │ eadm_cache_wrapper.erl            │ │ ← 缓存包装层
│  │ - 统一缓存接口                    │ │
│  │ - 自动缓存管理                    │ │
│  └───────────┬──────────────────────┘ │
│              │                          │
│  ┌───────────▼──────────┐              │
│  │ eadm_mnesia_api.erl  │              │ ← Mnesia数据源
│  └──────────────────────┘              │
│  ┌───────────▼──────────┐              │
│  │ eadm_pgpool.erl      │              │ ← PostgreSQL数据源
│  └──────────────────────┘              │
│  ┌───────────▼──────────┐              │
│  │ eadm_*_api.erl       │              │ ← 未来其他数据源
│  └──────────────────────┘              │
└──────────────────────────────────────────┘
```

**设计优势**：
1. **统一性**：所有数据源通过统一的缓存包装器访问
2. **透明性**：控制器层无需关心缓存实现细节
3. **可扩展性**：新增数据源只需在包装器中添加对应接口
4. **一致性**：缓存失效策略统一管理

### 2.3 ETS表设计

#### 2.2.1 主缓存表（eadm_cache_data）
- **表名**：`eadm_cache_data`
- **类型**：`set`
- **键结构**：`{CacheKey, CacheType}`
  - `CacheKey`：缓存键（binary或tuple）
  - `CacheType`：缓存类型（atom，如`user_permission`, `dashboard_data`等）
- **值结构**：`#cache_entry{}`
  ```erlang
  -record(cache_entry, {
      key,           % 缓存键
      value,         % 缓存值
      created_at,    % 创建时间（timestamp）
      expires_at,    % 过期时间（timestamp，0表示永不过期）
      ttl,           % TTL秒数
      hit_count,     % 命中次数（统计用）
      last_access   % 最后访问时间
  }).
  ```

#### 2.2.2 统计表（eadm_cache_stats）
- **表名**：`eadm_cache_stats`
- **类型**：`set`
- **键**：`CacheType`（缓存类型）
- **值结构**：`#cache_stats{}`
  ```erlang
  -record(cache_stats, {
      cache_type,      % 缓存类型
      total_hits,      % 总命中次数
      total_misses,    % 总未命中次数
      total_sets,      % 总设置次数
      total_deletes    % 总删除次数
  }).
  ```

### 2.4 缓存键设计规范

#### 2.3.1 键命名规则
- **格式**：`{Type, Scope, Identifier}`
- **示例**：
  - 用户权限：`{user_permission, user, LoginName}`
  - Dashboard数据：`{dashboard_data, user, LoginName}`
  - 租户信息：`{tenant_info, tenant, TenantId}`
  - 用户列表：`{user_list, global, all}`

#### 2.3.2 键生成函数
```erlang
% 生成缓存键
make_key(CacheType, Scope, Identifier) ->
    {CacheType, Scope, Identifier}.

% 生成用户相关键
make_user_key(CacheType, LoginName) ->
    {CacheType, user, LoginName}.

% 生成全局键
make_global_key(CacheType) ->
    {CacheType, global, all}.
```

## 三、缓存策略设计

### 3.1 缓存类型分类

#### 3.1.1 用户级缓存（User-Level Cache）
- **特点**：按用户隔离，用户数据变更时失效
- **TTL**：5-30分钟
- **适用场景**：
  - 用户权限信息（`user_permission`）
  - Dashboard数据（`dashboard_data`）
  - 用户角色列表（`user_roles`）

#### 3.1.2 全局缓存（Global Cache）
- **特点**：全局共享，数据变更时统一失效
- **TTL**：10-60分钟
- **适用场景**：
  - 租户列表（`tenant_list`）
  - 系统配置（`system_config`）
  - 角色权限模板（`role_permission_template`）

#### 3.1.3 查询结果缓存（Query Result Cache）
- **特点**：缓存SQL查询结果，参数化查询
- **TTL**：1-10分钟
- **适用场景**：
  - 设备列表查询（`device_list`）
  - 用户列表查询（`user_list`）
  - 财务数据统计（`finance_stats`）

### 3.2 TTL策略

| 缓存类型 | TTL | 说明 |
|---------|-----|------|
| `user_permission` | 30分钟 | 权限变更频率低 |
| `dashboard_data` | 5分钟 | 数据更新频繁 |
| `tenant_info` | 60分钟 | 租户信息稳定 |
| `user_list` | 10分钟 | 用户列表变化中等 |
| `device_list` | 5分钟 | 设备列表变化频繁 |
| `query_result` | 1-10分钟 | 根据查询类型动态设置 |

### 3.3 缓存失效策略

#### 3.3.1 主动失效（Explicit Invalidation）
- **触发时机**：数据更新、删除操作
- **实现方式**：在控制器中调用`eadm_cache:invalidate/2`
- **示例**：
  ```erlang
  % 用户更新后，失效用户相关缓存
  eadm_cache:invalidate(user_permission, LoginName),
  eadm_cache:invalidate(dashboard_data, LoginName).
  ```

#### 3.3.2 被动失效（TTL Expiration）
- **触发时机**：缓存过期
- **实现方式**：读取时检查`expires_at`，过期则删除并重新查询
- **清理机制**：后台进程定期清理过期缓存

#### 3.3.3 模式失效（Pattern Invalidation）
- **触发时机**：批量失效需求
- **实现方式**：支持通配符模式匹配
- **示例**：
  ```erlang
  % 失效所有用户权限缓存
  eadm_cache:invalidate_pattern({user_permission, user, '_'}).
  ```

## 四、核心API设计

### 4.1 基础API

#### 4.1.1 获取缓存
```erlang
-spec get(CacheType :: atom(), Key :: any()) ->
    {ok, Value :: any()} | {error, not_found}.

% 示例
case eadm_cache:get(user_permission, LoginName) of
    {ok, Permission} ->
        Permission;
    {error, not_found} ->
        % 从数据库查询并缓存
        Permission = fetch_permission_from_db(LoginName),
        eadm_cache:set(user_permission, LoginName, Permission, 1800),
        Permission
end.
```

#### 4.1.2 设置缓存
```erlang
-spec set(CacheType :: atom(), Key :: any(), Value :: any(), TTL :: integer()) ->
    ok.

% 示例
eadm_cache:set(dashboard_data, LoginName, DashboardData, 300).
```

#### 4.1.3 删除缓存
```erlang
-spec delete(CacheType :: atom(), Key :: any()) ->
    ok.

% 示例
eadm_cache:delete(user_permission, LoginName).
```

#### 4.1.4 失效缓存
```erlang
-spec invalidate(CacheType :: atom(), Key :: any()) ->
    ok.

% 示例
eadm_cache:invalidate(user_permission, LoginName).
```

### 4.2 高级API

#### 4.2.1 带回调的获取（Get with Callback）
```erlang
-spec get_or_set(CacheType :: atom(), Key :: any(), 
                 FetchFun :: fun(() -> any()), TTL :: integer()) ->
    any().

% 示例
Permission = eadm_cache:get_or_set(
    user_permission, 
    LoginName,
    fun() -> fetch_permission_from_db(LoginName) end,
    1800
).
```

#### 4.2.2 批量失效
```erlang
-spec invalidate_pattern(Pattern :: tuple()) ->
    integer(). % 返回失效的缓存数量

% 示例
Count = eadm_cache:invalidate_pattern({user_permission, user, '_'}).
```

#### 4.2.3 清空缓存
```erlang
-spec clear(CacheType :: atom()) ->
    integer(). % 返回清空的缓存数量

% 示例
eadm_cache:clear(user_permission).
```

### 4.3 统计API

#### 4.3.1 获取缓存统计
```erlang
-spec stats(CacheType :: atom()) ->
    {ok, #cache_stats{}} | {error, not_found}.

% 示例
{ok, Stats} = eadm_cache:stats(user_permission),
io:format("命中率: ~p%", [Stats#cache_stats.total_hits / 
                          (Stats#cache_stats.total_hits + Stats#cache_stats.total_misses) * 100]).
```

#### 4.3.2 获取所有统计
```erlang
-spec all_stats() ->
    [#cache_stats{}].

% 示例
AllStats = eadm_cache:all_stats().
```

## 五、集成方案（统一数据访问层缓存）

### 5.1 设计理念

为了确保缓存机制对所有数据源统一生效，采用**数据访问层（DAL）缓存包装**策略：

1. **在数据访问层封装缓存**：而不是在控制器层
2. **统一缓存接口**：所有数据源通过统一的缓存包装器访问
3. **自动缓存管理**：数据更新时自动失效相关缓存

### 5.2 缓存包装器设计

#### 5.2.1 Mnesia API缓存包装

创建 `eadm_mnesia_api_cached.erl`，包装 `eadm_mnesia_api`：

```erlang
-module(eadm_mnesia_api_cached).

-export([read/2, read/3, query_all/1, query_all/2, 
         create/2, update/3, delete/2]).

%% 带缓存的读取
read(Table, Key) ->
    read(Table, Key, 1800). % 默认30分钟TTL

read(Table, Key, TTL) ->
    CacheKey = {mnesia, Table, Key},
    case eadm_cache:get(mnesia_read, CacheKey) of
        {ok, CachedValue} ->
            CachedValue;
        {error, not_found} ->
            Result = eadm_mnesia_api:read(Table, Key),
            eadm_cache:set(mnesia_read, CacheKey, Result, TTL),
            Result
    end.

%% 带缓存的查询所有
query_all(Table) ->
    query_all(Table, 600). % 默认10分钟TTL

query_all(Table, TTL) ->
    CacheKey = {mnesia, Table, all},
    case eadm_cache:get(mnesia_query_all, CacheKey) of
        {ok, CachedValue} ->
            CachedValue;
        {error, not_found} ->
            Result = eadm_mnesia_api:query_all(Table),
            eadm_cache:set(mnesia_query_all, CacheKey, Result, TTL),
            Result
    end.

%% 创建时失效相关缓存
create(Table, Record) ->
    Result = eadm_mnesia_api:create(Table, Record),
    case Result of
        ok ->
            % 失效该表的所有查询缓存
            eadm_cache:invalidate_pattern({mnesia_query_all, {mnesia, Table, '_'}}),
            eadm_cache:invalidate_pattern({mnesia_read, {mnesia, Table, '_'}});
        _ ->
            ok
    end,
    Result.

%% 更新时失效相关缓存
update(Table, Key, UpdateFun) ->
    Result = eadm_mnesia_api:update(Table, Key, UpdateFun),
    case Result of
        ok ->
            % 失效该记录的缓存
            eadm_cache:delete(mnesia_read, {mnesia, Table, Key}),
            % 失效该表的所有查询缓存
            eadm_cache:invalidate_pattern({mnesia_query_all, {mnesia, Table, '_'}});
        _ ->
            ok
    end,
    Result.

%% 删除时失效相关缓存
delete(Table, Key) ->
    Result = eadm_mnesia_api:delete(Table, Key),
    case Result of
        ok ->
            eadm_cache:delete(mnesia_read, {mnesia, Table, Key}),
            eadm_cache:invalidate_pattern({mnesia_query_all, {mnesia, Table, '_'}});
        _ ->
            ok
    end,
    Result.
```

#### 5.2.2 PostgreSQL API缓存包装

创建 `eadm_pgpool_cached.erl`，包装 `eadm_pgpool`：

```erlang
-module(eadm_pgpool_cached).

-export([equery/3, equery/4, equery_cached/4, equery_cached/5]).

%% 直接透传（写操作、不需要缓存的查询）
equery(PoolName, Sql, Params) ->
    eadm_pgpool:equery(PoolName, Sql, Params).

equery(PoolName, Sql, Params, Timeout) ->
    eadm_pgpool:equery(PoolName, Sql, Params, Timeout).

%% 带缓存的查询（读操作）
equery_cached(PoolName, Sql, Params, TTL) ->
    equery_cached(PoolName, Sql, Params, TTL, undefined).

equery_cached(PoolName, Sql, Params, TTL, CacheKey) ->
    % 生成缓存键（如果未提供）
    FinalCacheKey = case CacheKey of
        undefined -> 
            % 基于SQL和参数生成键
            {pg_query, PoolName, erlang:phash2({Sql, Params})};
        _ -> 
            {pg_query, PoolName, CacheKey}
    end,
    
    % 尝试从缓存获取
    case eadm_cache:get(pg_query, FinalCacheKey) of
        {ok, CachedResult} ->
            CachedResult;
        {error, not_found} ->
            % 查询数据库
            Result = eadm_pgpool:equery(PoolName, Sql, Params),
            case Result of
                {ok, Columns, Rows} ->
                    % 只缓存成功的结果
                    eadm_cache:set(pg_query, FinalCacheKey, Result, TTL),
                    Result;
                _ ->
                    % 错误不缓存
                    Result
            end
    end.

%% 失效PostgreSQL查询缓存
invalidate_pg_cache(PoolName, Pattern) ->
    eadm_cache:invalidate_pattern({pg_query, PoolName, Pattern}).
```

### 5.3 集成步骤

#### 步骤1：创建缓存核心模块
1. ✅ 创建`src/eadm_cache.erl` - 缓存核心API
2. ✅ 创建`src/eadm_cache_manager.erl` - 缓存管理器
3. ✅ 创建`src/eadm_cache_helper.erl` - 辅助函数

#### 步骤2：创建数据访问层缓存包装器
1. ✅ 创建`src/eadm_mnesia_api_cached.erl` - Mnesia缓存包装
2. ✅ 创建`src/eadm_pgpool_cached.erl` - PostgreSQL缓存包装

#### 步骤3：在应用启动时初始化
在`eadm_app:start/2`中启动缓存管理器：
```erlang
ok = eadm_cache_manager:start(),
```

#### 步骤4：逐步迁移控制器使用缓存包装器

**方案A：渐进式迁移（推荐）**
- 保持原有API不变，新增缓存包装器
- 逐步将控制器中的调用改为缓存版本
- 示例：
  ```erlang
  % 原代码
  Users = eadm_mnesia_api:query_all(eadm_user),
  
  % 改为
  Users = eadm_mnesia_api_cached:query_all(eadm_user),
  ```

**方案B：完全替换（激进）**
- 直接修改`eadm_mnesia_api`和`eadm_pgpool`，内部集成缓存
- 优点：控制器代码无需修改
- 缺点：需要仔细处理所有边界情况

#### 步骤5：特殊场景缓存处理

对于复杂查询（如Dashboard），可以在控制器层使用缓存：

```erlang
search(#{auth_data := #{<<"authed">> := true, <<"loginname">> := LoginName}}) ->
    % 使用缓存包装器的get_or_set
    FinalData = eadm_cache:get_or_set(
        dashboard_data,
        LoginName,
        fun() ->
            % 原有的查询逻辑
            {ok, _, ResData} = eadm_pgpool_cached:equery_cached(
                pool_pg,
                "select ...",
                [LoginName],
                300  % 5分钟TTL
            ),
            % ... 数据处理 ...
            FinalData
        end,
        300
    ),
    {json, FinalData}.
```

### 5.4 缓存失效策略

#### 5.4.1 自动失效（数据访问层）
- **Mnesia更新/删除**：自动失效相关缓存（已在包装器中实现）
- **PostgreSQL更新/删除**：需要手动调用失效函数

#### 5.4.2 手动失效（控制器层）
对于跨数据源的复杂操作，在控制器中手动失效：

```erlang
edit(#{...}) ->
    % 更新Mnesia
    ok = eadm_mnesia_api_cached:update(eadm_user, UserId, UpdateFun),
    % 更新PostgreSQL（如果有）
    ok = eadm_pgpool:equery(pool_pg, "update ...", [...]),
    % 手动失效跨数据源的缓存
    eadm_cache:invalidate(user_permission, LoginName),
    eadm_cache:invalidate(dashboard_data, LoginName),
    % ... 返回结果 ...
```

### 5.5 未来数据源扩展

当添加新的数据源（如Redis、MongoDB）时：

1. **创建对应的缓存包装器**：
   ```erlang
   -module(eadm_redis_api_cached).
   -export([get/2, set/3, ...]).
   ```

2. **在包装器中集成缓存逻辑**：
   ```erlang
   get(Key) ->
       CacheKey = {redis, Key},
       case eadm_cache:get(redis_read, CacheKey) of
           {ok, Cached} -> Cached;
           {error, not_found} ->
               Result = eadm_redis_api:get(Key),
               eadm_cache:set(redis_read, CacheKey, Result, 600),
               Result
       end.
   ```

3. **控制器代码无需修改**：只需使用新的缓存包装器即可

## 六、性能优化

### 6.1 ETS优化
1. **使用dirty操作**：读操作使用`ets:lookup`（dirty read），避免事务开销
2. **批量操作**：支持批量获取和设置
3. **内存管理**：定期清理过期缓存，防止内存泄漏

### 6.2 缓存预热
在应用启动时，预加载常用数据：
```erlang
% 预热租户信息
preload_tenant_cache() ->
    Tenants = eadm_mnesia_api:query_all(eadm_tenant),
    lists:foreach(fun(Tenant) ->
        eadm_cache:set(tenant_info, Tenant#eadm_tenant.id, 
                      Tenant#eadm_tenant.tenantname, 3600)
    end, Tenants).
```

### 6.3 缓存穿透防护
对于不存在的键，也进行短时间缓存（空值缓存）：
```erlang
case eadm_cache:get(user_permission, LoginName) of
    {ok, null} -> % 空值缓存，防止频繁查询数据库
        #{<<"data">> => #{}};
    {ok, Permission} ->
        Permission;
    {error, not_found} ->
        case fetch_permission_from_db(LoginName) of
            [] ->
                % 缓存空值，TTL较短
                eadm_cache:set(user_permission, LoginName, null, 60),
                #{<<"data">> => #{}};
            Permission ->
                eadm_cache:set(user_permission, LoginName, Permission, 1800),
                Permission
        end
end.
```

## 七、监控与运维

### 7.1 缓存监控指标
1. **命中率**：`hits / (hits + misses)`
2. **内存使用**：ETS表大小
3. **缓存数量**：各类型缓存条目数
4. **过期清理频率**：定期清理的缓存数量

### 7.2 日志记录
- 缓存命中/未命中日志（debug级别）
- 缓存失效日志（info级别）
- 缓存异常日志（error级别）

### 7.3 管理接口
提供HTTP接口查看缓存状态：
```erlang
% 在eadm_sys_sysinfo_controller中添加
{"/cache/stats", fun eadm_sys_cache_controller:stats/1, #{methods => [get]}},
{"/cache/clear/:type", fun eadm_sys_cache_controller:clear/1, #{methods => [delete]}},
```

## 八、实施计划

### 阶段1：基础框架（第1周）
1. ✅ 创建缓存模块结构
2. ✅ 实现基础API（get/set/delete）
3. ✅ 实现TTL机制
4. ✅ 实现缓存管理器

### 阶段2：集成高频接口（第2周）
1. ✅ 集成Dashboard查询缓存
2. ✅ 集成用户权限缓存
3. ✅ 集成租户信息缓存
4. ✅ 添加缓存失效逻辑

### 阶段3：扩展优化（第3周）
1. ✅ 集成更多查询接口缓存
2. ✅ 实现缓存统计功能
3. ✅ 实现缓存预热
4. ✅ 性能测试和优化

### 阶段4：监控运维（第4周）
1. ✅ 添加缓存监控接口
2. ✅ 完善日志记录
3. ✅ 文档完善
4. ✅ 生产环境部署

## 九、风险评估

### 9.1 风险点
1. **内存占用**：ETS表过大可能导致内存压力
   - **缓解措施**：设置合理的TTL，定期清理过期缓存
2. **数据一致性**：缓存与数据库不一致
   - **缓解措施**：数据更新时主动失效缓存
3. **缓存穿透**：大量查询不存在的数据
   - **缓解措施**：实现空值缓存机制

### 9.2 回滚方案
如果缓存出现问题，可以通过配置开关禁用缓存：
```erlang
% 在sys.config中添加
{eadm, [
    {cache_enabled, true}  % 设置为false即可禁用缓存
]}.
```

## 十、多数据源统一缓存支持

### 10.1 设计保证

**问题**：数据来源多样化（Mnesia、PostgreSQL，未来可能还有其他数据库），缓存机制如何统一生效？

**答案**：通过**数据访问层（DAL）缓存包装**设计，确保所有数据源都能统一享受缓存机制。

### 10.2 统一性保证

#### 10.2.1 统一的缓存接口
所有数据源通过各自的缓存包装器访问，包装器内部使用统一的`eadm_cache`模块：

```
Mnesia数据源
  ↓
eadm_mnesia_api_cached.erl (缓存包装器)
  ↓
eadm_cache.erl (统一缓存接口)
  ↓
ETS缓存存储

PostgreSQL数据源
  ↓
eadm_pgpool_cached.erl (缓存包装器)
  ↓
eadm_cache.erl (统一缓存接口)
  ↓
ETS缓存存储

未来Redis数据源
  ↓
eadm_redis_api_cached.erl (缓存包装器)
  ↓
eadm_cache.erl (统一缓存接口)
  ↓
ETS缓存存储
```

#### 10.2.2 统一的缓存键命名规范
所有数据源的缓存键都遵循统一规范：
- Mnesia：`{mnesia_read, {mnesia, Table, Key}}`
- PostgreSQL：`{pg_query, PoolName, Hash}`
- Redis：`{redis_read, {redis, Key}}`
- 业务缓存：`{CacheType, Scope, Identifier}`

#### 10.2.3 统一的缓存策略
- **TTL管理**：所有数据源都支持TTL配置
- **失效策略**：数据更新时自动失效相关缓存
- **统计监控**：统一的缓存统计接口

### 10.3 实际应用示例

#### 示例1：Mnesia查询（用户信息）
```erlang
% 控制器代码
Users = eadm_mnesia_api_cached:query_all(eadm_user, 600).
% ↑ 自动缓存，TTL 10分钟，无需修改控制器代码
```

#### 示例2：PostgreSQL查询（Dashboard数据）
```erlang
% 控制器代码
{ok, _, ResData} = eadm_pgpool_cached:equery_cached(
    pool_pg,
    "select ... from eadm_dashboard where loginname = $1",
    [LoginName],
    300  % TTL 5分钟
).
% ↑ 自动缓存，查询结果自动缓存
```

#### 示例3：混合数据源查询
```erlang
% 控制器代码
% 从Mnesia获取用户信息（自动缓存）
User = eadm_mnesia_api_cached:read(eadm_user, UserId),

% 从PostgreSQL获取统计数据（自动缓存）
{ok, _, Stats} = eadm_pgpool_cached:equery_cached(
    pool_pg,
    "select count(*) from ...",
    [],
    600
),

% 业务逻辑缓存（手动管理）
Permission = eadm_cache:get_or_set(
    user_permission,
    LoginName,
    fun() -> fetch_permission(User) end,
    1800
).
```

#### 示例4：数据更新时的自动失效
```erlang
% 更新Mnesia数据
ok = eadm_mnesia_api_cached:update(eadm_user, UserId, UpdateFun).
% ↑ 自动失效相关缓存，无需手动处理

% 更新PostgreSQL数据
ok = eadm_pgpool:equery(pool_pg, "update ...", [...]),
% 手动失效相关缓存（因为PostgreSQL更新需要业务逻辑判断）
eadm_cache:invalidate_pattern({pg_query, pool_pg, dashboard_pattern}).
```

### 10.4 扩展性保证

当添加新的数据源时：

1. **创建缓存包装器模块**（如`eadm_mongodb_api_cached.erl`）
2. **实现统一的缓存接口**（使用`eadm_cache`模块）
3. **控制器代码无需修改**（只需替换API调用）

**示例：添加MongoDB支持**
```erlang
-module(eadm_mongodb_api_cached).

-export([find_one/3, find_one/4]).

find_one(Collection, Filter, TTL) ->
    CacheKey = {mongodb, Collection, erlang:phash2(Filter)},
    case eadm_cache:get(mongodb_query, CacheKey) of
        {ok, Cached} -> Cached;
        {error, not_found} ->
            Result = eadm_mongodb_api:find_one(Collection, Filter),
            eadm_cache:set(mongodb_query, CacheKey, Result, TTL),
            Result
    end.
```

### 10.5 总结

**设计优势**：
1. ✅ **统一性**：所有数据源通过统一的缓存机制
2. ✅ **透明性**：控制器代码无需关心缓存实现
3. ✅ **可扩展性**：新增数据源只需添加包装器
4. ✅ **一致性**：统一的缓存策略和失效机制
5. ✅ **性能**：所有数据源都能享受缓存带来的性能提升

**关键设计点**：
- 缓存层位于数据访问层（DAL），而非控制器层
- 每个数据源都有对应的缓存包装器
- 统一的缓存接口和键命名规范
- 自动缓存管理和失效机制

## 十一、后续优化方向

1. **分布式缓存**：如果需要多节点部署，考虑使用Redis
2. **缓存预热策略**：根据访问模式智能预热
3. **缓存压缩**：对大对象进行压缩存储
4. **缓存分层**：L1（ETS）+ L2（Redis）两级缓存
5. **智能缓存策略**：根据数据访问频率自动调整TTL
