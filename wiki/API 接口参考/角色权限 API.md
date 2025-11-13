# 角色权限 API

<cite>
**本文档引用文件**  
- [eadm_role_controller.erl](file://src/controllers/eadm_role_controller.erl)
- [eadm_auth.erl](file://src/eadm_auth.erl)
</cite>

## 目录
1. [简介](#简介)
2. [API 端点详情](#api-端点详情)
   - [角色列表查询 `/role/search`](#角色列表查询-rolesearch)
   - [新增角色 `/role/add`](#新增角色-roleadd)
   - [删除角色 `/role/delete/{roleId}`](#删除角色-roledeleteroleid)
   - [禁用/启用角色 `/role/disable/{roleId}`](#禁用启用角色-roledisableroleid)
   - [加载角色权限 `/role/loadpermission/{roleId}`](#加载角色权限-roleloadpermissionroleid)
   - [更新角色权限 `/role/updatepermission`](#更新角色权限-roleupdatepermission)
   - [获取用户可添加的角色列表 `/role/getrolelist/{userId}`](#获取用户可添加的角色列表-rolegetrolelistuserid)
3. [角色权限 JSON 结构说明](#角色权限-json-结构说明)
4. [认证与权限要求](#认证与权限要求)
5. [示例请求与响应](#示例请求与响应)
6. [错误处理](#错误处理)

## 简介
本 API 文档详细描述了系统中角色权限管理模块的 HTTP 接口。所有接口均在 `eadm_role_controller.erl` 模块中实现，用于支持角色的增删改查及权限配置功能。这些接口要求调用者具备 `usermanage` 权限，并通过会话认证。

**Section sources**  
- [eadm_role_controller.erl](file://src/controllers/eadm_role_controller.erl#L1-L248)

## API 端点详情

### 角色列表查询 `/role/search`
获取所有角色的列表信息。

- **HTTP 方法**: `GET`
- **路径**: `/role/search`
- **认证要求**: 已登录且具有 `usermanage` 权限
- **请求参数**: 无
- **请求体**: 无
- **响应格式**:
  ```json
  [
    {
      "id": 1,
      "rolename": "管理员",
      "rolestatus": 0,
      "createdat": "2024-03-26T10:00:00Z"
    }
  ]
  ```
- **状态码**:
  - `200`: 成功返回角色列表
  - `401`: 未认证，重定向至登录页
  - `403`: 权限不足

**Section sources**  
- [eadm_role_controller.erl](file://src/controllers/eadm_role_controller.erl#L37-L55)

### 新增角色 `/role/add`
创建一个新的角色。

- **HTTP 方法**: `POST`
- **路径**: `/role/add`
- **认证要求**: 已登录且具有 `usermanage` 权限
- **请求参数**:
  - `roleName` (string, 必填): 角色名称
- **请求体示例**:
  ```json
  { "roleName": "财务专员" }
  ```
- **响应格式**:
  ```json
  [{ "Alert": "角色【财务专员】新增成功！" }]
  ```
- **状态码**:
  - `200`: 角色创建成功
  - `400`: 参数缺失或数据库错误
  - `401`: 未认证
  - `403`: 权限不足

**Section sources**  
- [eadm_role_controller.erl](file://src/controllers/eadm_role_controller.erl#L57-L78)

### 删除角色 `/role/delete/{roleId}`
逻辑删除指定 ID 的角色。

- **HTTP 方法**: `DELETE`
- **路径**: `/role/delete/{roleId}`
- **认证要求**: 已登录且具有 `usermanage` 权限
- **路径参数**:
  - `roleId` (integer): 要删除的角色 ID
- **请求体**: 无
- **响应格式**:
  ```json
  [{ "Alert": "角色删除成功！" }]
  ```
- **状态码**:
  - `200`: 删除成功
  - `401`: 未认证
  - `403`: 权限不足
  - `500`: 删除失败（如数据库异常）

**Section sources**  
- [eadm_role_controller.erl](file://src/controllers/eadm_role_controller.erl#L156-L186)

### 禁用/启用角色 `/role/disable/{roleId}`
切换角色的启用状态（0 启用，1 禁用）。

- **HTTP 方法**: `PUT`
- **路径**: `/role/disable/{roleId}`
- **认证要求**: 已登录且具有 `usermanage` 权限
- **路径参数**:
  - `roleId` (integer): 目标角色 ID
- **请求体**: 无
- **响应格式**:
  ```json
  [{ "Alert": "角色启禁用成功！" }]
  ```
- **状态码**:
  - `200`: 状态切换成功
  - `401`: 未认证
  - `403`: 权限不足
  - `500`: 操作失败

**Section sources**  
- [eadm_role_controller.erl](file://src/controllers/eadm_role_controller.erl#L134-L154)

### 加载角色权限 `/role/loadpermission/{roleId}`
获取指定角色当前的权限配置。

- **HTTP 方法**: `GET`
- **路径**: `/role/loadpermission/{roleId}`
- **认证要求**: 已登录且具有 `usermanage` 权限
- **路径参数**:
  - `roleId` (integer): 角色 ID
- **响应格式**:
  ```json
  {
    "dashboard": true,
    "health": true,
    "locate": false,
    "finance": {
      "finlist": true,
      "finimp": false,
      "findel": true
    },
    "device": {
      "devlist": true,
      "devadd": false,
      "devedit": true,
      "devdel": false,
      "devassign": true
    },
    "crontab": false,
    "usermanage": true
  }
  ```
- **状态码**:
  - `200`: 成功返回权限数据
  - `401`: 未认证
  - `403`: 权限不足
  - `404`: 角色不存在或已删除
  - `500`: 查询失败

**Section sources**  
- [eadm_role_controller.erl](file://src/controllers/eadm_role_controller.erl#L112-L132)

### 更新角色权限 `/role/updatepermission`
更新指定角色的权限配置。

- **HTTP 方法**: `POST`
- **路径**: `/role/updatepermission`
- **认证要求**: 已登录且具有 `usermanage` 权限
- **请求参数**:
  - `roleId` (string, 必填): 角色 ID
  - `dashBoard`, `health`, `locate`, `finance`, `finimp`, `findel`, `crontab`, `userManage` (boolean, string): 各模块权限（`true`/`false` 字符串）
  - `devlist`, `devadd`, `devedit`, `devdel`, `devassign` (boolean, string): 设备管理子权限
- **请求体示例**:
  ```json
  {
    "roleId": "1",
    "dashBoard": "true",
    "health": "false",
    "locate": "true",
    "finance": "true",
    "finimp": "false",
    "findel": "true",
    "crontab": "false",
    "userManage": "true",
    "devlist": "true",
    "devadd": "false",
    "devedit": "true",
    "devdel": "false",
    "devassign": "true"
  }
  ```
- **响应格式**:
  ```json
  [{ "Alert": "权限更新成功！" }]
  ```
- **状态码**:
  - `200`: 权限更新成功
  - `400`: 参数缺失或类型错误
  - `401`: 未认证
  - `403`: 权限不足
  - `500`: 更新失败

**Section sources**  
- [eadm_role_controller.erl](file://src/controllers/eadm_role_controller.erl#L134-L154)

### 获取用户可添加的角色列表 `/role/getrolelist/{userId}`
查询某用户尚未关联的角色列表，用于添加角色时的选择。

- **HTTP 方法**: `GET`
- **路径**: `/role/getrolelist/{userId}`
- **认证要求**: 已登录且具有 `usermanage` 权限
- **路径参数**:
  - `userId` (integer): 用户 ID
- **响应格式**:
  ```json
  [
    {
      "id": 2,
      "rolename": "运维人员",
      "createdat": "2024-03-27T09:00:00Z"
    }
  ]
  ```
- **状态码**:
  - `200`: 成功返回可用角色列表
  - `401`: 未认证
  - `403`: 权限不足
  - `500`: 查询失败

**Section sources**  
- [eadm_role_controller.erl](file://src/controllers/eadm_role_controller.erl#L188-L214)

## 角色权限 JSON 结构说明
角色权限以嵌套 JSON 对象形式存储，包含以下模块：

- `dashboard`: 是否可访问仪表盘
- `health`: 是否可访问健康监控
- `locate`: 是否可访问位置服务
- `finance`: 财务模块主权限
  - `finlist`: 查看财务列表
  - `finimp`: 导入财务数据
  - `findel`: 删除财务记录
- `device`: 设备管理主权限
  - `devlist`: 查看设备列表
  - `devadd`: 添加设备
  - `devedit`: 编辑设备
  - `devdel`: 删除设备
  - `devassign`: 分配设备
- `crontab`: 是否可访问定时任务
- `usermanage`: 是否可访问用户管理

所有布尔值在数据库中以原子 `true`/`false` 存储，前端传参为字符串 `"true"`/`"false"`。

**Section sources**  
- [eadm_role_controller.erl](file://src/controllers/eadm_role_controller.erl#L134-L154)

## 认证与权限要求
所有角色权限 API 均依赖 `eadm_auth` 模块进行会话认证。用户必须：
1. 已登录并持有有效会话
2. 其权限映射中 `usermanage` 字段为 `true`

若未认证，则返回 `{redirect, "/login"}`；若权限不足，则返回 `API鉴权失败！` 提示。

**Section sources**  
- [eadm_auth.erl](file://src/eadm_auth.erl#L1-L49)
- [eadm_role_controller.erl](file://src/controllers/eadm_role_controller.erl#L25-L35)

## 示例请求与响应

### curl 示例：更新角色权限
```bash
curl -X POST "http://localhost:8080/role/updatepermission" \
  -H "Content-Type: application/json" \
  -d '{
    "roleId": "1",
    "dashBoard": "true",
    "health": "false",
    "finance": "true",
    "finimp": "false",
    "findel": "true",
    "userManage": "true",
    "devlist": "true",
    "devadd": "false"
  }'
```

### 响应示例
```json
[{ "Alert": "权限更新成功！" }]
```

**Section sources**  
- [eadm_role_controller.erl](file://src/controllers/eadm_role_controller.erl#L134-L154)

## 错误处理
所有接口均使用 `try-catch` 捕获数据库异常，并记录日志。常见错误包括：
- 数据库查询失败 → 返回 `数据查询失败！`
- 插入/更新失败 → 返回具体操作失败提示
- 权限不足 → 返回 `API鉴权失败！`
- 未登录 → 重定向至 `/login`

建议前端统一处理 `Alert` 字段提示用户。

**Section sources**  
- [eadm_role_controller.erl](file://src/controllers/eadm_role_controller.erl#L45-L55)
- [eadm_role_controller.erl](file://src/controllers/eadm_role_controller.erl#L65-L75)