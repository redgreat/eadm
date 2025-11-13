# 用户管理 API

<cite>
**本文档中引用的文件**  
- [eadm_user_controller.erl](file://src/controllers/eadm_user_controller.erl)
- [eadm_auth.erl](file://src/eadm_auth.erl)
- [user.js](file://priv/assets/js/user.js)
</cite>

## 目录
1. [简介](#简介)
2. [权限校验逻辑](#权限校验逻辑)
3. [API 端点详情](#api-端点详情)
   - [用户列表查询 `/user/search`](#用户列表查询-usersearch)
   - [新增用户 `/user/add`](#新增用户-useradd)
   - [编辑用户 `/user/edit`](#编辑用户-useredit)
   - [删除用户 `/user/delete/{userId}`](#删除用户-userdeleteuserid)
   - [重置密码 `/user/reset/{userId}`](#重置密码-userresetuserid)
   - [禁用/启用用户 `/user/disable/{userId}`](#禁用启用用户-userdisableuserid)
   - [获取用户角色 `/user/role/{userId}`](#获取用户角色-userroleuserid)
   - [添加用户角色 `/user/roleadd`](#添加用户角色-userroleadd)
   - [删除用户角色 `/user/roledel/{userRoleId}`](#删除用户角色-userroledeluserroleid)
   - [获取用户权限 `/user/permission`](#获取用户权限-userpermission)

## 简介
本API文档详细描述了系统中用户管理模块的所有HTTP端点。所有接口均需通过身份认证，并对部分操作要求具备 `usermanage` 权限。每个端点都提供了完整的请求方式、参数说明、响应格式、状态码及错误处理机制。

**Section sources**
- [eadm_user_controller.erl](file://src/controllers/eadm_user_controller.erl#L1-L480)

## 权限校验逻辑
所有用户管理API均依赖于 `eadm_auth:auth/1` 模块进行权限校验。校验流程如下：
1. 检查会话中的 `exp`（过期时间）是否有效。
2. 若会话有效，则提取 `username`、`loginname` 和 `permission` 信息。
3. 更新会话有效期并返回认证成功标志。
4. 若会话无效或过期，则返回未认证状态，重定向至登录页。

对于需要 `usermanage` 权限的操作，控制器函数会检查 `auth_data` 中的 `permission` 字段是否包含 `{<<"usermanage">>, true}`。若无此权限，将返回“API鉴权失败！”提示。

**Section sources**
- [eadm_auth.erl](file://src/eadm_auth.erl#L1-L48)
- [eadm_user_controller.erl](file://src/controllers/eadm_user_controller.erl#L1-L480)

## API 端点详情

### 用户列表查询 `/user/search`
**HTTP 方法**: `GET`  
**权限要求**: 需 `usermanage` 权限  
**描述**: 查询所有用户信息，返回包含ID、租户名、登录名、用户名、邮箱、状态和创建时间的列表。  

**请求参数**:
- 无路径或查询参数

**成功响应**:
```json
[
  {
    "id": "string",
    "tenantname": "string",
    "loginname": "string",
    "username": "string",
    "email": "string",
    "userstatus": "启用|禁用",
    "createdat": "datetime"
  }
]
```

**错误响应**:
```json
[{"Alert": "用户查询失败！"}]
```

**HTTP 状态码**:
- `200 OK`: 查询成功
- `401 Unauthorized`: 未登录，重定向到 `/login`
- `403 Forbidden`: 无 `usermanage` 权限

**curl 示例**:
```bash
curl -X GET http://localhost/user/search -H "Cookie: sessionid=..."
```

**Section sources**
- [eadm_user_controller.erl](file://src/controllers/eadm_user_controller.erl#L65-L85)

### 新增用户 `/user/add`
**HTTP 方法**: `POST`  
**权限要求**: 需 `usermanage` 权限  
**描述**: 创建新用户，需提供登录名、用户名、邮箱和密码。

**请求体 (JSON)**:
```json
{
  "loginName": "string (6-18位，仅支持英文+数字)",
  "userName": "string",
  "email": "string (格式校验)",
  "password": "string (6-36位，支持英文、数字及符号: ,._-)"
}
```

**验证规则**:
- 登录名长度必须在6-18位之间
- 登录名仅允许英文、数字、下划线和连字符
- 登录名不能重复
- 邮箱格式必须符合标准正则表达式
- 密码长度6-36位，仅支持特定字符集

**成功响应**:
```json
[{"Alert": "用户【xxx】新增成功！"}]
```

**常见错误响应**:
```json
[{"Alert": "登录名【xxx】已存在！"}]
[{"Alert": "邮箱【xxx】格式错误！"}]
[{"Alert": "密码不能少于6位！"}]
```

**HTTP 状态码**:
- `200 OK`: 创建成功
- `400 Bad Request`: 参数验证失败
- `401 Unauthorized`: 未登录
- `403 Forbidden`: 无权限

**curl 示例**:
```bash
curl -X POST http://localhost/user/add \
  -H "Content-Type: application/x-www-form-urlencoded" \
  -d "loginName=newuser" \
  -d "userName=New User" \
  -d "email=newuser@example.com" \
  -d "password=Pass123" \
  -H "Cookie: sessionid=..."
```

**Section sources**
- [eadm_user_controller.erl](file://src/controllers/eadm_user_controller.erl#L90-L155)

### 编辑用户 `/user/edit`
**HTTP 方法**: `POST`  
**权限要求**: 需 `usermanage` 权限  
**描述**: 修改现有用户信息。

**请求体 (JSON)**:
```json
{
  "userId": "string",
  "loginName": "string",
  "userName": "string",
  "email": "string"
}
```

**验证规则**:
- 同新增用户，但登录名可保留原值（排除自身ID）

**成功响应**:
```json
[{"Alert": "用户【xxx】编辑成功！"}]
```

**错误响应**:
同 `/user/add` 接口

**HTTP 状态码**:
- `200 OK`: 更新成功
- `400 Bad Request`: 参数错误
- `401 Unauthorized`: 未登录
- `403 Forbidden`: 无权限

**curl 示例**:
```bash
curl -X POST http://localhost/user/edit \
  -H "Content-Type: application/x-www-form-urlencoded" \
  -d "userId=123" \
  -d "loginName=updateduser" \
  -d "userName=Updated User" \
  -d "email=updated@example.com" \
  -H "Cookie: sessionid=..."
```

**Section sources**
- [eadm_user_controller.erl](file://src/controllers/eadm_user_controller.erl#L160-L210)

### 删除用户 `/user/delete/{userId}`
**HTTP 方法**: `DELETE`  
**权限要求**: 需 `usermanage` 权限  
**描述**: 软删除指定用户（标记为已删除）。

**路径参数**:
- `userId`: 用户唯一标识

**成功响应**:
```json
[{"Alert": "用户删除成功！"}]
```

**错误响应**:
```json
[{"Alert": "用户删除失败！"}]
```

**HTTP 状态码**:
- `200 OK`: 删除成功
- `401 Unauthorized`: 未登录
- `403 Forbidden`: 无权限

**curl 示例**:
```bash
curl -X DELETE http://localhost/user/delete/123 -H "Cookie: sessionid=..."
```

**Section sources**
- [eadm_user_controller.erl](file://src/controllers/eadm_user_controller.erl#L255-L280)

### 重置密码 `/user/reset/{userId}`
**HTTP 方法**: `POST`  
**权限要求**: 需 `usermanage` 权限  
**描述**: 将指定用户的密码重置为默认值 `123456`。

**路径参数**:
- `userId`: 用户唯一标识

**成功响应**:
```json
[{"Alert": "用户密码重置成功！"}]
```

**错误响应**:
```json
[{"Alert": "用户密码重置失败！"}]
```

**HTTP 状态码**:
- `200 OK`: 重置成功
- `401 Unauthorized`: 未登录
- `403 Forbidden`: 无权限

**curl 示例**:
```bash
curl -X POST http://localhost/user/reset/123 -H "Cookie: sessionid=..."
```

**Section sources**
- [eadm_user_controller.erl](file://src/controllers/eadm_user_controller.erl#L225-L245)

### 禁用/启用用户 `/user/disable/{userId}`
**HTTP 方法**: `POST`  
**权限要求**: 需 `usermanage` 权限  
**描述**: 切换用户状态（启用 ↔ 禁用）。

**路径参数**:
- `userId`: 用户唯一标识

**成功响应**:
```json
[{"Alert": "用户启禁用成功！"}]
```

**错误响应**:
```json
[{"Alert": "用户操作失败！"}]
```

**HTTP 状态码**:
- `200 OK`: 操作成功
- `401 Unauthorized`: 未登录
- `403 Forbidden`: 无权限

**curl 示例**:
```bash
curl -X POST http://localhost/user/disable/123 -H "Cookie: sessionid=..."
```

**Section sources**
- [eadm_user_controller.erl](file://src/controllers/eadm_user_controller.erl#L247-L253)

### 获取用户角色 `/user/role/{userId}`
**HTTP 方法**: `GET`  
**权限要求**: 需 `usermanage` 权限  
**描述**: 查询指定用户所拥有的角色列表。

**路径参数**:
- `userId`: 用户唯一标识

**成功响应**:
```json
[
  {
    "id": "string",
    "rolename": "string",
    "updatedat": "datetime"
  }
]
```

**错误响应**:
```json
[{"Alert": "用户角色查询失败！"}]
```

**HTTP 状态码**:
- `200 OK`: 查询成功
- `401 Unauthorized`: 未登录
- `403 Forbidden`: 无权限

**curl 示例**:
```bash
curl -X GET http://localhost/user/role/123 -H "Cookie: sessionid=..."
```

**Section sources**
- [eadm_user_controller.erl](file://src/controllers/eadm_user_controller.erl#L285-L305)

### 添加用户角色 `/user/roleadd`
**HTTP 方法**: `POST`  
**权限要求**: 需 `usermanage` 权限  
**描述**: 为用户批量添加角色。

**请求体 (JSON 数组)**:
```json
[
  {
    "userId": "string",
    "roleId": "string"
  }
]
```

**成功响应**:
```json
[{"Alert": "用户角色新增成功！"}]
```

**错误响应**:
```json
[{"Alert": "用户角色新增失败！"}]
```

**HTTP 状态码**:
- `200 OK`: 添加成功
- `400 Bad Request`: 数据格式错误
- `401 Unauthorized`: 未登录
- `403 Forbidden`: 无权限

**curl 示例**:
```bash
curl -X POST http://localhost/user/roleadd \
  -H "Content-Type: application/x-www-form-urlencoded" \
  -d 'roleIds=[{"userId":"1","roleId":"2"}]' \
  -H "Cookie: sessionid=..."
```

**Section sources**
- [eadm_user_controller.erl](file://src/controllers/eadm_user_controller.erl#L310-L335)

### 删除用户角色 `/user/roledel/{userRoleId}`
**HTTP 方法**: `DELETE`  
**权限要求**: 需 `usermanage` 权限  
**描述**: 删除指定的用户-角色关联记录。

**路径参数**:
- `userRoleId`: 用户角色关联ID

**成功响应**:
```json
[{"Alert": "用户角色删除成功！"}]
```

**错误响应**:
```json
[{"Alert": "用户角色删除失败！"}]
```

**HTTP 状态码**:
- `200 OK`: 删除成功
- `401 Unauthorized`: 未登录
- `403 Forbidden`: 无权限

**curl 示例**:
```bash
curl -X DELETE http://localhost/user/roledel/456 -H "Cookie: sessionid=..."
```

**Section sources**
- [eadm_user_controller.erl](file://src/controllers/eadm_user_controller.erl#L340-L365)

### 获取用户权限 `/user/permission`
**HTTP 方法**: `GET`  
**权限要求**: 仅需登录，无需 `usermanage` 权限  
**描述**: 获取当前登录用户的权限数据。

**成功响应**:
```json
[{"data": {"usermanage": true, ...}}]
```

**错误响应**:
```json
[{"Alert": "权限获取失败！"}]
```

**HTTP 状态码**:
- `200 OK`: 获取成功
- `401 Unauthorized`: 未登录

**curl 示例**:
```bash
curl -X GET http://localhost/user/permission -H "Cookie: sessionid=..."
```

**Section sources**
- [eadm_user_controller.erl](file://src/controllers/eadm_user_controller.erl#L370-L385)