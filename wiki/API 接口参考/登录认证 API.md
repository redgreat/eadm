# 登录认证 API

<cite>
**本文档中引用的文件**  
- [eadm_login_controller.erl](file://src/controllers/eadm_login_controller.erl)
- [eadm_router.erl](file://src/eadm_router.erl)
- [eadm_auth.erl](file://src/eadm_auth.erl)
- [login.js](file://priv/assets/js/login.js)
- [basic.js](file://priv/assets/js/basic.js)
- [eadm_utils.erl](file://src/eadm_utils.erl)
</cite>

## 目录
1. [简介](#简介)
2. [API 端点概览](#api-端点概览)
3. [用户登录接口](#用户登录接口)
4. [用户登出接口](#用户登出接口)
5. [用户信息查询接口](#用户信息查询接口)
6. [用户信息编辑接口](#用户信息编辑接口)
7. [用户密码修改接口](#用户密码修改接口)
8. [会话管理机制](#会话管理机制)
9. [典型请求/响应示例](#典型请求响应示例)
10. [错误码说明](#错误码说明)

## 简介
本 API 参考文档为 `eadm` 系统的登录认证模块提供完整的接口说明。文档基于 `eadm_login_controller.erl` 模块，详细描述了用户登录、登出、会话验证、用户信息管理等核心认证接口。开发者可依据本文档集成身份认证功能，实现安全的用户访问控制。

## API 端点概览
`eadm_login_controller.erl` 模块暴露了以下五个核心 HTTP 端点，用于处理用户认证和信息管理：

| 接口功能 | HTTP 方法 | URL 路径 | 认证要求 |
| :--- | :--- | :--- | :--- |
| 用户登录 | `GET`, `POST` | `/login` | 无需认证 |
| 用户登出 | `POST` | `/logout` | 需要认证 |
| 查询用户信息 | `GET` | `/userinfo` | 需要认证 |
| 编辑用户信息 | `POST` | `/useredit` | 需要认证 |
| 修改用户密码 | `POST` | `/userpwd` | 需要认证 |

**Section sources**
- [eadm_login_controller.erl](file://src/controllers/eadm_login_controller.erl#L15-L244)
- [eadm_router.erl](file://src/eadm_router.erl#L37-L63)

## 用户登录接口

### 接口说明
该接口处理用户的登录请求。支持 `GET` 和 `POST` 方法，`GET` 方法用于访问登录页面，`POST` 方法用于提交登录凭据。

### 请求详情
- **HTTP 方法**: `POST`
- **URL 路径**: `/login`
- **请求头**: `Content-Type: application/x-www-form-urlencoded`
- **请求体参数**:
  - `loginName` (字符串): 用户的登录名。
  - `password` (字符串): 用户的明文密码。

### 响应格式
- **状态码**: `200 OK`
- **响应体**: JSON 数组，包含一个对象。
  - `Alert` (字符串): 登录结果提示信息（如“欢迎【张三】登录! ”）。
  - `logined` (整数): 登录状态标志。`1` 表示成功，`0` 表示失败。

### 成功响应
当登录成功时，服务器会：
1. 验证用户名和密码。
2. 在会话（Session）中设置用户信息（`loginname`, `username`, `permission`, `exp`）。
3. 返回包含成功提示和 `logined=1` 的 JSON 响应。

### 失败响应
登录失败可能由以下原因导致：
- 用户名不存在（返回 `logined=0`, 提示“用户不存在”）。
- 用户被禁用（返回 `logined=0`, 提示“用户已禁用”）。
- 密码错误（返回 `logined=0`, 提示“用户名或密码错误”）。

**Section sources**
- [eadm_login_controller.erl](file://src/controllers/eadm_login_controller.erl#L25-L55)
- [login.js](file://priv/assets/js/login.js#L15-L40)

## 用户登出接口

### 接口说明
该接口处理用户的登出请求，清除用户的会话信息。

### 请求详情
- **HTTP 方法**: `POST`
- **URL 路径**: `/logout`
- **认证要求**: 需要有效的会话。

### 响应格式
- **状态码**: `200 OK`
- **响应类型**: 重定向（Redirect）。
- **重定向目标**: `/login`

### 处理流程
1. 服务器调用 `nova_session:delete(Req)` 删除当前请求的会话。
2. 客户端收到响应后，浏览器会自动跳转到登录页面。

**Section sources**
- [eadm_login_controller.erl](file://src/controllers/eadm_login_controller.erl#L57-L61)
- [basic.js](file://priv/assets/js/basic.js#L76-L88)

## 用户信息查询接口

### 接口说明
该接口用于获取当前登录用户的详细信息。

### 请求详情
- **HTTP 方法**: `GET`
- **URL 路径**: `/userinfo`
- **认证要求**: 需要有效的会话。

### 响应格式
- **状态码**: `200 OK`
- **响应体**: JSON 数组，包含一个对象，其元素顺序为：
  - `loginname` (字符串): 用户登录名。
  - `username` (字符串): 用户显示名称。
  - `email` (字符串): 用户邮箱。

### 认证检查
该接口依赖于 `eadm_auth` 模块进行认证。如果用户未登录或会话过期，将返回 `302 Found` 状态码，并重定向到 `/login`。

**Section sources**
- [eadm_login_controller.erl](file://src/controllers/eadm_login_controller.erl#L63-L82)
- [eadm_auth.erl](file://src/eadm_auth.erl#L15-L47)

## 用户信息编辑接口

### 接口说明
该接口允许用户修改自己的个人信息，如用户名和邮箱。

### 请求详情
- **HTTP 方法**: `POST`
- **URL 路径**: `/useredit`
- **认证要求**: 需要有效的会话。
- **请求体参数**:
  - `loginName` (字符串): 用户的登录名（通常不可修改）。
  - `userName` (字符串): 新的用户显示名称。
  - `email` (字符串): 新的用户邮箱。

### 响应格式
- **状态码**: `200 OK`
- **响应体**: JSON 数组，包含一个对象。
  - `Alert` (字符串): 操作结果提示信息（如“用户【张三】编辑成功！”）。

### 验证规则
- 邮箱地址必须符合标准的电子邮件格式（通过正则表达式验证）。
- 如果邮箱格式错误，将返回错误提示。

**Section sources**
- [eadm_login_controller.erl](file://src/controllers/eadm_login_controller.erl#L84-L114)
- [basic.js](file://priv/assets/js/basic.js#L112-L129)

## 用户密码修改接口

### 接口说明
该接口允许用户修改自己的登录密码。

### 请求详情
- **HTTP 方法**: `POST`
- **URL 路径**: `/userpwd`
- **认证要求**: 需要有效的会话。
- **请求体参数**:
  - `passwordOld` (字符串): 当前的旧密码。
  - `passwordNew` (字符串): 新的密码。

### 响应格式
- **状态码**: `200 OK`
- **响应体**: JSON 数组，包含一个对象。
  - `Alert` (字符串): 操作结果提示信息（如“密码修改成功！”）。

### 验证规则
1. **新密码验证**:
   - 长度必须在 6 到 36 位之间。
   - 只能包含英文、数字以及符号 `,._-`。
2. **旧密码验证**:
   - 必须与数据库中存储的密码匹配。
3. 如果任何一项验证失败，将返回相应的错误提示。

**Section sources**
- [eadm_login_controller.erl](file://src/controllers/eadm_login_controller.erl#L116-L168)
- [basic.js](file://priv/assets/js/basic.js#L131-L153)
- [eadm_utils.erl](file://src/eadm_utils.erl#L288-L315)

## 会话管理机制

### 会话实现
系统使用 `nova_session` 库来管理用户会话。会话数据存储在服务端（如内存或数据库），并通过 Cookie 中的会话 ID 与客户端关联。

### 会话内容
登录成功后，以下信息将被存储在会话中：
- `loginname`: 用户登录名。
- `username`: 用户显示名称。
- `permission`: 用户权限数据。
- `exp`: 会话过期时间戳（Unix 时间戳，单位为秒）。

### 会话过期
- 会话的有效期由 `application:get_env(nova, session_expire, 3600)` 决定，默认为 3600 秒（1 小时）。
- 每次用户发起需要认证的请求时，`eadm_auth:auth/1` 函数会被调用，它会检查 `exp` 是否过期，并自动刷新 `exp` 的值，实现会话的自动续期。

**Section sources**
- [eadm_login_controller.erl](file://src/controllers/eadm_login_controller.erl#L35-L45)
- [eadm_auth.erl](file://src/eadm_auth.erl#L15-L47)

## 典型请求/响应示例

### 用户登录请求示例
```bash
curl -X POST \
  http://localhost:8080/login \
  -H 'Content-Type: application/x-www-form-urlencoded' \
  -d 'loginName=admin&password=123456'
```

### 用户登录成功响应
```json
[
  {
    "Alert": "欢迎【管理员】登录! ",
    "logined": 1
  }
]
```

### 用户登录失败响应
```json
[
  {
    "Alert": "用户名或密码错误，请重新登录！",
    "logined": 0
  }
]
```

### 查询用户信息请求示例
```bash
curl -X GET \
  http://localhost:8080/userinfo \
  -H 'Cookie: nova_session=your_session_id_here'
```

### 查询用户信息成功响应
```json
[
  "admin",
  "管理员",
  "admin@example.com"
]
```

### 修改用户密码请求示例
```bash
curl -X POST \
  http://localhost:8080/userpwd \
  -H 'Content-Type: application/x-www-form-urlencoded' \
  -H 'Cookie: nova_session=your_session_id_here' \
  -d 'passwordOld=123456&passwordNew=newSecurePass123'
```

### 修改用户密码成功响应
```json
[
  {
    "Alert": "密码修改成功！"
  }
]
```

## 错误码说明
本模块不使用标准的 HTTP 状态码进行错误分类，而是通过 JSON 响应体中的 `Alert` 字段提供用户友好的错误信息。主要错误类型如下：

| 错误类型 | `Alert` 字段内容 | 说明 |
| :--- | :--- | :--- |
| 用户不存在 | `用户不存在，请联系管理员！` | 提供的 `loginName` 在数据库中找不到。 |
| 用户被禁用 | `用户已禁用，请联系管理员！` | 用户账户状态为禁用（`userstatus = 1`）。 |
| 登录失败 | `用户名或密码错误，请重新登录！` | 密码验证失败。 |
| 邮箱格式错误 | `邮箱【xxx】格式错误！` | 提供的邮箱地址不符合标准格式。 |
| 密码格式错误 | `密码不能少于6位！` 或 `密码仅支持【英文、数字、符号：,._-】` | 新密码不符合系统要求。 |
| 通用错误 | `用户登录失败！` 或 `用户查询失败！` | 发生了未预期的系统错误。 |