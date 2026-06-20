# API 约定

本文档定义新前端和后续 Cowboy handler 使用的 JSON API 约定。旧 Nova 页面接口可逐步迁移，不要求一次性改完。

## 响应结构

新 API 统一返回：

```json
{
  "success": true,
  "code": "ok",
  "message": "",
  "data": {}
}
```

失败返回：

```json
{
  "success": false,
  "code": "validation_error",
  "message": "参数错误",
  "data": {}
}
```

字段说明：

- `success`：是否成功。
- `code`：机器可读状态码，前端可用于分支处理。
- `message`：用户可读提示。
- `data`：业务数据。列表、详情、分页信息都放这里。

## 常用 code

- `ok`：成功。
- `validation_error`：请求参数错误。
- `unauthorized`：未登录或登录态失效。
- `forbidden`：无权限。
- `not_found`：资源不存在。
- `conflict`：数据冲突，例如登录名重复。
- `internal_error`：服务端异常。

## HTTP 状态码

- `200`：成功查询、成功修改。
- `201`：创建成功。
- `400`：参数错误。
- `401`：未登录。
- `403`：无权限。
- `404`：资源不存在。
- `409`：数据冲突。
- `500`：服务端异常。

## 分页结构

列表接口建议：

```json
{
  "success": true,
  "code": "ok",
  "message": "",
  "data": {
    "items": [],
    "page": 1,
    "pageSize": 20,
    "total": 0
  }
}
```

## 命名规范

- URL 使用小写短横线或资源名复数，例如 `/api/users`、`/api/system/processes`。
- JSON 字段使用 camelCase，方便 SolidJS/TypeScript 使用。
- 后端内部数据库字段可继续保持现状，在 API 层转换。

## 迁移策略

1. 旧接口继续返回当前结构，保证现有 jQuery 页面可用。
2. 新 SolidJS 页面只调用新 API。
3. 新 API 使用 `eadm_api_response` 生成响应。
4. 模块迁移完成后，再删除对应旧页面接口。

## 已开始迁移的接口

### GET /api/auth/me

返回当前登录用户信息，用于新前端初始化登录态。

成功：

```json
{
  "success": true,
  "code": "ok",
  "message": "",
  "data": {
    "authed": true,
    "loginName": "admin",
    "userName": "管理员",
    "permission": {}
  }
}
```

### GET /api/dashboard/summary

返回新前端首页汇总数据。该接口替代旧 `/dashboard` 数组下标结构。

成功：

```json
{
  "success": true,
  "code": "ok",
  "message": "",
  "data": {
    "cards": {
      "health": "0",
      "location": "0",
      "financeIncome": "0",
      "financeExpense": "0"
    },
    "locationTrend": {
      "labels": ["1月"],
      "values": ["0"]
    },
    "financeTrend": {
      "labels": ["1月"],
      "income": ["0"],
      "expense": ["0"]
    }
  }
}
```

### GET /api/users

返回用户列表。需要 `usermanage` 权限。

成功：

```json
{
  "success": true,
  "code": "ok",
  "message": "",
  "data": {
    "items": [
      {
        "id": 1,
        "tenantName": "默认租户",
        "loginName": "admin",
        "userName": "管理员",
        "email": "admin@example.com",
        "userStatus": 0,
        "createdAt": "2024-01-01 00:00:00"
      }
    ],
    "total": 1
  }
}
```

### GET /api/roles

返回角色列表。需要 `usermanage` 权限。

成功：

```json
{
  "success": true,
  "code": "ok",
  "message": "",
  "data": {
    "items": [
      {
        "id": 1,
        "roleName": "管理员",
        "roleStatus": 0,
        "createdAt": "2024-01-01 00:00:00"
      }
    ],
    "total": 1
  }
}
```

### GET /api/devices

返回设备列表。需要 `device.devlist` 权限。支持 `deviceNo` 查询参数。

成功：

```json
{
  "success": true,
  "code": "ok",
  "message": "",
  "data": {
    "items": [
      {
        "deviceNo": "D001",
        "imei": "000000000000000",
        "simNo": "13000000000",
        "remark": "",
        "enable": true,
        "createdAt": "2024-01-01 00:00:00"
      }
    ],
    "total": 1
  }
}
```

### GET /api/health

返回健康数据。需要 `health` 权限。查询参数：

- `dataType`：`1` 步数、`2` 心率、`3` 体温、`4` 血压、`5` 睡眠、`6` 信号/电量。
- `startTime`：`YYYY-MM-DD HH:mm:ss`。
- `endTime`：`YYYY-MM-DD HH:mm:ss`。

成功：

```json
{
  "success": true,
  "code": "ok",
  "message": "",
  "data": {
    "items": [
      {
        "utcTime": "2024-01-01 00:00:00",
        "steps": 1000
      }
    ],
    "total": 1
  }
}
```

### GET /api/location

返回轨迹坐标。需要 `locate` 权限。查询参数：

- `deviceNo`：可选，为空时查询当前用户有权限的全部设备。
- `startTime`：`YYYY-MM-DD HH:mm:ss`。
- `endTime`：`YYYY-MM-DD HH:mm:ss`。

成功：

```json
{
  "success": true,
  "code": "ok",
  "message": "",
  "data": {
    "items": [
      {
        "utcTime": "2024-01-01 00:00:00",
        "deviceNo": "D001",
        "lng": "120.0",
        "lat": "36.0"
      }
    ],
    "total": 1
  }
}
```

### GET /api/finance

返回财务流水。需要 `finance.finlist` 权限。查询参数：

- `sourceType`：`0` 全部、`1` 支付宝、`2` 微信、`3` 银行。
- `inOrOut`：`0` 全部、`1` 收入、`2` 支出、`3` 其他。
- `startTime`：`YYYY-MM-DD HH:mm:ss`。
- `endTime`：`YYYY-MM-DD HH:mm:ss`。

成功：

```json
{
  "success": true,
  "code": "ok",
  "message": "",
  "data": {
    "items": [
      {
        "id": 1,
        "sourceType": 1,
        "inOrOut": "支出",
        "tradeType": "餐饮",
        "amount": "20.00",
        "tradeTime": "2024-01-01 00:00:00"
      }
    ],
    "total": 1
  }
}
```

### GET /api/crontabs

返回定时任务列表。需要 `crontab` 权限。支持 `cronName` 查询参数。

成功：

```json
{
  "success": true,
  "code": "ok",
  "message": "",
  "data": {
    "items": [
      {
        "id": 1,
        "cronName": "同步任务",
        "cronExp": "0 * * * *",
        "cronMfa": "mod:fun/0",
        "startTime": "2024-01-01 00:00:00",
        "endTime": null,
        "cronStatus": 0,
        "createdAt": "2024-01-01 00:00:00"
      }
    ],
    "total": 1
  }
}
```

### GET /api/system/info

返回 Erlang VM 系统信息。

成功：

```json
{
  "success": true,
  "code": "ok",
  "message": "",
  "data": {
    "items": [
      {
        "key": "otpRelease",
        "value": "27"
      }
    ]
  }
}
```

### GET /api/ping

迁移期原生 Cowboy 健康检查接口。仅在可选 `eadm_cowboy_http` 监听器开启时可用。

成功：

```json
{
  "success": true,
  "code": "ok",
  "message": "",
  "data": {
    "service": "eadm",
    "runtime": "cowboy"
  }
}
```

### GET /api/internal/system/info

迁移期原生 Cowboy 系统信息验证接口。仅在可选 `eadm_cowboy_http` 监听器开启时可用。正式认证接入前不要作为主前端接口使用。

### GET /api/internal/users

迁移期原生 Cowboy 用户列表验证接口。仅用于确认 Cowboy handler 可复用 `eadm_user_service`，正式认证接入前不要作为主前端接口使用。

### GET /api/internal/roles

迁移期原生 Cowboy 角色列表验证接口。仅用于确认 Cowboy handler 可复用 `eadm_role_service`，正式认证接入前不要作为主前端接口使用。

### GET /api/internal/devices

迁移期原生 Cowboy 设备列表验证接口。支持 `deviceNo` 查询参数。

### GET /api/internal/crontabs

迁移期原生 Cowboy 定时任务列表验证接口。支持 `cronName` 查询参数。

### GET /api/internal/health

迁移期原生 Cowboy 健康数据验证接口。支持 `dataType`、`startTime`、`endTime` 查询参数。

### GET /api/internal/location

迁移期原生 Cowboy 轨迹验证接口。支持 `loginName`、`deviceNo`、`startTime`、`endTime` 查询参数。`loginName` 仅用于迁移验证，正式接口会从登录态读取。

### GET /api/internal/finance

迁移期原生 Cowboy 财务流水验证接口。支持 `sourceType`、`inOrOut`、`startTime`、`endTime` 查询参数。

### /api/internal/auth/*

迁移期原生 Cowboy 认证验证接口：

- `POST /api/internal/auth/login`
- `POST /api/internal/auth/logout`
- `GET /api/internal/auth/me`

登录接口接收 JSON body：

```json
{
  "loginName": "admin",
  "password": "123456"
}
```

这些接口使用 `eadm_cowboy_session` 签名 Cookie。正式切换前，前端仍使用 Nova `/api/auth/*`。

## Cowboy 正式路径迁移状态

可选 `eadm_cowboy_http` 监听器已挂载以下正式路径，并使用 `eadm_cowboy_session` 签名 Cookie 做登录态：

- `POST /api/auth/login`
- `POST /api/auth/logout`
- `GET /api/auth/me`
- `GET /api/system/info`
- `GET /api/users`
- `GET /api/roles`
- `GET /api/devices`
- `GET /api/crontabs`
- `GET /api/health`
- `GET /api/location`
- `GET /api/finance`

这些路径仅在 `eadm.cowboy_enabled=true` 时由可选 Cowboy listener 提供。当前 Nova 监听器上的同名 API 仍然保留。

未登录：

```json
{
  "success": false,
  "code": "unauthorized",
  "message": "请先登录",
  "data": {}
}
```
