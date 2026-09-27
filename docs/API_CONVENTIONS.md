# API 约定

本文档定义 SolidJS 前端和 Cowboy handler 使用的 JSON API 约定。

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

## 路径分组

新接口统一使用 `/api/v1` 作为版本前缀，便于后续 Web、iOS、Android、微信小程序共用同一套协议。

| 分组 | 用途 | 当前路径 |
| --- | --- | --- |
| `auth` | 登录态、登录、退出 | `/api/v1/auth/*` |
| `dashboard` | 首页看板 | `/api/v1/dashboard/summary` |
| `admin` | 后台用户、角色等管理资源 | `/api/v1/admin/users`、`/api/v1/admin/roles` |
| `devices` | 设备资源 | `/api/v1/devices` |
| `jobs` | 定时任务、后台任务 | `/api/v1/jobs/crontabs` |
| `health` | 健康数据 | `/api/v1/health/records` |
| `location` | 轨迹位置 | `/api/v1/location/points` |
| `finance` | 财务流水 | `/api/v1/finance/records` |
| `system` | 系统运行信息 | `/api/v1/system/info` |
| `ping` | 服务探活 | `/api/v1/ping` |

前端和后续移动端、小程序客户端都只调用 `/api/v1/*`。旧 `/api/*` 路径不再保留。

## 命名规范

- URL 使用小写短横线或资源名复数，例如 `/api/v1/admin/users`、`/api/v1/system/info`。
- JSON 字段使用 camelCase，方便多端客户端和 TypeScript 使用。
- 后端内部数据库字段可继续保持现状，在 API 层转换。
- 需要区分端能力时优先通过请求头、查询参数或 feature flag 协商，不为 iOS/Android/小程序复制一套平行路径。

## API 实现约定

1. SolidJS 页面只调用 `/api/v1/*` JSON API。
2. API 使用 `eadm_api_response` 生成响应。
3. 认证态使用 `eadm_cowboy_session` 签名 Cookie；后续移动端可在同一分组下扩展 Token 认证。

## 已开始迁移的接口

### GET /api/v1/auth/me

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

### GET /api/v1/dashboard/summary

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

### GET /api/v1/admin/users

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

### GET /api/v1/admin/roles

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

### GET /api/v1/devices

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

### GET /api/v1/health/records

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

### GET /api/v1/location/points

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

### GET /api/v1/finance/records

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

### GET /api/v1/jobs/crontabs

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

### GET /api/v1/system/info

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

### GET /api/v1/ping

Cowboy 健康检查接口。

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

### /api/v1/auth/*

认证接口：

- `POST /api/v1/auth/login`
- `POST /api/v1/auth/logout`
- `GET /api/v1/auth/me`

登录接口接收 JSON body：

```json
{
  "loginName": "admin",
  "password": "123456"
}
```

这些接口使用 `eadm_cowboy_session` 签名 Cookie。

## Cowboy 正式路径

`eadm_cowboy_http` 挂载以下正式路径，并使用 `eadm_cowboy_session` 签名 Cookie 做登录态：

- `POST /api/v1/auth/login`
- `POST /api/v1/auth/logout`
- `GET /api/v1/auth/me`
- `GET /api/v1/system/info`
- `GET /api/v1/admin/users`
- `GET /api/v1/admin/roles`
- `GET /api/v1/devices`
- `GET /api/v1/jobs/crontabs`
- `GET /api/v1/health/records`
- `GET /api/v1/location/points`
- `GET /api/v1/finance/records`

未登录：

```json
{
  "success": false,
  "code": "unauthorized",
  "message": "请先登录",
  "data": {}
}
```
