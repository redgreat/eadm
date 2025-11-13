# 健康监测 API

<cite>
**本文档引用的文件**  
- [eadm_health_controller.erl](file://src/controllers/eadm_health_controller.erl)
- [health.js](file://priv/assets/js/health.js)
- [eadm_utils.erl](file://src/eadm_utils.erl)
- [eadm_auth.erl](file://src/eadm_auth.erl)
</cite>

## 目录
1. [简介](#简介)
2. [API 端点](#api-端点)
3. [请求参数说明](#请求参数说明)
4. [响应格式](#响应格式)
5. [状态码说明](#状态码说明)
6. [权限要求](#权限要求)
7. [时间跨度限制](#时间跨度限制)
8. [健康数据类型字段说明](#健康数据类型字段说明)
9. [示例请求与响应](#示例请求与响应)
10. [curl 示例](#curl-示例)

## 简介

本 API 用于查询用户的健康监测数据，支持按数据类型和时间范围进行检索。所有健康数据均来自可穿戴设备，通过后端服务持久化至数据库，并通过本接口提供安全、结构化的访问。

该接口由 `eadm_health_controller.erl` 模块实现，是健康监测功能的核心数据查询入口。前端通过 `health.js` 调用此 API 展示数据表格。

**Section sources**
- [eadm_health_controller.erl](file://src/controllers/eadm_health_controller.erl#L1-L147)
- [health.js](file://priv/assets/js/health.js#L0-L187)

## API 端点

| 端点 | HTTP 方法 | 描述 |
|------|---------|------|
| `/health` | GET | 查询指定时间范围内的健康数据 |

**Section sources**
- [eadm_health_controller.erl](file://src/controllers/eadm_health_controller.erl#L50-L147)

## 请求参数说明

| 参数名 | 类型 | 必填 | 说明 |
|--------|------|------|------|
| dataType | 字符串 | 是 | 健康数据类型编码：<br>• `"1"`：步数<br>• `"2"`：心率<br>• `"3"`：体温<br>• `"4"`：血压<br>• `"5"`：睡眠<br>• `"6"`：信号/电量 |
| startTime | 字符串 | 是 | 查询开始时间，格式为 `"YYYY-MM-DD HH:MM:SS"` |
| endTime | 字符串 | 是 | 查询结束时间，格式为 `"YYYY-MM-DD HH:MM:SS"` |

**Section sources**
- [eadm_health_controller.erl](file://src/controllers/eadm_health_controller.erl#L50-L147)

## 响应格式

响应为 JSON 格式，包含一个对象数组，每个对象代表一条健康记录。具体字段根据 `dataType` 不同而变化。

- **通用字段**：
  - `utctime`: 数据记录的 UTC 时间，格式为 `"yyyy-mm-dd hh24:mi:ss"`

- **按 dataType 的特定字段**：
  - `dataType="1"`（步数）：`steps`（步数）
  - `dataType="2"`（心率）：`heartbeat`（心率值）
  - `dataType="3"`（体温）：`bodytemperature`（体温），`wristtemperature`（腕温）
  - `dataType="4"`（血压）：`diastolic`（舒张压），`shrink`（收缩压）
  - `dataType="5"`（睡眠）：`sleeptype`（睡眠类型），`starttime`（开始时间），`endtime`（结束时间），`minute`（持续分钟数）
  - `dataType="6"`（信号/电量）：`battery`（电量百分比），`signal`（信号强度）

若查询失败或鉴权不通过，响应为包含 `Alert` 字段的错误信息数组。

**Section sources**
- [eadm_health_controller.erl](file://src/controllers/eadm_health_controller.erl#L70-L130)
- [eadm_utils.erl](file://src/eadm_utils.erl#L300-L383)

## 状态码说明

| 状态码 | 说明 |
|--------|------|
| 200 | 请求成功，返回健康数据或空数组 |
| 400 | 请求参数错误（如时间格式错误） |
| 401 | 未登录或会话过期，需重新登录 |
| 403 | 权限不足，用户无 health 权限 |
| 413 | 查询时间跨度超过限制（默认3天） |
| 500 | 服务器内部错误（如数据库查询失败） |

**Section sources**
- [eadm_health_controller.erl](file://src/controllers/eadm_health_controller.erl#L50-L147)

## 权限要求

访问此 API 需要用户具备 `health` 权限。

- 用户必须已登录（`authed` 为 true）。
- 用户权限对象中 `health` 字段必须为 true。
- 权限验证由 `eadm_auth.erl` 模块在请求预处理阶段完成。

**Section sources**
- [eadm_health_controller.erl](file://src/controllers/eadm_health_controller.erl#L30-L50)
- [eadm_auth.erl](file://src/eadm_auth.erl#L1-L48)

## 时间跨度限制

为防止长时间范围查询对数据库造成过大压力，系统限制单次查询的最大时间跨度。

- **默认最大跨度**：3天
- 该值由应用配置 `restwong_cfg` 中的 `max_search_span` 参数决定，若未设置则默认为 3。
- 若 `endTime` 与 `startTime` 的时间差超过最大跨度，将返回错误信息。

**Section sources**
- [eadm_health_controller.erl](file://src/controllers/eadm_health_controller.erl#L54-L60)
- [eadm_location_controller.erl](file://src/controllers/eadm_location_controller.erl#L49-L55)

## 健康数据类型字段说明

| 数据类型 | 字段名 | 数据库字段 | 说明 |
|----------|--------|------------|------|
| 步数 | `steps` | `steps` | 单位：步 |
| 心率 | `heartbeat` | `heartbeat` | 单位：bpm (每分钟心跳次数) |
| 体温 | `bodytemperature` | `bodytemperature` | 单位：°C |
| 体温 | `wristtemperature` | `wristtemperature` | 单位：°C，腕部温度 |
| 血压 | `diastolic` | `diastolic` | 舒张压，单位：mmHg |
| 血压 | `shrink` | `shrink` | 收缩压，单位：mmHg |
| 睡眠 | `sleeptype` | `sleeptype` | 睡眠类型编码（需查 i18n 翻译） |
| 睡眠 | `starttime` | `starttime` | 睡眠开始时间 |
| 睡眠 | `endtime` | `endtime` | 睡眠结束时间 |
| 睡眠 | `minute` | `minute` | 睡眠持续时间，单位：分钟 |
| 信号/电量 | `battery` | `battery` | 电量百分比，单位：% |
| 信号/电量 | `signal` | `signal` | 信号强度，单位：dBm |

**Section sources**
- [eadm_health_controller.erl](file://src/controllers/eadm_health_controller.erl#L70-L130)
- [eadm.pdma.json](file://script/eadm/eadm.pdma.json#L7408-L7499)

## 示例请求与响应

### 示例 1：查询心率数据

**请求**
```
GET /health?dataType=2&startTime=2024-03-01%2000:00:00&endTime=2024-03-01%2023:59:59
```

**响应**
```json
[
  {
    "utctime": "2024-03-01 23:55:00",
    "heartbeat": 72
  },
  {
    "utctime": "2024-03-01 23:50:00",
    "heartbeat": 70
  }
]
```

### 示例 2：查询体温数据

**请求**
```
GET /health?dataType=3&startTime=2024-03-01%2000:00:00&endTime=2024-03-01%2023:59:59
```

**响应**
```json
[
  {
    "utctime": "2024-03-01 23:55:00",
    "bodytemperature": 36.5,
    "wristtemperature": 32.1
  }
]
```

### 示例 3：鉴权失败

**响应**
```json
[
  {
    "Alert": "API鉴权失败！"
  }
]
```

**Section sources**
- [eadm_health_controller.erl](file://src/controllers/eadm_health_controller.erl#L50-L147)

## curl 示例

```bash
# 查询2024年3月1日的步数数据
curl -G "http://your-server/health" \
  --data-urlencode "dataType=1" \
  --data-urlencode "startTime=2024-03-01 00:00:00" \
  --data-urlencode "endTime=2024-03-01 23:59:59" \
  -H "Cookie: your-auth-session-cookie"
```

```bash
# 查询2024年3月1日的心率数据
curl -G "http://your-server/health" \
  --data-urlencode "dataType=2" \
  --data-urlencode "startTime=2024-03-01 00:00:00" \
  --data-urlencode "endTime=2024-03-01 23:59:59" \
  -H "Cookie: your-auth-session-cookie"
```

**Section sources**
- [eadm_health_controller.erl](file://src/controllers/eadm_health_controller.erl#L50-L147)
- [health.js](file://priv/assets/js/health.js#L46-L55)