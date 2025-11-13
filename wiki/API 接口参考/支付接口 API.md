# 支付接口 API

<cite>
**本文档引用的文件**  
- [eadm_payment_controller.erl](file://src/controllers/eadm_payment_controller.erl)
- [api_payment.erl](file://src/apis/api_payment.erl)
- [eadm_router.erl](file://src/eadm_router.erl)
</cite>

## 目录
1. [简介](#简介)
2. [API 端点概览](#api-端点概览)
3. [获取支付宝交易数据](#获取支付宝交易数据)
4. [获取微信支付交易数据](#获取微信支付交易数据)
5. [保存支付 API 配置](#保存支付-api-配置)
6. [安全与认证](#安全与认证)
7. [错误码说明](#错误码说明)
8. [示例代码](#示例代码)

## 简介
本 API 文档为开发者提供与第三方支付平台（支付宝、微信支付）集成的接口说明。通过 `eadm_payment_controller.erl` 和 `api_payment.erl` 模块，系统支持从支付宝和微信获取交易数据，并保存支付网关配置。所有接口均需身份验证和权限控制，确保数据安全。

该模块主要功能包括：
- 从支付宝拉取指定时间段的交易记录
- 从微信支付获取账单下载链接并下载交易数据
- 保存支付宝和微信支付的 API 配置信息
- 自动化定时同步交易数据

**Section sources**
- [eadm_payment_controller.erl](file://src/controllers/eadm_payment_controller.erl#L1-L20)
- [api_payment.erl](file://src/apis/api_payment.erl#L1-L20)

## API 端点概览
以下是支付模块提供的所有 HTTP 接口：

| 端点 | 方法 | 描述 | 认证要求 |
|------|------|------|---------|
| `/api/finance/alipay` | GET | 获取支付宝交易数据 | 是 |
| `/api/finance/wechat` | GET | 获取微信支付交易数据 | 是 |
| `/api/finance/config` | POST | 保存支付 API 配置 | 是 |

**Section sources**
- [eadm_router.erl](file://src/eadm_router.erl#L139-L163)

## 获取支付宝交易数据

### HTTP 方法与路径
```
GET /api/finance/alipay?startDate=2025-05-01&endDate=2025-05-16
```

### 请求参数
| 参数 | 类型 | 必需 | 描述 |
|------|------|------|------|
| `startDate` | string | 是 | 查询起始日期，格式：`YYYY-MM-DD` |
| `endDate` | string | 是 | 查询结束日期，格式：`YYYY-MM-DD` |

### 请求头
| 头部字段 | 值 | 说明 |
|--------|-----|------|
| `Authorization` | Bearer `<token>` | 用户认证令牌 |

### 响应格式（JSON）
成功响应：
```json
{
  "success": true,
  "count": 25
}
```

失败响应：
```json
{
  "success": false,
  "message": "获取支付宝交易数据失败"
}
```

### 状态码
| 状态码 | 说明 |
|-------|------|
| `200 OK` | 请求成功，返回交易数据数量 |
| `401 Unauthorized` | 未登录或认证失败 |
| `403 Forbidden` | 用户无财务模块访问权限 |
| `500 Internal Server Error` | 服务器内部错误 |

### 安全考虑
- 必须通过 `eadm_auth` 模块进行身份验证
- 用户必须拥有 `finance.finlist` 权限
- 所有请求需通过 HTTPS 传输
- 私钥签名在服务端完成，不暴露于客户端

**Section sources**
- [eadm_payment_controller.erl](file://src/controllers/eadm_payment_controller.erl#L25-L40)
- [api_payment.erl](file://src/apis/api_payment.erl#L41-L85)

## 获取微信支付交易数据

### HTTP 方法与路径
```
GET /api/finance/wechat?startDate=2025-05-01&endDate=2025-05-16
```

### 请求参数
| 参数 | 类型 | 必需 | 描述 |
|------|------|------|------|
| `startDate` | string | 是 | 查询起始日期，格式：`YYYY-MM-DD` |
| `endDate` | string | 是 | 查询结束日期，格式：`YYYY-MM-DD`（实际仅使用 `startDate`） |

### 请求头
| 头部字段 | 值 | 说明 |
|--------|-----|------|
| `Authorization` | Bearer `<token>` | 用户认证令牌 |

### 响应格式（JSON）
成功响应：
```json
{
  "success": true,
  "count": 18
}
```

失败响应：
```json
{
  "success": false,
  "message": "获取微信支付交易数据失败"
}
```

### 状态码
| 状态码 | 说明 |
|-------|------|
| `200 OK` | 请求成功，交易数据已处理 |
| `401 Unauthorized` | 未登录或认证失败 |
| `403 Forbidden` | 用户无财务模块访问权限 |
| `500 Internal Server Error` | 服务器内部错误 |

### 安全考虑
- 使用微信支付 v3 API 的证书签名认证机制
- 请求签名包含 `mchid`、`serial_no`、`nonce_str`、`timestamp` 和 `signature`
- 私钥存储于应用环境变量中，不硬编码在代码中
- 需要 `finance.finlist` 权限才能访问

**Section sources**
- [eadm_payment_controller.erl](file://src/controllers/eadm_payment_controller.erl#L52-L70)
- [api_payment.erl](file://src/apis/api_payment.erl#L102-L140)

## 保存支付 API 配置

### HTTP 方法与路径
```
POST /api/finance/config
```

### 请求体结构
根据 `type` 字段区分不同支付平台配置。

#### 支付宝配置
```json
{
  "type": "alipay",
  "appId": "2021000000000000",
  "privateKey": "MIIEvQIBADANBgkqhkiG...",
  "publicKey": "MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQC..."
}
```

#### 微信支付配置
```json
{
  "type": "wechat",
  "appId": "wx1234567890abcdef",
  "mchId": "1900000000",
  "apiKey": "rEaLkEy20250516WeChatPay",
  "apiV3Key": "v3Key20250516Secure",
  "serialNo": "5A9B8C7D6E5F4G3H2J1K",
  "privateKey": "MIIEvQIBADANBgkqhkiG..."
}
```

### 请求头
| 头部字段 | 值 | 说明 |
|--------|-----|------|
| `Authorization` | Bearer `<token>` | 用户认证令牌 |
| `Content-Type` | `application/json` | 请求体为 JSON 格式 |

### 响应格式（JSON）
成功响应：
```json
{
  "success": true
}
```

失败响应：
```json
{
  "success": false,
  "message": "不支持的API类型"
}
```

### 状态码
| 状态码 | 说明 |
|-------|------|
| `200 OK` | 配置保存成功 |
| `400 Bad Request` | 请求体格式错误或类型不支持 |
| `401 Unauthorized` | 未登录或认证失败 |
| `403 Forbidden` | 用户无财务模块访问权限 |
| `500 Internal Server Error` | 保存配置失败 |

### 安全考虑
- 敏感信息（如私钥）通过安全通道传输
- 配置信息存储于 Erlang 应用环境变量中，避免明文存储
- 支持动态更新配置而无需重启服务
- 仅允许具有 `finance.finlist` 权限的用户修改配置

**Section sources**
- [eadm_payment_controller.erl](file://src/controllers/eadm_payment_controller.erl#L70-L116)
- [api_payment.erl](file://src/apis/api_payment.erl#L226-L256)

## 安全与认证
本支付接口采用多层安全机制保障数据安全：

1. **身份认证**：基于 `eadm_auth` 模块实现 JWT 或 Session 认证。
2. **权限控制**：用户必须具备 `finance.finlist` 权限才能访问支付相关接口。
3. **敏感数据保护**：
   - 支付宝私钥、微信 API 密钥等存储于应用环境变量
   - 不在日志中打印敏感信息
4. **通信安全**：
   - 所有外部 API 调用使用 HTTPS
   - 微信支付使用平台证书进行双向认证
5. **签名验证**：
   - 支付宝使用 RSA2 签名算法
   - 微信支付使用 SHA256-RSA2048 签名

**Section sources**
- [eadm_payment_controller.erl](file://src/controllers/eadm_payment_controller.erl#L25-L116)
- [api_payment.erl](file://src/apis/api_payment.erl#L41-L140)

## 错误码说明
| 错误码/消息 | 含义 | 建议操作 |
|------------|------|---------|
| `API鉴权失败` | 用户未登录或无权限 | 检查登录状态和权限设置 |
| `支付宝API配置未设置` | 缺少 appId 或私钥 | 调用 `/config` 接口保存配置 |
| `微信支付API配置未设置` | 缺少 appId 或商户号 | 调用 `/config` 接口保存配置 |
| `获取支付宝交易数据失败` | 内部异常或网络错误 | 检查服务日志，重试请求 |
| `获取微信支付交易数据失败` | 内部异常或网络错误 | 检查服务日志，重试请求 |
| `不支持的API类型` | `type` 字段值非法 | 检查请求体，应为 `alipay` 或 `wechat` |

**Section sources**
- [eadm_payment_controller.erl](file://src/controllers/eadm_payment_controller.erl#L35-L38)
- [api_payment.erl](file://src/apis/api_payment.erl#L45-L48)

## 示例代码

### curl 示例：获取支付宝交易数据
```bash
curl -X GET "http://localhost:8000/api/finance/alipay?startDate=2025-05-01&endDate=2025-05-16" \
  -H "Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.xxxxx"
```

### curl 示例：获取微信支付交易数据
```bash
curl -X GET "http://localhost:8000/api/finance/wechat?startDate=2025-05-01&endDate=2025-05-16" \
  -H "Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.xxxxx"
```

### curl 示例：保存支付宝配置
```bash
curl -X POST "http://localhost:8000/api/finance/config" \
  -H "Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.xxxxx" \
  -H "Content-Type: application/json" \
  -d '{
    "type": "alipay",
    "appId": "2021000000000000",
    "privateKey": "MIIEvQIBADANBgkqhkiG...",
    "publicKey": "MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQC..."
  }'
```

### curl 示例：保存微信支付配置
```bash
curl -X POST "http://localhost:8000/api/finance/config" \
  -H "Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.xxxxx" \
  -H "Content-Type: application/json" \
  -d '{
    "type": "wechat",
    "appId": "wx1234567890abcdef",
    "mchId": "1900000000",
    "apiKey": "rEaLkEy20250516WeChatPay",
    "apiV3Key": "v3Key20250516Secure",
    "serialNo": "5A9B8C7D6E5F4G3H2J1K",
    "privateKey": "MIIEvQIBADANBgkqhkiG..."
  }'
```

**Section sources**
- [eadm_payment_controller.erl](file://src/controllers/eadm_payment_controller.erl#L70-L116)
- [api_payment.erl](file://src/apis/api_payment.erl#L41-L140)