# DNS 提供商配置指南

本文档详细说明各个 DNS 提供商的 API 配置方法。

## 目录

- [DNSPod](#dnspod)
- [阿里云 DNS](#阿里云-dns)
- [Cloudflare](#cloudflare)
- [腾讯云 DNS](#腾讯云-dns)
- [其他提供商](#其他提供商)

---

## DNSPod

### 配置示例

```erlang
{ssl_validation, dns},
{ssl_dns_provider, "dns_dp"},
{ssl_dns_credentials, [
    {"DP_Id", "123456"},           %% 你的 DNSPod ID
    {"DP_Key", "your_token_here"}  %% 你的 DNSPod Token
]},
```

### 获取 API 凭证

1. 登录 [DNSPod 控制台](https://console.dnspod.cn/)
2. 点击右上角头像 -> "用户中心"
3. 左侧菜单选择 "安全设置" -> "API Token"
4. 点击 "创建 Token"
5. 输入 Token 名称（如：eadm-ssl）
6. 创建后会显示：
   - **ID**：一串数字（如：123456）
   - **Token**：一串字符串

**重要提示：**
- Token 只显示一次，请立即复制保存
- 如果忘记 Token，需要删除旧的重新创建
- 建议为不同应用创建不同的 Token

### 权限要求

Token 需要以下权限：
- ✅ 域名解析记录管理
- ✅ 添加/删除 TXT 记录

### 测试配置

```bash
# 进入容器
docker exec -it eadm sh

# 设置环境变量
export DP_Id="123456"
export DP_Key="your_token_here"

# 测试申请（测试模式，不会真正申请）
. /root/.acme.sh/acme.sh.env
acme.sh --issue --dns dns_dp -d test.yourdomain.com --test
```

### 常见问题

**Q: Token 格式是什么？**
- ID：纯数字，如 `123456`
- Token：字母数字组合，如 `abc123def456`

**Q: 提示 "Invalid token"？**
- 检查 ID 和 Token 是否正确
- 确认 Token 未过期
- 检查 Token 权限是否足够

---

## 阿里云 DNS

### 配置示例

```erlang
{ssl_validation, dns},
{ssl_dns_provider, "dns_ali"},
{ssl_dns_credentials, [
    {"Ali_Key", "LTAI5txxxxxxxxxxxxx"},      %% AccessKey ID
    {"Ali_Secret", "xxxxxxxxxxxxxxxxxxxxxxx"} %% AccessKey Secret
]},
```

### 获取 API 凭证

1. 登录 [阿里云控制台](https://ram.console.aliyun.com/)
2. 进入 "访问控制 RAM" -> "用户"
3. 创建用户或选择现有用户
4. 点击 "创建 AccessKey"
5. 保存 AccessKey ID 和 AccessKey Secret

### 权限要求

需要授予以下权限策略：
- `AliyunDNSFullAccess`（完整权限）
- 或自定义策略：
  ```json
  {
    "Version": "1",
    "Statement": [
      {
        "Effect": "Allow",
        "Action": [
          "alidns:AddDomainRecord",
          "alidns:DeleteDomainRecord",
          "alidns:DescribeDomainRecords"
        ],
        "Resource": "*"
      }
    ]
  }
  ```

### 测试配置

```bash
export Ali_Key="LTAI5txxxxxxxxxxxxx"
export Ali_Secret="xxxxxxxxxxxxxxxxxxxxxxx"
. /root/.acme.sh/acme.sh.env
acme.sh --issue --dns dns_ali -d test.yourdomain.com --test
```

---

## Cloudflare

### 配置示例

**方式1：使用 API Token（推荐）**

```erlang
{ssl_validation, dns},
{ssl_dns_provider, "dns_cf"},
{ssl_dns_credentials, [
    {"CF_Token", "your_cloudflare_api_token"},
    {"CF_Account_ID", "your_account_id"},  %% 可选
    {"CF_Zone_ID", "your_zone_id"}         %% 可选
]},
```

**方式2：使用 Global API Key**

```erlang
{ssl_dns_credentials, [
    {"CF_Key", "your_global_api_key"},
    {"CF_Email", "your@email.com"}
]},
```

### 获取 API Token（推荐）

1. 登录 [Cloudflare Dashboard](https://dash.cloudflare.com/)
2. 点击右上角头像 -> "My Profile"
3. 左侧选择 "API Tokens"
4. 点击 "Create Token"
5. 使用模板 "Edit zone DNS" 或自定义
6. 权限设置：
   - Zone - DNS - Edit
   - Zone - Zone - Read
7. 选择要管理的域名
8. 创建并复制 Token

### 获取 Zone ID（可选）

1. 在 Cloudflare Dashboard 选择域名
2. 右侧 "API" 部分可以看到 Zone ID

### 测试配置

```bash
export CF_Token="your_token"
. /root/.acme.sh/acme.sh.env
acme.sh --issue --dns dns_cf -d test.yourdomain.com --test
```

---

## 腾讯云 DNS

### 配置示例

```erlang
{ssl_validation, dns},
{ssl_dns_provider, "dns_tencent"},
{ssl_dns_credentials, [
    {"Tencent_SecretId", "AKIDxxxxxxxxxxxxx"},
    {"Tencent_SecretKey", "xxxxxxxxxxxxxxxx"}
]},
```

### 获取 API 凭证

1. 登录 [腾讯云控制台](https://console.cloud.tencent.com/)
2. 进入 "访问管理" -> "访问密钥" -> "API密钥管理"
3. 点击 "新建密钥"
4. 保存 SecretId 和 SecretKey

### 权限要求

需要授予 DNS 解析相关权限：
- `QcloudDNSPodFullAccess`

### 测试配置

```bash
export Tencent_SecretId="AKIDxxxxxxxxxxxxx"
export Tencent_SecretKey="xxxxxxxxxxxxxxxx"
. /root/.acme.sh/acme.sh.env
acme.sh --issue --dns dns_tencent -d test.yourdomain.com --test
```

---

## 其他提供商

### GoDaddy

```erlang
{ssl_dns_provider, "dns_gd"},
{ssl_dns_credentials, [
    {"GD_Key", "your_api_key"},
    {"GD_Secret", "your_api_secret"}
]},
```

### AWS Route53

```erlang
{ssl_dns_provider, "dns_aws"},
{ssl_dns_credentials, [
    {"AWS_ACCESS_KEY_ID", "your_access_key"},
    {"AWS_SECRET_ACCESS_KEY", "your_secret_key"}
]},
```

### Google Cloud DNS

```erlang
{ssl_dns_provider, "dns_gcloud"},
{ssl_dns_credentials, [
    {"GCE_PROJECT", "your_project_id"},
    {"GCE_SERVICE_ACCOUNT_FILE", "/path/to/service-account.json"}
]},
```

### Azure DNS

```erlang
{ssl_dns_provider, "dns_azure"},
{ssl_dns_credentials, [
    {"AZUREDNS_SUBSCRIPTIONID", "your_subscription_id"},
    {"AZUREDNS_TENANTID", "your_tenant_id"},
    {"AZUREDNS_APPID", "your_app_id"},
    {"AZUREDNS_CLIENTSECRET", "your_client_secret"}
]},
```

---

## 完整提供商列表

acme.sh 支持 100+ DNS 提供商，完整列表请参考：
https://github.com/acmesh-official/acme.sh/wiki/dnsapi

常用提供商：

| 提供商 | dns_provider | 文档链接 |
|--------|--------------|----------|
| DNSPod | `dns_dp` | [文档](https://github.com/acmesh-official/acme.sh/wiki/dnsapi#dns_dp) |
| 阿里云 | `dns_ali` | [文档](https://github.com/acmesh-official/acme.sh/wiki/dnsapi#dns_ali) |
| Cloudflare | `dns_cf` | [文档](https://github.com/acmesh-official/acme.sh/wiki/dnsapi#dns_cf) |
| 腾讯云 | `dns_tencent` | [文档](https://github.com/acmesh-official/acme.sh/wiki/dnsapi#dns_tencent) |
| GoDaddy | `dns_gd` | [文档](https://github.com/acmesh-official/acme.sh/wiki/dnsapi#dns_gd) |
| AWS Route53 | `dns_aws` | [文档](https://github.com/acmesh-official/acme.sh/wiki/dnsapi#dns_aws) |
| Google Cloud | `dns_gcloud` | [文档](https://github.com/acmesh-official/acme.sh/wiki/dnsapi#dns_gcloud) |
| Azure | `dns_azure` | [文档](https://github.com/acmesh-official/acme.sh/wiki/dnsapi#dns_azure) |
| 华为云 | `dns_huaweicloud` | [文档](https://github.com/acmesh-official/acme.sh/wiki/dnsapi#dns_huaweicloud) |
| 百度云 | `dns_baidu` | [文档](https://github.com/acmesh-official/acme.sh/wiki/dnsapi#dns_baidu) |

---

## 安全建议

### 1. 使用最小权限原则

只授予 DNS 记录管理权限，不要使用根账号或全局管理员权限。

### 2. 定期轮换密钥

建议每 3-6 个月更换一次 API 密钥。

### 3. 分离环境

为不同环境（开发、测试、生产）使用不同的 API 密钥。

### 4. 监控 API 使用

启用 API 调用日志，监控异常访问。

### 5. 保护配置文件

```bash
# 设置正确的文件权限
chmod 600 config/sys.config
chmod 600 docker/sys.config

# 不要提交到版本控制
echo "config/sys.config" >> .gitignore
echo "docker/sys.config" >> .gitignore
```

---

## 故障排查

### 通用排查步骤

1. **检查凭证格式**
   ```bash
   # 进入容器
   docker exec -it eadm sh
   
   # 查看环境变量
   env | grep -E "DP_|Ali_|CF_|Tencent_"
   ```

2. **测试 DNS API**
   ```bash
   # 使用测试模式
   . /root/.acme.sh/acme.sh.env
   acme.sh --issue --dns dns_dp -d test.yourdomain.com --test --debug
   ```

3. **查看详细日志**
   ```bash
   cat /root/.acme.sh/acme.sh.log
   ```

### 常见错误

**错误：Invalid credentials**
- 检查 API 密钥是否正确
- 确认密钥未过期
- 验证权限是否足够

**错误：DNS record not found**
- DNS 传播需要时间（通常 1-5 分钟）
- 检查域名是否在该 DNS 提供商管理

**错误：Rate limit exceeded**
- 等待一段时间后重试
- 检查是否频繁申请证书

---

## 参考资源

- [acme.sh DNS API 文档](https://github.com/acmesh-official/acme.sh/wiki/dnsapi)
- [Let's Encrypt 文档](https://letsencrypt.org/docs/)
- [DNS 验证原理](https://letsencrypt.org/docs/challenge-types/#dns-01-challenge)
