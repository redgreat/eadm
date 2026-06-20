# 本地开发指南

## 环境要求

- Erlang/OTP：项目配置要求 `27.2.3`，Docker 文档中记录过 `27.2.1`
- rebar3：建议 `3.24.0` 或更新的兼容版本
- Docker / Docker Compose：用于容器构建和联调
- 数据库：根据配置选择 PostgreSQL、TiDB/MySQL 或其他受支持数据库

Windows 本地可安装仓库内 rebar3：

```powershell
.\tools\install-rebar3.ps1
.\tools\rebar3.cmd compile *> rebar3-compile.log
```

`tools/rebar3` 是下载的本地二进制，已加入 `.gitignore`；`tools/rebar3.cmd` 和安装脚本用于复用。

## 快速开始

```powershell
rebar3 compile
rebar3 shell
```

应用启动后，日志会提示本地访问地址。端口以 `config/` 或 Docker 配置为准。

新 SolidJS 前端：

```powershell
cd frontend
npm install
npm run dev
```

前端开发服务默认使用 Vite `5173` 端口，并将 `/api`、`/login`、`/logout` 代理到 `http://127.0.0.1:8080`。

如需让前端直接访问可选 Cowboy listener，可复制 `frontend/.env.example` 为 `frontend/.env.local` 并设置：

```env
VITE_API_BASE=http://127.0.0.1:8081
```

## 配置文件

- `config/dev_sys.config.src`：本地 shell 使用的开发配置。
- `config/sys.config`、`config/vm.args`：运行配置。
- `config/db.config.sample`：数据库配置示例。
- `docker/sys.config`、`docker/vm.args`：容器内配置。

不要把真实密码、Token、Cookie、支付密钥或个人数据提交到配置文件。

## 编译与运行

```powershell
rebar3 compile
rebar3 shell
```

发布构建：

```powershell
cd frontend
npm run build
cd ..
rebar3 as prod release
```

Docker 运行：

```powershell
docker build -t eadm:migration .
docker compose up --build
```

Docker 构建会先执行前端 `npm ci` 和 `npm run build`，再把 `frontend/dist` 复制进 Erlang release 的 `priv/spa`。

迁移期可选 Cowboy 监听器默认关闭。需要并行验证 SolidJS SPA 静态托管时，可在配置中临时开启：

```erlang
{eadm, [
  {cowboy_enabled, true},
  {cowboy_port, 8081}
]}
```

开启后访问 `http://127.0.0.1:8081/app/`，该监听器只用于迁移期静态托管和原生 handler 验证。Docker 配置中预留端口为 `8091`。

前端构建：

```powershell
cd frontend
npm run build
```

迁移期自动化验证：

```powershell
.\script\verify-migration.ps1
```

只验证后端新增迁移模块：

```powershell
.\script\verify-migration.ps1 -SkipFrontend
```

## 主要开发入口

- 应用启动：`src/eadm_app.erl`
- 监督树：`src/eadm_sup.erl`
- 路由：`src/eadm_router.erl`
- 认证授权：`src/eadm_auth.erl`
- 控制器：`src/controllers/`
- 外部 API：`src/apis/`
- 页面模板：`src/views/`
- 前端脚本：`priv/assets/js/`
- 样式：`priv/assets/css/`
- 新前端工程：`frontend/`
- 数据库脚本：`script/`

## 新增页面或接口清单

1. 在 `src/eadm_router.erl` 增加路由，并确认 `security` 策略。
2. 在 `src/controllers/` 或 `src/apis/` 增加处理函数。
3. 如需页面，新增或更新 `src/views/*.dtl`。
4. 如需交互，新增或更新 `priv/assets/js/*.js`。
5. 如需样式，优先复用现有 CSS，再局部补充。
6. 如需文案国际化，更新 `priv/assets/i18n/`。
7. 如需数据结构，更新相关 `script/<db>/` 脚本和 wiki。
8. 运行 `rebar3 compile`，必要时启动应用手工验证。

## 敏感模块检查

修改以下模块时需要额外谨慎：

- 登录、认证、权限：`eadm_auth`、`eadm_login_controller`
- 支付：`eadm_payment_controller`、`eadm_wechat`
- 健康与轨迹：`eadm_health_controller`、`eadm_location_controller`、`api_watch`
- 财务导入：`eadm_finance_controller`、`eadm_xlsx`
- 定时任务：`eadm_crontab_controller`

关注点包括输入校验、权限边界、日志脱敏、错误返回和数据库兼容性。

## AI 协作建议

- 先读 `AGENTS.md`，再读本文件。
- 不确定业务含义时，先查 `wiki/`。
- 输出补丁前说明验证方式。
- 遇到缺少本地运行环境时，明确说明未验证项，不要假设通过。
