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

应用启动后，日志会提示本地访问地址。端口以 `config/` 或 Docker 配置为准，默认 `8090`。

新 SolidJS 前端：

```powershell
cd frontend
npm install
npm run dev
```

前端开发服务默认使用 Vite `5173` 端口，并将 `/api` 代理到 `http://127.0.0.1:8090`。

如需让前端访问其它后端地址，可复制 `frontend/.env.example` 为 `frontend/.env.local` 并设置 `VITE_API_BASE`。

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

Cowboy 是主 HTTP 服务，负责提供 `/api/*` JSON API 和 SolidJS SPA。

前端构建：

```powershell
cd frontend
npm run build
```

迁移期自动化验证：

```powershell
.\script\verify-migration.ps1
```

完整自动化检查：

```powershell
.\script\test-all.ps1
```

该命令执行 Erlang warnings-as-errors 编译、EUnit、xref、Cowboy 迁移检查、ESLint、Vitest 覆盖率、前端构建、Playwright 浏览器冒烟和生产依赖审计。首次运行 E2E 前执行 `cd frontend; npx playwright install chromium`。Dialyzer 当前仍有存量告警，使用 `-RunDialyzer` 单独查看；CI 将其作为非阻断分析任务，债务清零后改为阻断。

只验证后端新增迁移模块：

```powershell
.\script\verify-migration.ps1 -SkipFrontend
```

## 主要开发入口

- 应用启动：`src/eadm_app.erl`
- 监督树：`src/eadm_sup.erl`
- 路由：`src/eadm_cowboy_http.erl`
- 认证授权：`src/eadm_cowboy_guard.erl`、`src/eadm_cowboy_session.erl`、`src/eadm_auth_service.erl`
- Cowboy Handler：`src/eadm_cowboy_*_handler.erl`
- 外部 API：`src/apis/`
- 前端工程：`frontend/`
- 数据库脚本：`script/`

## 新增页面或接口清单

1. 在 `src/eadm_cowboy_http.erl` 增加路由，并确认权限策略。
2. 在 `src/eadm_cowboy_*_handler.erl` 或对应 service 增加处理逻辑。
3. 如需页面，新增或更新 `frontend/src/routes/*.tsx`。
4. 如需交互或样式，优先放在 `frontend/src/` 对应组件和样式文件。
5. 如需数据结构，更新相关 `script/<db>/` 脚本和 wiki。
6. 运行 `rebar3 compile` 与 `npm run build`，必要时启动应用手工验证。

## 敏感模块检查

修改以下模块时需要额外谨慎：

- 登录、认证、权限：`eadm_cowboy_auth_handler`、`eadm_cowboy_guard`、`eadm_cowboy_session`
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
