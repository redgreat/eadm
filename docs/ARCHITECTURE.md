# EADM 当前架构

本文记录仓库当前实现，是架构判断的事实入口。历史 wiki 中的 Nova、ErlyDTL、Bootstrap、jQuery、DataTables 描述尚未全部清理，不代表当前运行结构。

## 1. 技术栈

- 运行时：Erlang/OTP 27.2.3，rebar3。
- HTTP：Cowboy 2，监听器由 `eadm_cowboy_http` 启动并受 `eadm_sup` 监督。
- 前端：SolidJS + TypeScript + Vite + Tailwind CSS 4，构建产物为 `frontend/dist`。
- 数据访问：`epgsql` + `poolboy`，当前运行代码面向 PostgreSQL 协议。
- JSON：`thoas`。
- 日志与任务：`lager`、`ecron`。
- 部署：rebar3 release、Docker multi-stage build、docker compose、GitHub Actions。

Nova、ErlyDTL、旧 controller/view 及 Bootstrap/jQuery 前端已经退出当前运行链路，不应在新功能中恢复。

## 2. 启动与请求链路

```text
eadm_app
  -> eadm_sup
       -> poolboy 数据库连接池
       -> eadm_cowboy_http
            -> /api/v1/* -> eadm_cowboy_*_handler
                              -> eadm_cowboy_guard / eadm_cowboy_session
                              -> eadm_*_service
                              -> eadm_pgpool -> PostgreSQL
            -> /assets/* -> frontend/dist/assets
            -> 其他路径 -> eadm_spa_handler -> frontend/dist/index.html
```

`cowboy_enabled` 默认开启，`cowboy_port` 默认是 `8090`。路由唯一事实源是 `src/eadm_cowboy_http.erl`。

## 3. 后端分层

| 层 | 职责 | 主要位置 |
| --- | --- | --- |
| listener/router | 启动监听器、声明路由、托管静态文件 | `src/eadm_cowboy_http.erl` |
| handler | HTTP method、body/query 解析、鉴权、状态码与响应 | `src/eadm_cowboy_*_handler.erl` |
| guard/session | 签名 Cookie、登录态和权限判断 | `src/eadm_cowboy_guard.erl`、`src/eadm_cowboy_session.erl` |
| service | 业务规则、查询组织、数据库结果到 API DTO 的转换 | `src/eadm_*_service.erl` |
| data access | 连接池、参数化 SQL 执行 | `src/eadm_pgpool*.erl` |
| external API | 手表、支付等外部协议入口 | `src/apis/` |

新增接口时保持 handler 薄：不要把大段 SQL、权限 JSON 解析或业务分支重新堆入 handler。数据库列名可保持现状，API 边界转换为 camelCase。

## 4. 前端结构

- 路由入口：`frontend/src/main.tsx`。
- 应用外壳：`frontend/src/app/App.tsx`。
- 页面：`frontend/src/routes/`。
- 布局与可复用组件：`frontend/src/components/`。
- API client：`frontend/src/lib/api/`，统一经 `client.ts` 请求 `/api/v1`。
- 全局样式：`frontend/src/styles/`。

SolidJS SPA 与 API 同源部署；开发期由 Vite 代理到 Cowboy。前端不得直接依赖数据库字段、拼接后端 SQL 或复制鉴权规则。

## 5. 数据与配置

- PostgreSQL 主结构：`script/postgres/datastruct.sql`。
- PostgreSQL 配套初始化、过程与任务：`script/postgres/` 其余脚本。
- MySQL/TiDB、Kingbase、Oracle、DB2 脚本是兼容资产，但覆盖范围并不完全一致。
- 数据库连接由外部 `prod_db` 配置注入；示例配置只能使用无效占位值。
- Session 密钥、推送 Token、支付配置和数据库密码必须由环境或部署配置提供，不得提交真实值。

数据库设计与变更规则见 `docs/postgresql-db-design.RULE.md`。

## 6. 当前边界

- 当前 `/api/v1` 业务资源多数只有读取接口；不能从页面存在推断 CRUD 已完成。
- Cookie 登录已实现，但必须在生产环境验证 `Secure`、HTTPS、密钥轮换和跨站策略。
- SPA 构建已接入 Docker/release；CI 是否在 release 前显式构建前端仍需逐条核验。
- wiki 大量页面仍引用已删除路径，迁移状态见 `docs/COWBOY_SOLIDJS_STATUS.md`。
