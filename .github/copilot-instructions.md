# Copilot Instructions for EADM

先阅读根目录 `AGENTS.md`。当前系统使用 Erlang/OTP 27 + Cowboy，前端是 SolidJS + TypeScript + Vite；不要恢复 Nova、ErlyDTL、Bootstrap/jQuery、`src/controllers/` 或 `src/views/` 旧结构。

- Cowboy 路由：`src/eadm_cowboy_http.erl`。
- HTTP Handler：`src/eadm_cowboy_*_handler.erl`。
- 业务与查询：`src/eadm_*_service.erl`。
- 外部设备/支付 API：`src/apis/`。
- SolidJS 页面、组件和 API client：`frontend/src/`。
- PostgreSQL 基线：`script/postgres/datastruct.sql`。

新接口使用 `/api/v1/*`、`eadm_api_response`、camelCase JSON 和签名 Cookie session。Handler 保持轻量，权限、输入校验、参数化 SQL 和敏感日志脱敏不可省略。数据库设计遵循 `docs/postgresql-db-design.RULE.md`。

至少按改动执行：

```powershell
rebar3 compile
cd frontend
npm run build
```

迁移链路优先运行 `script/verify-migration.ps1`。接口、数据库、部署或页面行为变化时同步更新 `docs/` 或 `wiki/`；未实际运行的验证不得声称通过。
