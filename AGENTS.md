# EADM AI 开发入口

本文件是 Codex、Claude、Cursor、Copilot 等 AI 辅助工具的最小入口。它只记录必须先知道的事实和检查项；详细规则以链接文档为准，避免在多个文件中重复维护。

## 开始前必读

1. 阅读本文件并执行 `git status --short`，不要覆盖工作区中的既有改动。
2. 按任务阅读：
   - 架构与调用链：`docs/ARCHITECTURE.md`
   - 开发规范：`CONTRIBUTING.md`
   - 本地环境与命令：`docs/DEVELOPMENT.md`
   - API 约定：`docs/API_CONVENTIONS.md`
   - PostgreSQL 设计：`docs/postgresql-db-design.RULE.md`
   - 迁移状态与待办：`docs/COWBOY_SOLIDJS_STATUS.md`
3. 再阅读 `wiki/` 中与业务模块对应的文档。注意：部分 wiki 仍是 Nova、ErlyDTL、Bootstrap/jQuery 旧架构资料；若与代码冲突，以当前代码和上述 `docs/` 为准，并在本次改动中修正文档。

## 项目概览

- 项目名称：`eadm`
- 类型：个人后台管理系统
- 后端：Erlang/OTP 27 + rebar3 + Cowboy
- 前端：SolidJS + Vite
- 数据库：以 PostgreSQL/TiDB/MySQL 脚本为主，同时保留 Kingbase、Oracle、DB2 等脚本
- 部署：Docker、docker-compose、GitHub Actions

## 目录边界

- `src/`：Erlang OTP 应用、服务模块、Cowboy 路由和 Handler。
- `src/eadm_cowboy_http.erl`：Cowboy 主监听器与路由表。
- `src/eadm_cowboy_*_handler.erl`：页面 API Handler。
- `src/apis/`：外部 API 入口。
- `frontend/`：SolidJS 前端工程。
- `script/`：数据库初始化、迁移、辅助脚本。
- `config/`：本地和发布配置模板。
- `docker/`、`Dockerfile`、`docker-compose.yml`：容器运行配置。
- `release/`：发布脚本。
- `wiki/`：项目知识库和模块说明；迁移中的旧文档不能作为当前架构事实源。

## 常用命令

```powershell
rebar3 compile
cd frontend; npm run build
cd ..; .\script\verify-migration.ps1
.\script\test-all.ps1
rebar3 shell
rebar3 as prod release
docker compose up --build
```

说明：

- 完整测试入口是 `script/test-all.ps1`。后端改动至少执行 EUnit/xref，前端改动至少执行 ESLint/Vitest/Playwright/build；CI 另外验证 release 和 PostgreSQL 空库结构。
- 如果改了 Docker、发布、配置或数据库脚本，补充执行对应的 Docker 或数据库验证。
- 如果本地缺少 Erlang/rebar3，不要伪造验证结果，在回复中明确说明未运行。

## 编码规范

- 遵循 `.editorconfig`：UTF-8、LF、4 空格缩进、文件末尾保留换行。
- Erlang 模块命名沿用 `eadm_*`，HTTP Handler 命名沿用 `eadm_cowboy_*_handler`。
- Erlang 代码保持现有风格：模块头注释、`-author`、导出分组、函数注释可按周边文件补充。
- 路由集中维护在 `src/eadm_cowboy_http.erl`，新增业务接口时同步 Handler、service、前端和 wiki。
- 新接口统一使用 `/api/v1/*`、`eadm_api_response` 和 camelCase JSON；Handler 只负责 HTTP/鉴权/参数转换，SQL 与业务逻辑放 service。
- 前端页面和组件按功能拆分到 `frontend/src/`。
- 配置文件和示例配置不要写入真实密码、密钥、Token、Cookie、连接串。
- 数据库脚本涉及多数据库支持时，优先保持各数据库目录的结构一致。
- 不恢复 Nova、ErlyDTL、Bootstrap/jQuery 或 `src/controllers`、`src/views`、旧 `priv/assets/js` 架构。

## 安全与隐私

- 登录、权限、支付、健康数据、财务数据、设备轨迹属于敏感域，修改时默认按最小权限和输入校验处理。
- 不要把真实个人数据、账单、设备号、地理位置、支付配置写入仓库。
- 涉及 `eadm_cowboy_session`、登录态、Cookie、密码、支付回调、导入文件解析时，需要额外说明风险和验证方式。

## AI 修改约束

- 保持改动小而清晰，不做与任务无关的重构。
- 修改前先定位调用链和已有模式，优先复用现有 Handler、service、工具函数和 SolidJS 组件结构。
- 改动用户可见页面时，检查对应 SolidJS 路由、组件、样式和文案是否需要同步。
- 改动接口时，检查 `wiki/API 接口参考/` 是否需要更新。
- 改动数据库字段时，检查各数据库脚本、数据访问代码、导入导出逻辑、wiki 数据库文档。
- 输出结果时说明：改了哪些文件、跑了哪些验证、哪些验证没跑以及原因。
- 不把“能编译”等同于功能完成；接口改动应验证 HTTP 状态、响应结构、未登录与无权限分支，数据库改动应说明在哪些数据库上实际执行过。
