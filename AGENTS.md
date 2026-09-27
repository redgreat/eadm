# EADM Agent Guide

本文件给 Codex、Claude、Cursor、Copilot 等 AI 辅助工具读取。开始修改前先阅读本文件，再按任务需要阅读 `README.md`、`CONTRIBUTING.md`、`docs/DEVELOPMENT.md` 和 `wiki/` 中的对应文档。

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
- `wiki/`：项目知识库和模块说明，改动功能时优先补充对应文档。

## 常用命令

```powershell
rebar3 compile
rebar3 shell
rebar3 as prod release
docker compose up --build
```

说明：

- 当前仓库没有稳定的自动化测试约定，修改后至少执行 `rebar3 compile`。
- 如果改了 Docker、发布、配置或数据库脚本，补充执行对应的 Docker 或数据库验证。
- 如果本地缺少 Erlang/rebar3，不要伪造验证结果，在回复中明确说明未运行。

## 编码规范

- 遵循 `.editorconfig`：UTF-8、LF、4 空格缩进、文件末尾保留换行。
- Erlang 模块命名沿用 `eadm_*`，HTTP Handler 命名沿用 `eadm_cowboy_*_handler`。
- Erlang 代码保持现有风格：模块头注释、`-author`、导出分组、函数注释可按周边文件补充。
- 路由集中维护在 `src/eadm_cowboy_http.erl`，新增业务接口时同步 Handler、service、前端和 wiki。
- 前端页面和组件按功能拆分到 `frontend/src/`。
- 配置文件和示例配置不要写入真实密码、密钥、Token、Cookie、连接串。
- 数据库脚本涉及多数据库支持时，优先保持各数据库目录的结构一致。

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
