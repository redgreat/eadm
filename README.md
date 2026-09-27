# eadm

EADM 是一个基于 Erlang/OTP + Cowboy 的个人后台管理系统，前端使用 SolidJS，支持 Docker 部署和多数据库脚本维护。

## 项目入口

- 后端源码：`src/`
- Cowboy Handler：`src/eadm_cowboy_*_handler.erl`
- 前端工程：`frontend/`
- 数据库脚本：`script/`
- 运行配置：`config/`
- Docker 配置：`Dockerfile`、`docker-compose.yml`、`docker/`
- 项目文档：`wiki/`

## 快速开始

```powershell
rebar3 compile
rebar3 shell
```

Docker 运行：

```powershell
docker compose up --build
```

## 开发协作

- AI/Agent 指南：`AGENTS.md`
- 开发规范：`CONTRIBUTING.md`
- 本地开发指南：`docs/DEVELOPMENT.md`
- 当前架构：`docs/ARCHITECTURE.md`
- API 约定：`docs/API_CONVENTIONS.md`
- PostgreSQL 设计规范：`docs/postgresql-db-design.RULE.md`
- Cowboy/SolidJS 迁移状态：`docs/COWBOY_SOLIDJS_STATUS.md`
- Docker 说明：`README.Docker.md`

修改接口、数据库、部署或页面结构时，请同步更新相关文档。

运行完整自动化检查：

```powershell
.\script\test-all.ps1
```
