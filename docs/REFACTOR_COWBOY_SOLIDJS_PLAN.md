# Nova 到 Cowboy、Bootstrap/jQuery 到 SolidJS 重构计划

## 目标

将 EADM 从当前的 Nova 后端框架和 ErlyDTL/Bootstrap/jQuery 前端，逐步重构为：

- 后端：纯 Cowboy + Erlang/OTP
- 前端：SolidJS 管理后台
- 接口：前后端分离的 JSON API
- 部署：保留 Docker/rebar3 发布能力，新增前端构建产物集成

本计划优先保证可分阶段迁移，避免一次性重写导致功能不可用。

## 推荐技术选型

### 后端

- Cowboy：作为 HTTP 服务和路由入口。
- Erlang/OTP supervision tree：保留现有 OTP 应用结构。
- jsx 或 jiffy：JSON 编解码。优先选一个维护活跃、项目易编译的库。
- lager 或 logger：短期保留 lager，后续可评估迁移 OTP logger。
- poolboy + epgsql：短期保留现有数据库连接方式。

### 前端

推荐组合：

- SolidJS 或 SolidStart
- TypeScript
- Vite
- Tailwind CSS
- Solid UI + Kobalte
- TanStack Table 或自封装表格
- Chart.js 或 ECharts
- fetch/自封装 API client

说明：

- 如果前端只作为纯 SPA，由 Cowboy 托管静态文件，选择 `SolidJS + Vite` 即可。
- 如果未来需要 SSR、文件路由、服务端渲染或更完整的前端工程能力，选择 `SolidStart`。
- 本项目是后台系统，推荐第一阶段用 `SolidJS + Vite SPA`，架构简单，和 Erlang/Cowboy 边界清晰。

### Admin UI 推荐

首选：`Tailwind CSS + Solid UI + Kobalte`

- Solid UI 提供 shadcn/ui 风格的复制式组件，适合后台系统做按钮、表单、弹窗、卡片、菜单。
- Kobalte 提供可访问性良好的底层组件，适合补齐 Dialog、Select、Tabs、Dropdown 等交互。
- Tailwind 方便从现有 Bootstrap 页面逐步复刻布局。

备选：`Tailwind CSS + daisyUI`

- 适合快速搭建后台页面和主题。
- 组件语义类名简单，迁移成本低。
- 但复杂交互和可访问性控制不如 Kobalte/Solid UI 精细。

不建议作为主选：

- 继续使用 Bootstrap：迁移 SolidJS 的收益会被旧样式体系牵制。
- 引入重型企业级 React Admin 方案：Solid 生态适配成本高。

## 现状拆解

当前项目主要耦合点：

- `src/eadm_router.erl`：Nova 路由，包含页面、接口和静态资源路由。
- `src/controllers/`：控制器同时承担页面渲染和 JSON/API 返回。
- `src/views/`：ErlyDTL 模板。
- `priv/assets/js/`：jQuery 页面逻辑，和模板 DOM 强耦合。
- `priv/assets/vendor/`：Bootstrap、DataTables、Chart.js、jQuery 等前端依赖。
- `config/sys.config`、`config/dev_sys.config.src`：Nova/Cowboy 相关启动配置。
- `rebar.config`：Nova、rebar3_nova、ErlyDTL 插件依赖。

## 迁移原则

- 先抽 API，再换前端，再替换 Nova。
- 每个阶段都能编译和启动。
- 旧页面和新前端可短期共存。
- 数据库访问、认证、业务逻辑尽量从控制器中下沉为服务函数。
- 先迁移高价值、低耦合页面，再迁移复杂导入、地图、支付等模块。

## 阶段 0：准备与基线

目标：建立可验证的重构基线。

任务：

1. 补齐开发文档和 AI 协作规则。
2. 确认本地 Erlang/OTP、rebar3、Node.js、pnpm/npm 环境。
3. 记录当前页面和接口清单。
4. 为核心页面建立手工验收清单。
5. 确认 Docker 当前能构建和启动。

验收：

- `rebar3 compile` 通过。
- 当前主干功能可启动。
- 有页面/API 迁移清单。

## 阶段 1：API 分层

目标：把页面控制器里的业务能力整理为 JSON API。

任务：

1. 梳理现有路由，区分页面路由、JSON 查询、写操作、外部开放 API。
2. 统一 API 返回结构，例如：

```json
{
  "success": true,
  "data": {},
  "message": ""
}
```

3. 新增或整理 API 命名空间：

```text
/api/auth/*
/api/dashboard/*
/api/health/*
/api/location/*
/api/finance/*
/api/crontab/*
/api/users/*
/api/roles/*
/api/devices/*
/api/system/*
```

4. 将控制器中的数据查询、参数处理、业务逻辑拆到服务模块。
5. 保留旧页面路由，确保旧前端仍可使用。

验收：

- 核心列表/详情/新增/编辑/删除 API 可通过 curl 或 Postman 验证。
- 旧页面不受影响。
- API 文档同步更新。

## 阶段 2：SolidJS 前端骨架

目标：新增独立前端工程，不立即移除旧前端。

建议目录：

```text
frontend/
  package.json
  vite.config.ts
  tsconfig.json
  src/
    app/
    components/
    layouts/
    routes/
    lib/
    styles/
```

任务：

1. 初始化 SolidJS + Vite + TypeScript。
2. 配置 Tailwind CSS。
3. 引入 Solid UI/Kobalte 基础组件。
4. 建立后台布局：登录页、侧边栏、顶部栏、内容区、面包屑。
5. 建立 API client、认证状态、路由守卫。
6. 先迁移登录页和 Dashboard。

验收：

- `npm run dev` 或 `pnpm dev` 可启动前端。
- 登录和 Dashboard 能走新 API。
- 新前端可通过代理访问 Erlang API。

## 阶段 3：业务页面迁移

推荐顺序：

1. 系统信息、进程、端口、表查看等只读页面。
2. 用户、角色、权限。
3. 设备管理。
4. 健康数据。
5. 财务数据和 Excel 导入。
6. 轨迹地图。
7. 定时任务。
8. 支付配置。

每个模块迁移清单：

- API 是否稳定。
- 表格列、搜索项、分页、排序是否一致。
- 新增/编辑/删除是否有权限和错误提示。
- i18n 文案是否迁移。
- 旧页面是否需要保留跳转。

验收：

- 每个模块有手工验收记录。
- 旧模板对应页面可以逐步下线。

## 阶段 4：Nova 替换为纯 Cowboy

目标：移除 Nova 路由和模板渲染依赖。

建议 Cowboy 结构：

```text
src/
  eadm_app.erl
  eadm_sup.erl
  eadm_http.erl
  eadm_router.erl
  eadm_json.erl
  eadm_handlers/
    eadm_auth_handler.erl
    eadm_dashboard_handler.erl
    eadm_user_handler.erl
```

任务：

1. 在 supervision tree 中启动 Cowboy listener。
2. 使用 `cowboy_router:compile/1` 定义路由。
3. 用 Cowboy handler 或 REST handler 替代 Nova controller。
4. 新增统一请求解析和 JSON 响应模块。
5. 处理 Cookie/session 或改为 Token/session API。
6. 托管 `frontend/dist` 静态资源。
7. 将未知前端路由 fallback 到 `index.html`。
8. 从 `rebar.config` 移除 Nova、rebar3_nova、ErlyDTL 相关依赖。

验收：

- 不依赖 Nova 也能启动 HTTP 服务。
- 新 SolidJS 前端能由 Cowboy 托管。
- API、静态资源、登录态正常。
- `rebar3 compile` 和 release 构建通过。

## 阶段 5：清理旧前端和文档

任务：

1. 移除不再使用的 `src/views/` 模板。
2. 移除不再使用的 Bootstrap/jQuery/DataTables vendor 文件。
3. 清理旧 JS/CSS。
4. 更新 Dockerfile，将前端构建纳入镜像。
5. 更新 GitHub Actions。
6. 更新 `wiki/` 架构、部署、接口文档。

验收：

- 镜像构建成功。
- 发布包启动成功。
- 文档能指导新开发者启动前后端。

## 风险与处理

- 认证迁移风险：先保留 Cookie/session 方案，待 API 稳定后再考虑 Token。
- 页面一次性重写风险：按模块迁移，新旧并存。
- 表格能力缺口：优先评估 TanStack Table，复杂场景再封装。
- 地图和文件导入复杂：后置迁移，先迁移只读和 CRUD 页面。
- 多数据库脚本影响：后端重构时不要顺手改 schema。
- Docker 构建变慢：前端构建单独分层缓存。

## 建议里程碑

1. M1：API 清单和 SolidJS skeleton 完成。
2. M2：登录、Dashboard、系统信息迁移完成。
3. M3：用户、角色、设备迁移完成。
4. M4：健康、财务、定时任务迁移完成。
5. M5：Cowboy 替换 Nova 完成。
6. M6：删除旧模板和旧前端依赖，完成 Docker/CI 文档收尾。

## 第一批建议提交

1. `docs: 增加 Nova 到 Cowboy 和 SolidJS 重构计划`
2. `refactor: 整理 API 返回结构和公共 JSON 工具`
3. `feat: 新增 SolidJS 前端工程骨架`
4. `feat: 迁移登录和 Dashboard 到 SolidJS`
5. `refactor: 引入 Cowboy 并并行提供 API`

## 当前进展

- 已建立 `eadm_api_response`，用于新 API 的统一响应结构。
- 已新增 `GET /api/auth/me`，作为 SolidJS 前端登录态初始化接口。
- 已新增 `POST /api/auth/login` 和 `POST /api/auth/logout`，新前端登录页已接入。
- 已新增 `GET /api/dashboard/summary`，新前端仪表盘已接入结构化首页数据。
- 已新增 `GET /api/users`，新前端用户列表页已接入。
- 已新增 `GET /api/roles`，新前端角色列表页已接入。
- 已新增 `GET /api/devices`，新前端设备列表页已接入。
- 已新增 `GET /api/health`，新前端健康数据页已接入。
- 已新增 `GET /api/location`，新前端轨迹位置页已接入表格查询。
- 已新增 `GET /api/finance`，新前端财务数据页已接入列表查询。
- 已新增 `GET /api/crontabs`，新前端定时任务页已接入列表查询。
- 已新增 `GET /api/system/info`，新前端系统信息页已接入。
- 已新增 `frontend/` SolidJS + Vite + TypeScript + Tailwind CSS 4 工程骨架。
- Docker 构建已增加前端构建阶段，release 会复制 `frontend/dist` 到 `priv/spa`。
- 已新增 `eadm_spa_handler`，为后续纯 Cowboy 托管 SolidJS SPA 做准备。
- 已新增可选 `eadm_cowboy_http` 监听器，默认关闭，可在独立端口验证 SolidJS SPA 静态托管。
- 已新增原生 Cowboy `GET /api/ping`，作为后续 API handler 迁移的最小证明点。
- 已抽取 `eadm_system_service`，Nova `/api/system/info` 与原生 Cowboy `/api/internal/system/info` 共用系统信息逻辑。
- 已抽取 `eadm_auth_service`，Nova auth API 与未来 Cowboy 登录/session handler 可共用认证逻辑。
- 已抽取 `eadm_dashboard_service`，Dashboard 数据查询逻辑可被 Nova 和未来 Cowboy handler 复用。
- 已抽取 `eadm_user_service`，用户列表查询逻辑可被 Nova 和未来 Cowboy handler 复用。
- 已抽取 `eadm_role_service`，角色列表查询逻辑可被 Nova 和未来 Cowboy handler 复用。
- 已抽取 `eadm_device_service` 和 `eadm_health_service`，设备与健康数据查询逻辑开始脱离 Nova controller。
- 已抽取 `eadm_location_service` 和 `eadm_finance_service`，轨迹与财务查询逻辑开始脱离 Nova controller。
- 已抽取 `eadm_crontab_service`，定时任务列表查询逻辑开始脱离 Nova controller。
- 已新增 `eadm_cowboy_req`，开始沉淀原生 Cowboy handler 的请求解析工具。
- 已新增原生 Cowboy internal users/roles handler，验证 service 可被 Cowboy 直接复用。
- 已新增原生 Cowboy internal devices/crontabs handler，继续验证带查询参数的 service 复用。
- 已新增原生 Cowboy internal health/location/finance handler，查询型 service 已基本具备 Cowboy 复用入口。
- 已新增 `eadm_cowboy_session`，为原生 Cowboy 登录态提供签名 Cookie 基础能力。
- 已新增原生 Cowboy internal auth handler，验证签名 Cookie 登录/退出/当前用户链路。
- 可选 Cowboy listener 已挂载正式 `/api/auth/*` 和主要只读 `/api/*` 路径，正式路径会检查签名 Cookie 与权限，internal 路径继续用于迁移验证。
- SolidJS API client 已支持 `VITE_API_BASE`，可在开发时切换到可选 Cowboy listener。
- 已完成基础后台布局、登录页占位、Dashboard 占位和 API client。
- 已新增 `script/verify-migration.ps1`，用于迁移期自动化验证。
