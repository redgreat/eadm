# Cowboy + SolidJS 迁移状态与待办

更新时间：2026-09-27。状态基于当前仓库代码、构建配置和文档静态核对；未连接真实数据库、未做浏览器端到端验收的项目不标记为“已验证”。

## 已落地

- Cowboy 成为默认 HTTP 入口，Nova、rebar3_nova、ErlyDTL 依赖已从 `rebar.config` 移除。
- `eadm_sup` 默认监督 Cowboy listener，集中路由位于 `eadm_cowboy_http.erl`。
- API 已统一到 `/api/v1`，具备响应封装、签名 Cookie session 和权限 guard。
- Dashboard、用户、角色、设备、健康、轨迹、财务、定时任务、系统信息已有只读 service、handler 和 SolidJS 页面。
- SolidJS/Vite 工程、统一 API client、后台布局和 SPA fallback 已建立。
- Docker 构建与 release overlay 已包含 `frontend/dist`。
- 旧模板、旧业务 JS/CSS 和大部分 vendor 静态资源已从工作树移除。
- 已建立 EUnit、Cowboy HTTP 冒烟、Vitest、ESLint、覆盖率、xref、依赖审计和统一测试脚本。
- CI 已包含后端/前端测试、release 验证、PostgreSQL 空库基线验证、依赖审查和 CodeQL。

## P0：发布前必须完成

- [ ] 在真实 PostgreSQL 数据库执行登录及全部 `/api/v1` 查询接口的冒烟测试，覆盖正常、未登录、无权限、非法参数和数据库异常。
- [ ] 做浏览器端到端验收：登录/退出、刷新后登录态、菜单权限、查询筛选、空数据、错误提示、移动宽度布局和 SPA 深链接刷新。
- [ ] 清除配置文件中的真实或疑似真实密钥、Token、Cookie 签名密钥和数据库密码；改用环境/挂载配置并轮换已暴露值。
- [x] 修正 CI release 流程：在 `rebar3 as prod release` 前安装 Node 依赖并执行 `npm run build`，确保 `frontend/dist` 不是依赖开发机残留。
- [ ] 验证 `rebar3 as prod release`、`docker compose up --build`、容器健康检查以及静态资源缓存/SPA fallback。

## P1：功能迁移缺口

- [ ] 用户、角色、权限和设备页面补齐新增、编辑、删除/停用及服务端校验；当前主要是列表读取。
- [ ] 定时任务补齐创建、编辑、启停、立即执行、执行日志与调度器状态一致性。
- [ ] 财务导入恢复并验收微信、支付宝；明确青岛银行、中国银行格式支持范围，补齐样例文件管理和导入错误报告。
- [ ] 轨迹页恢复地图轨迹回放，而不只是坐标表格；外部地图 Key 必须走部署配置。
- [ ] Dashboard 补齐图表交互、空数据与异常态，并核对存储过程/调度产出的数据口径。
- [ ] 支付相关外部 API 明确是否保留；若保留，补齐鉴权、签名、幂等、回调重放防护和文档。
- [ ] 国际化需求重新定级；旧 jQuery i18n 文件已移除，SolidJS 尚无对应体系。

## P1：质量与安全

- [ ] 扩展后端数据库集成测试，覆盖 service 参数边界和写事务；当前已覆盖 session、API response、坐标转换及 Cowboy ping。
- [x] 建立前端 Vitest、ESLint、覆盖率门槛和 Playwright 登录/仪表盘浏览器冒烟测试。
- [ ] 清理现有 Dialyzer 技术债务并将 CI job 从非阻断改为阻断；xref 已作为阻断检查。
- [x] 将 `rebar.lock` 纳入版本控制，确保 CI 使用当前锁定的 Erlang 依赖提交；后续仍建议把 `rebar.config` 的分支/`.*` 声明改为明确版本。
- [ ] 为 Cookie 明确 `Secure`、`HttpOnly`、`SameSite`、有效期、注销失效与密钥轮换策略，并增加 CSRF 威胁评估。
- [ ] 检查所有动态 SQL，只允许白名单控制列名/排序，值参数必须走 epgsql 参数绑定。
- [ ] 为敏感数据日志制定脱敏规则，尤其是认证、支付、健康、财务和轨迹。

## P2：文档与兼容资产

- [ ] 重写仍指向 Nova/controller/view/jQuery/DataTables 的 wiki 页面；优先处理后端架构、前端架构、部署、API 和核心模块。
- [ ] 更新 `.github/copilot-instructions.md`，使其指向 Cowboy、service/handler 与 SolidJS。
- [ ] 将旧 `TODO.TXT` 归档或改为链接本文；其中 `base.dtl`、`foot.dtl`、jQuery 学习等事项已失效。
- [ ] 明确多数据库策略：当前运行层固定使用 epgsql，而 MySQL/TiDB/Kingbase/Oracle/DB2 脚本覆盖不一致。决定是“正式支持”还是“历史参考”。
- [ ] 若继续支持多数据库，为每种数据库建立结构差异表和可执行验证；若不支持，在 README 和脚本目录标为非运行时支持。

## 完成定义

一项迁移任务只有在代码、文档和对应验证同时完成后才可勾选。页面能渲染、项目能编译或接口返回 200 都不能单独代表业务迁移完成。
