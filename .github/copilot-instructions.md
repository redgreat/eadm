# Copilot Instructions for EADM

EADM 是 Erlang/OTP + Nova 的个人后台管理系统，前端使用 Bootstrap 5、jQuery、DataTables 和 ErlyDTL 模板。

请遵守：

- 后端代码位于 `src/`，控制器位于 `src/controllers/`，外部 API 位于 `src/apis/`。
- 路由统一在 `src/eadm_router.erl` 中注册。
- 模板位于 `src/views/`，静态资源位于 `priv/assets/`。
- 不要修改 `priv/assets/vendor/` 中的第三方库，除非明确是在升级依赖。
- Erlang 代码使用 4 空格缩进，沿用现有模块头、函数分组和命名风格。
- 新增控制器函数时，优先参考同类模块的参数获取、返回格式、日志和错误处理方式。
- 前端页面脚本按业务文件拆分，公共逻辑放入已有公共工具模块。
- 不要生成真实密钥、密码、Token、Cookie、个人账单、设备轨迹或支付配置。
- 修改接口、数据库、部署流程时，同步更新 `wiki/` 或根目录文档。

常用验证命令：

```powershell
rebar3 compile
rebar3 shell
rebar3 as prod release
docker compose up --build
```

当前没有统一测试套件。至少保证相关 Erlang 代码能通过 `rebar3 compile`。
