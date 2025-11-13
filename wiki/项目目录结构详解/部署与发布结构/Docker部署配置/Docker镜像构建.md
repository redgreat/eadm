<cite>
**本文档引用的文件**   
- [Dockerfile](file://Dockerfile)
- [docker/docker-entrypoint.sh](file://docker/docker-entrypoint.sh)
- [release/dockerbuild.sh](file://release/dockerbuild.sh)
- [README.Docker.md](file://README.Docker.md)
</cite>

## 目录
1. [简介](#简介)
2. [项目结构](#项目结构)
3. [多阶段构建流程](#多阶段构建流程)
4. [镜像优化策略](#镜像优化策略)
5. [运行时机制分析](#运行时机制分析)
6. [元数据与标签管理](#元数据与标签管理)
7. [构建与部署实践](#构建与部署实践)
8. [总结](#总结)

## 简介
本文档详细解析 `eadm` 项目的 Docker 镜像构建流程，重点围绕其多阶段构建策略、镜像优化手段、运行时初始化机制及元数据管理。该系统为基于 Erlang 的个人后台管理系统，采用 Nova 框架与 TiDB 数据库，通过 Docker 实现轻量级容器化部署。

## 项目结构
项目采用典型的 Erlang 应用结构，核心组件包括：
- `src/`：Erlang 源码，包含控制器、应用逻辑与工具模块
- `priv/assets/`：前端静态资源（CSS、JS、国际化脚本）
- `docker/`：Docker 相关脚本，含入口点脚本
- `release/`：发布与构建脚本
- 根目录：`Dockerfile`、`docker-compose.yml` 及文档

**Section sources**
- [Dockerfile](file://Dockerfile)
- [docker/docker-entrypoint.sh](file://docker/docker-entrypoint.sh)
- [release/dockerbuild.sh](file://release/dockerbuild.sh)

## 多阶段构建流程

### 第一阶段：编译构建（builder）
第一阶段使用 `erlang:27.2.3-alpine` 镜像作为基础环境，专用于编译 Erlang 应用。该阶段执行以下关键操作：
1. 设置工作目录 `/eadmbuild`
2. 复制整个项目源码至容器
3. 安装 Git 工具以支持依赖拉取
4. 执行 `rebar3 as prod release` 命令，生成生产环境的 Erlang 发布包（release）

此阶段封装了完整的编译环境，确保依赖解析与代码编译在隔离环境中完成。

### 第二阶段：运行时构建
第二阶段基于极简的 `alpine:3.21` 镜像构建最终运行环境，实现运行时与编译环境的完全分离。主要步骤包括：
1. 设置运行目录 `/opt/eadm`
2. 定义环境变量（时区、语言、IPv6 状态）
3. 使用 `--no-cache` 安装必要运行时库（`ncurses-libs`, `libgcc`, `libstdc++`）
4. 从测试仓库安装 `gosu` 用于用户切换
5. 从第一阶段复制编译生成的发布包及入口脚本
6. 设置入口脚本权限

通过多阶段构建，最终镜像仅包含运行所需文件，显著减小体积。

**Section sources**
- [Dockerfile](file://Dockerfile#L1-L42)

## 镜像优化策略

### 轻量级基础镜像
采用 Alpine Linux 作为基础镜像，因其极小的体积（通常 < 10MB）和良好的安全性，是容器化应用的首选。

### 分层构建与体积控制
通过多阶段构建（multi-stage build），将编译环境与运行环境分离。编译阶段的 Erlang 镜像（包含 SDK、编译器等）不包含在最终镜像中，仅复制编译产物，有效减少镜像体积。

### 无缓存安装
在 `RUN apk add` 命令中使用 `--no-cache` 参数，确保安装的软件包不保留索引缓存，避免不必要的磁盘占用。例如：
```dockerfile
RUN apk add --no-cache ncurses-libs libgcc libstdc++
```

### 依赖最小化
仅安装运行 Erlang 发布包所必需的库：
- `ncurses-libs`：支持终端界面输出
- `libgcc` 和 `libstdc++`：C/C++ 运行时支持
- `dumb-init`：作为 PID 1 进程处理信号
- `gosu`：安全的用户切换工具

**Section sources**
- [Dockerfile](file://Dockerfile#L15-L30)

## 运行时机制分析

### dumb-init 的作用
`dumb-init` 是一个简单的进程初始化系统（init system），用于在容器中作为 PID 1 进程运行。其核心作用包括：
- **信号转发**：正确接收并转发 SIGTERM、SIGINT 等信号至子进程，确保应用能优雅关闭
- **僵尸进程回收**：防止子进程成为僵尸进程，避免资源泄漏
- **简化容器生命周期管理**：无需复杂的 init 系统，保持容器轻量化

在 `Dockerfile` 中，`dumb-init` 被配置为入口点前缀：
```dockerfile
ENTRYPOINT ["/usr/bin/dumb-init", "-c", "--", "/opt/eadm/docker/docker-entrypoint.sh"]
```

### 入口脚本（docker-entrypoint.sh）功能
该脚本负责容器启动时的初始化工作：
1. **用户与组创建**：根据配置文件权限动态创建 `eadm` 用户，避免以 root 运行
2. **权限设置**：将应用目录所有权赋予 `eadm` 用户
3. **进程启动**：使用 `gosu` 切换至 `eadm` 用户并以前台模式启动 Erlang 应用

`gosu` 提供了比 `su` 更安全的用户切换机制，特别适用于容器环境。

**Section sources**
- [Dockerfile](file://Dockerfile#L26-L39)
- [docker/docker-entrypoint.sh](file://docker/docker-entrypoint.sh)

## 元数据与标签管理
`Dockerfile` 使用 `LABEL` 指令为镜像添加结构化元数据，便于管理和追溯：
- `org.label-schema.name`：镜像名称
- `org.label-schema.description`：功能描述
- `org.label-schema.version`：版本号，支持构建时注入
- `org.label-schema.vcs-url`：源码仓库地址
- `org.label-schema.maintainer`：维护者信息
- `org.opencontainers.image.source`：符合 OCI 规范的源码地址

这些标签可通过 `docker inspect` 命令查看，提升镜像的可追溯性与可管理性。

**Section sources**
- [Dockerfile](file://Dockerfile#L32-L40)

## 构建与部署实践

### 构建脚本（dockerbuild.sh）
`release/dockerbuild.sh` 提供了完整的构建、清理与运行流程：
1. 预拉取基础镜像
2. 清理旧容器与镜像
3. 执行 `docker build -t eadm .` 构建镜像
4. 启动容器并映射端口
5. 输出日志便于调试

该脚本简化了本地构建与测试流程。

### 部署命令
`README.Docker.md` 提供了生产环境部署示例，包含资源限制（内存、CPU）、重启策略、卷挂载等最佳实践配置。

**Section sources**
- [release/dockerbuild.sh](file://release/dockerbuild.sh)
- [README.Docker.md](file://README.Docker.md)

## 总结
`eadm` 项目的 Docker 构建流程体现了现代容器化应用的最佳实践：通过多阶段构建实现编译与运行环境分离，利用 Alpine 镜像与无缓存安装优化体积，结合 `dumb-init` 与 `gosu` 确保运行时安全与稳定性，并通过标准化标签提升可管理性。整体设计简洁高效，适合 Erlang 应用的容器化部署。