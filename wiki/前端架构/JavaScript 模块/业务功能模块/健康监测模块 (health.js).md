
# 健康监测模块 (health.js)

<cite>
**本文档引用的文件**  
- [health.js](file://priv\assets\js\health.js)
- [eadm_health_controller.erl](file://src\controllers\eadm_health_controller.erl)
- [api_watch.erl](file://src\apis\api_watch.erl)
- [i18n-health.js](file://priv\assets\i18n\i18n-health.js)
</cite>

## 目录
1. [简介](#简介)
2. [核心功能分析](#核心功能分析)
3. [健康数据获取机制](#健康数据获取机制)
4. [前端可视化呈现](#前端可视化呈现)
5. [异常状态告警逻辑](#异常状态告警逻辑)
6. [多节点数据聚合策略](#多节点数据聚合策略)
7. [失败重试与用户提示](#失败重试与用户提示)
8. [前后端协同监控机制](#前后端协同监控机制)
9. [结论](#结论)

## 简介
本模块 `health.js` 是系统健康状态前端展示的核心组件，负责从后端获取用户的健康监测数据，并以表格形式进行可视化呈现。该模块通过调用 `eadm_health_controller.erl` 提供的接口，查询步数、心率、体温、血压、睡眠、电量与信号强度等多维度健康指标。同时，前端与后端探针 `api_watch.erl` 协同工作，实现端到端的健康数据采集与展示闭环。

## 核心功能分析

### 健康数据加载与渲染
`health.js` 模块通过 `loadHealthData(dataType, startTime, endTime)` 函数实现健康数据的动态加载。该函数构造包含数据类型和时间范围的查询参数，通过 `$.getJSON('/health', searchParams)` 向后端 `/health` 接口发起异步请求，获取指定时间段内的健康数据。

**Section sources**
- [health.js](file://priv\assets\js\health.js#L45-L74)

### 国际化支持
模块通过 `translateColumnNames()` 和 `translateSleepType()` 函数实现界面的国际化。前者根据 `i18n-health.js` 中的配置将英文列名（如 "utctime"）转换为中文（如 "时间"）；后者将睡眠类型编码（1, 2, 3）转换为对应的中文描述（"深度睡眠", "浅度睡眠", "醒来时长"）。

**Section sources**
- [health.js](file://priv\assets\js\health.js#L1-L44)
- [i18n-health.js](file://priv\assets\i18n\i18n-health.js#L1-L38)

### 数据导出功能