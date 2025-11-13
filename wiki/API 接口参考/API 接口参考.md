
# API 接口参考

<cite>
**本文档中引用的文件**  
- [eadm_router.erl](file://src/eadm_router.erl)
- [api_payment.erl](file://src/apis/api_payment.erl)
- [api_watch.erl](file://src/apis/api_watch.erl)
- [eadm_login_controller.erl](file://src/controllers/eadm_login_controller.erl)
- [eadm_dashboard_controller.erl](file://src/controllers/eadm_dashboard_controller.erl)
- [eadm_health_controller.erl](file://src/controllers/eadm_health_controller.erl)
- [eadm_location_controller.erl](file://src/controllers/eadm_location_controller.erl)
- [eadm_finance_controller.erl](file://src/controllers/eadm_finance_controller.erl)
- [eadm_crontab_controller.erl](file://src/controllers/eadm_crontab_controller.erl)
- [eadm_user_controller.erl](file://src/controllers/eadm_user_controller.erl)
- [eadm_role_controller.erl](file://src/controllers/eadm_role_controller.erl)
- [eadm_device_controller.erl](file://src/controllers/eadm_device_controller.erl)
- [eadm_sys_sysinfo_controller.erl](file://src/controllers/eadm_sys_sysinfo_controller.erl)
- [eadm_sys_processes_controller.erl](file://src/controllers/eadm_sys_processes_controller.erl)
- [eadm_sys_ports_controller.erl](file://src/controllers/eadm_sys_ports_controller.erl)
- [eadm_sys_tv_controller.erl](file://src/controllers/eadm_sys_tv_controller.erl)
</cite>

## 目录
1. [简介](#简介)
2. [认证要求](#认证要求)
3. [登录与用户信息接口](#登录与用户信息接口)
4. [仪表盘接口](#仪表盘接口)
5. [健康数据接口](#健康数据接口)
6. [位置轨迹接口](#位置轨迹接口)
7. [财务数据接口](#财务数据接口)
8. [定时任务接口](#定时任务接口)
9. [用户管理接口](#用户管理接口)
10. [角色管理接口](#角色管理接口)
11. [设备管理接口](#设备管理接口)
12. [系统信息接口](#系统信息接口)
13. [特殊接口](#特殊接口)
14. [附录：响应格式](#附录响应格式)

## 简介
本文档为 `eadm` 系统的所有公共 API 接口提供详尽的参考。文档基于 `src/controllers/` 目录下的控制器模块和 `src/apis/` 目录下的特殊接口模块，详细列出了每个 HTTP 端点的请求方法、路径、参数、响应格式和认证要求。开发者可依据此文档进行系统集成。

## 认证要求
大多数 API 接口需要用户登录后才能访问。系统通过会话（Session）进行认证。用户需先调用 `/login` 接口成功登录，服务器会返回一个会话 Cookie。后续请求必须在 HTTP 头部中包含此 Cookie 才能通过认证。未认证的请求将被重定向到 `/login` 页面或返回鉴权失败信息。

## 登录与用户信息接口

### 用户登录
- **HTTP 方法**: POST
- **URL 路径**: `/login`
- **请求参数 (Body)**:
  - `loginName` (string, 必需): 用户登录名。
  - `password` (string, 必需): 用户密码。
- **响应格式 (成功)**:
  ```json
  {
    "Alert": "欢迎【用户名】登录! ",
    "logined": 1
  }
  ```
- **响应格式 (错误)**:
  ```json
  {
    "Alert": "用户名或密码错误，请重新登录！",
    "logined": 0
  }
  ```
- **HTTP 状态码**: `200 OK`
- **认证要求**: 否

**curl 命令示例**:
```bash
curl -X POST http://localhost:8080/login \
  -H "Content-Type: application/json" \
  -d '{"loginName": "admin", "password": "password123"}'
```

### 获取用户信息
- **HTTP 方法**: GET
- **URL 路径**: `/userinfo`
- **请求参数**: 无
- **响应格式 (成功)**:
  ```json
  [
    {
      "loginname": "admin",
      "username": "管理员",
      "email": "admin@example.com"
    }
  ]
  ```
- **HTTP 状态码**: `200 OK`
- **认证要求**: 是

**curl 命令示例**:
```bash
curl -X GET http://localhost:8080/userinfo -b "session_cookie=your_cookie_value"
```

### 修改用户信息
- **HTTP 方法**: POST
- **URL 路径**: `/useredit`
- **请求参数 (Body)**:
  - `loginName` (string, 必需): 新的登录名。
  - `userName` (string, 必需): 新的用户名。
  - `email` (string, 必需): 新的邮箱地址。
- **响应格式 (成功)**:
  ```json
  {
    "Alert": "用户【用户名】编辑成功！"
  }
  ```
- **HTTP 状态码**: `200 OK`
- **认证要求**: 是

### 修改用户密码
- **HTTP 方法**: POST
- **URL 路径**: `/userpwd`
- **请求参数 (Body)**:
  - `passwordOld` (string, 必需): 旧密码。
  - `passwordNew` (string, 必需): 新密码。
- **响应格式 (成功)**:
  ```json
  {
    "Alert": "密码修改成功！"
  }
  ```
- **HTTP 状态码**: `200 OK`
- **认证要求**: 是

**Section sources**
- [eadm_login_controller.erl](file://src/controllers/eadm_login_controller.erl#L45-L155)

## 仪表盘接口

### 获取仪表盘数据
- **HTTP 方法**: GET
- **URL 路径**: `/dashboard`
- **请求参数**: 无
- **响应格式 (成功)**:
  - 响应是一个包含多个数据项的数组，顺序为：健康周数据、健康年数据、地理位置月份标签、地理位置数据、财务月份标签、收入数据、支出数据。
- **HTTP 状态码**: `200 OK`
- **认证要求**: 是

**curl 命令示例**:
```bash
curl -X GET http://localhost:8080/dashboard -b "session_cookie=your_cookie_value"
```

**Section sources**
- [eadm_dashboard_controller.erl](file://src/controllers/eadm_dashboard_controller.erl#L45-L102)

## 健康数据接口

### 获取健康数据
- **HTTP 方法**: GET
- **URL 路径**: `/health`
- **请求参数 (查询参数)**:
  - `dataType` (string, 必需): 数据类型。`1`: 步数, `2`: 心率, `3`: 体温, `4`: 血压, `5`: 睡眠, `6`: 信号/电量。
  - `startTime` (string, 必需): 开始时间，格式 `YYYY-MM-DD HH:MM:SS`。
  - `endTime` (string, 必需): 结束时间，格式 `YYYY-MM-DD HH:MM:SS`。
- **响应格式 (成功)**:
  ```json
  {
    "data": [
      {
        "utctime": "2024-05-20 10:00:00",
        "steps": 1234
      },
      ...
    ]
  }
  ```
- **HTTP 状态码**: `200 OK`
- **认证要求**: 是

**curl 命令示例**:
```bash
curl -X GET "http://localhost:8080/health?dataType=1&startTime=2024-05-20%2000:00:00&endTime=2024-05-20%2023:59:59" -b "session_cookie=your_cookie_value"
```

**Section sources**
- [eadm_health_controller.erl](file://src/controllers/eadm_health_controller.erl#L75-L147)

## 位置轨迹接口

### 查询位置轨迹
- **HTTP 方法**: GET
- **URL 路径**: `/location`
- **请求参数 (查询参数)**:
  - `startTime` (string, 必需): 开始时间，格式 `YYYY-MM-DD HH:MM:SS`。
  - `endTime` (string, 必需): 结束时间，格式 `YYYY-MM-DD HH:MM:SS`。
  - `deviceNo` (string, 可选): 设备号。如果未提供，则查询用户有权限的所有设备的轨迹。
- **响应格式 (成功)**:
  - 响应是一个包含经纬度坐标的二维数组，例如 `[[116.404, 39.915], ...]`。
- **HTTP 状态码**: `200 OK`
- **认证要求**: 是

**curl 命令示例**:
```bash
curl -X GET "http://localhost:8080/location?startTime=2024-05-20%2000:00:00&endTime=2024-05-20%2023:59:59&deviceNo=DEV001" -b "session_cookie=your_cookie_value"
```

**Section sources**
- [eadm_location_controller.erl](file://src/controllers/eadm_location_controller.erl#L75-L116)

## 财务数据接口

### 查询财务数据
- **HTTP 方法**: GET
- **URL 路径**: `/finance`
- **请求参数 (查询参数)**:
  - `sourceType` (string, 必需): 来源类型，0表示全部。
  - `inorOut` (string, 必需): 收支类型，1: 收入, 2: 支出, 3: 其他, 0表示全部。
  - `startTime` (string, 必需): 开始时间，格式 `YYYY-MM-DD HH:MM:SS`。
  - `endTime` (string, 必需): 结束时间，格式 `YYYY-MM-DD HH:MM:SS`。
- **响应格式 (成功)**:
  ```json
  {
    "data": [
      {
        "id": 1,
        "sourcetype": "支付宝",
        "inorout": "收入",
        "tradetype": "转账",
        "amount": 100.00,
        "tradetime": "2024-05-20 10:00:00"
      },
      ...
    ]
  }
  ```
- **HTTP 状态码**: `200 OK`
- **认证要求**: 是

### 查询财务明细
- **HTTP 方法**: GET
- **URL 路径**: `/finance/:detailId`
- **请求参数 (路径参数)**:
  - `detailId` (string, 必需): 财务记录的ID。
- **响应格式 (成功)**:
  ```json
  {
    "data": {
      "owner": "张三",
      "sourcetype": "微信",
      "inorout": "支出",
      "amount": 50.00,
      "tradetime": "2024-05-20 10:00:00",
      "billcomment": "午餐"
    }
  }
  ```
- **HTTP 状态码**: `200 OK`
- **认证要求**: 是

### 删除财务数据
- **HTTP 方法**: DELETE
- **URL 路径**: `/finance/:detailId`
- **请求参数 (路径参数)**:
  - `detailId` (string, 必需): 财务记录的ID。
- **响应格式 (成功)**:
  ```json
  {
    "Alert": "数据删成功！"
  }
  ```
- **HTTP 状态码**: `200 OK`
- **认证要求**: 是

### 上传财务数据
- **HTTP 方法**: POST
- **URL 路径**: `/finance/upload`
- **请求参数 (Body JSON)**:
  - `importType` (string, 必需): 导入类型。
  - `uploadJson` (array, 必需): 包含财务数据的JSON对象数组。
- **响应格式 (成功)**:
  ```json
  {
    "Alert": "导入成功10行！"
  }
  ```
- **HTTP 状态码**: `200 OK`
- **认证要求**: 是

**Section sources**
- [eadm_finance_controller.erl](file://src/controllers/eadm_finance_controller.erl#L75-L338)

## 定时任务接口

### 查询定时任务
- **HTTP 方法**: GET
- **URL 路径**: `/crontab`
- **请求参数 (查询参数)**:
  - `cronName` (string, 必需): 任务名称，支持模糊查询。
- **响应格式 (成功)**:
  ```json
  {
    "data": [
      {
        "id": "1",
        "cronname": "数据同步任务",
        "cronexp": "0 0 * * *",
        "cronmfa": "my_module:my_function/1",
        "starttime": "00:00:00",
        "endtime": "23:59:59",
        "cronstatus": 0,
        "createdat": "2024-05-20 10:00:00"
      },
      ...
    ]
  }
  ```
- **HTTP 状态码**: `200 OK`
- **认证要求**: 是

### 查询任务日志
- **HTTP 方法**: GET
- **URL 路径**: `/crontab/detail/:cronId`
- **请求参数 (路径参数)**:
  - `cronId` (string, 必需): 任务ID。
- **响应格式 (成功)**:
  ```json
  {
    "data": [
      {
        "cronname": "数据同步任务",
        "cronlog": "任务执行成功。",
        "exectime": "2024-05-20 10:00:00"
      },
      ...
    ]
  }
  ```
- **HTTP 状态码**: `200 OK`
- **认证要求**: 是

### 添加定时任务
- **HTTP 方法**: POST
- **URL 路径**: `/crontab/add`
- **请求参数 (Body)**:
  - `cronName` (string, 必需): 任务名称。
  - `cronExp` (string, 必需): Cron表达式。
  - `cronModule` (string, 必需): 任务执行的模块、函数和参数，格式为 `Module:Function/Arity`。
  - `startTime` (string, 必需): 任务开始时间，格式 `HH:MM:SS`。
  - `endTime` (string, 可选): 任务结束时间，格式 `HH:MM:SS`。
- **响应格式 (成功)**:
  ```json
  {
    "status": true,
    "message": "定时任务添加成功，请手动激活任务",
    "id": "2",
    "refresh": true
  }
  ```
- **HTTP 状态码**: `200 OK`
- **认证要求**: 是

### 编辑定时任务
- **HTTP 方法**: POST
- **URL 路径**: `/crontab/edit`
- **请求参数 (Body)**:
  - `cronId` (string, 必需): 任务ID。
  - `cronName` (string, 必需): 任务名称。
  - `cronExp` (string, 必需): Cron表达式。
  - `cronModule` (string, 必需): 任务执行的模块、函数和参数。
  - `startTime` (string, 必需): 任务开始时间。
  - `endTime` (string, 可选): 任务结束时间。
- **响应格式 (成功)**:
  ```json
  {
    "status": true,
    "message": "定时任务更新成功",
    "refresh": true
  }
  ```
- **HTTP 状态码**: `200 OK`
- **认证要求**: 是

### 删除定时任务
- **HTTP 方法**: DELETE
- **URL 路径**: `/crontab/delete/:cronId`
- **请求参数 (路径参数)**:
  - `cronId` (string, 必需): 任务ID。
- **响应格式 (成功)**:
  ```json
  {
    "status": true,
    "message": "任务删除成功！",
    "refresh": true
  }
  ```
- **HTTP 状态码**: `200 OK`
- **认证要求**: 是

### 切换任务状态
- **HTTP 方法**: POST
- **URL 路径**: `/crontab/toggle`
- **请求参数 (Body)**:
  - `cronId` (string, 必需): 任务ID。
- **响应格式 (成功)**:
  ```json
  {
    "status": true,
    "message": "任务已激活",
    "refresh": true
  }
  ```
- **HTTP 状态码**: `200 OK`
- **认证要求**: 是

**Section sources**
- [eadm_crontab_controller.erl](file://src/controllers/eadm_crontab_controller.erl#L245-L596)

## 用户管理接口

### 查询用户列表
- **HTTP 方法**: GET
- **URL 路径**: `/user`
- **请求参数**: 无
- **响应格式 (成功)**:
  ```json
  {
    "data": [
      {
        "id": "1",
        "tenantname": "默认租户",
        "loginname": "admin",
        "username": "管理员",
        "email": "admin@example.com",
        "userstatus": 0,
        "createdat": "2024-05-20 10:00:00"
      },
      ...
    ]
  }
  ```
- **HTTP 状态码**: `200 OK`
- **认证要求**: 是

### 添加用户
- **HTTP 方法**: POST
- **URL 路径**: `/user/add`
- **请求参数 (Body)**:
  - `loginName` (string, 必需): 登录名。
  - `userName` (string, 必需): 用户名。
  - `email` (string, 必需): 邮箱。
  - `password` (string, 必需): 密码。
- **响应格式 (成功)**:
  ```json
  {
    "Alert": "用户【用户名】新增成功！"
  }
  ```
- **HTTP 状态码**: `200 OK`
- **认证要求**: 是

### 编辑用户
- **HTTP 方法**: POST
- **URL 路径**: `/user/edit`
- **请求参数 (Body)**:
  - `userId` (string, 必需): 用户ID。
  - `loginName` (string, 必需): 登录名。
  - `userName` (string, 必需): 用户名。
  - `email` (string, 必需): 邮箱。
- **响应格式 (成功)**:
  ```json
  {
    "Alert": "用户【用户名】编辑成功！"
  }
  ```
- **HTTP 状态码**: `200 OK`
- **认证要求**: 是

### 删除用户
- **HTTP 方法**: DELETE
- **URL 路径**: `/user/delete/:userId`
- **请求参数 (路径参数)**:
  - `userId` (string, 必需): 用户ID。
- **响应格式 (成功)**:
  ```json
  {
    "Alert": "用户删除成功！"
  }
  ```
- **HTTP 状态码**: `200 OK`
- **认证要求**: 是

### 禁用/启用用户
- **HTTP 方法**: POST
- **URL 路径**: `/user/disable/:userId`
- **请求参数 (路径参数)**:
  - `userId` (string, 必需): 用户ID。
- **响应格式 (成功)**:
  ```json
  {
    "Alert": "用户启禁用成功！"
  }
  ```
- **HTTP 状态码**: `200 OK`
- **认证要求**: 是

### 重置用户密码
- **HTTP 方法**: POST
- **URL 路径**: `/user/reset/:userId`
- **请求参数 (路径参数)**:
  - `userId` (string, 必需): 用户ID。
- **响应格式 (成功)**:
  ```json
  {
    "Alert": "用户密码重置成功！"
  }
  ```
- **HTTP 状态码**: `200 OK`
- **认证要求**: 是

**Section sources**
- [eadm_user_controller.erl](file://src/controllers/eadm_user_controller.erl#L75-L480)

## 角色管理接口

### 查询角色列表
- **HTTP 方法**: GET
- **URL 路径**: `/role`
- **请求参数**: 无
- **响应格式 (成功)**:
  ```json
  {
    "data": [
      {
        "id": "1",
        "rolename": "管理员",
        "rolestatus": 0,
        "createdat": "2024-05-20 10:00:00"
      },
      ...
    ]
  }
  ```
- **HTTP 状态码**: `200 OK`
- **认证要求**: 是

### 添加角色
- **HTTP 方法**: POST
- **URL 路径**: `/role/add`
- **请求参数 (Body)**:
  - `roleName` (string, 必需): 角色名称。
- **响应格式 (成功)**:
  ```json
  {
    "Alert": "角色【角色名】新增成功！"
  }
  ```
- **HTTP 状态码**: `200 OK`
- **认证要求**: 是

### 删除角色
- **HTTP 方法**: DELETE
- **URL 路径**: `/role/delete/:roleId`
- **请求参数 (路径参数)**:
  - `roleId` (string, 必需): 角色ID。
- **响应格式 (成功)**:
  ```json
  {
    "Alert": "角色删除成功！"
  }
  ```
- **HTTP 状态码**: `200 OK`
- **认证要求**: 是

### 禁用/启用角色
- **HTTP 方法**: POST
- **URL 路径**: `/role/disable/:roleId`
- **请求参数 (路径参数)**:
  - `roleId` (string, 必需): 角色ID。
- **响应格式 (成功)**:
  ```json
  {
    "Alert": "角色启禁用成功！"
  }
  ```
- **HTTP 状态码**: `200 OK`
- **认证要求**: 是

### 获取角色权限
- **HTTP 方法**: GET
- **URL 路径**: `/permission/:roleId`
- **请求参数 (路径参数)**:
  - `roleId` (string, 必需): 角色ID。
- **响应格式 (成功)**:
  ```json
  {
    "data": {
      "dashboard": true,
      "health": false,
      "finance": {
        "finlist": true,
        "finimp": false,
        "findel": false
      },
      ...
    }
  }
  ```
- **HTTP 状态码**: `200 OK`
- **认证要求**: 是

### 更新角色权限
- **HTTP 方法**: POST
- **URL 路径**: `/permission/edit`
- **请求参数 (Body)**:
  - `roleId` (string, 必需): 角色ID。
  - `dashBoard`, `health`, `locate`, `crontab`, `userManage` (string, 必需): 布尔值，表示是否拥有该权限。
  - `finance` (string, 必需): 布尔值，表示财务列表权限。
  - `finimp`, `findel` (string, 必需): 布尔值，表示财务导入和删除权限。
  - `devlist`, `devadd`, `devedit`, `devdel`, `devassign` (string, 必需): 设备管理相关权限。
- **响应格式 (成功)**:
  ```json
  {
    "Alert": "权限更新成功！"
  }
  ```
- **HTTP 状态码**: `200 OK`
- **认证要求**: 是

**Section sources**
- [eadm_role_controller.erl](file://src/controllers/eadm_role_controller.erl#L75-L247)

## 设备管理接口

### 查询设备列表
- **HTTP 方法**: GET
- **URL 路径**: `/device`
- **请求参数 (查询参数)**:
  - `deviceNo` (string, 可选): 设备号，支持模糊查询。
- **响应格式 (成功)**:
  ```json
  {
    "data": [
      {
        "deviceno": "DEV001",
        "imei": "123456789012345",
        "simno": "861234567890123",
        "remark": "测试设备",
        "enable": true,
        "createdat": "2024-05-20 10:00:00"
      },
      ...
    ]
  }
  ```
- **HTTP 状态码**: `200 OK`
- **认证要求**: 是

### 添加设备
- **HTTP 方法**: POST
- **URL 路径**: `/device/add`
- **请求参数 (Body JSON)**:
  - `deviceNo` (string, 必需): 设备号。
  - `imei` (string, 必需): IMEI号。
  - `simNo` (string, 必需): SIM卡号。
  - `remark` (string, 可选): 备注。
- **响应格式 (成功)**:
  ```json
  {
    "Alert": "设备添加成功！"
  }
  ```
- **HTTP 状态码**: `200 OK`
- **认证要求**: 是

### 编辑设备
- **HTTP 方法**: POST
- **URL 路径**: `/device/edit`
- **请求参数 (Body JSON)**:
  - `deviceNo` (string, 必需): 设备号。
  - `imei` (string, 必需): IMEI号。
  - `simNo` (string, 必需): SIM卡号。
  - `remark` (string, 可选): 备注。
  - `enable` (boolean, 必需): 设备状态。
- **响应格式 (成功)**:
  ```json
  {
    "Alert": "设备更新成功！"
  }
  ```
- **HTTP 状态码**: `200 OK`
- **认证要求**: 是

### 删除设备
- **HTTP 方法**: DELETE
- **URL 路径**: `/device/delete/:deviceNo`
- **请求参数 (路径参数)**:
  - `deviceNo` (string, 必需): 设备号。
- **响应格式 (成功)**:
  ```json
  {
    "Alert": "设备删除成功！"
  }
  ```
- **HTTP 状态码**: `200 OK`
- **认证要求**: 是

### 启用/禁用设备
- **HTTP 方法**: POST
- **URL 路径**: `/device/toggle`
- **请求参数 (Body JSON)**:
  - `deviceNo` (string, 必需): 设备号。
- **响应格式 (成功)**:
  ```json
  {
    "Alert": "设备启用成功！"
  }
  ```
- **HTTP 状态码**: `200 OK`
- **认证要求**: 是

### 分配设备给用户
- **HTTP 方法**: POST
- **URL 路径**: `/device/assign`
- **请求参数 (Body JSON)**:
  - `deviceNo` (string, 必需): 设备号。
  - `userId` (string, 必需): 用户ID。
  - `userLoginName` (string, 必需): 用户登录名。
- **响应格式 (成功)**:
  ```json
  {
    "Alert": "设备分配成功！"
  }
  ```
- **HTTP 状态码**: `200 OK`
- **认证要求**: 是

### 取消设备分配
- **HTTP 方法**: DELETE
- **URL 路径**: `/device/unassign/:id`
- **请求参数 (路径参数)**:
  - `id` (string, 必需): 分配记录ID。
- **响应格式 (成功)**:
  ```json
  {
    "Alert": "设备取消分配成功！"
  }
  ```
- **HTTP 状态码**: `200 OK`
- **认证要求**: 是

### 获取设备的用户列表
- **HTTP 方法**: GET
- **URL 路径**: `/device/users/:deviceNo`
- **请求参数 (路径参数)**:
  - `deviceNo` (string, 必需): 设备号。
- **响应格式 (成功)**:
  ```json
  {
    "data": [
      {
        "id": 1,
        "userid": "1",
        "loginname": "user1",
        "username": "用户一"
      },
      ...
    ]
  }
  ```
- **HTTP 状态码**: `200 OK`
- **认证要求**: 是

### 获取用户可访问的设备列表
- **HTTP 方法**: GET
- **URL 路径**: `/device/user_devices`
- **请求参数**: 无
- **响应格式 (成功)**:
  ```json
  {
    "data": [
      {
        "deviceno": "DEV001",
        "imei": "123456789012345",
        "remark": "测试设备"
      },
      ...
    ]
  }
  ```
- **HTTP 状态码**: `200 OK`
- **认证要求**: 是

**Section sources**
- [eadm_device_controller.erl](file://src/controllers/eadm_device_controller.erl#L75-L389)

## 系统信息接口

### 获取系统信息
- **HTTP 方法**: GET
- **URL 路径**: `/sys/sysinfo`
- **请求参数**: 无
- **响应格式 (成功)**:
  - 响应包含一个名为 `sys_info` 的键，其值为一个包含 OTP 版本、Erlang 版本、内存使用、进程数等系统信息的列表。
- **HTTP 状态码**: `200 OK`
- **认证要求**: 是

### 获取进程列表
- **HTTP 方法**: GET
- **URL 路径**: `/sys/processes`
- **请求参数**: 无
- **响应格式 (成功)**:
  - 响应包含一个名为 `procs` 的键，其值为一个包含所有 Erlang 进程信息的数组。
- **HTTP 状态码**: `200 OK`
- **认证要求**: 是

### 获取进程详细信息
- **HTTP 方法**: GET
- **URL 路径**: `/sys/processes/:pid`
- **请求参数 (路径参数)**:
  - `pid` (string, 必需): 进程ID，URL编码。
- **响应格式 (成功)**:
  - 响应为一个包含指定进程所有信息的 JSON 对象。
- **HTTP 状态码**: `200 OK`
- **认证要求**: 是

### 获取端口列表
- **HTTP 方法**: GET
- **URL 路径**: `/sys/ports`
- **请求参数**: 无
- **响应格式 (成功)**:
  - 响应包含一个名为 `ports` 的键，其值为一个包含所有系统端口信息的数组。
- **HTTP 状态码**: `200 OK`
- **认证要求**: 是

### 获取表列表
- **HTTP 方法**: GET
- **URL 路径**: `/sys/tables`
- **请求参数**: 无
- **响应格式 (成功)**:
  - 响应包含一个名为 `tables` 的键，其值为一个包含所有 ETS 表信息的数组。
- **HTTP 状态码**: `200 OK`
- **认证要求**: 是

**Section sources**
- [eadm_sys_sysinfo_controller.erl](file://src/controllers/eadm_sys_sysinfo_controller.erl#L45-L148)
- [eadm_sys_processes_controller.erl](file://src/controllers/eadm_sys_processes_controller.erl#L45-L48)
- [eadm_sys_ports_controller.erl](file://src/controllers/eadm_sys_ports_controller.erl#L30-L34)
- [eadm_sys_tv_controller.erl](file://src/controllers/eadm_sys_tv_controller.erl#L45-L52)

## 特殊接口

### 手表数据上报接口
- **HTTP 方法**: POST
- **URL 路径**: `/api/watch`
- **请求参数 (查询参数)**:
  - `type` (string, 必需): 数据类型。`3`: 基站数据, `4`: 每日累计步数, `5`: WIFI定位, `6`: 心率数据, `8`: 血压, `10`: 血糖, `11`: 翻转数据, `12/14`: 体温数据, `16`: 定位数据, `30`: 信号/电量, `31`: 血氧, `58`: 睡眠, `59`: 蓝牙信息, `100`: 健康数据集合, `18-21,24,25,36,38,39,51,52,57,91,110,154-156`: 各类提醒。
  - 其他参数根据 `type` 的不同而变化，例如 `Lac`, `cid`, `steps`, `Latitude`, `Longitude`, `heartbeat`, `diastolic`, `shrink`, `bloodSugar`, `roll`, `bodyTemperature`, `wristTemperature`, `latStr`, `lngStr`, `speedStr`, `singal`, `battery`, `sleepType`, `minute`, `startTime`, `endTime`, `BTInfo` 等。
- **响应格式 (成功)**:
  ```json
  {
    "success": true
  }
  ```
- **HTTP 状态码**: `200 OK`
- **认证要求**: 否

**curl 命令示例**:
```bash
curl -X POST "http://localhost:8080/api/watch?type=6&heartbeat=75&BTUtcTime=2024-05-20%2010:00:00"
```

### 支付宝支付配置接口
- **HTTP 方法**: POST
- **URL 路径**: `/api/finance/config`
- **请求参数 (Body JSON