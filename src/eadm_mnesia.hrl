%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2026, REDGREAT
%%% @doc
%%% Mnesia表记录定义头文件
%%% 供各个controller模块引用
%%% @end
%%% Created : 2026-01-24 01:35:00
%%%-------------------------------------------------------------------

%% 租户表
-record(eadm_tenant, {
    % char(12) - 主键
    id,
    % varchar(20) - 租户名称
    tenantname,
    % varchar(100) - 备注
    remark,
    % boolean - 是否可用
    enable = true,
    % varchar(50) - 创建人
    createduser,
    % timestamptz - 创建时间
    createdat,
    % varchar(50) - 更新人
    updateduser,
    % timestamptz - 更新时间
    updatedat,
    % varchar(50) - 删除人
    deleteduser,
    % timestamptz - 删除时间
    deletedat,
    % boolean - 是否删除
    deleted = false
}).

%% 用户表
-record(eadm_user, {
    % char(12) - 主键
    id,
    % char(12) - 租户id
    tenantid,
    % varchar(50) - 登录名
    loginname,
    % varchar(50) - 用户姓名
    username,
    % varchar(20) - 邮箱
    email,
    % varchar(50) - 密码
    passwd,
    % smallint - 用户状态(0启用1禁用)
    userstatus = 0,
    % varchar(50) - 创建人
    createduser,
    % timestamptz - 创建时间
    createdat,
    % varchar(50) - 更新人
    updateduser,
    % timestamptz - 更新时间
    updatedat,
    % varchar(50) - 删除人
    deleteduser,
    % timestamptz - 删除时间
    deletedat,
    % boolean - 是否删除
    deleted = false
}).

%% 角色表
-record(eadm_role, {
    % char(12) - 主键
    id,
    % varchar(50) - 角色名称
    rolename,
    % json - 角色权限
    rolepermission,
    % smallint - 角色状态(0启用1禁用)
    rolestatus = 0,
    % varchar(50) - 创建人
    createduser,
    % timestamptz - 创建时间
    createdat,
    % varchar(50) - 更新人
    updateduser,
    % timestamptz - 更新时间
    updatedat,
    % varchar(50) - 删除人
    deleteduser,
    % timestamptz - 删除时间
    deletedat,
    % boolean - 是否删除
    deleted = false
}).

%% 用户角色关联表
-record(eadm_userrole, {
    % integer - 自增主键
    id,
    % char(12) - 用户id
    userid,
    % char(12) - 角色id
    roleid,
    % varchar(50) - 创建人
    createduser,
    % timestamptz - 创建时间
    createdat,
    % varchar(50) - 更新人
    updateduser,
    % timestamptz - 更新时间
    updatedat,
    % varchar(50) - 删除人
    deleteduser,
    % timestamptz - 删除时间
    deletedat,
    % boolean - 是否删除
    deleted = false
}).

%% 定时任务表
-record(eadm_crontab, {
    % char(12) - 主键
    id,
    % varchar(50) - 任务名称
    cronname,
    % varchar(50) - 定时表达式
    cronexp,
    % varchar(50) - 任务MFA
    cronmfa,
    % timestamptz - 开始时间
    starttime,
    % timestamptz - 结束时间
    endtime,
    % smallint - 任务状态(0启用1禁用)
    cronstatus = 0,
    % varchar(50) - 创建人
    createduser,
    % timestamptz - 创建时间
    createdat,
    % varchar(50) - 更新人
    updateduser,
    % timestamptz - 更新时间
    updatedat,
    % varchar(50) - 删除人
    deleteduser,
    % timestamptz - 删除时间
    deletedat,
    % boolean - 是否删除
    deleted = false
}).

%% 仪表盘表
-record(eadm_dashboard, {
    % integer - 自增主键
    id,
    % smallint - 数据类型
    datatype,
    % smallint - 统计周期类型
    datetype,
    % varchar(50) - 登录名
    loginname,
    % varchar(500) - 数据值
    datavalue,
    % json - 数据json
    datajson,
    % varchar(20) - 数据日期
    checkdate,
    % timestamptz - 更新时间
    updatedat,
    % timestamptz - 插入时间
    inserttime
}).

%% 设备表
-record(eadm_device, {
    % varchar(50) - 设备号(主键)
    deviceno,
    % varchar(50) - 设备imei
    imei,
    % varchar(50) - sim卡号
    simno,
    % boolean - 设备状态
    enable = true,
    % varchar(200) - 设备描述
    remark,
    % varchar(50) - 创建人
    createduser,
    % timestamptz - 创建时间
    createdat,
    % varchar(50) - 更新人
    updateduser,
    % timestamptz - 更新时间
    updatedat,
    % varchar(50) - 删除人
    deleteduser,
    % timestamptz - 删除时间
    deletedat,
    % boolean - 是否删除
    deleted = false
}).

%% 用户设备关联表
-record(eadm_userdevice, {
    % integer - 自增主键
    id,
    % char(12) - 用户id
    userid,
    % varchar(50) - 登录名
    loginname,
    % varchar(50) - 设备号
    deviceno,
    % varchar(50) - 创建人
    createduser,
    % timestamptz - 创建时间
    createdat,
    % varchar(50) - 更新人
    updateduser,
    % timestamptz - 更新时间
    updatedat,
    % varchar(50) - 删除人
    deleteduser,
    % timestamptz - 删除时间
    deletedat,
    % boolean - 是否删除
    deleted = false
}).
