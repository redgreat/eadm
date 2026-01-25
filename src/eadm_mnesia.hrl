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
    id,              % char(12) - 主键
    tenantname,      % varchar(20) - 租户名称
    remark,          % varchar(100) - 备注
    enable = true,   % boolean - 是否可用
    createduser,     % varchar(50) - 创建人
    createdat,       % timestamptz - 创建时间
    updateduser,     % varchar(50) - 更新人
    updatedat,       % timestamptz - 更新时间
    deleteduser,     % varchar(50) - 删除人
    deletedat,       % timestamptz - 删除时间
    deleted = false  % boolean - 是否删除
}).

%% 用户表
-record(eadm_user, {
    id,              % char(12) - 主键
    tenantid,        % char(12) - 租户id
    loginname,       % varchar(50) - 登录名
    username,        % varchar(50) - 用户姓名
    email,           % varchar(20) - 邮箱
    passwd,          % varchar(50) - 密码
    userstatus = 0,  % smallint - 用户状态(0启用1禁用)
    createduser,     % varchar(50) - 创建人
    createdat,       % timestamptz - 创建时间
    updateduser,     % varchar(50) - 更新人
    updatedat,       % timestamptz - 更新时间
    deleteduser,     % varchar(50) - 删除人
    deletedat,       % timestamptz - 删除时间
    deleted = false  % boolean - 是否删除
}).

%% 角色表
-record(eadm_role, {
    id,              % char(12) - 主键
    rolename,        % varchar(50) - 角色名称
    rolepermission,  % json - 角色权限
    rolestatus = 0,  % smallint - 角色状态(0启用1禁用)
    createduser,     % varchar(50) - 创建人
    createdat,       % timestamptz - 创建时间
    updateduser,     % varchar(50) - 更新人
    updatedat,       % timestamptz - 更新时间
    deleteduser,     % varchar(50) - 删除人
    deletedat,       % timestamptz - 删除时间
    deleted = false  % boolean - 是否删除
}).

%% 用户角色关联表
-record(eadm_userrole, {
    id,              % integer - 自增主键
    userid,          % char(12) - 用户id
    roleid,          % char(12) - 角色id
    createduser,     % varchar(50) - 创建人
    createdat,       % timestamptz - 创建时间
    updateduser,     % varchar(50) - 更新人
    updatedat,       % timestamptz - 更新时间
    deleteduser,     % varchar(50) - 删除人
    deletedat,       % timestamptz - 删除时间
    deleted = false  % boolean - 是否删除
}).

%% 定时任务表
-record(eadm_crontab, {
    id,              % char(12) - 主键
    cronname,        % varchar(50) - 任务名称
    cronexp,         % varchar(50) - 定时表达式
    cronmfa,         % varchar(50) - 任务MFA
    starttime,       % timestamptz - 开始时间
    endtime,         % timestamptz - 结束时间
    cronstatus = 0,  % smallint - 任务状态(0启用1禁用)
    createduser,     % varchar(50) - 创建人
    createdat,       % timestamptz - 创建时间
    updateduser,     % varchar(50) - 更新人
    updatedat,       % timestamptz - 更新时间
    deleteduser,     % varchar(50) - 删除人
    deletedat,       % timestamptz - 删除时间
    deleted = false  % boolean - 是否删除
}).

%% 仪表盘表
-record(eadm_dashboard, {
    id,              % integer - 自增主键
    datatype,        % smallint - 数据类型
    datetype,        % smallint - 统计周期类型
    loginname,       % varchar(50) - 登录名
    datavalue,       % varchar(500) - 数据值
    datajson,        % json - 数据json
    checkdate,       % varchar(20) - 数据日期
    updatedat,       % timestamptz - 更新时间
    inserttime       % timestamptz - 插入时间
}).

%% 设备表
-record(eadm_device, {
    deviceno,        % varchar(50) - 设备号(主键)
    imei,            % varchar(50) - 设备imei
    simno,           % varchar(50) - sim卡号
    enable = true,   % boolean - 设备状态
    remark,          % varchar(200) - 设备描述
    createduser,     % varchar(50) - 创建人
    createdat,       % timestamptz - 创建时间
    updateduser,     % varchar(50) - 更新人
    updatedat,       % timestamptz - 更新时间
    deleteduser,     % varchar(50) - 删除人
    deletedat,       % timestamptz - 删除时间
    deleted = false  % boolean - 是否删除
}).

%% 用户设备关联表
-record(eadm_userdevice, {
    id,              % integer - 自增主键
    userid,          % char(12) - 用户id
    loginname,       % varchar(50) - 登录名
    deviceno,        % varchar(50) - 设备号
    createduser,     % varchar(50) - 创建人
    createdat,       % timestamptz - 创建时间
    updateduser,     % varchar(50) - 更新人
    updatedat,       % timestamptz - 更新时间
    deleteduser,     % varchar(50) - 删除人
    deletedat,       % timestamptz - 删除时间
    deleted = false  % boolean - 是否删除
}).
