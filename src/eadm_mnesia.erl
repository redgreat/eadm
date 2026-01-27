%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2026, REDGREAT
%%% @doc
%%% Mnesia数据库管理模块
%%% 负责schema初始化、表创建、基础数据导入
%%% @end
%%% Created : 2026-01-24 01:31:00
%%%-------------------------------------------------------------------
-module(eadm_mnesia).
-author("wangcw").

%%%===================================================================
%%% 函数导出
%%%===================================================================
-export([init_schema/0, create_tables/0, wait_for_tables/0, init_seed_data/0]).
-export([get_all_tables/0, table_info/1]).

%%%===================================================================
%%% 记录定义
%%%===================================================================

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

%%%===================================================================
%%% API 函数
%%%===================================================================

%% @doc
%% 初始化mnesia schema
%% 只在第一次部署时调用,如果已存在则跳过
%% @end
init_schema() ->
    case mnesia:system_info(is_running) of
        yes ->
            lager:info("Mnesia已经在运行,跳过schema创建"),
            ok;
        no ->
            Node = node(),
            case mnesia:create_schema([Node]) of
                ok ->
                    lager:info("Mnesia schema创建成功"),
                    ok;
                {error, {Node, {already_exists, Node}}} ->
                    lager:info("Mnesia schema已存在,跳过创建"),
                    ok;
                {error, Reason} ->
                    lager:error("Mnesia schema创建失败: ~p", [Reason]),
                    {error, Reason}
            end
    end.

%% @doc
%% 创建所有mnesia表
%% @end
create_tables() ->
    %% 创建租户表
    create_table(
        eadm_tenant,
        record_info(fields, eadm_tenant),
        [
            {disc_copies, [node()]},
            {attributes, record_info(fields, eadm_tenant)},
            {type, set},
            {index, [tenantname, enable]}
        ]
    ),

    %% 创建用户表
    create_table(
        eadm_user,
        record_info(fields, eadm_user),
        [
            {disc_copies, [node()]},
            {attributes, record_info(fields, eadm_user)},
            {type, set},
            {index, [loginname, tenantid, userstatus]}
        ]
    ),

    %% 创建角色表
    create_table(
        eadm_role,
        record_info(fields, eadm_role),
        [
            {disc_copies, [node()]},
            {attributes, record_info(fields, eadm_role)},
            {type, set},
            {index, [rolename, rolestatus]}
        ]
    ),

    %% 创建用户角色关联表
    create_table(
        eadm_userrole,
        record_info(fields, eadm_userrole),
        [
            {disc_copies, [node()]},
            {attributes, record_info(fields, eadm_userrole)},
            {type, set},
            {index, [userid, roleid]}
        ]
    ),

    %% 创建定时任务表
    create_table(
        eadm_crontab,
        record_info(fields, eadm_crontab),
        [
            {disc_copies, [node()]},
            {attributes, record_info(fields, eadm_crontab)},
            {type, set},
            {index, [cronname, cronstatus]}
        ]
    ),

    %% 创建仪表盘表
    create_table(
        eadm_dashboard,
        record_info(fields, eadm_dashboard),
        [
            {disc_copies, [node()]},
            {attributes, record_info(fields, eadm_dashboard)},
            {type, set},
            {index, [datatype, datetype, loginname, checkdate]}
        ]
    ),

    %% 创建设备表
    create_table(
        eadm_device,
        record_info(fields, eadm_device),
        [
            {disc_copies, [node()]},
            {attributes, record_info(fields, eadm_device)},
            {type, set},
            {index, [simno, enable]}
        ]
    ),

    %% 创建用户设备关联表
    create_table(
        eadm_userdevice,
        record_info(fields, eadm_userdevice),
        [
            {disc_copies, [node()]},
            {attributes, record_info(fields, eadm_userdevice)},
            {type, set},
            {index, [userid, loginname, deviceno]}
        ]
    ),

    lager:info("所有Mnesia表创建成功"),
    ok.

%% @doc
%% 等待所有表就绪
%% @end
wait_for_tables() ->
    Tables = get_all_tables(),
    case mnesia:wait_for_tables(Tables, 30000) of
        ok ->
            lager:info("所有Mnesia表已就绪"),
            ok;
        {timeout, BadTables} ->
            lager:error("等待表超时: ~p", [BadTables]),
            {error, {timeout, BadTables}};
        {error, Reason} ->
            lager:error("等待表失败: ~p", [Reason]),
            {error, Reason}
    end.

%% @doc
%% 初始化种子数据(基础数据)
%% @end
init_seed_data() ->
    %% 检查是否已经初始化过
    case mnesia:dirty_read(eadm_tenant, <<"et0000000001">>) of
        [] ->
            lager:info("开始初始化Mnesia种子数据"),
            init_tenants(),
            init_users(),
            init_roles(),
            init_userroles(),
            init_devices(),
            init_userdevices(),
            lager:info("Mnesia种子数据初始化完成"),
            ok;
        [_] ->
            lager:info("种子数据已存在,跳过初始化"),
            ok
    end.

%% @doc
%% 获取所有表名
%% @end
get_all_tables() ->
    [
        eadm_tenant,
        eadm_user,
        eadm_role,
        eadm_userrole,
        eadm_crontab,
        eadm_dashboard,
        eadm_device,
        eadm_userdevice
    ].

%% @doc
%% 获取表信息
%% @end
table_info(Table) ->
    mnesia:table_info(Table, all).

%%%===================================================================
%%% 内部函数
%%%===================================================================

%% @private
%% 创建单个表
create_table(TableName, RecordFields, Options) ->
    case mnesia:create_table(TableName, Options) of
        {atomic, ok} ->
            lager:info("表 ~p 创建成功", [TableName]),
            ok;
        {aborted, {already_exists, TableName}} ->
            lager:info("表 ~p 已存在,跳过创建", [TableName]),
            ok;
        {aborted, Reason} ->
            lager:error("表 ~p 创建失败: ~p", [TableName, Reason]),
            {error, Reason}
    end.

%% @private
%% 生成ID
generate_id(Prefix, Seq) ->
    SeqStr = integer_to_list(Seq),
    Padded = string:pad(SeqStr, 10, leading, $0),
    list_to_binary(Prefix ++ Padded).

%% @private
%% 获取当前时间戳
now_timestamp() ->
    erlang:system_time(second).

%% @private
%% 初始化租户数据
init_tenants() ->
    Tenants = [
        #eadm_tenant{
            id = <<"et0000000001">>,
            tenantname = <<"redgreat">>,
            remark = <<"主租户"/utf8>>,
            enable = true,
            createdat = now_timestamp()
        },
        #eadm_tenant{
            id = <<"et0000000002">>,
            tenantname = <<"管理客户"/utf8>>,
            remark = <<"手动添加客户"/utf8>>,
            enable = true,
            createdat = now_timestamp()
        },
        #eadm_tenant{
            id = <<"et0000000003">>,
            tenantname = <<"注册客户"/utf8>>,
            remark = <<"界面注册客户"/utf8>>,
            enable = true,
            createdat = now_timestamp()
        }
    ],
    lists:foreach(fun(T) -> mnesia:dirty_write(T) end, Tenants).

%% @private
%% 初始化用户数据
init_users() ->
    Users = [
        #eadm_user{
            id = <<"eu0000000001">>,
            tenantid = <<"et0000000001">>,
            loginname = <<"wangcw">>,
            username = <<"王存伟"/utf8>>,
            email = <<"rubygreat@msn.com">>,
            passwd = <<"q122/4GBpiCNq83AbPQN/+kYq0KwczLxiWfLaLKk4NY=">>,
            userstatus = 0,
            createdat = now_timestamp()
        },
        #eadm_user{
            id = <<"eu0000000002">>,
            tenantid = <<"et0000000002">>,
            loginname = <<"wongcw">>,
            username = <<"王存偉"/utf8>>,
            email = <<"rubygreat@msn.com">>,
            passwd = <<"q122/4GBpiCNq83AbPQN/+kYq0KwczLxiWfLaLKk4NY=">>,
            userstatus = 0,
            createdat = now_timestamp()
        },
        #eadm_user{
            id = <<"eu0000000003">>,
            tenantid = <<"et0000000003">>,
            loginname = <<"jiangyf">>,
            username = <<"姜玉凤"/utf8>>,
            email = <<"1234567@qq.com">>,
            passwd = <<"q122/4GBpiCNq83AbPQN/+kYq0KwczLxiWfLaLKk4NY=">>,
            userstatus = 0,
            createdat = now_timestamp()
        }
    ],
    lists:foreach(fun(U) -> mnesia:dirty_write(U) end, Users).

%% @private
%% 初始化角色数据
init_roles() ->
    Roles = [
        #eadm_role{
            id = <<"er0000000001">>,
            rolename = <<"超级管理员"/utf8>>,
            rolepermission = #{
                <<"health">> => true,
                <<"locate">> => true,
                <<"crontab">> => true,
                <<"finance">> => #{
                    <<"findel">> => true,
                    <<"finimp">> => true,
                    <<"finlist">> => true
                },
                <<"dashboard">> => true,
                <<"usermanage">> => true,
                <<"sports">> => true
            },
            rolestatus = 0,
            createduser = <<"wangcw">>,
            createdat = now_timestamp()
        },
        #eadm_role{
            id = <<"er0000000002">>,
            rolename = <<"注册租户"/utf8>>,
            rolepermission = #{
                <<"health">> => false,
                <<"locate">> => true,
                <<"crontab">> => false,
                <<"finance">> => #{
                    <<"findel">> => false,
                    <<"finimp">> => false,
                    <<"finlist">> => false
                },
                <<"dashboard">> => true,
                <<"usermanage">> => false,
                <<"sports">> => false
            },
            rolestatus = 0,
            createduser = <<"wangcw">>,
            createdat = now_timestamp()
        },
        #eadm_role{
            id = <<"er0000000003">>,
            rolename = <<"分配租户"/utf8>>,
            rolepermission = #{
                <<"health">> => true,
                <<"locate">> => true,
                <<"crontab">> => false,
                <<"finance">> => #{
                    <<"findel">> => false,
                    <<"finimp">> => false,
                    <<"finlist">> => false
                },
                <<"dashboard">> => true,
                <<"usermanage">> => false,
                <<"sports">> => false
            },
            rolestatus = 0,
            createduser = <<"wangcw">>,
            createdat = now_timestamp()
        }
    ],
    lists:foreach(fun(R) -> mnesia:dirty_write(R) end, Roles).

%% @private
%% 初始化用户角色关联数据
init_userroles() ->
    UserRoles = [
        #eadm_userrole{
            id = 1,
            userid = <<"eu0000000001">>,
            roleid = <<"er0000000001">>,
            createdat = now_timestamp()
        }
    ],
    lists:foreach(fun(UR) -> mnesia:dirty_write(UR) end, UserRoles).

%% @private
%% 初始化设备数据
init_devices() ->
    Devices = [
        #eadm_device{
            deviceno = <<"16053489111">>,
            remark = <<"充电宝"/utf8>>,
            enable = true,
            createduser = <<"wangcw">>,
            createdat = now_timestamp()
        },
        #eadm_device{
            deviceno = <<"868977061978771">>,
            remark = <<"手表"/utf8>>,
            enable = true,
            createduser = <<"wangcw">>,
            createdat = now_timestamp()
        }
    ],
    lists:foreach(fun(D) -> mnesia:dirty_write(D) end, Devices).

%% @private
%% 初始化用户设备关联数据
init_userdevices() ->
    UserDevices = [
        #eadm_userdevice{
            id = 1,
            userid = <<"eu0000000001">>,
            loginname = <<"wangcw">>,
            deviceno = <<"16053489111">>,
            createduser = <<"wangcw">>,
            createdat = now_timestamp()
        },
        #eadm_userdevice{
            id = 2,
            userid = <<"eu0000000001">>,
            loginname = <<"wangcw">>,
            deviceno = <<"868977061978771">>,
            createduser = <<"wangcw">>,
            createdat = now_timestamp()
        }
    ],
    lists:foreach(fun(UD) -> mnesia:dirty_write(UD) end, UserDevices).
