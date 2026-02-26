-- @author wangcw
-- @copyright (c) 2024, redgreat
-- created : 2024-05-17 10:00:44
-- postgres表结构设计

-- 设置查询路径
alter role user_eadm set search_path to eadm, public;

--设置 本地时区
set time zone 'asia/shanghai';

-- 表最后一次更新时间函数
drop function if exists lastupdate cascade;
create or replace function lastupdate()
returns trigger as $$
begin
    new.updatedat := current_timestamp;
    return new;
end;
$$ language plpgsql;

-- 创建序列
drop sequence if exists sd cascade;
create sequence sd
start 1
increment by 1
maxvalue 9999999999
cache 10;

-- 系统_字典信息表
drop table if exists sys_dict cascade;
create table sys_dict (
  id char(12) default ('sd' || lpad((nextval('sd')::varchar), 10, '0')),
  dictno varchar(50) not null,
  dictname varchar(100) not null,
  parentid char(12),
  createduser varchar(50),
  createdat timestamptz default current_timestamp,
  updateduser varchar(50),
  updatedat timestamptz default current_timestamp,
  deleteduser varchar(50),
  deletedat timestamptz,
  deleted boolean not null default false
);

alter table sys_dict owner to user_eadm;
alter table sys_dict drop constraint if exists pk_sysdict_id cascade;
alter table sys_dict add constraint pk_sysdict_id primary key (id);
alter table sys_dict drop constraint if exists uni_sysdict_dictno cascade;
alter table sys_dict add constraint uni_sysdict_dictno unique (dictno);
alter table sys_dict drop constraint if exists fk_parentid_sysdict_id cascade;
alter table sys_dict add constraint fk_parentid_sysdict_id foreign key (parentid)
    references sys_dict (id) on delete restrict on update restrict;

drop index if exists non_sysdict_parentid;
create index non_sysdict_parentid on sys_dict using btree (parentid asc nulls last);

comment on column sys_dict.id is '自定义主键(sd)';
comment on column sys_dict.dictno is '字典编码';
comment on column sys_dict.dictname is '字典名称';
comment on column sys_dict.parentid is '父级id';
comment on column sys_dict.createduser is '创建人';
comment on column sys_dict.createdat is '创建时间';
comment on column sys_dict.updateduser is '更新人';
comment on column sys_dict.updatedat is '更新时间';
comment on column sys_dict.deleteduser is '删除人';
comment on column sys_dict.deletedat is '删除时间';
comment on column sys_dict.deleted is '是否删除(0否1是)';
comment on table sys_dict is '系统域_字典信息表';

-- 表最后一次更新时间触发器
drop trigger if exists dict_lastupdate on sys_dict cascade;

create or replace trigger dict_lastupdate
before update on sys_dict
for each row
execute function lastupdate();

-- 基础信息_租户信息表
drop sequence if exists et cascade;
create sequence et
start 1
increment by 1
maxvalue 9999999999
cache 10;

drop table if exists eadm_tenant cascade;
create table eadm_tenant (
  id char(12) not null default ('et' || lpad((nextval('et')::varchar), 10, '0')),
  tenantname varchar(20) not null,
  remark varchar(100),
  enable boolean not null default true,
  createduser varchar(50),
  createdat timestamptz default current_timestamp,
  updateduser varchar(50),
  updatedat timestamptz default current_timestamp,
  deleteduser varchar(50),
  deletedat timestamptz,
  deleted boolean not null default false
);

alter table eadm_tenant owner to user_eadm;
alter table eadm_tenant drop constraint if exists pk_tenant_id cascade;
alter table eadm_tenant add constraint pk_tenant_id primary key (id);
alter table eadm_tenant drop constraint if exists uni_tenant_tenantname cascade;
alter table eadm_tenant add constraint uni_tenant_tenantname unique (tenantname);

drop index if exists non_tenant_enable;
create index non_tenant_enable on eadm_tenant using btree (enable asc nulls last);

comment on column eadm_tenant.id is '自定义主键(et)';
comment on column eadm_tenant.tenantname is '租户名称';
comment on column eadm_tenant.remark is '备注信息';
comment on column eadm_tenant.enable is '是否可用(0否1是)';
comment on column eadm_tenant.createduser is '创建人';
comment on column eadm_tenant.createdat is '创建时间';
comment on column eadm_tenant.updateduser is '更新人';
comment on column eadm_tenant.updatedat is '更新时间';
comment on column eadm_tenant.deleteduser is '删除人';
comment on column eadm_tenant.deletedat is '删除时间';
comment on column eadm_tenant.deleted is '是否删除(0否1是)';
comment on table eadm_tenant is '基础信息_租户信息表';

-- 表最后一次更新时间触发器
drop trigger if exists tenant_lastupdate on eadm_tenant cascade;

create or replace trigger tenant_lastupdate
before update on eadm_tenant
for each row
execute function lastupdate();

--写入数据
truncate table eadm_tenant;

insert into eadm_tenant(tenantname, remark)
values('redgreat', '主租户');

insert into eadm_tenant(tenantname, remark)
values('管理客户', '手动添加客户');

insert into eadm_tenant(tenantname, remark)
values('注册客户', '界面注册客户');

-- select * from eadm_tenant;

-- 获取商户名称
drop function if exists gettenantname cascade;
create or replace function gettenantname(in inid char(12))
returns varchar(100) as $$
begin
    return (select tenantname from eadm_tenant where id=inid and enable is true and deleted is false limit 1);
end
$$ language plpgsql;

-- select gettenantname('et0000000001');

-- 用户表
drop sequence if exists eu cascade;
create sequence eu
start 1
increment by 1
maxvalue 9999999999
cache 10;

drop table if exists eadm_user cascade;
create table eadm_user (
  id char(12) not null default ('eu' || lpad((nextval('eu')::varchar), 10, '0')),
  tenantid char(12) not null,
  loginname varchar(50) not null,
  username varchar(50) not null,
  email varchar(20),
  passwd varchar(50) not null,
  userstatus smallint not null default 0,
  createduser varchar(50),
  createdat timestamptz default current_timestamp,
  updateduser varchar(50),
  updatedat timestamptz default current_timestamp,
  deleteduser varchar(50),
  deletedat timestamptz,
  deleted boolean not null default false
);

alter table eadm_user owner to user_eadm;
alter table eadm_user drop constraint if exists pk_user_id cascade;
alter table eadm_user add constraint pk_user_id primary key (id);

alter table eadm_user drop constraint if exists fk_tenantid_tenant_id cascade;
alter table eadm_user add constraint fk_tenantid_tenant_id foreign key (tenantid)
    references eadm_tenant (id) on delete restrict on update restrict;

alter table eadm_user drop constraint if exists uni_user_loginname cascade;
alter table eadm_user add constraint uni_user_loginname unique (loginname);

drop index if exists non_user_userstatus;
create index non_user_userstatus on eadm_user using btree (userstatus asc nulls last);
drop index if exists non_user_updatedat;
create index non_user_updatedat on eadm_user using btree (updatedat desc nulls last);

comment on column eadm_user.id is '自定义主键(eu)';
comment on column eadm_user.tenantid is '租户id';
comment on column eadm_user.loginname is '用户登录名';
comment on column eadm_user.username is '用户姓名';
comment on column eadm_user.email is '用户邮件';
comment on column eadm_user.passwd is '密码';
comment on column eadm_user.userstatus is '用户状态(0启用1禁用)';
comment on column eadm_user.createduser is '创建人';
comment on column eadm_user.createdat is '创建时间';
comment on column eadm_user.updateduser is '更新人';
comment on column eadm_user.updatedat is '更新时间';
comment on column eadm_user.deleteduser is '删除人';
comment on column eadm_user.deletedat is '删除时间';
comment on column eadm_user.deleted is '是否删除(0否1是)';
comment on table eadm_user is '基础信息_用户信息表';

-- 最后一次更新时间
drop trigger if exists user_lastupdate on eadm_user cascade;

create or replace trigger user_lastupdate
before update on eadm_user
for each row
execute function lastupdate();

-- 写入用户数据

-- select * from eadm_tenant;

truncate table eadm_user;

insert into eadm_user(tenantid, loginname, username, email, passwd)
values('et0000000001','wangcw', '王存伟', 'rubygreat@msn.com', 'q122/4GBpiCNq83AbPQN/+kYq0KwczLxiWfLaLKk4NY=');

insert into eadm_user(tenantid, loginname, username, email, passwd)
values('et0000000002','wongcw', '王存偉', 'rubygreat@msn.com', 'q122/4GBpiCNq83AbPQN/+kYq0KwczLxiWfLaLKk4NY=');

insert into eadm_user(tenantid, loginname, username, email, passwd)
values('et0000000003','jiangyf', '姜玉凤', '1234567@qq.com', 'q122/4GBpiCNq83AbPQN/+kYq0KwczLxiWfLaLKk4NY=');

-- 用户视图
create or replace view vi_user
as
select id, gettenantname(tenantid) as tenantname, loginname, username, email,
       case userstatus when 1 then '禁用' when 0 then '启用' end as userstatus, createdat
from eadm_user
  where deleted is false;

-- select * from vi_user;

-- 用户角色
drop sequence if exists er cascade;
create sequence er
start 1
increment by 1
maxvalue 9999999999
cache 10;

drop table if exists eadm_role cascade;
create table eadm_role (
  id char(12) not null default ('er' || lpad((nextval('er')::varchar), 10, '0')),
  rolename varchar(50) not null,
  rolepermission json default '{"crontab":false,"dashboard":true,"finance":{"findel":false,"finimp":false,"finlist":false},"health":false,"locate":false,"usermanage":false}'::json,
  rolestatus smallint not null default 0,
  createduser varchar(50),
  createdat timestamptz default current_timestamp,
  updateduser varchar(50),
  updatedat timestamptz default current_timestamp,
  deleteduser varchar(50),
  deletedat timestamptz,
  deleted boolean not null default false
);

alter table eadm_role owner to user_eadm;
alter table eadm_role drop constraint if exists pk_role_id cascade;
alter table eadm_role add constraint pk_role_id primary key (id);

drop index if exists non_role_rolename;
create index non_role_rolename on eadm_role using btree (rolename asc nulls last);
drop index if exists non_user_rolestatus;
create index non_user_rolestatus on eadm_role using btree (rolestatus asc nulls last);

comment on column eadm_role.id is '自定义主键(er)';
comment on column eadm_role.rolename is '角色名称';
comment on column eadm_role.rolepermission is '角色权限';
comment on column eadm_role.rolestatus is '角色状态(0启用1禁用)';
comment on column eadm_role.createduser is '创建人';
comment on column eadm_role.createdat is '创建时间';
comment on column eadm_role.updateduser is '更新人';
comment on column eadm_role.updatedat is '更新时间';
comment on column eadm_role.deleteduser is '删除人';
comment on column eadm_role.deletedat is '删除时间';
comment on column eadm_role.deleted is '是否删除(0否1是)';
comment on table eadm_role is '基础信息_角色信息表';

-- 最后一次更新时间
drop trigger if exists role_lastupdate on eadm_role cascade;

create or replace trigger role_lastupdate
before update on eadm_role
for each row
execute function lastupdate();

-- 写入数据
truncate table eadm_role;

insert into eadm_role(rolename, rolepermission, createduser)
values('超级管理员', '{"health": true, "locate": true, "crontab": true, "finance": {"findel": true, "finimp": true, "finlist": true}, "dashboard": true, "usermanage": true}', 'wangcw');

insert into eadm_role(rolename, rolepermission, createduser)
values('注册租户', '{"health": false, "locate": true, "crontab": false, "finance": {"findel": false, "finimp": false, "finlist": false}, "dashboard": true, "usermanage": false}', 'wangcw');

insert into eadm_role(rolename, rolepermission, createduser)
values('分配租户', '{"health": true, "locate": true, "crontab": false, "finance": {"findel": false, "finimp": false, "finlist": false}, "dashboard": true, "usermanage": false}', 'wangcw');

-- 角色视图
create or replace view vi_role
as
select id, rolename, rolepermission, case rolestatus when 1 then '禁用' when 0 then '启用' end as rolestatus, createdat
from eadm_role
  where deleted is false;

-- select * from eadm_role;

-- 用户角色对应关系
drop table if exists eadm_userrole cascade;
create table eadm_userrole (
  id serial,
  userid char(12) not null,
  roleid char(12) not null,
  createduser varchar(50),
  createdat timestamptz default current_timestamp,
  updateduser varchar(50),
  updatedat timestamptz default current_timestamp,
  deleteduser varchar(50),
  deletedat timestamptz,
  deleted boolean not null default false
);

alter table eadm_userrole owner to user_eadm;
alter table eadm_userrole drop constraint if exists pk_userrole_id cascade;
alter table eadm_userrole add constraint pk_userrole_id primary key (id);

alter table eadm_userrole drop constraint if exists fk_userid_user_id cascade;
alter table eadm_userrole add constraint fk_userid_user_id foreign key (userid)
    references eadm_user (id) on delete restrict on update restrict;

alter table eadm_userrole drop constraint if exists fk_roleid_role_id cascade;
alter table eadm_userrole add constraint fk_roleid_role_id foreign key (roleid)
    references eadm_role (id) on delete restrict on update restrict;

comment on column eadm_userrole.id is '自增主键';
comment on column eadm_userrole.userid is '用户id(eadm_user.id)';
comment on column eadm_userrole.roleid is '角色id(eadm_role.id)';
comment on column eadm_userrole.createduser is '创建人';
comment on column eadm_userrole.createdat is '创建时间';
comment on column eadm_userrole.updateduser is '更新人';
comment on column eadm_userrole.updatedat is '更新时间';
comment on column eadm_userrole.deleteduser is '删除人';
comment on column eadm_userrole.deletedat is '删除时间';
comment on column eadm_userrole.deleted is '是否删除(0否1是)';
comment on table eadm_userrole is '基础信息_用户角色对应关系表';

-- 最后一次更新时间
drop trigger if exists roleuser_lastupdate on eadm_userrole cascade;

create or replace trigger roleuser_lastupdate
before update on eadm_userrole
for each row
execute function lastupdate();

-- 写入数据
-- select * from eadm_user;
-- select * from eadm_role;

insert into eadm_userrole(userid, roleid)
values('eu0000000001', 'er0000000001');

-- 用户角色信息视图
create or replace view vi_userrole
as
select b.id, a.id as userid, c.id as roleid, c.rolename, b.updatedat
from eadm_user a
inner join eadm_userrole b
  on b.userid=a.id
  and b.deleted is false
inner join eadm_role c
  on c.id=b.roleid
  and c.rolestatus=0
  and c.deleted is false
where a.deleted is false;

-- 用户权限信息视图
create or replace view vi_userpermission
as
select b.id, a.loginname, c.rolepermission
from eadm_user a
inner join eadm_userrole b
  on b.userid=a.id
  and b.deleted is false
inner join eadm_role c
  on c.id=b.roleid
  and c.rolestatus=0
  and c.deleted is false
where a.deleted is false;

-- 定时任务信息
drop sequence if exists cr cascade;
create sequence cr
start 1
increment by 1
maxvalue 9999999999
cache 10;

drop table if exists eadm_crontab cascade;
create table eadm_crontab (
  id char(12) not null default ('cr' || lpad((nextval('cr')::varchar), 10, '0')),
  cronname varchar(50) not null,
  cronexp varchar(50),
  cronmfa varchar(50),
  starttime timestamptz default current_timestamp,
  endtime timestamptz,
  cronstatus smallint not null default 0,
  createduser varchar(50),
  createdat timestamptz default current_timestamp,
  updateduser varchar(50),
  updatedat timestamptz default current_timestamp,
  deleteduser varchar(50),
  deletedat timestamptz,
  deleted boolean not null default false
);

alter table eadm_crontab owner to user_eadm;
alter table eadm_crontab drop constraint if exists pk_crontab_id cascade;
alter table eadm_crontab add constraint pk_crontab_id primary key (id);

drop index if exists non_crontab_cronname;
create index non_crontab_cronname on eadm_crontab using btree (cronname asc nulls last);

comment on column eadm_crontab.id is '自定义主键(cr)';
comment on column eadm_crontab.cronname is '任务名称';
comment on column eadm_crontab.cronexp is '定时表达式';
comment on column eadm_crontab.cronmfa is '任务备注';
comment on column eadm_crontab.starttime is '任务开始时间';
comment on column eadm_crontab.endtime is '任务结束时间';
comment on column eadm_crontab.cronstatus is '任务状态(0启用1禁用)';
comment on column eadm_crontab.createduser is '创建人';
comment on column eadm_crontab.createdat is '创建时间';
comment on column eadm_crontab.updateduser is '更新人';
comment on column eadm_crontab.updatedat is '更新时间';
comment on column eadm_crontab.deleteduser is '删除人';
comment on column eadm_crontab.deletedat is '删除时间';
comment on column eadm_crontab.deleted is '是否删除(0否1是)';
comment on table eadm_crontab is '基础信息_定时任务信息表';

-- 最后一次更新时间
drop trigger if exists crontab_lastupdate on eadm_crontab cascade;

create or replace trigger crontab_lastupdate
before update on eadm_crontab
for each row
execute function lastupdate();

-- 查询视图
create or replace view vi_crontab
as
select id, cronname, cronexp, cronmfa,
       to_char(starttime, 'yyyy-mm-dd hh24:mi:ss') as starttime,
       to_char(endtime, 'yyyy-mm-dd hh24:mi:ss') as endtime,
       case cronstatus when 0 then '启用' else '禁用' end as cronstatus,
       to_char(createdat, 'yyyy-mm-dd hh24:mi:ss') as createdat
from eadm_crontab
where deleted is false;

-- 定时任务日志
drop table if exists sys_cronlog cascade;
create table sys_cronlog (
    id serial,
    cronid char(12),
    cronlog text,
    exectime timestamptz default current_timestamp
);

alter table sys_cronlog owner to user_eadm;
alter table sys_cronlog drop constraint if exists pk_cronlog_id cascade;
alter table sys_cronlog add constraint pk_cronlog_id primary key (id);

drop index if exists non_cronlog_cronid;
create index non_cronlog_cronid on sys_cronlog using btree (cronid asc nulls last);

comment on column sys_cronlog.id is '自增主键';
comment on column sys_cronlog.cronlog is '任务Id';
comment on column sys_cronlog.cronlog is '任务执行详情';
comment on column sys_cronlog.exectime is '任务执行时间';
comment on table sys_cronlog is '系统域_任务执行日志';

-- 首页报表
drop table if exists eadm_dashboard cascade;
create table eadm_dashboard(
    id serial,
    datatype smallint not null,
    datetype smallint not null,
    loginname varchar(50),
    datavalue varchar(500),
    datajson json,
    checkdate varchar(20) not null,
    updatedat timestamptz default current_timestamp,
    instertime timestamptz default current_timestamp
);

alter table eadm_dashboard owner to user_eadm;
alter table eadm_dashboard drop constraint if exists pk_dashboard_id cascade;
alter table eadm_dashboard add constraint pk_dashboard_id primary key (id);

alter table eadm_dashboard drop constraint if exists uni_dashboard_ddlc cascade;
alter table eadm_dashboard add constraint uni_dashboard_ddlc
    unique (datatype, datetype, loginname, checkdate);

drop index if exists non_dashboard_datatype;
create index non_dashboard_datatype on eadm_dashboard using btree (datatype asc nulls last);
drop index if exists non_dashboard_datetype;
create index non_dashboard_datetype on eadm_dashboard using btree (datetype asc nulls last);
drop index if exists non_dashboard_loginname;
create index non_dashboard_loginname on eadm_dashboard using btree (loginname asc nulls last);
drop index if exists non_dashboard_checkdate;
create index non_dashboard_checkdate on eadm_dashboard using btree (checkdate asc nulls last);

comment on column eadm_dashboard.id is '自增主键';
comment on column eadm_dashboard.datatype is '数据类型(1心率2步数3睡眠4里程5每月里程6每月收入7每月支出)';
comment on column eadm_dashboard.datetype is '统计周期类型(1日2周3月4年)';
comment on column eadm_dashboard.loginname is '登录名';
comment on column eadm_dashboard.datavalue is '数据值';
comment on column eadm_dashboard.datajson is '数据json';
comment on column eadm_dashboard.checkdate is '数据日期';
comment on column eadm_dashboard.updatedat is '更新时间';
comment on column eadm_dashboard.instertime is '插入时间';
comment on table eadm_dashboard is '首页_看板报表';

-- 最后一次更新时间
drop trigger if exists dashboard_lastupdate on eadm_dashboard cascade;

create or replace trigger dashboard_lastupdate
before update on eadm_dashboard
for each row
execute function lastupdate();

-- 过程运行日志
drop table if exists sys_proclog cascade;
create table sys_proclog (
  id serial,
  procname varchar(50),
  timespan int,
  result boolean not null default true,
  errcode varchar(5),
  errmessage varchar(5000),
  inserttime timestamptz default current_timestamp
);

alter table sys_proclog owner to user_eadm;
alter table sys_proclog drop constraint if exists pk_proclog_id cascade;
alter table sys_proclog add constraint pk_proclog_id primary key (id);

drop index if exists non_proclog_procname;
create index non_proclog_procname on sys_proclog using btree (procname asc nulls last);
drop index if exists non_proclog_inserttime;
create index non_proclog_inserttime on sys_proclog using btree (inserttime asc nulls last);

comment on column sys_proclog.id is '自增主键';
comment on column sys_proclog.procname is '过程名';
comment on column sys_proclog.timespan is '耗时时长(秒)';
comment on column sys_proclog.result is '是否成功(0否1是)';
comment on column sys_proclog.errcode is '错误代码';
comment on column sys_proclog.errmessage is '错误详细信息';
comment on column sys_proclog.inserttime is '日志记录时间';
comment on table sys_proclog is '系统域_过程执行日志';

-- 设备信息
drop table if exists eadm_device cascade;
create table eadm_device (
  deviceno varchar(50),
  imei varchar(50),
  simno varchar(50),
  enable boolean not null default true,
  remark varchar(200),
  createduser varchar(50),
  createdat timestamptz default current_timestamp,
  updateduser varchar(50),
  updatedat timestamptz default current_timestamp,
  deleteduser varchar(50),
  deletedat timestamptz,
  deleted boolean not null default false
);

alter table eadm_device owner to user_eadm;
alter table eadm_device drop constraint if exists pk_device_deviceno cascade;
alter table eadm_device add constraint pk_device_deviceno primary key (deviceno);

drop index if exists non_device_simno;
create index non_device_simno on eadm_device using btree (simno asc nulls last);

comment on column eadm_device.deviceno is '设备号(主键)';
comment on column eadm_device.imei is '设备imei';
comment on column eadm_device.simno is 'sim卡号';
comment on column eadm_device.enable is '设备状态(1启用0禁用)';
comment on column eadm_device.remark is '设备描述';
comment on column eadm_device.createduser is '创建人';
comment on column eadm_device.createdat is '创建时间';
comment on column eadm_device.updateduser is '更新人';
comment on column eadm_device.updatedat is '更新时间';
comment on column eadm_device.deleteduser is '删除人';
comment on column eadm_device.deletedat is '删除时间';
comment on column eadm_device.deleted is '是否删除(0否1是)';
comment on table eadm_device is '业务域_设备信息';

-- 最后一次更新时间
drop trigger if exists device_lastupdate on eadm_device cascade;

create or replace trigger device_lastupdate
before update on eadm_device
for each row
execute function lastupdate();

-- 写入数据
insert into eadm_device(deviceno, remark, createduser)
values('16053489111', '充电宝', 'wangcw'),
      ('868977061978771', '手表', 'wangcw');

-- 人员设备对应关系
drop table if exists eadm_userdevice cascade;
create table eadm_userdevice (
  id serial,
  userid char(12),
  loginname varchar(50),
  deviceno varchar(50) not null,
  createduser varchar(50),
  createdat timestamptz default current_timestamp,
  updateduser varchar(50),
  updatedat timestamptz default current_timestamp,
  deleteduser varchar(50),
  deletedat timestamptz,
  deleted boolean not null default false
);

alter table eadm_userdevice owner to user_eadm;
alter table eadm_userdevice drop constraint if exists pk_userdevice_id cascade;
alter table eadm_userdevice add constraint pk_userdevice_id primary key (id);

alter table eadm_userdevice drop constraint if exists fk_userid_user_id cascade;
alter table eadm_userdevice add constraint fk_userid_user_id foreign key (userid)
    references eadm_user (id) on delete restrict on update restrict;
alter table eadm_userdevice drop constraint if exists fk_loginname_user_loginname cascade;
alter table eadm_userdevice add constraint fk_loginname_user_loginname foreign key (loginname)
    references eadm_user (loginname) on delete restrict on update restrict;
alter table eadm_userdevice drop constraint if exists fk_deviceno_device_deviceno cascade;
alter table eadm_userdevice add constraint fk_deviceno_device_deviceno foreign key (deviceno)
    references eadm_device (deviceno) on delete restrict on update restrict;

comment on column eadm_userdevice.id is '自增主键';
comment on column eadm_userdevice.userid is '用户id(eadm_user.id)';
comment on column eadm_userdevice.loginname is '用户登录名(eadm_user.loginname)';
comment on column eadm_userdevice.deviceno is '设备id(eadm_device.deviceno)';
comment on column eadm_userdevice.createduser is '创建人';
comment on column eadm_userdevice.createdat is '创建时间';
comment on column eadm_userdevice.updateduser is '更新人';
comment on column eadm_userdevice.updatedat is '更新时间';
comment on column eadm_userdevice.deleteduser is '删除人';
comment on column eadm_userdevice.deletedat is '删除时间';
comment on column eadm_userdevice.deleted is '是否删除(0否1是)';
comment on table eadm_userdevice is '业务域_人员设备对应关系';

-- 最后一次更新时间
drop trigger if exists userdevice_lastupdate on eadm_userdevice cascade;

create or replace trigger userdevice_lastupdate
before update on eadm_userdevice
for each row
execute function lastupdate();

-- 写入数据
insert into eadm_userdevice(userid, loginname, deviceno, createduser)
select 'eu0000000001', 'wangcw', deviceno, 'wangcw'
from eadm_device;

-- select * from eadm_userdevice;

-- 业务数据_车辆定位信息
drop table if exists lc_carlocdaily;
create table lc_carlocdaily (
  ptime timestamptz,
  deviceno varchar(20),
  lat numeric(9,6),
  lng numeric(9,6),
  dirct int,
  speed int,
  mileage numeric(18,2),
  hight int,
  gnssnum int,
  rssi int,
  receivetime timestamptz,
  inserttime timestamptz not null default current_timestamp
);

alter table lc_carlocdaily owner to user_eadm;
alter table lc_carlocdaily drop constraint if exists pk_carlocdaily_ptime cascade;
alter table lc_carlocdaily add constraint pk_carlocdaily_ptime primary key (ptime);

drop index if exists non_carlocdaily_deviceno;
create index non_carlocdaily_deviceno on lc_carlocdaily using btree (deviceno asc nulls last);

comment on column lc_carlocdaily.ptime is '设备上传时间(主键)';
comment on column lc_carlocdaily.deviceno is '设备编码(iccid)';
comment on column lc_carlocdaily.lat is '经度';
comment on column lc_carlocdaily.lng is '纬度';
comment on column lc_carlocdaily.dirct is '方向角';
comment on column lc_carlocdaily.speed is '速度';
comment on column lc_carlocdaily.mileage is '里程';
comment on column lc_carlocdaily.hight is '海拔';
comment on column lc_carlocdaily.gnssnum is 'gps卫星数量';
comment on column lc_carlocdaily.rssi is '4g信号值';
comment on column lc_carlocdaily.receivetime is 'gateway处理时间';
comment on column lc_carlocdaily.inserttime is '数据写入时间';
comment on table lc_carlocdaily is '车辆日常定位信息';

-- 业务数据_手表信息
drop table if exists lc_watchdaily;
create table lc_watchdaily (
  ptime timestamptz not null,
  steps varchar(50),
  heartbeat varchar(50),
  roll varchar(50),
  bodytemperature varchar(50),
  wristtemperature varchar(50),
  bloodsugar varchar(50),
  diastolic varchar(50),
  shrink varchar(50),
  bloodoxygen varchar(50),
  sleeptype varchar(50),
  sleepstarttime varchar(50),
  sleependtime varchar(50),
  sleepminute varchar(50),
  signal varchar(50),
  battery varchar(50),
  lat varchar(50),
  lng varchar(50),
  speed varchar(50),
  inserttime timestamptz not null default current_timestamp
);

alter table lc_watchdaily owner to user_eadm;
alter table lc_watchdaily drop constraint if exists pk_watchdaily_ptime cascade;
alter table lc_watchdaily add constraint pk_watchdaily_ptime primary key (ptime);

comment on column lc_watchdaily.ptime is '数据获取时间';
comment on column lc_watchdaily.steps is '步数';
comment on column lc_watchdaily.heartbeat is '心率';
comment on column lc_watchdaily.roll is '翻转数';
comment on column lc_watchdaily.bodytemperature is '体温';
comment on column lc_watchdaily.wristtemperature is '腕温';
comment on column lc_watchdaily.bloodsugar is '血糖';
comment on column lc_watchdaily.diastolic is '舒张压';
comment on column lc_watchdaily.shrink is '收缩压';
comment on column lc_watchdaily.bloodoxygen is '血氧';
comment on column lc_watchdaily.sleeptype is '睡眠类型(1深度睡眠2浅度睡眠3醒来时长)';
comment on column lc_watchdaily.sleepstarttime is '睡眠开始时间';
comment on column lc_watchdaily.sleependtime is '睡眠结束时间';
comment on column lc_watchdaily.sleepminute is '睡眠时长(分钟)';
comment on column lc_watchdaily.signal is '信号值';
comment on column lc_watchdaily.battery is '电池电量';
comment on column lc_watchdaily.lat is '定位纬度(gps)';
comment on column lc_watchdaily.lng is '定位经度(gps)';
comment on column lc_watchdaily.speed is '速度';
comment on column lc_watchdaily.inserttime is '数据写入时间';
comment on table lc_watchdaily is '手表日常数据';

-- 业务数据_手表基站信息
drop table if exists lc_watchcell;
create table lc_watchcell (
  ptime timestamptz not null,
  lac smallint,
  cid smallint,
  db smallint,
  inserttime timestamptz not null default current_timestamp
);

alter table lc_watchcell owner to user_eadm;
alter table lc_watchcell drop constraint if exists pk_watchcell_ptime cascade;
alter table lc_watchcell add constraint pk_watchcell_ptime primary key (ptime);

comment on column lc_watchcell.ptime is '数据获取时间';
comment on column lc_watchcell.lac is '基站lac';
comment on column lc_watchcell.cid is '基站的cid';
comment on column lc_watchcell.db is '信号强度';
comment on column lc_watchcell.inserttime is '数据写入时间';
comment on table lc_watchcell is '业务数据_手表基站信息';

-- 业务数据_手表计步信息
drop table if exists lc_watchstep;
create table lc_watchstep (
  ptime timestamptz not null,
  steps smallint,
  inserttime timestamptz not null default current_timestamp
);

alter table lc_watchstep owner to user_eadm;
alter table lc_watchstep drop constraint if exists pk_watchstep_ptime cascade;
alter table lc_watchstep add constraint pk_watchstep_ptime primary key (ptime);

comment on column lc_watchstep.ptime is '数据获取时间';
comment on column lc_watchstep.steps is '步数';
comment on column lc_watchstep.inserttime is '数据写入时间';
comment on table lc_watchstep is '业务数据_手表计步信息';

-- 业务数据_手表定位信息
drop table if exists lc_watchlocation;
create table lc_watchlocation (
  ptime timestamptz not null,
  lat decimal(10, 7),
  lng decimal(10, 7),
  speed smallint,
  inserttime timestamptz not null default current_timestamp
);

alter table lc_watchlocation owner to user_eadm;
alter table lc_watchlocation drop constraint if exists pk_watchlocation_ptime cascade;
alter table lc_watchlocation add constraint pk_watchlocation_ptime primary key (ptime);

comment on column lc_watchlocation.ptime is '数据获取时间';
comment on column lc_watchlocation.lat is '定位纬度(gps)';
comment on column lc_watchlocation.lng is '定位经度(gps)';
comment on column lc_watchlocation.speed is '速度';
comment on column lc_watchlocation.inserttime is '数据写入时间';
comment on table lc_watchlocation is '业务数据_手表定位信息';

-- 业务数据_手表翻转信息
drop table if exists lc_watchroll;
create table lc_watchroll (
  ptime timestamptz not null,
  roll smallint,
  inserttime timestamptz not null default current_timestamp
);

alter table lc_watchroll owner to user_eadm;
alter table lc_watchroll drop constraint if exists pk_watchroll_ptime cascade;
alter table lc_watchroll add constraint pk_watchroll_ptime primary key (ptime);

comment on column lc_watchroll.ptime is '数据获取时间';
comment on column lc_watchroll.roll is '翻转数';
comment on column lc_watchroll.inserttime is '数据写入时间';
comment on table lc_watchroll is '业务数据_手表翻转信息';

-- 业务数据_手表血压信息
drop table if exists lc_watchbp;
create table lc_watchbp (
  ptime timestamptz not null,
  diastolic smallint,
  shrink smallint,
  inserttime timestamptz not null default current_timestamp
);

alter table lc_watchbp owner to user_eadm;
alter table lc_watchbp drop constraint if exists pk_watchbp_ptime cascade;
alter table lc_watchbp add constraint pk_watchbp_ptime primary key (ptime);

comment on column lc_watchbp.ptime is '数据获取时间';
comment on column lc_watchbp.diastolic is '舒张压';
comment on column lc_watchbp.shrink is '收缩压';
comment on column lc_watchbp.inserttime is '数据写入时间';
comment on table lc_watchbp is '业务数据_手表血压信息';

-- 业务数据_手表血糖信息
drop table if exists lc_watchbs;
create table lc_watchbs (
  ptime timestamptz not null,
  bloodsugar real,
  inserttime timestamptz not null default current_timestamp
);

alter table lc_watchbs owner to user_eadm;
alter table lc_watchbs drop constraint if exists pk_watchbs_ptime cascade;
alter table lc_watchbs add constraint pk_watchbs_ptime primary key (ptime);

comment on column lc_watchbs.ptime is '数据获取时间';
comment on column lc_watchbs.bloodsugar is '血糖';
comment on column lc_watchbs.inserttime is '数据写入时间';
comment on table lc_watchbs is '业务数据_手表血糖信息';

-- 业务数据_手表心率信息
drop table if exists lc_watchhb;
create table lc_watchhb (
   ptime timestamptz not null,
   heartbeat smallint,
   inserttime timestamptz not null default current_timestamp
);

alter table lc_watchhb owner to user_eadm;
alter table lc_watchhb drop constraint if exists pk_watchhb_ptime cascade;
alter table lc_watchhb add constraint pk_watchhb_ptime primary key (ptime);

comment on column lc_watchhb.ptime is '数据获取时间';
comment on column lc_watchhb.heartbeat is '心率';
comment on column lc_watchhb.inserttime is '数据写入时间';
comment on table lc_watchhb is '业务数据_手表心率信息';

-- 业务数据_手表体温信息
drop table if exists lc_watchbt;
create table lc_watchbt (
  ptime timestamptz not null,
  bodytemperature real,
  wristtemperature real,
  inserttime timestamptz not null default current_timestamp
);

alter table lc_watchbt owner to user_eadm;
alter table lc_watchbt drop constraint if exists pk_lc_watchbt_ptime cascade;
alter table lc_watchbt add constraint pk_lc_watchbt_ptime primary key (ptime);

comment on column lc_watchbt.ptime is '数据获取时间';
comment on column lc_watchbt.bodytemperature is '体温';
comment on column lc_watchbt.wristtemperature is '腕温';
comment on column lc_watchbt.inserttime is '数据写入时间';
comment on table lc_watchbt is '业务数据_手表体温信息';

-- 业务数据_手表信号/电量信息
drop table if exists lc_watchsb;
create table lc_watchsb (
  ptime timestamptz not null,
  signal smallint,
  battery smallint,
  inserttime timestamptz not null default current_timestamp
);

alter table lc_watchsb owner to user_eadm;
alter table lc_watchsb drop constraint if exists pk_watchsb_ptime cascade;
alter table lc_watchsb add constraint pk_watchsb_ptime primary key (ptime);

comment on column lc_watchsb.ptime is '数据获取时间';
comment on column lc_watchsb.signal is '信号值';
comment on column lc_watchsb.battery is '电池电量';
comment on column lc_watchsb.inserttime is '数据写入时间';
comment on table lc_watchsb is '业务数据_手表信号/电量信息';

-- 业务数据_手表血氧信息
drop table if exists lc_watchbo;
create table lc_watchbo (
  ptime timestamptz not null,
  bloodoxygen smallint,
  inserttime timestamptz not null default current_timestamp
);

alter table lc_watchbo owner to user_eadm;
alter table lc_watchbo drop constraint if exists pk_watchbo_ptime cascade;
alter table lc_watchbo add constraint pk_watchbo_ptime primary key (ptime);

comment on column lc_watchbo.ptime is '数据获取时间';
comment on column lc_watchbo.bloodoxygen is '血氧';
comment on column lc_watchbo.inserttime is '数据写入时间';
comment on table lc_watchbo is '业务数据_手表血氧信息';

-- 业务数据_手表睡眠信息
drop table if exists lc_watchsleep;
create table lc_watchsleep (
  ptime timestamptz not null,
  sleeptype smallint,
  starttime timestamptz,
  endtime timestamptz,
  minute smallint,
  inserttime timestamptz not null default current_timestamp
);

alter table lc_watchsleep owner to user_eadm;
alter table lc_watchsleep drop constraint if exists pk_watchsleep_ptime cascade;
alter table lc_watchsleep add constraint pk_watchsleep_ptime primary key (ptime);

comment on column lc_watchsleep.ptime is '数据获取时间';
comment on column lc_watchsleep.sleeptype is '睡眠类型(1深度睡眠2浅度睡眠3醒来时长)';
comment on column lc_watchsleep.starttime is '睡眠开始时间';
comment on column lc_watchsleep.endtime is '睡眠结束时间';
comment on column lc_watchsleep.minute is '睡眠时长(分钟)';
comment on column lc_watchsleep.inserttime is '数据写入时间';
comment on table lc_watchsleep is '业务数据_手表睡眠信息';

-- 业务数据_手表蓝牙信息
drop table if exists lc_watchbluet;
create table lc_watchbluet (
  ptime timestamptz not null,
  btinfo varchar(500),
  inserttime timestamptz not null default current_timestamp
);

alter table lc_watchbluet owner to user_eadm;
alter table lc_watchbluet drop constraint if exists pk_watchbluet_ptime cascade;
alter table lc_watchbluet add constraint pk_watchbluet_ptime primary key (ptime);

comment on column lc_watchbluet.ptime is '数据获取时间';
comment on column lc_watchbluet.btinfo is '蓝牙信息';
comment on column lc_watchbluet.inserttime is '数据写入时间';
comment on table lc_watchbluet is '业务数据_手表蓝牙信息';

drop table if exists lc_watchalarm;
create table lc_watchalarm (
  id serial,
  alarmtime timestamptz,
  alarmtype smallint,
  alarminfo varchar(1000),
  inserttime timestamptz not null default current_timestamp
);

alter table lc_watchalarm owner to user_eadm;
alter table lc_watchalarm drop constraint if exists pk_watchalarm_id cascade;
alter table lc_watchalarm add constraint pk_watchalarm_id primary key (id);

drop index if exists non_watchalarm_alarmtype;
create index non_watchalarm_alarmtype on lc_watchalarm using btree (alarmtype asc nulls last);

comment on column lc_watchalarm.id is '自增主键';
comment on column lc_watchalarm.alarmtime is '预警时间';
comment on column lc_watchalarm.alarmtype is '预警类型';
comment on column lc_watchalarm.alarminfo is '报警信息内容';
comment on table lc_watchalarm is '手表日常报警信息';

-- 财务数据
drop table if exists fn_paybilldetail;
create table fn_paybilldetail (
  id serial,
  owner varchar(50),
  sourcetype int2,
  inorout varchar(10),
  counterparty varchar(100),
  counterbank varchar(100),
  counteraccount varchar(50),
  goodscomment varchar(200),
  paymethod varchar(50),
  amount numeric(18,2),
  balance numeric(18,2),
  currency varchar(50),
  paystatus varchar(50),
  tradetype varchar(50),
  tradeorderno varchar(100),
  counterorderno varchar(100),
  tradetime timestamptz,
  billcomment varchar(500),
  inserttime timestamptz not null default current_timestamp,
  deleteduser varchar(50),
  deletedat timestamptz,
  deleted boolean not null default false
);

alter table fn_paybilldetail owner to user_eadm;

alter table fn_paybilldetail drop constraint if exists pk_paybilldetail_id cascade;
alter table fn_paybilldetail add constraint pk_paybilldetail_id primary key (id);

drop index if exists non_paybilldetail_sourcetype;
create index non_paybilldetail_sourcetype on fn_paybilldetail using btree (sourcetype asc nulls last);
drop index if exists non_paybilldetail_inorout;
create index non_paybilldetail_inorout on fn_paybilldetail using btree (inorout asc nulls last);
drop index if exists non_paybilldetail_paymethod;
create index non_paybilldetail_paymethod on fn_paybilldetail using btree (paymethod asc nulls last);
drop index if exists non_paybilldetail_paystatus;
create index non_paybilldetail_paystatus on fn_paybilldetail using btree (paystatus asc nulls last);
drop index if exists non_paybilldetail_tradetype;
create index non_paybilldetail_tradetype on fn_paybilldetail using btree (tradetype asc nulls last);
drop index if exists non_paybilldetail_tradetime;
create index non_paybilldetail_tradetime on fn_paybilldetail using btree (tradetime asc nulls last);

comment on column fn_paybilldetail.id is '自增主键';
comment on column fn_paybilldetail.owner is '来源人(姓名)';
comment on column fn_paybilldetail.sourcetype is '来源:1支付宝2微信3青岛银行4中国银行';
comment on column fn_paybilldetail.inorout is '收/支';
comment on column fn_paybilldetail.counterparty is '交易对方';
comment on column fn_paybilldetail.counterbank is '对方开户行';
comment on column fn_paybilldetail.counteraccount is '对方账号';
comment on column fn_paybilldetail.goodscomment is '商品说明';
comment on column fn_paybilldetail.paymethod is '收/付款方式';
comment on column fn_paybilldetail.amount is '金额';
comment on column fn_paybilldetail.balance is '余额';
comment on column fn_paybilldetail.currency is '币种';
comment on column fn_paybilldetail.paystatus is '交易状态';
comment on column fn_paybilldetail.tradetype is '交易分类';
comment on column fn_paybilldetail.tradeorderno is '交易订单号';
comment on column fn_paybilldetail.counterorderno is '商家订单号';
comment on column fn_paybilldetail.tradetime is '交易时间';
comment on column fn_paybilldetail.billcomment is '交易备注';
comment on column fn_paybilldetail.inserttime is '数据写入时间';
comment on column fn_paybilldetail.deleteduser is '删除人账号';
comment on column fn_paybilldetail.deletedat is '删除时间';
comment on column fn_paybilldetail.deleted is '是否删除(0否1是)';
comment on table fn_paybilldetail is '账单明细';

-- EMQX设备数据表
drop table if exists emqx_device_data cascade;
create table emqx_device_data (
  id serial,
  imei varchar(50) not null,
  imsi varchar(50),
  lat numeric(9,6),
  lng numeric(9,6),
  agps_lat numeric(9,6),
  agps_lng numeric(9,6),
  uptime bigint,
  rsrp smallint,
  csq smallint,
  vbat smallint,
  agps_ts bigint,
  gps_ts bigint,
  rssi smallint,
  rsrq smallint,
  snr smallint,
  receivetime timestamptz,
  inserttime timestamptz not null default current_timestamp
);

alter table emqx_device_data owner to user_eadm;
alter table emqx_device_data drop constraint if exists pk_emqx_device_data_id cascade;
alter table emqx_device_data add constraint pk_emqx_device_data_id primary key (id);

drop index if exists non_emqx_device_data_imei;
create index non_emqx_device_data_imei on emqx_device_data using btree (imei asc nulls last);
drop index if exists non_emqx_device_data_inserttime;
create index non_emqx_device_data_inserttime on emqx_device_data using btree (inserttime desc nulls last);

comment on column emqx_device_data.id is '自增主键';
comment on column emqx_device_data.imei is '设备IMEI号';
comment on column emqx_device_data.imsi is '设备IMSI号';
comment on column emqx_device_data.lat is 'GPS纬度';
comment on column emqx_device_data.lng is 'GPS经度';
comment on column emqx_device_data.agps_lat is 'AGPS纬度';
comment on column emqx_device_data.agps_lng is 'AGPS经度';
comment on column emqx_device_data.uptime is '设备运行时间(秒)';
comment on column emqx_device_data.rsrp is '参考信号接收功率(dBm)';
comment on column emqx_device_data.csq is '信号质量(0-31)';
comment on column emqx_device_data.vbat is '电池电压(mV)';
comment on column emqx_device_data.agps_ts is 'AGPS时间戳';
comment on column emqx_device_data.gps_ts is 'GPS时间戳';
comment on column emqx_device_data.rssi is '接收信号强度指示(dBm)';
comment on column emqx_device_data.rsrq is '参考信号接收质量(dB)';
comment on column emqx_device_data.snr is '信噪比(dB)';
comment on column emqx_device_data.receivetime is '数据接收时间';
comment on column emqx_device_data.inserttime is '数据写入时间';
comment on table emqx_device_data is 'EMQX设备数据表';

-- =============================================
-- 佳明_活动记录表
-- =============================================
drop table if exists garmin_activity cascade;
create table garmin_activity (
  id serial,
  activityid varchar(50) not null,
  activityname varchar(255),
  activitytype varchar(100),
  sporttype varchar(100),
  starttime timestamptz,
  endtime timestamptz,
  duration numeric(12,2),
  distance numeric(12,2),
  calories int,
  avghr int,
  maxhr int,
  avgspeed numeric(10,4),
  maxspeed numeric(10,4),
  avgcadence int,
  maxcadence int,
  elevationgain numeric(10,2),
  elevationloss numeric(10,2),
  startlat numeric(12,8),
  startlng numeric(12,8),
  endlat numeric(12,8),
  endlng numeric(12,8),
  trainingeffect numeric(4,2),
  anaerobiceffect numeric(4,2),
  avgpower int,
  maxpower int,
  vo2max numeric(6,2),
  rawjson json,
  createdat timestamptz default current_timestamp,
  updatedat timestamptz default current_timestamp
);

alter table garmin_activity owner to user_eadm;
alter table garmin_activity drop constraint if exists pk_activity_id cascade;
alter table garmin_activity add constraint pk_activity_id primary key (id);
alter table garmin_activity drop constraint if exists uni_activity_activityid cascade;
alter table garmin_activity add constraint uni_activity_activityid unique (activityid);

drop index if exists non_activity_starttime;
create index non_activity_starttime on garmin_activity using btree (starttime desc nulls last);
drop index if exists non_activity_activitytype;
create index non_activity_activitytype on garmin_activity using btree (activitytype asc nulls last);

comment on column garmin_activity.id is '自增主键';
comment on column garmin_activity.activityid is '佳明活动id';
comment on column garmin_activity.activityname is '活动名称';
comment on column garmin_activity.activitytype is '活动类型';
comment on column garmin_activity.sporttype is '运动类型';
comment on column garmin_activity.starttime is '开始时间';
comment on column garmin_activity.endtime is '结束时间';
comment on column garmin_activity.duration is '持续时长(秒)';
comment on column garmin_activity.distance is '距离(米)';
comment on column garmin_activity.calories is '消耗卡路里';
comment on column garmin_activity.avghr is '平均心率';
comment on column garmin_activity.maxhr is '最大心率';
comment on column garmin_activity.avgspeed is '平均速度(m/s)';
comment on column garmin_activity.maxspeed is '最大速度(m/s)';
comment on column garmin_activity.avgcadence is '平均步频';
comment on column garmin_activity.maxcadence is '最大步频';
comment on column garmin_activity.elevationgain is '累计爬升(米)';
comment on column garmin_activity.elevationloss is '累计下降(米)';
comment on column garmin_activity.startlat is '起点纬度';
comment on column garmin_activity.startlng is '起点经度';
comment on column garmin_activity.endlat is '终点纬度';
comment on column garmin_activity.endlng is '终点经度';
comment on column garmin_activity.trainingeffect is '有氧训练效果';
comment on column garmin_activity.anaerobiceffect is '无氧训练效果';
comment on column garmin_activity.avgpower is '平均功率(w)';
comment on column garmin_activity.maxpower is '最大功率(w)';
comment on column garmin_activity.vo2max is '最大摄氧量';
comment on column garmin_activity.rawjson is '原始json数据';
comment on column garmin_activity.createdat is '创建时间';
comment on column garmin_activity.updatedat is '更新时间';
comment on table garmin_activity is '佳明_活动记录表';

drop trigger if exists activity_lastupdate on garmin_activity cascade;
create or replace trigger activity_lastupdate
before update on garmin_activity
for each row
execute function lastupdate();

-- =============================================
-- 佳明_活动详情表（GPS轨迹点）
-- =============================================
drop table if exists garmin_activity_detail cascade;
create table garmin_activity_detail (
  id serial,
  activityid varchar(50) not null,
  pointtime timestamptz not null,
  latitude numeric(12,8),
  longitude numeric(12,8),
  elevation numeric(10,2),
  heartrate int,
  speed numeric(10,4),
  cadence int,
  power int,
  temperature numeric(5,1),
  distance numeric(12,2),
  createdat timestamptz default current_timestamp,
  updatedat timestamptz default current_timestamp
);

alter table garmin_activity_detail owner to user_eadm;
alter table garmin_activity_detail drop constraint if exists pk_activity_detail_id cascade;
alter table garmin_activity_detail add constraint pk_activity_detail_id primary key (id);
alter table garmin_activity_detail drop constraint if exists uni_activity_detail_point cascade;
alter table garmin_activity_detail add constraint uni_activity_detail_point unique (activityid, pointtime);

drop index if exists non_activity_detail_activityid;
create index non_activity_detail_activityid on garmin_activity_detail using btree (activityid asc nulls last);
drop index if exists non_activity_detail_pointtime;
create index non_activity_detail_pointtime on garmin_activity_detail using btree (pointtime desc nulls last);

comment on column garmin_activity_detail.id is '自增主键';
comment on column garmin_activity_detail.activityid is '活动id';
comment on column garmin_activity_detail.pointtime is '轨迹点时间';
comment on column garmin_activity_detail.latitude is '纬度';
comment on column garmin_activity_detail.longitude is '经度';
comment on column garmin_activity_detail.elevation is '海拔(米)';
comment on column garmin_activity_detail.heartrate is '心率';
comment on column garmin_activity_detail.speed is '速度(m/s)';
comment on column garmin_activity_detail.cadence is '步频';
comment on column garmin_activity_detail.power is '功率(w)';
comment on column garmin_activity_detail.temperature is '温度(℃)';
comment on column garmin_activity_detail.distance is '累计距离(米)';
comment on column garmin_activity_detail.createdat is '创建时间';
comment on column garmin_activity_detail.updatedat is '更新时间';
comment on table garmin_activity_detail is '佳明_活动详情表(gps轨迹点)';

drop trigger if exists activity_detail_lastupdate on garmin_activity_detail cascade;
create or replace trigger activity_detail_lastupdate
before update on garmin_activity_detail
for each row
execute function lastupdate();

-- =============================================
-- 佳明_心率时序表
-- =============================================
drop table if exists garmin_heartrate_detail cascade;
create table garmin_heartrate_detail (
  id serial,
  hrdate date not null,
  pointtime timestamptz not null,
  heartrate int not null,
  createdat timestamptz default current_timestamp,
  updatedat timestamptz default current_timestamp
);

alter table garmin_heartrate_detail owner to user_eadm;
alter table garmin_heartrate_detail drop constraint if exists pk_heartrate_detail_id cascade;
alter table garmin_heartrate_detail add constraint pk_heartrate_detail_id primary key (id);
alter table garmin_heartrate_detail drop constraint if exists uni_heartrate_detail_point cascade;
alter table garmin_heartrate_detail add constraint uni_heartrate_detail_point unique (hrdate, pointtime);

drop index if exists non_heartrate_detail_hrdate;
create index non_heartrate_detail_hrdate on garmin_heartrate_detail using btree (hrdate desc nulls last);

comment on column garmin_heartrate_detail.id is '自增主键';
comment on column garmin_heartrate_detail.hrdate is '心率日期';
comment on column garmin_heartrate_detail.pointtime is '时间点';
comment on column garmin_heartrate_detail.heartrate is '心率值';
comment on column garmin_heartrate_detail.createdat is '创建时间';
comment on column garmin_heartrate_detail.updatedat is '更新时间';
comment on table garmin_heartrate_detail is '佳明_心率时序明细表';

drop trigger if exists heartrate_detail_lastupdate on garmin_heartrate_detail cascade;
create or replace trigger heartrate_detail_lastupdate
before update on garmin_heartrate_detail
for each row
execute function lastupdate();

-- =============================================
-- 佳明_压力时序表
-- =============================================
drop table if exists garmin_stress_detail cascade;
create table garmin_stress_detail (
  id serial,
  stressdate date not null,
  pointtime timestamptz not null,
  stresslevel int not null,
  createdat timestamptz default current_timestamp,
  updatedat timestamptz default current_timestamp
);

alter table garmin_stress_detail owner to user_eadm;
alter table garmin_stress_detail drop constraint if exists pk_stress_detail_id cascade;
alter table garmin_stress_detail add constraint pk_stress_detail_id primary key (id);
alter table garmin_stress_detail drop constraint if exists uni_stress_detail_point cascade;
alter table garmin_stress_detail add constraint uni_stress_detail_point unique (stressdate, pointtime);

drop index if exists non_stress_detail_stressdate;
create index non_stress_detail_stressdate on garmin_stress_detail using btree (stressdate desc nulls last);

comment on column garmin_stress_detail.id is '自增主键';
comment on column garmin_stress_detail.stressdate is '压力日期';
comment on column garmin_stress_detail.pointtime is '时间点';
comment on column garmin_stress_detail.stresslevel is '压力值';
comment on column garmin_stress_detail.createdat is '创建时间';
comment on column garmin_stress_detail.updatedat is '更新时间';
comment on table garmin_stress_detail is '佳明_压力时序明细表';

drop trigger if exists stress_detail_lastupdate on garmin_stress_detail cascade;
create or replace trigger stress_detail_lastupdate
before update on garmin_stress_detail
for each row
execute function lastupdate();

-- =============================================
-- 佳明_睡眠明细表（睡眠阶段）
-- =============================================
drop table if exists garmin_sleep_detail cascade;
create table garmin_sleep_detail (
  id serial,
  sleepdate date not null,
  starttime timestamptz not null,
  endtime timestamptz,
  activitylevel numeric(4,1) not null,
  createdat timestamptz default current_timestamp,
  updatedat timestamptz default current_timestamp
);

alter table garmin_sleep_detail owner to user_garmin;
alter table garmin_sleep_detail drop constraint if exists pk_sleep_detail_id cascade;
alter table garmin_sleep_detail add constraint pk_sleep_detail_id primary key (id);
alter table garmin_sleep_detail drop constraint if exists uni_sleep_detail_point cascade;
alter table garmin_sleep_detail add constraint uni_sleep_detail_point unique (sleepdate, starttime);

drop index if exists non_sleep_detail_sleepdate;
create index non_sleep_detail_sleepdate on garmin_sleep_detail using btree (sleepdate desc nulls last);

comment on column garmin_sleep_detail.id is '自增主键';
comment on column garmin_sleep_detail.sleepdate is '睡眠日期';
comment on column garmin_sleep_detail.starttime is '阶段开始时间';
comment on column garmin_sleep_detail.endtime is '阶段结束时间';
comment on column garmin_sleep_detail.activitylevel is '睡眠阶段(0深睡/1浅睡/2rem/3清醒)';
comment on column garmin_sleep_detail.createdat is '创建时间';
comment on column garmin_sleep_detail.updatedat is '更新时间';
comment on table garmin_sleep_detail is '佳明_睡眠明细表(睡眠阶段)';

drop trigger if exists sleep_detail_lastupdate on garmin_sleep_detail cascade;
create or replace trigger sleep_detail_lastupdate
before update on garmin_sleep_detail
for each row
execute function lastupdate();


-- =============================================
drop table if exists garmin_sleep cascade;
create table garmin_sleep (
  id serial,
  sleepdate date not null,
  sleepstart timestamptz,
  sleepend timestamptz,
  totalsleep int,
  deepsleep int,
  lightsleep int,
  remsleep int,
  awaketime int,
  sleepscore int,
  sleepquality varchar(20),
  restlesscount int,
  avgspo2 numeric(5,2),
  lowspo2 numeric(5,2),
  highspo2 numeric(5,2),
  avgrespiration numeric(5,2),
  rawjson json,
  createdat timestamptz default current_timestamp,
  updatedat timestamptz default current_timestamp
);

alter table garmin_sleep owner to user_eadm;
alter table garmin_sleep drop constraint if exists pk_sleep_id cascade;
alter table garmin_sleep add constraint pk_sleep_id primary key (id);
alter table garmin_sleep drop constraint if exists uni_sleep_sleepdate cascade;
alter table garmin_sleep add constraint uni_sleep_sleepdate unique (sleepdate);

drop index if exists non_sleep_sleepdate;
create index non_sleep_sleepdate on garmin_sleep using btree (sleepdate desc nulls last);

comment on column garmin_sleep.id is '自增主键';
comment on column garmin_sleep.sleepdate is '睡眠日期';
comment on column garmin_sleep.sleepstart is '入睡时间';
comment on column garmin_sleep.sleepend is '起床时间';
comment on column garmin_sleep.totalsleep is '总睡眠时长(分钟)';
comment on column garmin_sleep.deepsleep is '深睡眠时长(分钟)';
comment on column garmin_sleep.lightsleep is '浅睡眠时长(分钟)';
comment on column garmin_sleep.remsleep is 'rem睡眠时长(分钟)';
comment on column garmin_sleep.awaketime is '清醒时长(分钟)';
comment on column garmin_sleep.sleepscore is '睡眠评分';
comment on column garmin_sleep.sleepquality is '睡眠质量';
comment on column garmin_sleep.restlesscount is '翻身次数';
comment on column garmin_sleep.avgspo2 is '平均血氧';
comment on column garmin_sleep.lowspo2 is '最低血氧';
comment on column garmin_sleep.highspo2 is '最高血氧';
comment on column garmin_sleep.avgrespiration is '平均呼吸频率';
comment on column garmin_sleep.rawjson is '原始json数据';
comment on column garmin_sleep.createdat is '创建时间';
comment on column garmin_sleep.updatedat is '更新时间';
comment on table garmin_sleep is '佳明_睡眠数据表';

drop trigger if exists sleep_lastupdate on garmin_sleep cascade;
create or replace trigger sleep_lastupdate
before update on garmin_sleep
for each row
execute function lastupdate();

-- =============================================
-- 佳明_心率数据表
-- =============================================
drop table if exists garmin_heartrate cascade;
create table garmin_heartrate (
  id serial,
  hrdate date not null,
  restinghr int,
  maxhr int,
  minhr int,
  rawjson json,
  createdat timestamptz default current_timestamp,
  updatedat timestamptz default current_timestamp
);

alter table garmin_heartrate owner to user_eadm;
alter table garmin_heartrate drop constraint if exists pk_heartrate_id cascade;
alter table garmin_heartrate add constraint pk_heartrate_id primary key (id);
alter table garmin_heartrate drop constraint if exists uni_heartrate_hrdate cascade;
alter table garmin_heartrate add constraint uni_heartrate_hrdate unique (hrdate);

drop index if exists non_heartrate_hrdate;
create index non_heartrate_hrdate on garmin_heartrate using btree (hrdate desc nulls last);

comment on column garmin_heartrate.id is '自增主键';
comment on column garmin_heartrate.hrdate is '心率日期';
comment on column garmin_heartrate.restinghr is '静息心率';
comment on column garmin_heartrate.maxhr is '最大心率';
comment on column garmin_heartrate.minhr is '最低心率';
comment on column garmin_heartrate.rawjson is '原始json数据';
comment on column garmin_heartrate.createdat is '创建时间';
comment on column garmin_heartrate.updatedat is '更新时间';
comment on table garmin_heartrate is '佳明_心率数据表';

drop trigger if exists heartrate_lastupdate on garmin_heartrate cascade;
create or replace trigger heartrate_lastupdate
before update on garmin_heartrate
for each row
execute function lastupdate();

-- =============================================
-- 佳明_压力数据表
-- =============================================
drop table if exists garmin_stress cascade;
create table garmin_stress (
  id serial,
  stressdate date not null,
  overalllevel int,
  restduration int,
  lowduration int,
  mediumduration int,
  highduration int,
  stressscore int,
  rawjson json,
  createdat timestamptz default current_timestamp,
  updatedat timestamptz default current_timestamp
);

alter table garmin_stress owner to user_eadm;
alter table garmin_stress drop constraint if exists pk_stress_id cascade;
alter table garmin_stress add constraint pk_stress_id primary key (id);
alter table garmin_stress drop constraint if exists uni_stress_stressdate cascade;
alter table garmin_stress add constraint uni_stress_stressdate unique (stressdate);

drop index if exists non_stress_stressdate;
create index non_stress_stressdate on garmin_stress using btree (stressdate desc nulls last);

comment on column garmin_stress.id is '自增主键';
comment on column garmin_stress.stressdate is '压力日期';
comment on column garmin_stress.overalllevel is '综合压力水平';
comment on column garmin_stress.restduration is '休息时长(秒)';
comment on column garmin_stress.lowduration is '低压力时长(秒)';
comment on column garmin_stress.mediumduration is '中等压力时长(秒)';
comment on column garmin_stress.highduration is '高压力时长(秒)';
comment on column garmin_stress.stressscore is '压力评分';
comment on column garmin_stress.rawjson is '原始json数据';
comment on column garmin_stress.createdat is '创建时间';
comment on column garmin_stress.updatedat is '更新时间';
comment on table garmin_stress is '佳明_压力数据表';

drop trigger if exists stress_lastupdate on garmin_stress cascade;
create or replace trigger stress_lastupdate
before update on garmin_stress
for each row
execute function lastupdate();

-- =============================================
-- 佳明_血氧数据表
-- =============================================
drop table if exists garmin_spo2 cascade;
create table garmin_spo2 (
  id serial,
  spo2date date not null,
  avgspo2 numeric(5,2),
  lowspo2 numeric(5,2),
  highspo2 numeric(5,2),
  latestspo2 numeric(5,2),
  rawjson json,
  createdat timestamptz default current_timestamp,
  updatedat timestamptz default current_timestamp
);

alter table garmin_spo2 owner to user_eadm;
alter table garmin_spo2 drop constraint if exists pk_spo2_id cascade;
alter table garmin_spo2 add constraint pk_spo2_id primary key (id);
alter table garmin_spo2 drop constraint if exists uni_spo2_spo2date cascade;
alter table garmin_spo2 add constraint uni_spo2_spo2date unique (spo2date);

drop index if exists non_spo2_spo2date;
create index non_spo2_spo2date on garmin_spo2 using btree (spo2date desc nulls last);

comment on column garmin_spo2.id is '自增主键';
comment on column garmin_spo2.spo2date is '血氧日期';
comment on column garmin_spo2.avgspo2 is '平均血氧';
comment on column garmin_spo2.lowspo2 is '最低血氧';
comment on column garmin_spo2.highspo2 is '最高血氧';
comment on column garmin_spo2.latestspo2 is '最近一次血氧';
comment on column garmin_spo2.rawjson is '原始json数据';
comment on column garmin_spo2.createdat is '创建时间';
comment on column garmin_spo2.updatedat is '更新时间';
comment on table garmin_spo2 is '佳明_脉搏血氧数据表';

drop trigger if exists spo2_lastupdate on garmin_spo2 cascade;
create or replace trigger spo2_lastupdate
before update on garmin_spo2
for each row
execute function lastupdate();

-- =============================================
-- 佳明_血氧明细表（时序数据点）
-- =============================================
drop table if exists garmin_spo2_detail cascade;
create table garmin_spo2_detail (
  id serial,
  spo2date date not null,
  pointtime timestamptz not null,
  spo2value numeric(5,2) not null,
  readingsource varchar(20),
  createdat timestamptz default current_timestamp,
  updatedat timestamptz default current_timestamp
);

alter table garmin_spo2_detail owner to user_garmin;
alter table garmin_spo2_detail drop constraint if exists pk_spo2_detail_id cascade;
alter table garmin_spo2_detail add constraint pk_spo2_detail_id primary key (id);
alter table garmin_spo2_detail drop constraint if exists uni_spo2_detail_point cascade;
alter table garmin_spo2_detail add constraint uni_spo2_detail_point unique (spo2date, pointtime);

drop index if exists non_spo2_detail_spo2date;
create index non_spo2_detail_spo2date on garmin_spo2_detail using btree (spo2date desc nulls last);

comment on column garmin_spo2_detail.id is '自增主键';
comment on column garmin_spo2_detail.spo2date is '血氧日期';
comment on column garmin_spo2_detail.pointtime is '采集时间';
comment on column garmin_spo2_detail.spo2value is '血氧值';
comment on column garmin_spo2_detail.readingsource is '读取来源(hourly/continuous/single)';
comment on column garmin_spo2_detail.createdat is '创建时间';
comment on column garmin_spo2_detail.updatedat is '更新时间';
comment on table garmin_spo2_detail is '佳明_血氧明细表(时序数据点)';

drop trigger if exists spo2_detail_lastupdate on garmin_spo2_detail cascade;
create or replace trigger spo2_detail_lastupdate
before update on garmin_spo2_detail
for each row
execute function lastupdate();

-- =============================================
-- 佳明_呼吸数据表
-- =============================================

drop table if exists garmin_respiration cascade;
create table garmin_respiration (
  id serial,
  respdate date not null,
  avgwaking numeric(5,2),
  highwaking numeric(5,2),
  lowwaking numeric(5,2),
  avgsleeping numeric(5,2),
  highsleeping numeric(5,2),
  lowsleeping numeric(5,2),
  rawjson json,
  createdat timestamptz default current_timestamp,
  updatedat timestamptz default current_timestamp
);

alter table garmin_respiration owner to user_eadm;
alter table garmin_respiration drop constraint if exists pk_respiration_id cascade;
alter table garmin_respiration add constraint pk_respiration_id primary key (id);
alter table garmin_respiration drop constraint if exists uni_respiration_respdate cascade;
alter table garmin_respiration add constraint uni_respiration_respdate unique (respdate);

drop index if exists non_respiration_respdate;
create index non_respiration_respdate on garmin_respiration using btree (respdate desc nulls last);

comment on column garmin_respiration.id is '自增主键';
comment on column garmin_respiration.respdate is '呼吸日期';
comment on column garmin_respiration.avgwaking is '清醒时平均呼吸(次/分钟)';
comment on column garmin_respiration.highwaking is '清醒时最高呼吸(次/分钟)';
comment on column garmin_respiration.lowwaking is '清醒时最低呼吸(次/分钟)';
comment on column garmin_respiration.avgsleeping is '睡眠时平均呼吸(次/分钟)';
comment on column garmin_respiration.highsleeping is '睡眠时最高呼吸(次/分钟)';
comment on column garmin_respiration.lowsleeping is '睡眠时最低呼吸(次/分钟)';
comment on column garmin_respiration.rawjson is '原始json数据';
comment on column garmin_respiration.createdat is '创建时间';
comment on column garmin_respiration.updatedat is '更新时间';
comment on table garmin_respiration is '佳明_呼吸数据表';

drop trigger if exists respiration_lastupdate on garmin_respiration cascade;
create or replace trigger respiration_lastupdate
before update on garmin_respiration
for each row
execute function lastupdate();

-- =============================================
-- 佳明_呼吸明细表（时序数据点）
-- =============================================
drop table if exists garmin_respiration_detail cascade;
create table garmin_respiration_detail (
  id serial,
  respdate date not null,
  pointtime timestamptz not null,
  respvalue numeric(5,2) not null,
  createdat timestamptz default current_timestamp,
  updatedat timestamptz default current_timestamp
);

alter table garmin_respiration_detail owner to user_garmin;
alter table garmin_respiration_detail drop constraint if exists pk_respiration_detail_id cascade;
alter table garmin_respiration_detail add constraint pk_respiration_detail_id primary key (id);
alter table garmin_respiration_detail drop constraint if exists uni_respiration_detail_point cascade;
alter table garmin_respiration_detail add constraint uni_respiration_detail_point unique (respdate, pointtime);

drop index if exists non_respiration_detail_respdate;
create index non_respiration_detail_respdate on garmin_respiration_detail using btree (respdate desc nulls last);

comment on column garmin_respiration_detail.id is '自增主键';
comment on column garmin_respiration_detail.respdate is '呼吸日期';
comment on column garmin_respiration_detail.pointtime is '采集时间';
comment on column garmin_respiration_detail.respvalue is '呼吸频率(次/分钟)';
comment on column garmin_respiration_detail.createdat is '创建时间';
comment on column garmin_respiration_detail.updatedat is '更新时间';
comment on table garmin_respiration_detail is '佳明_呼吸明细表(时序数据点)';

drop trigger if exists respiration_detail_lastupdate on garmin_respiration_detail cascade;
create or replace trigger respiration_detail_lastupdate
before update on garmin_respiration_detail
for each row
execute function lastupdate();

-- =============================================
-- 佳明_HRV数据表
-- =============================================

drop table if exists garmin_hrv cascade;
create table garmin_hrv (
  id serial,
  hrvdate date not null,
  weeklyavg numeric(8,2),
  lastnightavg numeric(8,2),
  lastnight5minhigh numeric(8,2),
  baselinelowupper numeric(8,2),
  baselinebalancedlow numeric(8,2),
  baselinebalancedupper numeric(8,2),
  hrvstatus varchar(20),
  rawjson json,
  createdat timestamptz default current_timestamp,
  updatedat timestamptz default current_timestamp
);

alter table garmin_hrv owner to user_eadm;
alter table garmin_hrv drop constraint if exists pk_hrv_id cascade;
alter table garmin_hrv add constraint pk_hrv_id primary key (id);
alter table garmin_hrv drop constraint if exists uni_hrv_hrvdate cascade;
alter table garmin_hrv add constraint uni_hrv_hrvdate unique (hrvdate);

drop index if exists non_hrv_hrvdate;
create index non_hrv_hrvdate on garmin_hrv using btree (hrvdate desc nulls last);

comment on column garmin_hrv.id is '自增主键';
comment on column garmin_hrv.hrvdate is 'hrv日期';
comment on column garmin_hrv.weeklyavg is '周平均值';
comment on column garmin_hrv.lastnightavg is '昨晚平均值';
comment on column garmin_hrv.lastnight5minhigh is '昨晚5分钟最高值';
comment on column garmin_hrv.baselinelowupper is '基线低值上限';
comment on column garmin_hrv.baselinebalancedlow is '基线平衡低值';
comment on column garmin_hrv.baselinebalancedupper is '基线平衡上限';
comment on column garmin_hrv.hrvstatus is 'hrv状态';
comment on column garmin_hrv.rawjson is '原始json数据';
comment on column garmin_hrv.createdat is '创建时间';
comment on column garmin_hrv.updatedat is '更新时间';
comment on table garmin_hrv is '佳明_hrv数据表';

drop trigger if exists hrv_lastupdate on garmin_hrv cascade;
create or replace trigger hrv_lastupdate
before update on garmin_hrv
for each row
execute function lastupdate();

-- =============================================
-- 数据同步记录表（下载保存记录，做去重用）
-- =============================================
drop table if exists garmin_sync cascade;
create table garmin_sync (
  id serial,
  datasource varchar(20) not null,
  datatype varchar(50) not null,
  datadate date not null,
  dataid varchar(50),
  syncstatus smallint not null default 1,
  errmessage text,
  createdat timestamptz default current_timestamp,
  updatedat timestamptz default current_timestamp
);

alter table garmin_sync owner to user_eadm;
alter table garmin_sync drop constraint if exists pk_sync_id cascade;
alter table garmin_sync add constraint pk_sync_id primary key (id);
alter table garmin_sync drop constraint if exists uni_sync_source_type_date cascade;
alter table garmin_sync add constraint uni_sync_source_type_date unique (datasource, datatype, datadate);

drop index if exists non_sync_datasource;
create index non_sync_datasource on garmin_sync using btree (datasource asc nulls last);
drop index if exists non_sync_datatype;
create index non_sync_datatype on garmin_sync using btree (datatype asc nulls last);
drop index if exists non_sync_datadate;
create index non_sync_datadate on garmin_sync using btree (datadate desc nulls last);

comment on column garmin_sync.id is '自增主键';
comment on column garmin_sync.datasource is '数据来源(garmin/polar/coros)';
comment on column garmin_sync.datatype is '数据类型(activity/sleep/heartrate/stress/spo2/respiration/hrv)';
comment on column garmin_sync.datadate is '数据日期';
comment on column garmin_sync.dataid is '数据唯一标识(如activityid)';
comment on column garmin_sync.syncstatus is '同步状态(1成功0失败)';
comment on column garmin_sync.errmessage is '错误信息';
comment on column garmin_sync.createdat is '创建时间';
comment on column garmin_sync.updatedat is '更新时间';
comment on table garmin_sync is '数据同步记录表';

drop trigger if exists sync_lastupdate on garmin_sync cascade;
create or replace trigger sync_lastupdate
before update on garmin_sync
for each row
execute function lastupdate();