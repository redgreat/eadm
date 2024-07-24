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
comment on table sys_dict is '系统_字典信息表';

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
  rolepermission json,
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
  starttime timestamptz,
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
  lac varchar(50),
  cid varchar(50),
  db varchar(50),
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
  steps varchar(50),
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
  lat varchar(50),
  lng varchar(50),
  speed varchar(50),
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
  roll varchar(50),
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
  diastolic varchar(50),
  shrink varchar(50),
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
  bloodsugar varchar(50),
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
   heartbeat varchar(50),
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
  bodytemperature varchar(50),
  wristtemperature varchar(50),
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
  signal varchar(50),
  battery varchar(50),
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
  bloodoxygen varchar(50),
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
  sleeptype varchar(50),
  starttime varchar(50),
  endtime varchar(50),
  minute varchar(50),
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
  alarmtype varchar(10),
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
