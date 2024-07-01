-- @author wangcw
-- @copyright (C) 2024, REDGREAT
-- created : 2024-04-03 15:17
-- comment : proc for kingbase
-- db version : KingbaseES V009R001C001B0025 on x86_64-pc-linux-gnu, compiled by gcc (GCC) 4.8.5 20150623 (Red Hat 4.8.5-28), 64-bit

CREATE OR REPLACE PROCEDURE public.proc_dashboard(indate character varying)
    LANGUAGE plpgsql
AS $procedure$
    -- author: wangcw
    -- create: 2024-06-07 09:23:40
declare
    sys_starttime timestamp := current_timestamp;
    sys_errcode varchar(10) := '00000';
    sys_errmessage varchar(200);
    result boolean := false;
begin

    truncate table eadm_dashboard;

    set time zone 'Asia/Shanghai';

    -- 周平均心率
    insert into eadm_dashboard(datatype, datetype, loginname, datavalue, checkdate)
    select 1, 2, 'wangcw', nullif(round(avg(cast(heartbeat as int))),0), indate
    from lc_watchdaily
    where ptime >= indate::date - interval '7 days'
      and ptime < indate
      and heartbeat is not null;

    -- 周平均步数
    insert into eadm_dashboard(datatype, datetype, loginname, datavalue, checkdate)
    select 2, 2, 'wangcw', nullif(round(avg(s.maxsteps)),0), indate
    from (
             select max(cast(steps as int)) as maxsteps
             from public.lc_watchdaily
             where ptime >= indate::date - interval '7 days'
               and ptime < indate
               and Steps is not null
             group by to_char(ptime, 'yyyy-mm-dd')) as s
    where s.maxsteps is not null;

    -- 周平均睡眠
    insert into eadm_dashboard(datatype, datetype, loginname, datavalue, checkdate)
    select 3, 2, 'wangcw', nullif(round(avg(s.sumsleepminute/60)),0), indate
    from (
             select sum(cast(sleepminute as int)) as sumsleepminute
             from lc_watchdaily
             where ptime >= indate::date - interval '7 days'
               and ptime < indate
               and sleepminute is not null
             group by to_char(ptime, 'yyyy-mm-dd')) as s
    where s.sumsleepminute is not null;

    -- 周平均里程
    insert into eadm_dashboard(datatype, datetype, loginname, datavalue, checkdate)
    select 4, 2, 'wangcw', nullif(round(avg(duamileage)),0), indate
    from (
             select max(cast(mileage as int)) - min(cast(mileage as int)) as duamileage
             from lc_carlocdaily
             where ptime >= indate::date - interval '7 days'
               and ptime < indate
               and mileage is not null
             group by to_char(ptime, 'yyyy-mm-dd')) as s
    where s.duamileage is not null
      and s.duamileage !=0;

    -- 年度平均心率
    insert into eadm_dashboard(datatype, datetype, loginname, datavalue, checkdate)
    select 1, 4, 'wangcw', nullif(round(avg(cast(heartbeat as int))),0), indate
    from lc_watchdaily
    where ptime >= indate::date - interval '1 years'
      and ptime < indate
      and heartbeat is not null;

    -- 年度平均步数
    insert into eadm_dashboard(datatype, datetype, loginname, datavalue, checkdate)
    select 2, 4, 'wangcw', nullif(round(avg(s.maxsteps)),0), indate
    from (
             select max(cast(steps as int)) as maxsteps
             from lc_watchdaily
             where ptime >= indate::date - interval '1 years'
               and ptime < indate
               and steps is not null
             group by to_char(ptime, 'yyyy-mm-dd')) as s
    where s.maxsteps is not null;

    -- 年度平均睡眠
    insert into eadm_dashboard(datatype, datetype, loginname, datavalue, checkdate)
    select 3, 4, 'wangcw', nullif(round(avg(s.sumsleepminute/60)),0), indate
    from (
             select sum(cast(sleepminute as int)) as sumsleepminute
             from lc_watchdaily
             where ptime >= indate::date - interval '1 years'
               and ptime < indate
               and sleepminute is not null
             group by to_char(ptime, 'yyyy-mm-dd')) as s
    where s.sumsleepminute is not null;

    -- 年度总里程
    insert into eadm_dashboard(datatype, datetype, loginname, datavalue, checkdate)
    select 4, 4, 'wangcw', nullif(max(mileage) - min(mileage),0), indate
    from lc_carlocdaily
    where ptime >= indate::date - INTERVAL '1 years'
      and ptime < indate
      and mileage is not null;

    -- 每月里程
    insert into eadm_dashboard(datatype, datetype, datavalue, loginname, checkdate)
    values (5, 3, 0, 'wangcw', to_char(indate::date, 'yyyy-mm')),
           (5, 3, 0, 'wangcw', to_char(indate::date - interval '1 months', 'yyyy-mm')),
           (5, 3, 0, 'wangcw', to_char(indate::date - interval '2 months', 'yyyy-mm')),
           (5, 3, 0, 'wangcw', to_char(indate::date - interval '3 months', 'yyyy-mm')),
           (5, 3, 0, 'wangcw', to_char(indate::date - interval '4 months', 'yyyy-mm')),
           (5, 3, 0, 'wangcw', to_char(indate::date - interval '5 months', 'yyyy-mm')),
           (5, 3, 0, 'wangcw', to_char(indate::date - interval '6 months', 'yyyy-mm')),
           (5, 3, 0, 'wangcw', to_char(indate::date - interval '7 months', 'yyyy-mm')),
           (5, 3, 0, 'wangcw', to_char(indate::date - interval '8 months', 'yyyy-mm')),
           (5, 3, 0, 'wangcw', to_char(indate::date - interval '9 months', 'yyyy-mm')),
           (5, 3, 0, 'wangcw', to_char(indate::date - interval '10 months', 'yyyy-mm')),
           (5, 3, 0, 'wangcw', to_char(indate::date - interval '11 months', 'yyyy-mm'));

    update eadm_dashboard a
    set datavalue=d.mileage
    from (select to_char(ptime, 'yyyy-mm') as checkmonth,
                 max(b.mileage)-min(b.mileage) as mileage
          from lc_carlocdaily b,
               eadm_userdevice c
          where b.deviceno=c.deviceno
            and c.loginname='wangcw'
          group by to_char(b.ptime, 'yyyy-mm')
          ORDER BY checkmonth) d
    where a.checkdate=d.checkmonth
      and a.datetype=3
      and a.datatype=5;

    -- 每月收入
    insert into eadm_dashboard(datatype, datetype, datavalue, loginname, checkdate)
    values (6, 3, 0, 'wangcw', to_char(indate::date, 'yyyy-mm')),
           (6, 3, 0, 'wangcw', to_char(indate::date - interval '1 months', 'yyyy-mm')),
           (6, 3, 0, 'wangcw', to_char(indate::date - interval '2 months', 'yyyy-mm')),
           (6, 3, 0, 'wangcw', to_char(indate::date - interval '3 months', 'yyyy-mm')),
           (6, 3, 0, 'wangcw', to_char(indate::date - interval '4 months', 'yyyy-mm')),
           (6, 3, 0, 'wangcw', to_char(indate::date - interval '5 months', 'yyyy-mm')),
           (6, 3, 0, 'wangcw', to_char(indate::date - interval '6 months', 'yyyy-mm')),
           (6, 3, 0, 'wangcw', to_char(indate::date - interval '7 months', 'yyyy-mm')),
           (6, 3, 0, 'wangcw', to_char(indate::date - interval '8 months', 'yyyy-mm')),
           (6, 3, 0, 'wangcw', to_char(indate::date - interval '9 months', 'yyyy-mm')),
           (6, 3, 0, 'wangcw', to_char(indate::date - interval '10 months', 'yyyy-mm')),
           (6, 3, 0, 'wangcw', to_char(indate::date - interval '11 months', 'yyyy-mm'));

    update eadm_dashboard a
    set datavalue=c.amount
    from (select to_char(tradetime, 'yyyy-mm') as checkmonth,
                 sum(nullif(amount,0)) as amount
          from fn_paybilldetail b
          where b.inorout='收入'
          group by to_char(tradetime, 'yyyy-mm')) c
    where a.datetype=3
      and a.datatype=6
      and a.checkdate=c.checkmonth;

    -- 每月支出
    insert into eadm_dashboard(datatype, datetype, datavalue, loginname, checkdate)
    values (7, 3, 0, 'wangcw', to_char(indate::date, 'yyyy-mm')),
           (7, 3, 0, 'wangcw', to_char(indate::date - interval '1 months', 'yyyy-mm')),
           (7, 3, 0, 'wangcw', to_char(indate::date - interval '2 months', 'yyyy-mm')),
           (7, 3, 0, 'wangcw', to_char(indate::date - interval '3 months', 'yyyy-mm')),
           (7, 3, 0, 'wangcw', to_char(indate::date - interval '4 months', 'yyyy-mm')),
           (7, 3, 0, 'wangcw', to_char(indate::date - interval '5 months', 'yyyy-mm')),
           (7, 3, 0, 'wangcw', to_char(indate::date - interval '6 months', 'yyyy-mm')),
           (7, 3, 0, 'wangcw', to_char(indate::date - interval '7 months', 'yyyy-mm')),
           (7, 3, 0, 'wangcw', to_char(indate::date - interval '8 months', 'yyyy-mm')),
           (7, 3, 0, 'wangcw', to_char(indate::date - interval '9 months', 'yyyy-mm')),
           (7, 3, 0, 'wangcw', to_char(indate::date - interval '10 months', 'yyyy-mm')),
           (7, 3, 0, 'wangcw', to_char(indate::date - interval '11 months', 'yyyy-mm'));

    update eadm_dashboard a
    set datavalue=c.amount
    from (select to_char(tradetime, 'yyyy-mm') as checkmonth,
                 sum(NULLIF(amount,0)) as amount
          from fn_paybilldetail b
          where b.inorout != '收入'
          group by to_char(tradetime, 'yyyy-mm')) c
    where a.datetype=3
      and a.datatype=7
      and a.checkdate=c.checkmonth;

    result := true;

    -- 日志记录
    insert into sys_proclog(procname, timespan, result, errcode, errmessage)
    values ('proc_dashboard', floor(extract(epoch from (clock_timestamp() - sys_starttime))),
            result, sys_errcode, sys_errmessage);
    commit;

exception when others then
    sys_errcode := sqlstate;
    sys_errmessage := sqlerrm;
    result := false;
    rollback;
    insert into sys_proclog(procname, timespan, result, errcode, errmessage)
    values ('proc_dashboard', floor(extract(epoch from (clock_timestamp() - sys_starttime))),
            result, sys_errcode, sys_errmessage);

end;
$procedure$
;

