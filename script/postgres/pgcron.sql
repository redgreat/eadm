-- @author wangcw
-- @copyright (C) 2024, REDGREAT
-- Created : 2024-04-09 下午1:48

-- postgres 库内执行

create extension if not exists pg_cron;

select cron.schedule_in_database('eadm_dashboard', '0 4 * * *', 'call prod_dashboard(current_date);', 'eadm');

select * from cron.job;

select * from cron.job_run_details order by start_time desc limit 5;

-- 检查执行情况 eadm库内执行

call proc_dashboard(current_date);

select * from sys_proclog order by inserttime desc limit 10;

select * from eadm_dashboard;

-- 定时同步数据至pg

