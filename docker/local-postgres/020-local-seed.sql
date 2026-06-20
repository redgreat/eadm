-- Local preview seed data. Keep this file out of the production compose path.
-- Password for seeded users: 123456
update eadm_user
set passwd = 'qE4epSs2WkiXhIXCu95AwRVBsHVrQzabrmGAp9Jh5Fs=',
    updateduser = 'local-preview',
    updatedat = current_timestamp
where loginname in ('wangcw', 'wongcw', 'jiangyf');
