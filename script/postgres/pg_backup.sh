#!/usr/bin/env bash
# @author wangcw
# @copyright (c) 2024, REDGREAT
# created : 2024-09-26 13:50
# comment: pg数据库备份脚本
# crontab: 0 2 * * * /var/lib/postgresql/back/pg_back.sh


# PostgreSQL数据库相关信息
db_host="127.0.0.1"
db_port="5432"
db_name="eadm"
db_user="user_eadm"
db_password="iyS62bvt"

# 备份存储目录
backup_dir="/var/lib/postgresql/back"

# 保留备份的天数
retention_days=7

# 创建备份目录
mkdir -p $backup_dir

# 备份文件名
backup_file="$backup_dir/backup_${db_name}_$(date +'%Y%m%d%H%M%S').sql"

# 执行备份
PGPASSWORD=$db_password pg_dump -h $db_host -p $db_port -U $db_user --role=$db_user \
 -O -x -a -n public -T cc_pg_ddl_capture_tab -T spatial_ref_sys --column-inserts \
 --no-tablespaces --no-security-labels --disable-dollar-quoting -f "$backup_file" $db_name

if [ $? -eq 0 ]; then
echo "数据库备份成功: $backup_file"

# 删除旧的备份文件
find $backup_dir -name "backup_*.sql" -type f -mtime +$retention_days -exec rm -f {} \;
else
echo "数据库备份失败."
fi
