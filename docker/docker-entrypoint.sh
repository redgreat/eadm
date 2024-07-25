#!/bin/sh

set -x

# 用户ID/组ID定义
USER_ID=`stat -c '%u' /opt/eadm/config/db.config`
GROUP_ID=`stat -c '%g' /opt/eadm/config/db.config`
USER_ID=$([ "$USER_ID" = "0" ] && echo -n "1000" || echo -n "$USER_ID")
GROUP_ID=$([ "$GROUP_ID" = "0" ] && echo -n "1000" || echo -n "$GROUP_ID")

# 初始化时创建用户
if id "eadm" &>/dev/null
then
    echo "Found user eadm"
else
    echo "Create user eadm"
    addgroup -S -g $GROUP_ID eadm
    adduser -S -D -u $USER_ID -G eadm eadm
fi

# 创建文件夹
mkdir -p /opt/eadm/log && chown -R eadm:eadm /opt/eadm

# 前台运行
exec /usr/bin/gosu eadm /opt/eadm/bin/eadm foreground
