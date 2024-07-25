#!/bin/bash

set -x

SHELL=/bin/sh
EADM_HOME=/opt/eadm
EADM_CONF=$EADM_HOME/config
EADM_LOG=$EADM_HOME/log

export EADM_HOME SHELL EADM_CONF EADM_LOG

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
mkdir -p $EADM_CONF && chown -R eadm:eadm $EADM_CONF
mkdir -p $EADM_LOG && chown -R eadm:eadm $EADM_LOG

# 复制配置文件
if [ ! -f "$EADM_CONF/sys.config" ]
then
    cp ./docker/sys.config $EADM_CONF/sys.config
fi

if [ ! -f "$EADM_CONF/db.config" ]
then
    cp ./docker/vm.args $EADM_CONF/db.config
fi

if [ ! -f "$EADM_CONF/vm.args" ]
then
    cp ./docker/vm.args $EADM_CONF/vm.args
fi

# 变更项目文件夹权限
chown eadm:eadm $EADM_HOME

# pid文件创建并赋值权限
touch /run/eadm.pid && chown eadm /run/eadm.pid

# 前台运行
exec /usr/bin/gosu eadm /opt/eadm/bin/eadm foreground
