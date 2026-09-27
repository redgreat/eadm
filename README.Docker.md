# eadm 个人后台管理系统

项目介绍
---

使用 Erlang/OTP + Cowboy 做后台，SolidJS 做前台，TiDB/PostgreSQL 等数据库脚本按环境选择。
初学项目。

 - erlang: 27.2.1
 - rebar3: 3.24.0
 - cowboy
 - SolidJS + Vite

---

## 运行
```shell
docker run -itd \
-m 1G \
--memory-reservation 500M \
--memory-swappiness=0 \
-oom-kill-disable \
--cpu-shares=0 \
--restart=always \
-v ./config/prod_db.config:/opt/eadm/releases/0.1.0/prod_db.config \
-v ./config/prod_sys.config.src:/opt/eadm/releases/0.1.0/prod_sys.config.src \
-v ./config/vm.args.src:/opt/eadm/releases/0.1.0/vm.args.src
-p 8080:8090 \
--name eadm redgreat/eadm
```
