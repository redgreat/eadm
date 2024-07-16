# eadm 个人后台管理系统

项目介绍
---

使用erlang做后台，web框架为基于cowboy的nova，bootstrap5+jQuery做前台，TiDB做后台数据库。
初学项目。

 - erlang: 26.2.4
 - rebar3: 3.22.1
 - [nova](https://github.com/novaframework/nova): 0.9.22
 - bootstrap5: 5.3.0
 - jQuery: 3.6.0

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
