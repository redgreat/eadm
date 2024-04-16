# eadm 个人后台管理系统

项目介绍
---

使用erlang做后台，web框架为基于cowboy的nova，bootstrap5+jQuery做前台，TiDB做后台数据库。
初学项目。

 - erlang: 26.2.2
 - rebar3: 3.22.1
 - [nova](https://github.com/novaframework/nova): 0.9.22
 - bootstrap5: 5.3.0
 - jQuery: 3.6.0

---

## 运行
```shell
docker run -d \
    --name=eadm \
    -p 8080:8080 \
    -v /docker/appdata/nginx-proxy-manager:/config:rw \
    redgreat/eadm
```
