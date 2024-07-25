#!/usr/bin/env bash
# docker build script

# 提前拉取镜像（反复打包测试时免除每次pull）
docker pull alpine:3.20
docker pull erlang:26.2.4-alpine

# 清理镜像
docker stop eadm
docker rm eadm
docker rmi eadm

# 清理build缓存
# docker builder prune -a

# 清理逻辑卷
# docker system prune -a

# 打包
# docker build --no-cache -t eadm .
docker build -t eadm .

# 运行
docker run -itd --name eadm -p 8080:8090 eadm

# 查看日志
docker logs -n 100 eadm
