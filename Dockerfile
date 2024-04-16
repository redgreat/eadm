ARG DOCKER_IMAGE_VERSION

FROM amd64/alpine:3.19

MAINTAINER wangcw <rubygreat@msn.com>

WORKDIR /opt/eadm

RUN apk add --no-cache ncurses-libs libgcc libstdc++

RUN ls _build/prod/rel/eadm
COPY _build/prod/rel/eadm /opt/eadm/

VOLUME /opt/eadm

ENV \
    DISABLE_IPV6=1 \
    TZ='Asia/Shanghai' \
    LANG='zh_CN.UTF-8'

EXPOSE 8080

LABEL \
      org.label-schema.name="eadm" \
      org.label-schema.description="erlang书写自用管理，提供日常数据统计查询。" \
      org.label-schema.version="${DOCKER_IMAGE_VERSION:-unknown}" \
      org.label-schema.vcs-url="https://github.com/redgreat/eadm" \
      org.label-schema.schema-version="1.0"

CMD ["/opt/eadm/bin/eadm", "foreground"]
