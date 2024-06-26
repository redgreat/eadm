FROM --platform=$BUILDPLATFORM erlang:26.2.4-alpine AS builder

WORKDIR /eadmbuild

COPY . .

RUN apk add --update git
RUN rebar3 as prod release

FROM --platform=$BUILDPLATFORM alpine:3.19

ARG DOCKER_IMAGE_VERSION

MAINTAINER wangcw <rubygreat@msn.com>

WORKDIR /opt/eadm

RUN apk add --no-cache ncurses-libs libgcc libstdc++

COPY --from=builder /eadmbuild/_build/prod/rel/eadm /opt/eadm/

VOLUME /opt/eadm

ENV \
    DISABLE_IPV6=1 \
    TZ='Asia/Shanghai' \
    LANG='zh_CN.UTF-8'

EXPOSE 8090

LABEL \
      org.label-schema.name="eadm" \
      org.label-schema.description="erlang书写自用管理，提供日常数据统计查询。" \
      org.label-schema.version="${DOCKER_IMAGE_VERSION:-unknown}" \
      org.label-schema.vcs-url="https://github.com/redgreat/eadm" \
      org.label-schema.schema-version="1.0"

CMD ["/opt/eadm/bin/eadm", "foreground"]
