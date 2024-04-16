FROM amd64/alpine:3.19

MAINTAINER wangcw <rubygreat@msn.com>

WORKDIR /opt/eadm

RUN apk add --no-cache ncurses-libs libgcc libstdc++

COPY _build/prod/rel/eadm /opt/eadm/

VOLUME /opt/eadm

EXPOSE 8080

CMD ["/opt/eadm/bin/eadm", "foreground"]
