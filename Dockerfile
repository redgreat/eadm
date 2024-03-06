FROM amd64/erlang:26.2.1-alpine AS builder

ARG SSH_KEY
ARG PUB_KEY

WORKDIR /eadm

COPY . .

RUN apk add --no-cache git openssh-client

RUN mkdir /root/.ssh/ \
    && echo $SSH_KEY > /root/.ssh/id_rsa \
    && sed -i -e 's/-----BEGIN OPENSSH PRIVATE KEY----- //g' -e 's/ -----END OPENSSH PRIVATE KEY-----//g' -e 's/ /\n/g' -e '1i\-----BEGIN OPENSSH PRIVATE KEY-----' -e '$a\-----END OPENSSH PRIVATE KEY-----' /root/.ssh/id_rsa \
    && echo $PUB_KEY > /root/.ssh/id_rsa.pub \
    && chmod 600 /root/.ssh/id_rsa \
    && chmod 644 /root/.ssh/id_rsa.pub

RUN ssh-keyscan github.com > /root/.ssh/known_hosts

RUN rebar3 as prod release

FROM amd64/alpine:3.18 AS runner

MAINTAINER wangcw <rubygreat@msn.com>

WORKDIR /opt/eadm

RUN apk add --no-cache ncurses-libs libgcc libstdc++

COPY --from=builder /eadm/_build/prod/rel/eadm /opt/eadm/

VOLUME /opt/eadm

EXPOSE 8080

CMD ["/opt/eadm/bin/eadm", "foreground"]
