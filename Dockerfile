FROM openjdk:alpine
ENV AWS_CLI_VERSION 1.15.60
WORKDIR /opt/docker
ADD server/target/docker/stage/opt /opt
RUN apk add --update bash && rm -rf /var/cache/apk/*
RUN apk --no-cache update && \
    apk --no-cache add python py-pip py-setuptools ca-certificates groff less && \
    pip --no-cache-dir install awscli==${AWS_CLI_VERSION} && \
RUN adduser -D -u 1000 drt-admin
RUN mkdir /home/drt-admin/.ssh
ADD know_hosts /home/drt-admin/.ssh/
RUN ["chown", "-R", "1000:1000", "."]
RUN ["chown", "-R", "1000:1000", "/home/drt-admin/.ssh"]
RUN mkdir /var/lib/drt-v2
RUN mkdir -p /var/run/drt && chown 1000 /var/run/drt
RUN mkdir -p /var/log/drt && chown 1000 /var/log/drt
RUN apk --update add openssh-client \
    bash \
    && \
    rm -rf /var/cache/apk/*
USER 1000

CMD bin/drt
