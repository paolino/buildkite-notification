FROM ubuntu:latest

WORKDIR /opt
COPY build/buildkite-notification buildkite-notification
ENTRYPOINT /opt/buildkite-notification
