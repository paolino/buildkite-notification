FROM ubuntu:latest


RUN apt-get update && apt-get install -y ca-certificates

WORKDIR /opt
COPY build/buildkite-notification buildkite-notification
ENTRYPOINT /opt/buildkite-notification
