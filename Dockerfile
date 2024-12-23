FROM alpine:latest

WORKDIR /app

# Copy /nix/store
COPY ./tmp /nix/store
COPY ./tmp/buildkite-notification /app
CMD ["/app/buildkite-notification"]