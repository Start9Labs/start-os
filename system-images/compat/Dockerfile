FROM alpine:latest

RUN apk update && apk add duplicity && apk add curl
ADD ./target/aarch64-unknown-linux-musl/release/compat /usr/local/bin/compat

ENTRYPOINT ["compat"]
