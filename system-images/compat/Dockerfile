FROM alpine:latest

ARG ARCH

RUN apk update && apk add duplicity curl
ADD ./target/$ARCH-unknown-linux-musl/release/compat /usr/local/bin/compat

ENTRYPOINT ["compat"]
