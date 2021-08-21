FROM alpine:latest

RUN apt-get update && apt-get install -y duplicity
ADD ./target/aarch64-unknown-linux-musl/release/compat /usr/local/bin/compat

ENTRYPOINT ["compat"]
