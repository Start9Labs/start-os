FROM alpine:latest

ARG TARGETARCH

RUN apk update && apk add duplicity curl
ADD ./target/$TARGETARCH/release/compat /usr/local/bin/compat

ENTRYPOINT ["compat"]
