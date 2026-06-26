#!/usr/bin/env bash

docker run -it --rm -p 8000:80 \
  -v $(pwd)/../../docs:/usr/share/nginx/html \
  -v $(pwd)/nginx.conf:/etc/nginx/nginx.conf \
  nginx