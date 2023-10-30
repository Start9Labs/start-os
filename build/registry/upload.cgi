#!/bin/bash

declare -A params
while IFS='=' read -r -d '&' key value && [[ -n "$key" ]]; do
    params["$key"]=$value
done <<<"${QUERY_STRING}&"

index_key="${params['key']}"
if [ -z "$index_key" ] || [ "$index_key" != "$(cat /var/www/index_key.txt)" ]; then
    echo "HTTP/1.1 401 UNAUTHORIZED"
    echo "Content-Type: text/html"
    echo
    echo "UNAUTHORIZED"
    exit
fi

git_hash="${params['gitHash']}"
version="${params['version']}"
platform="${params['platform']}"
shasum="${params['shasum']}"
if [ -z "$git_hash" ] || [ -z "$version" ] || [ -z "$platform" ] || [ -z "$shasum" ]; then
    echo "HTTP/1.1 400 BAD REQUEST"
    echo "Content-Type: text/html"
    echo
    echo "BAD REQUEST: missing param"
    exit
fi

tmp_file=$(mktemp /var/tmp/tmp.XXXXXXXXXX.squashfs)
cat > $tmp_file

if ! sha256sum $tmp_file | grep "$shasum"; then
    rm $tmp_file
    echo "HTTP/1.1 400 BAD REQUEST"
    echo "Content-Type: text/html"
    echo
    echo "BAD REQUEST: shasum mismatch"
fi

mkdir -p /var/www/resources/eos/${version}/${git_hash}
mv $tmp_file /var/www/resources/eos/${version}/${git_hash}/startos-${version}-${git_hash}_${platform}.squashfs
rm /var/www/resources/eos/${version}/eos.${platform}.squashfs
ln -rs /var/www/resources/eos/${version}/${git_hash}/startos-${version}-${git_hash}_${platform}.squashfs /var/www/resources/eos/${version}/eos.${platform}.squashfs

echo "HTTP/1.1 200 OK"
echo "Content-Type: text/html"
echo
echo "OK: Upload successful"