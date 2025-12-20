#!/bin/bash

pwd=$(pwd)

cd "$(dirname "${BASH_SOURCE[0]}")/../.."

set -e

rel_pwd="${pwd#"$(pwd)"}"

COMPAT_ARCH=$(uname -m)

case $COMPAT_ARCH in
    x86_64)
        platform=linux/amd64;;
    aarch64|arm64)
        platform=linux/arm64;;
    *)
        echo "Unsupported architecture: $COMPAT_ARCH" >&2
        exit 1
        ;;
esac

if [ "$FORCE_COMPAT" = 1 ] || ( [ "$REQUIRES" = "linux" ] && [ "$(uname -s)" != "Linux" ] ) || ( [ "$REQUIRES" = "debian" ] && ! which dpkg > /dev/null ); then
    if tty -s; then
        USE_TTY="-it"
    fi

    docker run $USE_TTY --platform=$platform -eARCH -eENVIRONMENT -ePLATFORM -eGIT_BRANCH_AS_HASH -ePROJECT -eDEPENDS -eCONFLICTS -w "/root/start-os${rel_pwd}" --rm -v "$(pwd):/root/start-os" start9/build-env $@
else 
    exec $@
fi
