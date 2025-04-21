#!/bin/bash

if [ "$FORCE_COMPAT" -eq 1 ] || ( [ "$REQUIRES" = "linux" ] && [ "$(uname -s)" != "Linux" ] ) || ( [ "$REQUIRES" = "debian" ] && ! which dpkg > /dev/null ); then
    project_pwd="$(cd "$(dirname "${BASH_SOURCE[0]}")"/../.. && pwd)/"
    pwd="$(pwd)/"
    if ! [[ "$pwd" = "$project_pwd"* ]]; then
        >&2 echo "Must be run from start-os project dir"
        exit 1
    fi
    rel_pwd="${pwd#"$project_pwd"}"

    SYSTEMD_TTY="-P"
    USE_TTY=
    if tty -s; then
        USE_TTY="-it"
        SYSTEMD_TTY="-t"
    fi

    docker run -d --rm --name os-compat --privileged --security-opt apparmor=unconfined -v "${project_pwd}:/root/start-os" -v /lib/modules:/lib/modules:ro start9/build-env
    while ! docker exec os-compat systemctl is-active --quiet multi-user.target 2> /dev/null; do sleep .5; done
    docker exec -eARCH -eENVIRONMENT -ePLATFORM -eGIT_BRANCH_AS_HASH $USE_TTY -w "/root/start-os${rel_pwd}" os-compat $@
    code=$?
    docker stop os-compat
    exit $code
else 
    exec $@
fi