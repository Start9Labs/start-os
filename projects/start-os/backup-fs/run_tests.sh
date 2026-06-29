#!/usr/bin/env bash
set -e
podman build -t backupfs-testing:latest -f testing.Dockerfile
podman run --cap-add SYS_ADMIN --cap-add IPC_OWNER --device /dev/fuse --security-opt apparmor:unconfined -it backupfs-testing:latest "$@"
