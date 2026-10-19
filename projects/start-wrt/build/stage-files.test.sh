#!/bin/bash
set -euo pipefail

BUILD_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

awk '
    /^cat >.*91-startwrt-pppoe-ipv6.*PPPOEEOF/ { copying = 1; next }
    /^PPPOEEOF$/ { copying = 0 }
    copying { print }
' "$BUILD_DIR/stage-files.sh" > "$TMP/migrate"
test -s "$TMP/migrate"
sh -n "$TMP/migrate"

mkdir "$TMP/bin"
cat > "$TMP/bin/uci" << 'EOF'
#!/bin/sh
case "$*" in
    '-q get network.wan.proto') printf '%s' "$PROTO" ;;
    '-q get network.wan.ipv6') cat "$STATE/ipv6" ;;
    'set network.wan.ipv6=1')
        [ "${FAIL_SET:-0}" = 0 ] || exit 1
        printf '1' > "$STATE/ipv6"
        printf '%s\n' "$*" >> "$STATE/writes"
        ;;
    *) exit 2 ;;
esac
EOF
chmod +x "$TMP/bin/uci"
export PATH="$TMP/bin:$PATH" STATE="$TMP"

for PROTO in pppoe dhcp static ''; do
    export PROTO
    for ipv6 in '' 0 1 auto; do
        printf '%s' "$ipv6" > "$STATE/ipv6"
        : > "$STATE/writes"
        sh "$TMP/migrate"
        if [ "$PROTO" = pppoe ] && [ -z "$ipv6" ]; then
            test "$(cat "$STATE/ipv6")" = 1
            test "$(cat "$STATE/writes")" = 'set network.wan.ipv6=1'
        else
            test "$(cat "$STATE/ipv6")" = "$ipv6"
            test ! -s "$STATE/writes"
        fi
        cp "$STATE/writes" "$STATE/previous-writes"
        sh "$TMP/migrate"
        cmp "$STATE/writes" "$STATE/previous-writes"
    done
done

export PROTO=pppoe FAIL_SET=1
: > "$STATE/ipv6"
if sh "$TMP/migrate"; then
    echo 'Failed UCI writes must leave the defaults script pending' >&2
    exit 1
fi

echo 'PPPoE upgrade defaults: 16 configurations, repeat runs, and write failure passed'
