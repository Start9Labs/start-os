#!/bin/bash

if [ -z "$VERSION" ]; then
    >&2 echo '$VERSION required'
    exit 2
fi

set -e

if [ "$SKIP_DL" != "1" ]; then
    if [ "$SKIP_CLEAN" != "1" ]; then
        rm -rf ~/Downloads/v$VERSION
        mkdir ~/Downloads/v$VERSION
        cd ~/Downloads/v$VERSION
    fi

    if [ -n "$RUN_ID" ]; then
        for arch in aarch64 aarch64-nonfree riscv64 x86_64 x86_64-nonfree raspberrypi; do
            while ! gh run download -R Start9Labs/start-os $RUN_ID -n $arch.squashfs -D $(pwd); do sleep 1; done
        done
        for arch in aarch64 aarch64-nonfree riscv64 x86_64 x86_64-nonfree; do
            while ! gh run download -R Start9Labs/start-os $RUN_ID -n $arch.iso -D $(pwd); do sleep 1; done
        done
        while ! gh run download -R Start9Labs/start-os $RUN_ID -n raspberrypi.img -D $(pwd); do sleep 1; done
    fi

    if [ -n "$ST_RUN_ID" ]; then
        for arch in aarch64 riscv64 x86_64; do
            while ! gh run download -R Start9Labs/start-os $ST_RUN_ID -n start-tunnel_$arch.deb -D $(pwd); do sleep 1; done
    done
    fi

    if [ -n "$CLI_RUN_ID" ]; then
        for arch in aarch64 riscv64 x86_64; do
            for os in linux macos; do
                pair=${arch}-${os}
                if [ "${pair}" = "riscv64-linux" ]; then
                    target=riscv64gc-unknown-linux-musl
                elif [ "${pair}" = "riscv64-macos" ]; then
                    continue
                elif [ "${os}" = "linux" ]; then
                    target="${arch}-unknown-linux-musl"
                elif [ "${os}" = "macos" ]; then
                    target="${arch}-apple-darwin"
                fi
                while ! gh run download -R Start9Labs/start-os $CLI_RUN_ID -n start-cli_$target -D $(pwd); do sleep 1; done
                mv start-cli "start-cli_${pair}"
            done
        done
    fi
else
    cd ~/Downloads/v$VERSION
fi

start-cli --registry=https://alpha-registry-x.start9.com registry os version add $VERSION "v$VERSION" '' ">=0.3.5 <=$VERSION"

if [ "$SKIP_UL" = "2" ]; then
    exit 2
elif [ "$SKIP_UL" != "1" ]; then
    for file in *.squashfs *.iso *.deb start-cli_*; do
        gh release upload -R Start9Labs/start-os v$VERSION $file
    done
    for file in *.img; do
        if ! [ -f $file.gz ]; then
            cat $file | pigz > $file.gz
        fi
        gh release upload -R Start9Labs/start-os v$VERSION $file.gz
    done
fi

if [ "$SKIP_INDEX" != "1" ]; then
    for arch in aarch64 aarch64-nonfree x86_64 x86_64-nonfree; do
        for file in *_$arch.squashfs *_$arch.iso; do
            start-cli --registry=https://alpha-registry-x.start9.com registry os asset add --platform=$arch --version=$VERSION $file https://github.com/Start9Labs/start-os/releases/download/v$VERSION/$(echo -n "$file" | sed 's/~/./g')
        done
    done
    for arch in raspberrypi; do
        for file in *_$arch.squashfs; do
            start-cli --registry=https://alpha-registry-x.start9.com registry os asset add --platform=$arch --version=$VERSION $file https://github.com/Start9Labs/start-os/releases/download/v$VERSION/$(echo -n "$file" | sed 's/~/./g')
        done
    done
fi

for file in *.iso *.img *.img.gz *.squashfs *.deb start-cli_*; do
    gpg -u 7CFFDA41CA66056A --detach-sign --armor -o "${file}.asc" "$file"
done

gpg --export -a 7CFFDA41CA66056A > dr-bonez.key.asc
tar -czvf signatures.tar.gz *.asc

gh release upload -R Start9Labs/start-os v$VERSION signatures.tar.gz

cat << 'EOF'
# StartOS Checksums

## SHA-256
```
EOF
sha256sum *.iso *.img *img.gz *.squashfs
cat << 'EOF'
```

## BLAKE-3
```
EOF
b3sum *.iso *.img *.img.gz *.squashfs
cat << 'EOF'
```

# Start-Tunnel Checksums

## SHA-256
```
EOF
sha256sum start-tunnel*.deb
cat << 'EOF'
```

## BLAKE-3
```
EOF
b3sum start-tunnel*.deb
cat << 'EOF'
```

# start-cli Checksums

## SHA-256
```
EOF
sha256sum start-cli_*
cat << 'EOF'
```

## BLAKE-3
```
EOF
b3sum start-cli_*
cat << 'EOF'
```
EOF

