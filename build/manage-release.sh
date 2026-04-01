#!/bin/bash

set -e

REPO="Start9Labs/start-os"
REGISTRY="https://alpha-registry-x.start9.com"
S3_BUCKET="s3://startos-images"
S3_CDN="https://startos-images.nyc3.cdn.digitaloceanspaces.com"
START9_GPG_KEY="2D63C217"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ARCHES="aarch64 aarch64-nonfree aarch64-nvidia riscv64 riscv64-nonfree x86_64 x86_64-nonfree x86_64-nvidia"
CLI_ARCHES="aarch64 riscv64 x86_64"

parse_run_id() {
    local val="$1"
    if [[ "$val" =~ /actions/runs/([0-9]+) ]]; then
        echo "${BASH_REMATCH[1]}"
    else
        echo "$val"
    fi
}

require_version() {
    if [ -z "${VERSION:-}" ]; then
        read -rp "VERSION: " VERSION
        if [ -z "$VERSION" ]; then
            >&2 echo '$VERSION required'
            exit 2
        fi
    fi
}

release_dir() {
    echo "$HOME/Downloads/v$VERSION"
}

ensure_release_dir() {
    local dir
    dir=$(release_dir)
    if [ "$CLEAN" = "1" ]; then
        rm -rf "$dir"
    fi
    mkdir -p "$dir"
    cd "$dir"
}

enter_release_dir() {
    local dir
    dir=$(release_dir)
    if [ ! -d "$dir" ]; then
        >&2 echo "Release directory $dir does not exist. Run 'download' or 'pull' first."
        exit 1
    fi
    cd "$dir"
}

cli_target_for() {
    local arch=$1 os=$2
    local pair="${arch}-${os}"
    if [ "$pair" = "riscv64-linux" ]; then
        echo "riscv64gc-unknown-linux-musl"
    elif [ "$pair" = "riscv64-macos" ]; then
        return 1
    elif [ "$os" = "linux" ]; then
        echo "${arch}-unknown-linux-musl"
    elif [ "$os" = "macos" ]; then
        echo "${arch}-apple-darwin"
    fi
}

release_files() {
    for file in *.iso *.squashfs *.deb; do
        [ -f "$file" ] && echo "$file"
    done
    for file in start-cli_*; do
        [[ "$file" == *.asc ]] && continue
        [ -f "$file" ] && echo "$file"
    done
}

resolve_gh_user() {
    GH_USER=${GH_USER:-$(gh api user -q .login 2>/dev/null || true)}
    GH_GPG_KEY=$(git config user.signingkey 2>/dev/null || true)
}

# Fetch the URL for an OS asset from the registry index.
# Usage: registry_url <iso|squashfs|img> <platform>
registry_url() {
    local ext=$1 platform=$2
    if [ -z "${_REGISTRY_INDEX:-}" ]; then
        _REGISTRY_INDEX=$(start-cli --registry=$REGISTRY registry os index)
    fi
    echo "$_REGISTRY_INDEX" | jq -r ".versions[\"$VERSION\"].$ext[\"$platform\"].urls[0]"
}

# --- Subcommands ---

cmd_download() {
    require_version

    if [ -z "${ST_RUN_ID:-}" ]; then
        read -rp "ST_RUN_ID (start-tunnel, leave blank to skip): " ST_RUN_ID
    fi
    ST_RUN_ID=$(parse_run_id "${ST_RUN_ID:-}")

    if [ -z "${CLI_RUN_ID:-}" ]; then
        read -rp "CLI_RUN_ID (start-cli, leave blank to skip): " CLI_RUN_ID
    fi
    CLI_RUN_ID=$(parse_run_id "${CLI_RUN_ID:-}")

    ensure_release_dir

    # Download OS images from registry (deployed by GitHub workflow)
    echo "Downloading OS images from registry..."
    for arch in $ARCHES; do
        for ext in squashfs iso; do
            echo "  $ext $arch"
            start-cli --registry=$REGISTRY registry os asset get "$ext" "$VERSION" "$arch" -d "$(pwd)"
        done
    done

    if [ -n "$ST_RUN_ID" ]; then
        for arch in $CLI_ARCHES; do
            while ! gh run download -R $REPO "$ST_RUN_ID" -n "start-tunnel_$arch.deb" -D "$(pwd)"; do sleep 1; done
        done
    fi

    if [ -n "$CLI_RUN_ID" ]; then
        for arch in $CLI_ARCHES; do
            for os in linux macos; do
                local target
                target=$(cli_target_for "$arch" "$os") || continue
                while ! gh run download -R $REPO "$CLI_RUN_ID" -n "start-cli_$target" -D "$(pwd)"; do sleep 1; done
                mv start-cli "start-cli_${arch}-${os}"
            done
        done
    fi
}

cmd_pull() {
    require_version
    ensure_release_dir

    echo "Downloading release assets from tag v$VERSION..."

    # Download debs and CLI binaries from the GH release
    for file in $(gh release view -R $REPO "v$VERSION" --json assets -q '.assets[].name' | grep -E '\.(deb)$|^start-cli_'); do
        gh release download -R $REPO "v$VERSION" -p "$file" -D "$(pwd)" --clobber
    done

    # Download ISOs and squashfs from registry
    echo "Downloading OS images from registry..."
    for arch in $ARCHES; do
        for ext in squashfs iso; do
            echo "  $ext $arch"
            start-cli --registry=$REGISTRY registry os asset get "$ext" "$VERSION" "$arch" -d "$(pwd)"
        done
    done
}

cmd_register() {
    require_version
    enter_release_dir
    start-cli --registry=$REGISTRY registry os version add "$VERSION" "v$VERSION" '' ">=0.3.5 <=$VERSION"
}

cmd_upload() {
    require_version
    enter_release_dir

    # OS images (iso/squashfs) are already on S3 via the GitHub workflow.
    # Upload only debs and CLI binaries to the GitHub Release.
    for file in $(release_files); do
        case "$file" in
            *.iso|*.squashfs) ;;
            *) gh release upload -R $REPO "v$VERSION" "$file" ;;
        esac
    done
}

cmd_index() {
    require_version
    enter_release_dir

    for arch in $ARCHES; do
        for file in *_"$arch".squashfs *_"$arch".iso; do
            start-cli --registry=$REGISTRY registry os asset add --platform="$arch" --version="$VERSION" "$file" "$S3_CDN/v$VERSION/$file"
        done
    done
}

cmd_sign() {
    require_version
    enter_release_dir
    resolve_gh_user

    mkdir -p signatures

    for file in $(release_files); do
        gpg -u $START9_GPG_KEY --detach-sign --armor -o "signatures/${file}.start9.asc" "$file"
        if [ -n "$GH_USER" ] && [ -n "$GH_GPG_KEY" ]; then
            gpg -u "$GH_GPG_KEY" --detach-sign --armor -o "signatures/${file}.${GH_USER}.asc" "$file"
        fi
    done

    gpg --export -a $START9_GPG_KEY > signatures/start9.key.asc
    if [ -n "$GH_USER" ] && [ -n "$GH_GPG_KEY" ]; then
        gpg --export -a "$GH_GPG_KEY" > "signatures/${GH_USER}.key.asc"
    else
        >&2 echo 'Warning: could not determine GitHub user or GPG signing key, skipping personal signature'
    fi
    tar -czvf signatures.tar.gz -C signatures .

    gh release upload -R $REPO "v$VERSION" signatures.tar.gz --clobber
}

cmd_cosign() {
    require_version
    enter_release_dir
    resolve_gh_user

    if [ -z "$GH_USER" ] || [ -z "$GH_GPG_KEY" ]; then
        >&2 echo 'Error: could not determine GitHub user or GPG signing key'
        >&2 echo "Set GH_USER and/or configure git user.signingkey"
        exit 1
    fi

    echo "Downloading existing signatures..."
    gh release download -R $REPO "v$VERSION" -p "signatures.tar.gz" -D "$(pwd)" --clobber
    mkdir -p signatures
    tar -xzf signatures.tar.gz -C signatures

    echo "Adding personal signatures as $GH_USER..."
    for file in $(release_files); do
        gpg -u "$GH_GPG_KEY" --detach-sign --armor -o "signatures/${file}.${GH_USER}.asc" "$file"
    done

    gpg --export -a "$GH_GPG_KEY" > "signatures/${GH_USER}.key.asc"

    echo "Re-packing signatures..."
    tar -czvf signatures.tar.gz -C signatures .

    gh release upload -R $REPO "v$VERSION" signatures.tar.gz --clobber
    echo "Done. Personal signatures for $GH_USER added to v$VERSION."
}

cmd_publish_tunnel() {
    require_version
    enter_release_dir

    local tunnel_debs=()
    for file in start-tunnel*.deb; do
        [ -f "$file" ] && tunnel_debs+=("$file")
    done

    if [ ${#tunnel_debs[@]} -eq 0 ]; then
        >&2 echo "No start-tunnel .deb files found in release directory"
        exit 1
    fi

    echo "Publishing start-tunnel debs to apt repository..."
    "$SCRIPT_DIR/apt/publish-deb.sh" "${tunnel_debs[@]}"
}

cmd_notes() {
    require_version
    enter_release_dir

    cat << EOF
# ISO Downloads

- [x86_64/AMD64]($(registry_url iso x86_64-nonfree))
- [x86_64/AMD64 + NVIDIA]($(registry_url iso x86_64-nvidia))
- [x86_64/AMD64-slim (FOSS-only)]($(registry_url iso x86_64) "Without proprietary software or drivers")
- [aarch64/ARM64]($(registry_url iso aarch64-nonfree))
- [aarch64/ARM64 + NVIDIA]($(registry_url iso aarch64-nvidia))
- [aarch64/ARM64-slim (FOSS-Only)]($(registry_url iso aarch64) "Without proprietary software or drivers")
- [RISCV64 (RVA23)]($(registry_url iso riscv64-nonfree))
- [RISCV64 (RVA23)-slim (FOSS-only)]($(registry_url iso riscv64) "Without proprietary software or drivers")

EOF
    cat << 'EOF'
# StartOS Checksums

## SHA-256
```
EOF
    sha256sum *.iso *.squashfs
    cat << 'EOF'
```

## BLAKE-3
```
EOF
    b3sum *.iso *.squashfs
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
    release_files | grep '^start-cli_' | xargs sha256sum
    cat << 'EOF'
```

## BLAKE-3
```
EOF
    release_files | grep '^start-cli_' | xargs b3sum
    cat << 'EOF'
```
EOF
}

cmd_full_release() {
    cmd_download
    cmd_upload
    cmd_publish_tunnel
    cmd_sign
    cmd_notes
}

usage() {
    cat << 'EOF'
Usage: manage-release.sh <subcommand>

Subcommands:
  download        Download OS images from registry + other artifacts from GH Actions
                  OS images are pulled via start-cli from the registry (deployed by GH workflow)
                  Requires: ST_RUN_ID, CLI_RUN_ID (any combination)
  pull            Download an existing release from the GH tag and S3
  register        Register the version in the Start9 registry
  upload          Upload artifacts to GitHub Releases and S3
  index           Add assets to the registry index
  publish-tunnel  Publish start-tunnel .deb files to the apt repository
  sign            Sign all artifacts with Start9 org key (+ personal key if available)
                  and upload signatures.tar.gz
  cosign          Add personal GPG signature to an existing release's signatures
                  (requires 'pull' first so you can verify assets before signing)
  notes           Print release notes with download links and checksums
  full-release    Run: download → register → upload → publish-tunnel → sign → notes

Environment variables:
  VERSION     (required) Release version
  ST_RUN_ID   GitHub Actions run ID for start-tunnel (download subcommand)
  CLI_RUN_ID  GitHub Actions run ID for start-cli (download subcommand)
  GH_USER     Override GitHub username (default: autodetected via gh cli)
  CLEAN       Set to 1 to wipe and recreate the release directory
EOF
}

case "${1:-}" in
    download)       cmd_download ;;
    pull)           cmd_pull ;;
    register)       cmd_register ;;
    upload)         cmd_upload ;;
    index)          cmd_index ;;
    publish-tunnel) cmd_publish_tunnel ;;
    sign)           cmd_sign ;;
    cosign)         cmd_cosign ;;
    notes)          cmd_notes ;;
    full-release)   cmd_full_release ;;
    *)              usage; exit 1 ;;
esac
