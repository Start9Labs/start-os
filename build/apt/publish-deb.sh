#!/bin/bash
#
# Publish .deb files to an S3-hosted apt repository.
#
# Usage: publish-deb.sh <deb-file-or-directory> [<deb-file-or-directory> ...]
#
# Environment variables:
#   GPG_PRIVATE_KEY  - Armored GPG private key (imported if set)
#   GPG_KEY_ID       - GPG key ID for signing
#   S3_ACCESS_KEY    - S3 access key
#   S3_SECRET_KEY    - S3 secret key
#   S3_ENDPOINT      - S3 endpoint (default: https://nyc3.digitaloceanspaces.com)
#   S3_BUCKET        - S3 bucket name (default: start9-debs)
#   SUITE            - Apt suite name (default: stable)
#   COMPONENT        - Apt component name (default: main)

set -e

if [ $# -eq 0 ]; then
    echo "Usage: $0 <deb-file-or-directory> [...]" >&2
    exit 1
fi

BUCKET="${S3_BUCKET:-start9-debs}"
ENDPOINT="${S3_ENDPOINT:-https://nyc3.digitaloceanspaces.com}"
SUITE="${SUITE:-stable}"
COMPONENT="${COMPONENT:-main}"
REPO_DIR="$(mktemp -d)"

cleanup() {
    rm -rf "$REPO_DIR"
}
trap cleanup EXIT

# Import GPG key if provided
if [ -n "$GPG_PRIVATE_KEY" ]; then
    echo "$GPG_PRIVATE_KEY" | gpg --batch --import 2>/dev/null
fi

# Configure s3cmd
if [ -n "$S3_ACCESS_KEY" ] && [ -n "$S3_SECRET_KEY" ]; then
    S3CMD_CONFIG="$(mktemp)"
    cat > "$S3CMD_CONFIG" <<EOF
[default]
access_key = ${S3_ACCESS_KEY}
secret_key = ${S3_SECRET_KEY}
host_base = $(echo "$ENDPOINT" | sed 's|https://||')
host_bucket = %(bucket)s.$(echo "$ENDPOINT" | sed 's|https://||')
use_https = True
EOF
    s3() {
        s3cmd -c "$S3CMD_CONFIG" "$@"
    }
else
    # Fall back to default ~/.s3cfg
    S3CMD_CONFIG=""
    s3() {
        s3cmd "$@"
    }
fi

# Sync existing repo from S3
echo "Syncing existing repo from s3://${BUCKET}/ ..."
s3 sync --no-mime-magic "s3://${BUCKET}/" "$REPO_DIR/" 2>/dev/null || true

# Collect all .deb files from arguments
DEB_FILES=()
for arg in "$@"; do
    if [ -d "$arg" ]; then
        while IFS= read -r -d '' f; do
            DEB_FILES+=("$f")
        done < <(find "$arg" -name '*.deb' -print0)
    elif [ -f "$arg" ]; then
        DEB_FILES+=("$arg")
    else
        echo "Warning: $arg is not a file or directory, skipping" >&2
    fi
done

if [ ${#DEB_FILES[@]} -eq 0 ]; then
    echo "No .deb files found" >&2
    exit 1
fi

# Copy each deb to the pool, renaming to standard format
for deb in "${DEB_FILES[@]}"; do
    PKG_NAME="$(dpkg-deb --field "$deb" Package)"
    POOL_DIR="$REPO_DIR/pool/${COMPONENT}/${PKG_NAME:0:1}/${PKG_NAME}"
    mkdir -p "$POOL_DIR"
    cp "$deb" "$POOL_DIR/"
    dpkg-name -o "$POOL_DIR/$(basename "$deb")" 2>/dev/null || true
    echo "Added: $(basename "$deb") -> pool/${COMPONENT}/${PKG_NAME:0:1}/${PKG_NAME}/"
done

# Generate Packages indices for each architecture
for arch in amd64 arm64 riscv64; do
    BINARY_DIR="$REPO_DIR/dists/${SUITE}/${COMPONENT}/binary-${arch}"
    mkdir -p "$BINARY_DIR"
    (
        cd "$REPO_DIR"
        dpkg-scanpackages --arch "$arch" pool/ > "$BINARY_DIR/Packages"
        gzip -k -f "$BINARY_DIR/Packages"
    )
    echo "Generated Packages index for ${arch}"
done

# Generate Release file
(
    cd "$REPO_DIR/dists/${SUITE}"
    apt-ftparchive release \
        -o "APT::FTPArchive::Release::Origin=Start9" \
        -o "APT::FTPArchive::Release::Label=Start9" \
        -o "APT::FTPArchive::Release::Suite=${SUITE}" \
        -o "APT::FTPArchive::Release::Codename=${SUITE}" \
        -o "APT::FTPArchive::Release::Architectures=amd64 arm64 riscv64" \
        -o "APT::FTPArchive::Release::Components=${COMPONENT}" \
        . > Release
)
echo "Generated Release file"

# Sign if GPG key is available
if [ -n "$GPG_KEY_ID" ]; then
    (
        cd "$REPO_DIR/dists/${SUITE}"
        gpg --default-key "$GPG_KEY_ID" --batch --yes --detach-sign -o Release.gpg Release
        gpg --default-key "$GPG_KEY_ID" --batch --yes --clearsign -o InRelease Release
    )
    echo "Signed Release file with key ${GPG_KEY_ID}"
else
    echo "Warning: GPG_KEY_ID not set, Release file is unsigned" >&2
fi

# Upload to S3
echo "Uploading to s3://${BUCKET}/ ..."
s3 sync --acl-public --no-mime-magic "$REPO_DIR/" "s3://${BUCKET}/"

[ -n "$S3CMD_CONFIG" ] && rm -f "$S3CMD_CONFIG"
echo "Done."
