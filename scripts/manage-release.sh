#!/bin/bash
#
# manage-release.sh — drive a monorepo product through its release steps.
#
# Usage: ./scripts/manage-release.sh <subcommand> <project>
#
# See usage() for the subcommands. The <project> is one of the monorepo's
# releasable products; its version is read from that product's canonical
# manifest (Cargo.toml for the Rust products, package.json for the SDK) and its
# git tag / GitHub release is <project>_v<version>.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

REPO="Start9Labs/start-technologies"
REGISTRY="https://alpha-registry-x.start9.com"
S3_BUCKET="s3://startos-images"
S3_CDN="https://startos-images.nyc3.cdn.digitaloceanspaces.com"
START9_GPG_KEY="2D63C217"
SDK_NPM_PACKAGE="@start9labs/start-sdk"

APT_BASE_URL="https://start9-debs.nyc3.digitaloceanspaces.com"
APT_SUITE="stable"
APT_COMPONENT="main"

# Slim (FOSS-only) + nonfree + nvidia variants; matches the OS build/deploy set.
# raspberrypi is a .img build and is not S3-hosted/indexed, so it is excluded.
OS_PLATFORMS="x86_64 x86_64-nonfree x86_64-nvidia aarch64 aarch64-nonfree aarch64-nvidia riscv64 riscv64-nonfree"
CLI_TRIPLES="x86_64-unknown-linux-musl x86_64-apple-darwin aarch64-unknown-linux-musl aarch64-apple-darwin riscv64gc-unknown-linux-musl"
DEB_ARCHES="x86_64 aarch64 riscv64"

PROJECTS="start-os start-cli start-tunnel start-registry start-sdk"

# --- Project metadata ---

project_kind() {
    case "$1" in
        start-os) echo os ;;
        start-cli) echo cli ;;
        start-tunnel | start-registry) echo deb ;;
        start-sdk) echo npm ;;
        *) return 1 ;;
    esac
}

derive_version() {
    local project=$1 version
    if [ "$(project_kind "$project")" = npm ]; then
        jq -r .version "$REPO_ROOT/projects/$project/package.json"
        return
    fi
    local toml="$REPO_ROOT/projects/$project/Cargo.toml"
    version=$(grep -m1 'VERSION_BUMP' "$toml" 2>/dev/null | sed -E 's/.*version *= *"([^"]+)".*/\1/')
    if [ -z "$version" ]; then
        version=$(sed -nE '/^\[package\]/,/^\[/{s/^version *= *"([^"]+)".*/\1/p}' "$toml" | head -1)
    fi
    echo "$version"
}

changelog_path() { echo "$REPO_ROOT/projects/$1/CHANGELOG.md"; }

cli_asset_name() {
    case "$1" in
        x86_64-unknown-linux-musl) echo x86_64-linux ;;
        aarch64-unknown-linux-musl) echo aarch64-linux ;;
        riscv64gc-unknown-linux-musl) echo riscv64-linux ;;
        x86_64-apple-darwin) echo x86_64-macos ;;
        aarch64-apple-darwin) echo aarch64-macos ;;
        *) return 1 ;;
    esac
}

deb_arch() {
    case "$1" in
        x86_64) echo amd64 ;;
        aarch64) echo arm64 ;;
        riscv64) echo riscv64 ;;
        *) return 1 ;;
    esac
}

os_platform_label() {
    case "$1" in
        x86_64-nonfree) echo "x86_64/AMD64" ;;
        x86_64-nvidia) echo "x86_64/AMD64 + NVIDIA" ;;
        x86_64) echo "x86_64/AMD64-slim (FOSS-only)" ;;
        aarch64-nonfree) echo "aarch64/ARM64" ;;
        aarch64-nvidia) echo "aarch64/ARM64 + NVIDIA" ;;
        aarch64) echo "aarch64/ARM64-slim (FOSS-only)" ;;
        riscv64-nonfree) echo "RISCV64 (RVA23)" ;;
        riscv64) echo "RISCV64 (RVA23)-slim (FOSS-only)" ;;
        *) echo "$1" ;;
    esac
}

# --- Helpers ---

parse_run_id() {
    local val="$1"
    if [[ "$val" =~ /actions/runs/([0-9]+) ]]; then
        echo "${BASH_REMATCH[1]}"
    else
        echo "$val"
    fi
}

release_dir() { echo "$HOME/Downloads/${PROJECT}_v${VERSION}"; }

ensure_release_dir() {
    local dir
    dir=$(release_dir)
    if [ "${CLEAN:-}" = "1" ]; then
        rm -rf "$dir"
    fi
    mkdir -p "$dir"
    cd "$dir"
}

enter_release_dir() {
    local dir
    dir=$(release_dir)
    if [ ! -d "$dir" ]; then
        >&2 echo "Release directory $dir does not exist. Run 'pull-gha' or 'pull' first."
        exit 1
    fi
    cd "$dir"
}

# List the files in the (current) release dir that this project actually ships,
# one per line. Consume with `mapfile -t files < <(release_files)`.
release_files() {
    local f
    case "$KIND" in
        os) for f in *.iso *.squashfs; do [ -f "$f" ] && echo "$f"; done ;;
        cli) for f in start-cli_*; do
            case "$f" in *.asc) continue ;; esac
            [ -f "$f" ] && echo "$f"
        done ;;
        deb) for f in *.deb; do [ -f "$f" ] && echo "$f"; done ;;
    esac
}

resolve_gh_user() {
    GH_USER=${GH_USER:-$(gh api user -q .login 2>/dev/null || true)}
    GH_GPG_KEY=$(git config user.signingkey 2>/dev/null || true)
}

require_kind() {
    local ok
    for ok in "$@"; do
        [ "$KIND" = "$ok" ] && return 0
    done
    >&2 echo "Subcommand '$SUBCOMMAND' does not apply to $PROJECT (kind: $KIND)."
    exit 2
}

# Print the CHANGELOG body for $VERSION (between its heading and the next `## `).
changelog_section() {
    awk -v v="$VERSION" '
        /^## / {
            if (started) exit
            if (index($0, v) > 0) { started = 1; next }
        }
        started { print }
    ' "$(changelog_path "$PROJECT")"
}

# --- Subcommands ---

cmd_pre_check() {
    local errors=0
    echo "Pre-checking ${PROJECT} v${VERSION} (tag ${TAG})..."

    # 1. Changelog must document this version explicitly (not just Unreleased).
    local changelog
    changelog=$(changelog_path "$PROJECT")
    local ver_re=${VERSION//./\\.}
    if [ ! -f "$changelog" ]; then
        >&2 echo "  ✗ no CHANGELOG.md at $changelog"
        errors=1
    elif ! grep -qE "^##[[:space:]]+\[?${ver_re}(]| |\$)" "$changelog"; then
        >&2 echo "  ✗ CHANGELOG.md has no explicit heading for ${VERSION}"
        errors=1
    else
        echo "  ✓ changelog documents ${VERSION}"
    fi

    # 2. Git tag must not already exist on the remote.
    if git ls-remote --tags origin "refs/tags/${TAG}" 2>/dev/null | grep -q .; then
        >&2 echo "  ✗ tag ${TAG} already exists on origin"
        errors=1
    else
        echo "  ✓ tag ${TAG} is free"
    fi

    # 3. Version must not already be published at its official location.
    case "$KIND" in
        os)
            if start-cli --registry=$REGISTRY registry os index 2>/dev/null \
                | jq -e ".versions[\"$VERSION\"] // empty" >/dev/null 2>&1; then
                >&2 echo "  ✗ OS version ${VERSION} already in registry ${REGISTRY}"
                errors=1
            else
                echo "  ✓ OS version ${VERSION} not yet in registry"
            fi
            ;;
        npm)
            if [ -n "$(npm view "${SDK_NPM_PACKAGE}@${VERSION}" version 2>/dev/null || true)" ]; then
                >&2 echo "  ✗ ${SDK_NPM_PACKAGE}@${VERSION} already published to npm"
                errors=1
            else
                echo "  ✓ ${SDK_NPM_PACKAGE}@${VERSION} not yet on npm"
            fi
            ;;
        cli | deb)
            if gh release view -R "$REPO" "$TAG" >/dev/null 2>&1; then
                >&2 echo "  ✗ GitHub release ${TAG} already exists"
                errors=1
            else
                echo "  ✓ GitHub release ${TAG} does not exist"
            fi
            ;;
    esac

    if [ "$errors" -ne 0 ]; then
        >&2 echo "Pre-check failed."
        exit 1
    fi
    echo "Pre-check passed."
}

cmd_pull_gha() {
    require_kind os cli deb

    if [ -z "${RUN_ID:-}" ]; then
        read -rp "RUN_ID (GitHub Actions run for ${PROJECT}): " RUN_ID
    fi
    RUN_ID=$(parse_run_id "${RUN_ID:-}")
    if [ -z "$RUN_ID" ]; then
        >&2 echo "RUN_ID is required"
        exit 2
    fi

    ensure_release_dir
    echo "Downloading ${PROJECT} artifacts from run ${RUN_ID}..."

    case "$KIND" in
        os)
            for platform in $OS_PLATFORMS; do
                for ext in squashfs iso; do
                    echo "  ${platform}.${ext}"
                    gh run download -R "$REPO" "$RUN_ID" -n "${platform}.${ext}" -D "$(pwd)"
                done
            done
            ;;
        cli)
            for triple in $CLI_TRIPLES; do
                local name
                name=$(cli_asset_name "$triple")
                echo "  start-cli_${triple} -> start-cli_${name}"
                gh run download -R "$REPO" "$RUN_ID" -n "start-cli_${triple}" -D "$(pwd)"
                mv start-cli "start-cli_${name}"
            done
            ;;
        deb)
            for arch in $DEB_ARCHES; do
                echo "  ${PROJECT}_${arch}.deb"
                gh run download -R "$REPO" "$RUN_ID" -n "${PROJECT}_${arch}.deb" -D "$(pwd)"
            done
            ;;
    esac
}

cmd_pull() {
    ensure_release_dir
    echo "Downloading released ${PROJECT} v${VERSION} from its official location..."

    case "$KIND" in
        os)
            for platform in $OS_PLATFORMS; do
                for ext in squashfs iso; do
                    echo "  ${ext} ${platform}"
                    start-cli --registry=$REGISTRY registry os asset get "$ext" "$VERSION" "$platform" -d "$(pwd)"
                done
            done
            ;;
        cli)
            gh release download -R "$REPO" "$TAG" -p 'start-cli_*' -D "$(pwd)" --clobber
            ;;
        deb)
            for arch in $DEB_ARCHES; do
                local darch idx filename
                darch=$(deb_arch "$arch")
                idx="${APT_BASE_URL}/dists/${APT_SUITE}/${APT_COMPONENT}/binary-${darch}/Packages"
                filename=$(curl -fsSL "$idx" 2>/dev/null | awk -v pkg="$PROJECT" -v ver="$VERSION" '
                    /^$/ { p=""; v="" }
                    /^Package:/ { p=$2 }
                    /^Version:/ { v=$2 }
                    /^Filename:/ { if (p==pkg && index(v, ver) > 0) print $2 }
                ' | head -1)
                if [ -n "$filename" ]; then
                    echo "  ${arch}: ${filename}"
                    curl -fsSL "${APT_BASE_URL}/${filename}" -o "$(basename "$filename")"
                else
                    >&2 echo "  ! no ${PROJECT} ${arch} deb for ${VERSION} in apt repo"
                fi
            done
            ;;
        npm)
            npm pack "${SDK_NPM_PACKAGE}@${VERSION}"
            ;;
    esac
}

cmd_tag() {
    local commit="${COMMIT:-HEAD}"
    if [ -n "$(cd "$REPO_ROOT" && git status --porcelain)" ]; then
        >&2 echo "Warning: working tree is dirty; tagging ${commit} anyway."
    fi
    echo "Tagging ${TAG} at ${commit}..."
    (cd "$REPO_ROOT" && git tag ${FORCE:+-f} "$TAG" "$commit" && git push origin ${FORCE:+-f} "refs/tags/${TAG}")
}

cmd_create_gh_release() {
    require_kind os cli deb
    enter_release_dir
    local notes
    notes=$(release_notes)
    echo "Creating GitHub release ${TAG}..."
    if gh release view -R "$REPO" "$TAG" >/dev/null 2>&1; then
        gh release edit -R "$REPO" "$TAG" --notes "$notes"
    else
        gh release create -R "$REPO" "$TAG" --title "${PROJECT} v${VERSION}" --notes "$notes"
    fi
}

cmd_push() {
    case "$KIND" in
        os)
            enter_release_dir
            echo "Uploading OS images to ${S3_BUCKET}/v${VERSION}/ ..."
            for platform in $OS_PLATFORMS; do
                for file in *_"$platform".squashfs *_"$platform".iso; do
                    [ -f "$file" ] || continue
                    echo "  $file"
                    s3cmd put -P "$file" "${S3_BUCKET}/v${VERSION}/$file"
                done
            done
            ;;
        cli)
            enter_release_dir
            local files
            mapfile -t files < <(release_files)
            for file in "${files[@]}"; do
                gh release upload -R "$REPO" "$TAG" "$file" --clobber
            done
            ;;
        deb)
            enter_release_dir
            local debs=()
            for file in *.deb; do
                [ -f "$file" ] && debs+=("$file")
            done
            if [ ${#debs[@]} -eq 0 ]; then
                >&2 echo "No .deb files in $(release_dir)"
                exit 1
            fi
            echo "Publishing ${PROJECT} debs to the apt repository..."
            "$REPO_ROOT/debian/publish.sh" "${debs[@]}"
            echo "Uploading ${PROJECT} debs to GitHub release ${TAG}..."
            for file in "${debs[@]}"; do
                gh release upload -R "$REPO" "$TAG" "$file" --clobber
            done
            ;;
        npm)
            echo "Building and publishing ${SDK_NPM_PACKAGE}@${VERSION} to npm..."
            make -C "$REPO_ROOT/projects/start-sdk" publish ${OTP:+OTP=$OTP}
            ;;
    esac
}

cmd_index() {
    require_kind os
    enter_release_dir

    echo "Registering OS version ${VERSION} in ${REGISTRY}..."
    start-cli --registry=$REGISTRY registry os version add "$VERSION" "$TAG" '' ">=0.3.5 <=$VERSION"

    echo "Indexing OS assets..."
    for platform in $OS_PLATFORMS; do
        for file in *_"$platform".squashfs *_"$platform".iso; do
            [ -f "$file" ] || continue
            start-cli --registry=$REGISTRY registry os asset add \
                --platform="$platform" --version="$VERSION" "$file" "$S3_CDN/v$VERSION/$file"
        done
    done
}

cmd_sign() {
    require_kind os cli deb
    enter_release_dir
    resolve_gh_user

    local files
    mapfile -t files < <(release_files)
    mkdir -p signatures
    for file in "${files[@]}"; do
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
    tar -czf signatures.tar.gz -C signatures .

    gh release upload -R "$REPO" "$TAG" signatures.tar.gz --clobber
}

cmd_cosign() {
    require_kind os cli deb
    enter_release_dir
    resolve_gh_user

    if [ -z "$GH_USER" ] || [ -z "$GH_GPG_KEY" ]; then
        >&2 echo 'Error: could not determine GitHub user or GPG signing key'
        >&2 echo "Set GH_USER and/or configure git user.signingkey"
        exit 1
    fi

    echo "Downloading existing signatures..."
    gh release download -R "$REPO" "$TAG" -p "signatures.tar.gz" -D "$(pwd)" --clobber
    mkdir -p signatures
    tar -xzf signatures.tar.gz -C signatures

    echo "Adding personal signatures as $GH_USER..."
    local files
    mapfile -t files < <(release_files)
    for file in "${files[@]}"; do
        gpg -u "$GH_GPG_KEY" --detach-sign --armor -o "signatures/${file}.${GH_USER}.asc" "$file"
    done
    gpg --export -a "$GH_GPG_KEY" > "signatures/${GH_USER}.key.asc"

    tar -czf signatures.tar.gz -C signatures .
    gh release upload -R "$REPO" "$TAG" signatures.tar.gz --clobber
    echo "Done. Personal signatures for $GH_USER added to ${TAG}."
}

# Compose the release-notes body for the current project.
release_notes() {
    local files
    mapfile -t files < <(release_files)

    echo "## What's Changed"
    echo
    changelog_section
    echo

    case "$KIND" in
        os)
            echo "## ISO Downloads"
            echo
            local platform file
            for platform in $OS_PLATFORMS; do
                for file in *_"$platform".iso; do
                    [ -f "$file" ] || continue
                    echo "- [$(os_platform_label "$platform")]($S3_CDN/v$VERSION/$file)"
                done
            done
            echo
            checksum_block "OS Images" "${files[@]}"
            ;;
        cli)
            checksum_block "start-cli" "${files[@]}"
            ;;
        deb)
            checksum_block "${PROJECT} packages" "${files[@]}"
            ;;
    esac
}

checksum_block() {
    local title=$1
    shift
    echo "## ${title} Checksums"
    echo
    echo "### SHA-256"
    echo '```'
    sha256sum "$@" 2>/dev/null || true
    echo '```'
    echo
    echo "### BLAKE-3"
    echo '```'
    b3sum "$@" 2>/dev/null || true
    echo '```'
}

cmd_notes() {
    require_kind os cli deb
    enter_release_dir
    release_notes
}

cmd_release() {
    case "$KIND" in
        os)
            cmd_pre_check
            cmd_pull_gha
            cmd_tag
            cmd_create_gh_release
            cmd_push
            cmd_index
            cmd_sign
            ;;
        cli | deb)
            cmd_pre_check
            cmd_pull_gha
            cmd_tag
            cmd_create_gh_release
            cmd_push
            cmd_sign
            ;;
        npm)
            cmd_pre_check
            cmd_tag
            cmd_push
            ;;
    esac
}

usage() {
    cat << 'EOF'
Usage: manage-release.sh <subcommand> <project>

Projects:
  start-os        OS images (iso/squashfs) -> S3 + registry OS index
  start-cli       per-triple binaries      -> GitHub release
  start-tunnel    per-arch .deb            -> apt repo + GitHub release
  start-registry  per-arch .deb            -> apt repo + GitHub release
  start-sdk       npm package              -> npm

Version is read from the project's manifest (Cargo.toml, or package.json for
start-sdk); the git tag / GitHub release is <project>_v<version>.

Subcommands:
  pre-check          Verify the changelog documents this version and that the
                     version is not already tagged/released.
  pull-gha           Download build artifacts from a GitHub Actions run.
                     (os/cli/deb; set RUN_ID or you'll be prompted.)
  pull               Download the released assets from their official location
                     (registry / apt repo / GitHub release / npm).
  tag                Create and push the <project>_v<version> git tag.
  create-gh-release  Create (or update) the GitHub release with notes.
                     (os/cli/deb.)
  push               Upload artifacts to their destination (S3 for os, GitHub
                     release for cli, apt+GitHub for deb, npm publish for sdk).
  index              Register and index the version in the registry. (os only.)
  sign               Sign artifacts with the Start9 org key (+ personal key if
                     available) and upload signatures.tar.gz. (os/cli/deb.)
  cosign             Add your personal GPG signature to an existing release's
                     signatures.tar.gz. (os/cli/deb; run 'pull' first.)
  notes              Print the release notes to stdout. (os/cli/deb.)
  release            Run the full applicable pipeline for the project.

Environment variables:
  VERSION   Override the version (default: read from the manifest)
  RUN_ID    GitHub Actions run id/url for pull-gha
  COMMIT    Commit to tag (default: HEAD)
  FORCE     Set to 1 to force-move an existing tag
  CLEAN     Set to 1 to wipe and recreate the release directory
  GH_USER   Override GitHub username (default: autodetected via gh)
  OTP       npm one-time password (start-sdk publish)
EOF
}

# --- Dispatch ---

SUBCOMMAND="${1:-}"
PROJECT="${2:-}"

if [ -z "$SUBCOMMAND" ] || [ "$SUBCOMMAND" = "-h" ] || [ "$SUBCOMMAND" = "--help" ]; then
    usage
    exit 0
fi

if ! KIND=$(project_kind "$PROJECT"); then
    >&2 echo "Unknown or missing project: '${PROJECT}'"
    >&2 echo "Projects: ${PROJECTS}"
    exit 2
fi

VERSION="${VERSION:-$(derive_version "$PROJECT")}"
if [ -z "$VERSION" ]; then
    >&2 echo "Could not derive version for ${PROJECT}"
    exit 1
fi
TAG="${PROJECT}_v${VERSION}"

case "$SUBCOMMAND" in
    pre-check) cmd_pre_check ;;
    pull-gha) cmd_pull_gha ;;
    pull) cmd_pull ;;
    tag) cmd_tag ;;
    create-gh-release) cmd_create_gh_release ;;
    push) cmd_push ;;
    index) cmd_index ;;
    sign) cmd_sign ;;
    cosign) cmd_cosign ;;
    notes) cmd_notes ;;
    release) cmd_release ;;
    *)
        >&2 echo "Unknown subcommand: '${SUBCOMMAND}'"
        usage
        exit 2
        ;;
esac
