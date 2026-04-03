#!/bin/bash
set -euo pipefail

# Copy all categories from one registry to another, and update matching
# packages on the target to have the same category assignments.
#
# Usage:
#   ./scripts/copy-categories.sh <source-registry-url> <target-registry-url>
#
# Requires: start-cli, jq

if [ $# -ne 2 ]; then
    echo "Usage: $0 <source-registry-url> <target-registry-url>"
    echo "Example: $0 https://registry.start9.com https://community.start9.com"
    exit 1
fi

SOURCE="$1"
TARGET="$2"

command -v start-cli >/dev/null 2>&1 || { echo "Error: start-cli not found in PATH"; exit 1; }
command -v jq >/dev/null 2>&1 || { echo "Error: jq not found in PATH"; exit 1; }

echo "Source registry: $SOURCE"
echo "Target registry: $TARGET"
echo

# Fetch the full package index (categories + packages) from both registries
echo "Fetching source package index..."
SOURCE_INDEX=$(start-cli --registry="$SOURCE" registry package index --format json)

echo "Fetching target package index..."
TARGET_INDEX=$(start-cli --registry="$TARGET" registry package index --format json)

# Extract existing category IDs on the target so we can skip duplicates
TARGET_CATEGORY_IDS=$(echo "$TARGET_INDEX" | jq -r '.categories | keys[]')

# Step 1: Create categories on the target that don't already exist
echo
echo "=== Syncing categories ==="
echo "$SOURCE_INDEX" | jq -r '.categories | to_entries[] | @base64' | while read -r entry; do
    id=$(echo "$entry" | base64 -d | jq -r '.key')
    name=$(echo "$entry" | base64 -d | jq -c '.value.name')

    if echo "$TARGET_CATEGORY_IDS" | grep -qxF "$id"; then
        echo "  Category '$id' already exists on target, skipping"
    else
        echo "  Creating category '$id' on target..."
        start-cli --registry="$TARGET" registry package category add "$id" "$name"
    fi
done

# Step 2: Get the list of packages on the target registry
TARGET_PACKAGE_IDS=$(echo "$TARGET_INDEX" | jq -r '.packages | keys[]')

# Step 3: For each package on the target, if it exists on the source,
# sync its category assignments
echo
echo "=== Syncing package category assignments ==="
echo "$SOURCE_INDEX" | jq -r '.packages | to_entries[] | @base64' | while read -r entry; do
    pkg_id=$(echo "$entry" | base64 -d | jq -r '.key')
    source_categories=$(echo "$entry" | base64 -d | jq -r '.value.categories[]' 2>/dev/null || true)

    # Skip if this package doesn't exist on the target
    if ! echo "$TARGET_PACKAGE_IDS" | grep -qxF "$pkg_id"; then
        continue
    fi

    # Get existing categories for this package on the target
    target_pkg_categories=$(echo "$TARGET_INDEX" | jq -r --arg pkg "$pkg_id" '.packages[$pkg].categories[]' 2>/dev/null || true)

    if [ -z "$source_categories" ]; then
        continue
    fi

    echo "  Package '$pkg_id':"

    # Remove categories on target that aren't on source
    if [ -n "$target_pkg_categories" ]; then
        for cat_id in $target_pkg_categories; do
            if ! echo "$source_categories" | grep -qxF "$cat_id"; then
                echo "    Removing category '$cat_id'..."
                start-cli --registry="$TARGET" registry package category remove-package "$cat_id" "$pkg_id"
            fi
        done
    fi

    # Add categories from source that aren't on target
    for cat_id in $source_categories; do
        if echo "$target_pkg_categories" | grep -qxF "$cat_id"; then
            echo "    Category '$cat_id' already assigned, skipping"
        else
            echo "    Adding category '$cat_id'..."
            start-cli --registry="$TARGET" registry package category add-package "$cat_id" "$pkg_id"
        fi
    done
done

echo
echo "Done!"
