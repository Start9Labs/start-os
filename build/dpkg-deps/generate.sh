#!/bin/bash

set -e

cd "$(dirname "${BASH_SOURCE[0]}")"

IFS="-" read -ra FEATURES <<< "$ENVIRONMENT"

feature_file_checker='
/^#/ { next }
/^\+ [a-z0-9]+$/ { next }
/^- [a-z0-9]+$/ { next }
{ exit 1 }
'

for type in conflicts depends; do
    pkgs=()
    for feature in ${FEATURES[@]}; do
        file="$feature.$type"
        if [ -f $file ]; then
            # TODO check for syntax errrors
            cat $file | awk "$feature_file_checker"
            for pkg in $(cat $file | awk '/^\+/ {print $2}'); do
                pkgs+=($pkg)
            done
        fi
    done
    for pkg in $(cat $type); do
        SKIP=
        for feature in ${FEATURES[@]}; do
            file="$feature.$type"
            if [ -f $file ]; then
                if grep "^- $pkg$" $file; then
                    SKIP=1
                fi
            fi
        done
        if [ -z $SKIP ]; then
            pkgs+=($pkg)
        fi
    done
    (IFS=$'\n'; echo "${pkgs[*]}") | sort -u > ../lib/$type
done
