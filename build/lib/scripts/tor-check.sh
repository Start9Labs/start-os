#!/bin/bash

fail=$(printf " [\033[31m fail \033[0m]")
pass=$(printf " [\033[32m pass \033[0m]")

onion_list=(
    "Start9|http://privacy34kn4ez3y3nijweec6w4g54i3g54sdv7r5mr6soma3w4begyd.onion"
    "Mempool|http://mempoolhqx4isw62xs7abwphsq7ldayuidyx2v2oethdhhj6mlo2r6ad.onion"
    "DuckDuckGo|https://duckduckgogg42xjoc72x3sjasowoarfbgcmvfimaftt6twagswzczad.onion"
    "Brave Search|https://search.brave4u7jddbv7cyviptqjc7jusxh72uik7zt6adtckl5f4nwy2v72qd.onion"
)

# Check if ~/.startos/tor-check.list exists and read its contents if available
if [ -f ~/.startos/tor-check.list ]; then
    while IFS= read -r line; do
        # Check if the line starts with a #
        if [[ ! "$line" =~ ^# ]]; then
            onion_list+=("$line")
        fi
    done < ~/.startos/tor-check.list
fi

echo "Testing connection to Onion Pages ..."

for data in "${onion_list[@]}"; do
    name="${data%%|*}"
    url="${data#*|}"

    curl --socks5-hostname localhost:9050 "$url" > /dev/null 2>&1
    HC=$?

    if [ $HC -ne 0 ]; then
        echo " ${fail}: $name ($url) "
    else
        echo " ${pass}: $name ($url) "
    fi
done

echo
echo "Done."
