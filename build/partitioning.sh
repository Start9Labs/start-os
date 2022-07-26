#!/bin/bash

set -e

# Use fdisk to create DOS partition table with 4 primary partitions, set 1 as bootable, write, and quite
(echo o; echo x; echo i; echo "0xcb15ae4d"; echo r; echo n; echo p; echo 1; echo 2048; echo 526335; echo t; echo c; echo n; echo p; echo 2; echo 526336; echo 1050623; echo t; echo 2; echo c; echo n; echo p; echo 3; echo 1050624; echo 16083455; echo n; echo p; echo 16083456; echo 31116287; echo a; echo 1; echo w) | sudo fdisk ${OUTPUT_DEVICE} > /dev/null
