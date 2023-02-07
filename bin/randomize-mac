#!/bin/bash
INTERFACE=${1:-en0}
MAC=$(openssl rand -hex 6 | sed 's/\(..\)/\1:/g; s/.$//')
ifconfig $INTERFACE ether $MAC
ifconfig $INTERFACE down
ifconfig $INTERFACE up
