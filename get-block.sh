#!/bin/sh

n=$1

if [ x"${n}" = x ]; then
    cat <<EOF
Usage: $0 BLOCK_NUMBER
EOF
    exit 1
fi

set -x
curl \
    --data-binary '{"jsonrpc":"2.0","id":"curl","method":"getblockinfo","params":['"${n}"']}' \
    -H 'content-type:text/plain;' \
    -o /home/icfpc2019/blocks/"${n}".json.new \
    http://127.0.0.1:28332/

mv /home/icfpc2019/blocks/"${n}".json.new /home/icfpc2019/blocks/"${n}".json
