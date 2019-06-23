#!/bin/sh

zip_path="$1"

if [ ! -r $zip_path ]; then
    cat <<EOF
file not found: $zip_path

Usage: $0 ZIPFILE_PATH
EOF
fi

log_path=$(dirname $zip_path)/$(basename $zip_path .zip).log

pvfile=/home/icfpc2019/data/private-id.txt

set -e

curl \
    -F private_id="$(cat $pvfile)" \
    -F file="@${zip_path}" \
    https://monadic-lab.org/submit \
    >> $log_path 2>&1

echo '' >> $log_path

cat $log_path
