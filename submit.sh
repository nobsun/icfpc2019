#!/bin/sh

set -e

usage() {
    cat <<EOF
Usage: $0 DIRECTORY_TO_SUBMIT
EOF
}

dir="$1"

if [ x"$dir" = x ]; then
    echo "DIRECTORY_TO_SUBMIT is required."
    usage
    exit 1
fi

if [ ! -d "$dir" ]; then
    echo "directory not found: $dir"
    usage
    exit 1
fi

cwd=$(pwd)
ts=$(date +"%d-%H%M%S-%N")

set -x

rm -f ${cwd}/solutions.zip

( cd $dir && zip -r ${cwd}/solutions.zip *.sol )

# unzip -l ${cwd}/solutions.zip

cp -a ${cwd}/solutions.zip /home/icfpc2019/submit/post/${ts}.zip
