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

solutions_zip=${cwd}/to-enqueue-solutions.zip

rm -f ${solutions_zip}

( cd $dir && zip -r ${solutions_zip} *.sol )

# unzip -l ${cwd}/solutions.zip

cp -a ${solutions_zip} /home/icfpc2019/submit/post/${ts}.zip
