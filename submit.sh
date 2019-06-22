#!/bin/sh

ts=$(date +"%d-%H%M%S-%N")

set -e
set -x

rm -f solutions.zip
rm -f solutions/*~
( cd solutions/ && zip -r ../solutions.zip . )

unzip -l solutions.zip

cp -a solutions.zip /home/icfpc2019/submit/post/${ts}.zip
