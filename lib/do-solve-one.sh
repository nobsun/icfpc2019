#!/bin/sh

n="$1"
algo="$2"

logd=/home/icfpc2019/log/"${algo}"
mkdir -p ${logd}
./bin/solve-one "$n" "$algo" > ${logd}/${n}.log 2>&1
