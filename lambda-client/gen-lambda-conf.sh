#!/bin/sh

private_id=$(cat /home/icfpc2019/data/private-id.txt)

set -x
sed -e "s/@private_id@/$private_id/" \
    < lambda.conf.in \
    > lambda.conf
