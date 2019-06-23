#!/bin/sh

ln -s .stack-work/dist/x86_64-linux-tinfo6/Cabal-2.2.0.1/build/solver-simpleprime/solver-simpleprime solver-simpleprime

ls part-1-initial/*.desc | sed 's/part-1-initial\/\(.*\)\.desc/.\/solver-simpleprime part-1-initial\/\1.desc > solutions-solver-simpleprime\/\1.sol \&/'|sh

