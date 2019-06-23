#!/bin/sh

ln -s .stack-work/dist/x86_64-linux-tinfo6/Cabal-2.2.0.1/build/solver-simple/solver-simple solver-simple

ls part-1-initial/*.desc | sed 's/part-1-initial\/\(.*\)\.desc/.\/solver-simple part-1-initial\/\1.desc > solutions-simple-solver\/\1.sol \&/'|sh

