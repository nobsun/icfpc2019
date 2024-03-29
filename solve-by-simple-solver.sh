#!/bin/sh

ls part-1-initial/*.desc | sed 's/part-1-initial\/\(.*\)\.desc/.\/solver part-1-initial\/\1.desc --alg=simple > solutions-simple-solver\/\1.sol \&/'|sh

ls part-2-teleports/*.desc | sed 's/part-2-teleports\/\(.*\)\.desc/.\/solver part-2-teleports\/\1.desc --alg=simple > solutions-simple-solver\/\1.sol \&/'|sh

ls part-3-clones/*.desc | sed 's/part-3-clones\/\(.*\)\.desc/.\/solver part-3-clones\/\1.desc --alg=simple > solutions-simple-solver\/\1.sol \&/'|sh

