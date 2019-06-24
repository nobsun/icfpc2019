import RunConcurrent (solveConcurrent)

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  (n, x, algo) <- case args of
    n_ : x_ : algo_ : _ -> (,,) <$> readIO n_ <*> readIO x_ <*> readIO algo_
    _                   -> fail "invalid argument: solve-concurrent MIN MAX ALGORITHM_NAME"

  solveConcurrent 38 [ (i, algo) | i <- [n..x] ]
