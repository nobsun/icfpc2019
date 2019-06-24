import RunSolver (Algo (..))
import RunConcurrent (solveConcurrent)

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  (n, x, malgo) <- case args of
    n_ : x_ : algo_ : _ -> (,,) <$> readIO n_ <*> readIO x_ <*> fmap Just (readIO algo_)
    n_ : x_ : _         -> (,,) <$> readIO n_ <*> readIO x_ <*> pure Nothing
    _                   -> fail "invalid argument: solve-concurrent MIN MAX ALGORITHM_NAME"

  case malgo of
    Nothing   -> solveConcurrent 38 [ (i, algo) | i <- [n..x], algo <- [ SimplePrime ] ]
    Just algo -> solveConcurrent 38 [ (i, algo) | i <- [n..x] ]
