import RunSolver (solve1)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  (n, algo) <-  case args of
    n_ : algo_ : _  ->  (,) <$> readIO n_ <*> readIO algo_
    _                ->  fail "invalid argument: solve-one PROBLEM_NUM ALGORITHM_NAME"
  solve1 algo (n :: Int)
