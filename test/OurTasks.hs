module OurTasks where
import Control.Concurrent.CFuture
import Control.Concurrent (threadDelay)

task :: IO Int
task = sum <$> mapM subtask [1..10]
  where
  subtask :: Int -> IO Int
  subtask n = do
    print n
    threadDelay 1000000
    return $ 2 * n

cFunction :: CFuturePtr -> IO ()
cFunction = forkFutureC task
foreign export ccall cFunction :: CFuturePtr -> IO ()
