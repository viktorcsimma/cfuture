-- We call this from main.c.
module HsTestCase (hsTestCase) where

import Control.Concurrent.CFuture
import Control.Concurrent (threadDelay)
import OurTasks

hsTestCase :: IO Bool
hsTestCase = do
  print "Working"
  future <- forkFuture task
  print "Delay"
  threadDelay 2000000
  _ <- abort future
  print "Aborted"
  result <- safeGet future
  -- it should be an exception
  case result of
    Left _ -> return True
    _      -> return False

foreign export ccall hsTestCase :: IO Bool

