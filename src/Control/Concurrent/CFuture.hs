-- | A Future type that is easy to represent and handle
-- in C\/C++,
-- using two 'MVar's.
-- Moreover, it uses two new threads:
-- one (the "watcher thread") aborts the calculation
-- if triggered by filling the first 'MVar'.
module Control.Concurrent.CFuture
  (Future, FutureResult, CFuturePtr,
   forkFuture, writeFutureC, forkFutureC,
   safeGet, get, getC, waitC, abort)
  where

import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable
import Control.Concurrent
import Control.Exception(throw, toException, SomeException)
import BasePrelude (PrimMVar, newStablePtrPrimMVar,
                    Int8, Int16, Int32, Int64,
                    Word8, Word16, Word32, Word64)

-- | An exception is thrown
-- if the Future has been aborted.
type FutureResult a = Either SomeException a

-- | A C pointer to a C array of two 'StablePtr's.
type CFuturePtr = Ptr (StablePtr PrimMVar)

-- | A type similar to C++ futures,
-- that can be passed to C and used
-- to interrupt asynchronous calls
-- or to get their results.
--
-- From within Haskell, you can use
-- 'forkFuture' to start a calculation,
-- 'safeGet' or 'get' to wait on it
-- and 'abort' to abort it.
--
-- From C, interruption happens
-- via calling hs_try_putmvar
-- on the first 'StablePtr';
-- freeing the pointers is possible
-- via hs_free_stable_ptr --
-- all these without the cost of an FFI call.

data Future a = MkFuture 
  (MVar ())                -- interruption: fill it to interrupt the calculation
  (MVar (FutureResult a))  -- result:       where the result or the exception will be put

-- | Starts an asynchronous calculation
-- and returns a 'Future' to it.
forkFuture :: IO a -> IO (Future a)
forkFuture action = do
  intMVar <- (newEmptyMVar :: IO (MVar ()))
  resMVar <- (newEmptyMVar :: IO (MVar (FutureResult a)))

  -- The thread doing the actual calculation.
  -- It also wakes up the watcher thread.
  calculationThreadId <- forkIO $ (putMVar resMVar =<< (Right <$> action)) >> putMVar intMVar ()

  -- The thread killing the calculation thread if woken up.
  _ <- forkIO $ do
    -- this is activated if intMVar gets filled
    readMVar intMVar
    killThread calculationThreadId
    putMVar resMVar (Left (toException (userError "Calculation aborted")))

  return $ MkFuture intMVar resMVar

-- | Interrupts the calculation behind the 'Future'.
-- Do not call this from C;
-- use hs_try_putmvar instead
-- (that frees the first 'MVar', too).
-- Returns 'False' if it has already been interrupted
-- and 'True' otherwise.
abort :: Future a -> IO Bool
abort (MkFuture intMVar _) = tryPutMVar intMVar ()

-- | Creates 'StablePtr's
-- and writes them to a memory area
-- provided by a C caller.
-- Use this in functions where the C frontend provides
-- a 'CFuturePtr' to write the future to.
--
-- Note: it is the responsibility of the C side
-- to free the 'StablePtr's.
writeFutureC :: CFuturePtr -> Future a -> IO ()
writeFutureC ptr (MkFuture intMVar resMVar) = do
  intMVarSPtr <- newStablePtrPrimMVar intMVar
  resMVarSPtr <- newStablePtr resMVar
  let convPtr = (castPtr ptr :: CFuturePtr)
  poke convPtr intMVarSPtr
  pokeElemOff (castPtr convPtr) 1 resMVarSPtr

-- | Similar to 'forkFuture', but
-- we write the 'Future' into a location
-- given by the caller.
-- This makes it easier to create C exports
-- for actions.
--
-- Use this in functions where the C frontend provides
-- a 'CFuturePtr' to write the future to.
--
-- Note: it is the responsibility of the C side
-- to free the 'StablePtr's.
forkFutureC :: IO a -> CFuturePtr -> IO ()
forkFutureC action ptr = writeFutureC ptr =<< forkFuture action

-- | Reads the result from the 'Future'
-- but does not check whether there has been an exception
-- wrapped into a 'Left'.
-- This is a blocking call,
-- waiting for the result until it is ready.
safeGet :: Future a -> IO (FutureResult a)
safeGet (MkFuture _ resMVar) = readMVar resMVar

-- | Similar to 'safeGet', but checks whether we have an exception
-- and rethrows it if yes;
-- returning the plain result otherwise.
-- This is a blocking call,
-- waiting for the result until it is ready.
get :: Future a -> IO a
get future = safeGet future >>= either throw return

-- | A variant of 'get' to call from C
-- which writes the result to the memory location
-- defined by the pointer.
-- If there is an exception instead of a result,
-- it writes nothing to the pointer
-- and returns 'False';
-- on success, it returns 'True'.
getC :: Storable a => CFuturePtr -> Ptr a -> IO Bool
getC futurePtr destPtr = do
  -- we assume both StablePtrs are of the same size
  resMVarSPtr <- peekElemOff (castPtr futurePtr :: Ptr (StablePtr (MVar (FutureResult a)))) 1
  -- we catch the exception
  -- as C/C++ could not handle it
  result <- readMVar =<< deRefStablePtr resMVarSPtr
  case result of
    Right a -> poke destPtr a >> return True
    _       -> return False

-- | Only waits until the calculation gets finished;
-- then returns 'True' if it was successful and 'False' otherwise.
-- To be called from C.
waitC :: CFuturePtr -> IO Bool
waitC ptr = do
  -- we assume both StablePtrs are of the same size
  resMVarSPtr <- peekElemOff (castPtr ptr :: Ptr (StablePtr (MVar (Either String ())))) 1
  -- we catch the exception and return an illegal value
  -- in order to unblock a C++ thread waiting for the result
  result <- readMVar =<< deRefStablePtr resMVarSPtr
  case result of
    Right _ -> return True
    _       -> return False

-- Finally, the foreign export declarations:
-- this is quite ugly, but c'est la vie.
-- It does not work with a void pointer
-- because then, the Haskell side does not get to know the type
-- and tries to poke a ().
foreign export ccall "getC_Char" getC :: CFuturePtr -> Ptr Char -> IO Bool
foreign export ccall "getC_Int" getC :: CFuturePtr -> Ptr Int -> IO Bool
foreign export ccall "getC_Int8" getC :: CFuturePtr -> Ptr Int8 -> IO Bool
foreign export ccall "getC_Int16" getC :: CFuturePtr -> Ptr Int16 -> IO Bool
foreign export ccall "getC_Int32" getC :: CFuturePtr -> Ptr Int32 -> IO Bool
foreign export ccall "getC_Int64" getC :: CFuturePtr -> Ptr Int64 -> IO Bool
foreign export ccall "getC_Word" getC :: CFuturePtr -> Ptr Word -> IO Bool
foreign export ccall "getC_Word8" getC :: CFuturePtr -> Ptr Word8 -> IO Bool
foreign export ccall "getC_Word16" getC :: CFuturePtr -> Ptr Word16 -> IO Bool
foreign export ccall "getC_Word32" getC :: CFuturePtr -> Ptr Word32 -> IO Bool
foreign export ccall "getC_Word64" getC :: CFuturePtr -> Ptr Word64 -> IO Bool
foreign export ccall "getC_Float" getC :: CFuturePtr -> Ptr Float -> IO Bool
foreign export ccall "getC_Double" getC :: CFuturePtr -> Ptr Double -> IO Bool
foreign export ccall "getC_Bool" getC :: CFuturePtr -> Ptr Bool -> IO Bool
foreign export ccall "getC_Ptr" getC :: CFuturePtr -> Ptr (Ptr a) -> IO Bool
foreign export ccall "getC_FunPtr" getC :: CFuturePtr -> Ptr (FunPtr a) -> IO Bool
foreign export ccall "getC_StablePtr" getC :: CFuturePtr -> Ptr (StablePtr a) -> IO Bool
foreign export ccall waitC :: CFuturePtr -> IO Bool

