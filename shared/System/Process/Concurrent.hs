module System.Process.Concurrent where

import Control.Concurrent.MVar
import System.Process
import System.Exit (ExitCode)
import System.IO (Handle)

newtype CProcessHandle = CProcessHandle (MVar ProcessHandleState)

data ProcessHandleState = OpenHandle ProcessHandle
                        | WaitingOn  ProcessHandle (MVar ExitCode)
                        | ClosedHandle ExitCode

createCProcess :: CreateProcess
               -> IO ( Maybe Handle
                     , Maybe Handle
                     , Maybe Handle
                     , CProcessHandle
                     )
createCProcess p = do
  (i, o, e, h) <- createProcess p
  ch <- mkCProcessHandle h
  return (i, o, e, ch)

mkCProcessHandle :: ProcessHandle -> IO CProcessHandle
mkCProcessHandle handle =
  CProcessHandle <$> newMVar (OpenHandle handle)

waitForCProcess :: CProcessHandle -> IO ExitCode
waitForCProcess (CProcessHandle mv) = do
  phs <- takeMVar mv
  -- TODO: What happens when an exception occurs in here?
  case phs of
    OpenHandle handle -> do
        emv <- newEmptyMVar
        putMVar mv $ WaitingOn handle emv
        rv <- waitForProcess handle
        putMVar emv rv
        return rv
    WaitingOn _handle emv -> do
        putMVar mv phs
        takeMVar emv
    ClosedHandle rv -> do
        putMVar mv phs
        return rv

terminateCProcess :: CProcessHandle -> IO ()
terminateCProcess (CProcessHandle mv) = do
  phs <- takeMVar mv
  case phs of
    OpenHandle handle -> do
      terminateProcess handle
    WaitingOn handle _ -> do
      terminateProcess handle
    _ -> return ()

  putMVar mv phs
