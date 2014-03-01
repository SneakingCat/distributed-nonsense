module Master.RoundRobinProcList
       ( RoundRobinProcList
       , create
       , addProc
       , removeProc
       , nextProc
       ) where

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Distributed.Process (ProcessId)

data RoundRobinProcList = RoundRobinProcList (TVar [ProcessId])

create :: IO RoundRobinProcList
create = atomically $ RoundRobinProcList `fmap` (newTVar [])

addProc :: RoundRobinProcList -> ProcessId -> IO ()
addProc (RoundRobinProcList tvar) x = atomically $ modifyTVar tvar add
  where
    add :: [ProcessId] -> [ProcessId]
    add procs = x:procs
    
removeProc :: RoundRobinProcList -> ProcessId -> IO ()
removeProc (RoundRobinProcList tvar) x = 
  atomically $ modifyTVar tvar (filter (x /=))
    
nextProc :: RoundRobinProcList -> IO ProcessId
nextProc (RoundRobinProcList tvar) = 
  atomically $ do
    procs <- readTVar tvar
    case procs of
      []     -> retry
      (x:xs) -> do
        writeTVar tvar (xs ++ [x])
        return x

