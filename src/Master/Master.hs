{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveDataTypeable #-}
module Master.Master
       ( run
       , remoteTable
       ) where

import Control.Distributed.Process ( Process, NodeId, RemoteTable
                                   , NodeMonitorNotification (..)
                                   , ProcessId (..)
                                   , say, spawnLocal, getSelfPid, nsend
                                   , receiveWait, match, monitorNode
                                   , register, liftIO )
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Backend.SimpleLocalnet (Backend, findSlaves)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.List ((\\))
import Data.Binary
import Data.Typeable
import Text.Printf (printf)

import GHC.Generics

data NodeDetected = 
  NodeDetected !NodeId
    deriving (Generic, Typeable)
             
instance Binary NodeDetected

supervisor :: Process ()
supervisor = do
  register "supervisor" =<< getSelfPid
  forever $
    receiveWait
    [ match $ \(NodeDetected nodeid) -> do
         say $ "Detected node: " ++ show nodeid
         monitorNode nodeid >> return ()
    , match $ \(NodeMonitorNotification _ nodeid reason) ->
       say $ printf "Node %s died because of %s" (show nodeid) (show reason)
    ]

nodeDetector :: Backend -> Process ()
nodeDetector backend = loop []
  where
    loop :: [NodeId] -> Process ()
    loop nodes = do
      nodes' <- map processNodeId `fmap` findSlaves backend
      mapM_ (nsend "supervisor" . NodeDetected) $ nodes' \\ nodes
      loop nodes'

remotable []

remoteTable :: (RemoteTable -> RemoteTable)
remoteTable = __remoteTable

run :: Backend -> [NodeId] -> Process ()
run backend  _ = do
  _ <- spawnLocal supervisor
  _ <- spawnLocal (nodeDetector backend)
  liftIO $ threadDelay 100000000
  return ()
  