{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveDataTypeable #-}
module Master.Master
       ( run
       , remoteTable
       ) where

import Control.Distributed.Process ( Process, NodeId, RemoteTable
                                   , NodeMonitorNotification (..)
                                   , ProcessMonitorNotification (..)
                                   , DiedReason (..), ProcessId (..)
                                   , say, spawnSupervised, spawnLocal
                                   , getSelfPid, nsend
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

import Slave.Slave (slaveProc, slaveProc__static)

data NodeDetected = 
  NodeDetected !NodeId
    deriving (Generic, Typeable)
             
instance Binary NodeDetected

supervisor :: Process ()
supervisor = do
  register "supervisor" =<< getSelfPid
  forever $
    receiveWait
    [ match nodeDetected
    , match nodeMonitorNotification                                             
    , match processMonitorNotification
    ]
  where
    nodeDetected :: NodeDetected -> Process ()
    nodeDetected (NodeDetected nodeId) = do
      say $ "Detected node: " ++ show nodeId
      _ <- monitorNode nodeId
      newProc <- spawnSupervised nodeId $(mkStaticClosure 'slaveProc)
      say $ printf "Create process %s at node %s" (show newProc) 
                                                  (show nodeId)
      return ()                                             
                                             
    nodeMonitorNotification :: NodeMonitorNotification -> Process ()
    nodeMonitorNotification (NodeMonitorNotification _ nodeId reason) =
      say $ printf "Node %s died because of %s" (show nodeId) (show reason)
                                             
    processMonitorNotification :: ProcessMonitorNotification -> Process ()
    processMonitorNotification (ProcessMonitorNotification _ 
                                processId 
                                reason) =
      case reason of                                          
        (DiedException _) -> do
          say $ printf "Process %s crashed and will be restarted" 
                       (show processId)
          newProc <- spawnSupervised (processNodeId processId)
                                     $(mkStaticClosure 'slaveProc)
          return ()                                       
        _              ->                                          
          say $ printf "Process %s died because of %s" (show processId)
                                                       (show reason)
                                             
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
  