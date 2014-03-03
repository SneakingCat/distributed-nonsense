{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveDataTypeable #-}
module Slave.Slave
       ( Request (..)
       , Response (..)
       , slaveProc
       , slaveProc__static
       , remoteTable
       ) where

import Control.Distributed.Process ( Process, ProcessId, RemoteTable
                                   , expect, send, liftIO)
import Control.Distributed.Process.Closure
import Control.Concurrent (threadDelay)
import Control.Monad (forever)

import Data.Binary
import Data.Typeable
import GHC.Generics

data Request = Request !ProcessId !Int
  deriving (Generic, Typeable)
           
data Response = Response !Int           
  deriving (Generic, Typeable)
           
instance Binary Request
instance Binary Response

slaveProc :: Process ()
slaveProc =
  forever $ do
    (Request pid num) <- expect
    liftIO $ threadDelay 500000
    send pid $ Response (num * 2)

remotable ['slaveProc]
           
remoteTable :: (RemoteTable -> RemoteTable)
remoteTable = __remoteTable           



