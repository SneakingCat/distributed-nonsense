{-# LANGUAGE TemplateHaskell #-}
module Slave.Slave
       ( slaveProc
       , slaveProc__static
       , remoteTable
       ) where

import Control.Distributed.Process (Process, RemoteTable, say, expect)
import Control.Distributed.Process.Closure

slaveProc :: Process ()
slaveProc = do
  say "SlaveProc" >> error "III"
  --_ <- expect :: Process Int
  --return ()

remotable ['slaveProc]
           
remoteTable :: (RemoteTable -> RemoteTable)
remoteTable = __remoteTable           



