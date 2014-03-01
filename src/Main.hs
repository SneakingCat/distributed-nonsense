module Main 
       ( main
       ) where

import System.Environment (getArgs)
--import Control
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet ( initializeBackend 
                                                          , startMaster
                                                          , startSlave )

import qualified Master.Master as Master

main :: IO ()
main = do
  args <- getArgs
  
  let remoteTable = Master.remoteTable initRemoteTable
  case args of
    ["master", host, port] -> do
      backend <- initializeBackend host port remoteTable
      startMaster backend (Master.run backend)
      
    ["slave", host, port]  -> do
      backend <- initializeBackend host port remoteTable
      startSlave backend
      
    _                      -> putStrLn "Invalid options"
  