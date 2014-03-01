module Main 
       ( main
       ) where

import System.Environment (getArgs)
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet ( initializeBackend 
                                                          , startMaster
                                                          , startSlave )

import qualified Master.Master as Master

main :: IO ()
main = do
  args <- getArgs
  
  case args of
    ["master", host, port] -> do
      backend <- initializeBackend host port initRemoteTable
      startMaster backend (Master.run backend)
      
    ["slave", host, port]  -> do
      backend <- initializeBackend host port initRemoteTable
      startSlave backend
      
    _                      -> putStrLn "Invalid options"
  