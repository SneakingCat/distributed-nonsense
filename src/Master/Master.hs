module Master.Master
       ( run
       ) where

import Control.Distributed.Process (Process, NodeId)
import Control.Distributed.Process.Backend.SimpleLocalnet (Backend)

run :: Backend -> [NodeId] -> Process ()
run _ _ = return ()