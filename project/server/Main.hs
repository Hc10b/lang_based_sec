module Main where

import Lib
import Protocols.Smtp
import Protocols.Http
import Protocol
import RealConnector
import IoImpl
import Nanomsg
import Control.Monad.State
import Config

-- works
--main = simulateCommunication (SendA2B (return "hi") >> SendB2A (return "hi")) [] [] [] [] (const 1) (const 1)

main = httpExample

protAlgo :: RealConnector Pair SS ()
protAlgo = algoB curProt

httpExample :: IO ()
httpExample = do
    putStrLn "start"
    sock <- endPServer
    let ma1 = evalStateT protAlgo serverState
    let ma2 = evalStateT ma1 sock
    ma2
    return ()