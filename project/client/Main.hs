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

protAlgo :: CS -> RealConnector Pair CS ()
protAlgo _ = algoA curProt

httpExample :: IO ()
httpExample = do
    sock <- endPClient
    let ma1 = evalStateT (protAlgo clientState) clientState
    let ma2 = evalStateT ma1 sock
    ma2
    return ()

{-instance MonadIO m => Interactive m where
    readI = liftIO readLn 
    outputI = liftIO . putStrLn-}