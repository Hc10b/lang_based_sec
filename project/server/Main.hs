module Main where

import RealConnector
import IoImpl
import Control.Monad.State
import Config

main :: IO ()
main = do
    putStrLn "start"
    sock <- endPServer
    let ma1 = evalStateT (algoB sock (protocol curProt)) (initServerState curProt)
    let ma2 = evalStateT ma1 sock
    ma2
    return ()
