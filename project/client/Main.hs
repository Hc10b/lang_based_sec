module Main where

import RealConnector
import IoImpl
import Control.Monad.State
import Config

main :: IO ()
main = do
    sock <- endPClient
    let ma1 = evalStateT (algoA sock (protocol curProt)) (initClientState curProt)
    let ma2 = evalStateT ma1 sock
    ma2
    return ()
