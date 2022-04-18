module Main where

import Lib
import Protocols.Smtp
import Protocols.Http
import Protocol
import Simulation

-- works
--main = simulateCommunication (SendA2B (return "hi") >> SendB2A (return "hi")) [] [] [] [] (const 1) (const 1)
testMiddelware = HttpMiddleware id

main = httpExample

httpExample = simulateCommunication
 (http testMiddelware)
 [Input 1 "GET test"] undefined
 undefined
 (Nothing:: Maybe Int)
 (const 1) (const 1)

smtpExample = simulateCommunication
 smtpWithFlaw
 [] []
 [["hi", "", "this is just a short message"], ["the next line is only a dot",".","and this line is dropped symmetrically"]]
 undefined
 (const 0) (const 0)

{-instance MonadIO m => Interactive m where
    readI = liftIO readLn 
    outputI = liftIO . putStrLn-}