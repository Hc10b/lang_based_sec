module Main where

import Lib

main :: IO ()
main = someFunc

smtpExampleShowingFlaw = simulateCommunication smtpWithFlaw [] [] [["hi", "", "this is just a short message"], ["the next line is only a dot",".","and this line is dropped symmetrically"]]

{-instance MonadIO m => Interactive m where
    readI = liftIO readLn 
    outputI = liftIO . putStrLn-}