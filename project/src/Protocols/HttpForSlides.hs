{-# LANGUAGE FlexibleContexts #-}

module Protocols.Http where
import Protocol
--import Control.Monad
import ClientMonadClasses
import BlockingTimeout
import Control.Monad.IO.Class
import Data.Time (getCurrentTime)
import Data.List

http :: (MonadIO ma, Interactive mb) => Protocol ma mb ()
http = do
  req <- SendA2B generateRequest
  if         "GET" `isPrefixOf` req    then do
    res <- SendB2A $ handleGet req
    http
  else if    "POST" `isPrefixOf` req   then do
    res <- SendB2A $ handlePost req
    http
  else do
    LiftAC $ do
      liftIO $ putStrLn "We send an invalid request"

  where
    generateRequest :: ma String
    generateRequest = undefined

    handleGet :: String -> mb String
    handleGet = undefined

    handlePost :: String -> mb String
    handlePost = undefined
