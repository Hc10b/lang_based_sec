{-# LANGUAGE FlexibleContexts #-}

module Protocols.Http where
import Protocol
--import Control.Monad
import ClientMonadClasses
import BlockingTimeout

newtype HttpMiddleware = HttpMiddleware {respond :: String -> String}

http :: (Monad ma, Interactive ma, Monad mb, ClientState (Maybe Int) mb, Interactive mb) => HttpMiddleware -> Protocol ma mb ()
http mw = do
    mr <- timeoutB (do Just <$> readI) (return . respond mw) 60
    LiftAC $ do
      outputI $ "Exchanged" ++ show mr
    http mw
