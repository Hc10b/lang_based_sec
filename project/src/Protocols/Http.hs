{-# LANGUAGE FlexibleContexts #-}

module Protocols.Http where
import Protocol
--import Control.Monad
import ClientMonadClasses

newtype HttpMiddleware = HttpMiddleware {respond :: String -> String}

http :: (Medium ma, Interactive ma, Medium mb, ClientState (Maybe Int) mb, Interactive mb) => HttpMiddleware -> Protocol ma mb ()
http mw = do
    mr <- timeoutB (do Just <$> readI) (return . respond mw) 60
    case mr of
      Nothing -> return ()
      Just x0 -> http mw
