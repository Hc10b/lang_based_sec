{-# LANGUAGE FlexibleContexts #-}

module Protocols.Http where
import Protocol ( Protocol (LiftAC), Medium, timeoutB )
--import Control.Monad
import ClientMonadClasses

newtype HttpMiddleware = HttpMiddleware {respond :: Request -> String}

data RequestType = POST | GET

data Request = MkRequest {typ::RequestType, path::String, headers::[String]}


http :: (Medium ma, Interactive ma, Medium mb, ClientState (Maybe Int) mb, Interactive mb) => HttpMiddleware -> Protocol ma mb ()
http mw = do
    mr <- timeoutB (do Just <$> readI) (return . respond mw) 60
    case mr of
      Nothing -> return ()
      Just x0 -> do
        LiftAC (outputI "Answer from Server:" >> outputI (show x0))
        http mw
