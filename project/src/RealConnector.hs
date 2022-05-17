{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module RealConnector where

import Nanomsg
import Control.Monad.State
import ClientMonadClasses
import Control.Concurrent
import Data.Time.Clock.POSIX
import Data.String
import Data.ByteString.Char8 (unpack)
import Medium

s = socket Pair

endPServer = do
    sock <- s
    putStrLn "pre bind"
    bind sock "tcp://*:5560"
    putStrLn "post bind"
    return sock

endPClient = do
    sock <- s
    connect sock "tcp://localhost:5560"
    return sock

type RealConnector a cs = StateT cs (StateT (Socket a) IO)

instance MonadIO m => Medium m (Socket Pair) where
    send sock msg = do
        liftIO $ Nanomsg.send sock $ fromString msg
        return msg
    recv sock = do
        bs <- liftIO $ Nanomsg.recv sock
        return $ unpack bs
    generateRecv sock = do
        return (do
                bs <- Nanomsg.recv (sock::Nanomsg.Socket Nanomsg.Pair)
                return $ unpack bs)
    maybeRecv sock = do
        mbs <- liftIO $ recv' sock
        return $ unpack <$> mbs

instance ClientState cs (RealConnector a cs) where
    putC = put
    getC = get

instance Interactive (RealConnector a cs) where
    outputI = liftIO . putStrLn
    readI = liftIO getLine
    sleep = liftIO . threadDelay . (1000000*) --sleep for a million microseconds, or one second
    time = liftIO $ round `fmap` getPOSIXTime
