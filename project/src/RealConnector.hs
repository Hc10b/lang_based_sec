{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module RealConnector where

import Nanomsg
import Control.Monad.State
import ClientMonadClasses
--import System.System
import Control.Concurrent
import Data.Time.Clock.POSIX
import Protocol
--import qualified Data.ByteString.Lazy as BLU
import Data.String
--import Data.ByteString (unpack)
import Data.ByteString.Char8 (unpack)

s = socket Pair

endPServer = do
    sock <- s
    putStrLn "prä bind"
    bind sock "tcp://*:5560"
    putStrLn "post bind"
    return sock

endPClient = do
    sock <- s
    connect sock "tcp://localhost:5560"
    return sock

type RealConnector a cs = StateT cs (StateT (Socket a) IO)

instance Medium (RealConnector Pair cs) where
    send msg = do
        sock <- lift get
        lift $ lift $ Nanomsg.send sock $ fromString msg
        return msg
    recv = do
        sock <- lift get
        bs <- lift $ lift $ Nanomsg.recv sock
        return $ unpack bs
    maybeRecv = do
        sock <- lift get
        mbs <- lift $ lift $ recv' sock
        return $ unpack <$> mbs

instance ClientState cs (RealConnector a cs) where
    putC = put
    getC = get

instance Interactive (RealConnector a cs) where
    outputI = lift . lift . putStrLn
    readI = lift $ lift getLine
    sleep = lift . lift . threadDelay . (1000000*) --sleep for a million microseconds, or one second
    time = lift $ lift $ round `fmap` getPOSIXTime
