{-# LANGUAGE GADTs #-}
module IoImpl where

import ClientMonadClasses
import Protocol
import Data.Functor
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent
import Control.Monad.Catch
import Control.Monad.Trans.Class
import Control.Monad.State
import Data.ByteString.Char8 (unpack)
import RealConnector
import qualified Nanomsg
import Nanomsg (Pair)
import Control.Concurrent (tryTakeMVar)

data GotMessageException = GotMessage
    deriving Show

instance Exception GotMessageException where

-- And there will also be function to decompose values of ProtocolM into the actual algorithms for A and B:
algoA :: MonadIO ma => MonadCatch ma => Medium ma => Protocol ma mb z -> ma z
algoA (Preshared a) = return a
algoA (SendA2B f) = do
    z <- f
    str <- send $ show z
    return $ read str
algoA (SendB2A _) = recv <&> read
algoA (BindP pz f) = do
    z <- algoA pz
    algoA (f z)
algoA (LiftAC ma) = do ma
algoA (LiftBC mb) = return ()
algoA (CSend la lb ra rb) = do
    sendWaitThreadId <- liftIO myThreadId
    syncVar <- liftIO $ newMVar ()
    incomingMessage <- liftIO newEmptyMVar
    recv <- generateRecv
    remoteMessageWaitThread <- liftIO $ forkIO $ waitForIncomingMessage sendWaitThreadId syncVar incomingMessage recv

    catch (do
        x <- waitForOutgoingMessage
        liftIO $ takeMVar syncVar
        let strA = show x
        send strA
        let readA = read strA
        case rb readA of
          Nothing -> do
              liftIO $ killThread remoteMessageWaitThread
              return (Just readA, Nothing)
          Just mby -> do
              strY <- liftIO $ readMVar incomingMessage
              return (Just readA, Just $ read strY)
        )
        (\GotMessage -> do
            strB <- liftIO $ readMVar incomingMessage
            let y = read strB
            case ra y of
                            Nothing -> return (Nothing, Just y)
                            Just max -> do
                                x <- max
                                let strX = show x
                                let readX = read strX
                                send strX
                                return (Just readX, Just y))


    where
    waitForIncomingMessage mainThreadId syncVar resultVar recv = do
        strB <- recv
        m <- tryTakeMVar syncVar
        putMVar resultVar strB
        case m of
          Nothing -> return ()
          Just a -> throwTo mainThreadId GotMessage
    waitForOutgoingMessage = do
        maybeX <- la
        maybe waitForOutgoingMessage return maybeX

algoA (Async txa txb rxa rxb sa sb) = do
    recvMa <- liftIO newEmptyMVar
    syncB <- liftIO newEmptyMVar
    recv <- generateRecv
    liftIO $ forkIO $ receiveHandler recvMa syncB recv
    sendHandler recvMa syncB
    --receiveInThread
    where
        receiveHandler recvMa syncBVar recv= do
            strB <- recv
            let b = read strB
            ma <- takeMVar recvMa
            putMVar recvMa $ ma >> rxb b
            if sb b then
                putMVar syncBVar b
            else
                receiveHandler recvMa syncBVar recv
        sendHandler recvMa syncBVar= do
            ma <- txa
            join $ liftIO $ takeMVar recvMa
            liftIO $ putMVar recvMa $ return ()
            case ma of
                Nothing -> sendHandler recvMa syncBVar
                Just a -> do
                    let strA = show a
                    send strA
                    let readA = read strA
                    if sa readA then do
                        syncB <- liftIO $ takeMVar syncBVar
                        return (readA, syncB)
                    else
                        sendHandler recvMa syncBVar


algoB :: Protocol ma (RealConnector Pair cs) z -> (RealConnector Pair cs) z
algoB p = algoA $ flipProt p
