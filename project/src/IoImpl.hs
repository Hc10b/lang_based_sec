{-# LANGUAGE GADTs #-}
module IoImpl where

import Protocol
import Data.Functor
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent
import Control.Monad.Catch
import Control.Monad.State
import Data.ByteString.Char8 (unpack)
import qualified Nanomsg
import Nanomsg (Pair)
import Medium

data GotMessageException = GotMessage
    deriving Show

instance Exception GotMessageException where

algoA :: MonadIO ma => MonadCatch ma => Medium ma m => m -> Protocol ma mb z -> ma z
algoA m (Preshared a) = return a
algoA m (SendA2B f) = do
    z <- f
    str <- send m $ show z
    return $ read str
algoA m (SendB2A _) = recv m <&> read
algoA m (BindP pz f) = do
    z <- algoA m pz
    algoA m (f z)
algoA m (LiftAC ma) = do ma
algoA m (LiftBC mb) = return ()
algoA m (CSend la lb ra rb) = do
    sendWaitThreadId <- liftIO myThreadId
    syncVar <- liftIO $ newMVar ()
    incomingMessage <- liftIO newEmptyMVar
    recv <- generateRecv m
    remoteMessageWaitThread <- liftIO $ forkIO $ waitForIncomingMessage sendWaitThreadId syncVar incomingMessage recv

    catch (do
        x <- waitForOutgoingMessage
        liftIO $ takeMVar syncVar
        let strA = show x
        send m strA
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
                                send m strX
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

algoA m (Async txa txb rxa rxb sa sb) = do
    recvMa <- liftIO $ newMVar $ return ()
    syncB <- liftIO newEmptyMVar
    recv <- generateRecv m
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
                    send m strA
                    let readA = read strA
                    if sa readA then do
                        syncB <- liftIO $ takeMVar syncBVar
                        return (readA, syncB)
                    else
                        sendHandler recvMa syncBVar


algoB :: (MonadIO mb, MonadCatch mb, Medium mb m) => m -> Protocol ma mb z -> mb z
algoB m p = algoA m $ flipProt p
