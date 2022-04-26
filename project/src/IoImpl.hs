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

data GotMessageException = GotMessage
    deriving Show

instance Exception GotMessageException where

-- And there will also be function to decompose values of ProtocolM into the actual algorithms for A and B:
algoA :: Protocol (RealConnector Pair cs) mb z -> RealConnector Pair cs z
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
    sock <- lift get -- :: Nanomsg.Socket Nanomsg.Pair
    let recv = (do
            bs <- Nanomsg.recv (sock::Nanomsg.Socket Nanomsg.Pair)
            return $ unpack bs)
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

    {-}
    --TODO Multithreading
    maybeStrB <- maybeRecv
    case maybeStrB of
        Nothing -> do
            maybeX <- la
            case maybeX of
                Nothing -> algoA (CSend la lb ra rb)
                Just x -> do
                    strA <- send (show x)
                    -- check, whether B will answer
                    case rb $ read strA of
                        Nothing -> return (Just $ read strA, Nothing)
                        Just _ -> do
                            strB <- recv
                            return (Just $ read strA, Just $ read strB)
        Just strB -> case ra (read strB) of
            Nothing -> return (Nothing, Just $ read strB)
            Just mx -> do
                x <- mx
                strA <- send (show x)
                return (Just $ read strA, Just $ read strB)
                -}
algoA (Async txa txb rxa rxb sa sb) = undefined
    {-loopT
-- TODO multi-threading
    where
        loopR = do
                mb <- maybeRecv
                case mb of
                    Nothing -> loopT
                    Just strb ->
                        do
                            let cb = read strb
                            rxb cb
                            if sb cb then
                                wait_for_this cb
                            else
                                loopT
        loopT = do
            ma <- txa
            case ma of
              Nothing -> loopR
              Just ca ->
                  do
                      str <- send $ show ca
                      if sa $ read str then
                          wait_for_other (read str)
                      else
                          loopR
        wait_for_other this_sync = do
            mb <- maybeRecv
            case mb of
              Nothing -> wait_for_other this_sync
              Just strb ->
                  do
                      let cb = read strb
                      rxb cb
                      if sb cb then
                          return (this_sync, cb)
                      else wait_for_other this_sync
        wait_for_this other_sync = do
            ma <- txa
            case ma of
              Just ca ->
                  do
                      str <- send $ show ca
                      if sa $ read str then
                          return (read str, other_sync)
                      else
                          wait_for_this other_sync
              Nothing -> wait_for_this other_sync
              -}





algoB :: Protocol ma (RealConnector Pair cs) z -> (RealConnector Pair cs) z
algoB p = algoA $ flipProt p
