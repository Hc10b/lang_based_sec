{-# LANGUAGE GADTs #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
--{-# LANGUAGE RankNTypes #-}
--{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE UndecidableInstances #-}
--{-# LANGUAGE DatatypeContexts #-}
--{-# LANGUAGE TypeFamilies #-}
--{-# LANGUAGE DataKinds #-}

module Protocol where

import Control.Monad.State
--import Control.Monad.Writer
--import Control.Monad.Reader
--import qualified Control.Monad
--import qualified Data.Functor
import Data.Functor
import ClientMonadClasses

class Monad m => Medium m where
    send :: String -> m String
    recv :: m String
    maybeRecv :: m (Maybe String)

-- * Protocol definition

-- The monad where everything will be happening
data Protocol ma mb z where
    Preshared :: z -> Protocol ma mb z
    -- TODO: get rid of BindP?
    BindP :: Protocol ma mb z -> (z -> Protocol ma mb x) -> Protocol ma mb x
    -- simple message passing
    SendA2B :: (Medium ma, Medium mb, Show z, Read z) => ma z -> Protocol ma mb z
    SendB2A :: (Medium ma, Medium mb, Show z, Read z) => mb z -> Protocol ma mb z
    -- inject data into monads
    LiftA :: ma () -> Protocol ma mb ()
    --        L push public data into client monad
    LiftB :: mb () -> Protocol ma mb ()
    -- possible sychronized concurrent messages of both sides
    CSend :: (Medium ma, Medium mb, Show x, Read x, Show y, Read y) => ma (Maybe x) -> mb (Maybe y) -> (y -> Maybe (ma x)) -> (x -> Maybe (mb y)) -> Protocol ma mb (Maybe x, Maybe y)
    --                                                                     ^               ^                      ^                        ^
    --                                                                     |               |                      |                        L PartyB's response to PartA's message if PartyB hasn't send by now. May refuse to answer.
    --                                                                     |               |                      L PartyA's response to PartB's message if PartyA hasn't send by now. May refuse to answer.
    --                                                                     |               L For asking whether (Maybe) and what (y) to send for partB.
    --                                                                     L For asking whether (Maybe) and what (y) to send for partA.
    --
    -- -- possible async concurrent messages of both side with possibility to resync
    -- -- e.g. for live multiplayer game
    -- Async :: (Bool -> ma (Maybe ca)) -> (Bool -> mb (Maybe cb)) -> (ca -> mb ()) -> (cb -> ma ()) -> (ca -> Bool) -> (cb -> Bool) -> Protocol ma mb (ca, cb)
    -- --                ^                           ^                      ^                ^                 ^                ^
    -- --                |                           |                      |                |                 |                L Whether B's message is a sync request
    -- --                |                           |                      |                |                 L Whether A's message is a sync request
    -- --                |                           |                      |                L Processing a message from B on A's side
    -- --                |                           |                      L Processing a message from A on B's side
    -- --                |                           L Given whether party A want to sync, whether and what B want's to send.
    -- --                L Given whether party B want to sync, whether and what A want's to send.

timeoutB :: Medium ma => Medium mb => Interactive mb => ClientState (Maybe Int) mb => Show x => Read x => Show y => Read y => ma (Maybe x) -> (x -> mb y) -> Int -> Protocol ma mb (Maybe (x,y))
timeoutB client response timeout =  do
    let serverResponse x = Just $ do
        putC (Nothing::Maybe Int)
        y <- response x
        return $ Just y
    let serverCheck = do
        curTime <- time
        mTime <- getC
        case mTime of
            Nothing -> do
                putC $ Just curTime
                return Nothing
            Just startTime ->
                if curTime - startTime > timeout then
                    return $ Just Nothing
                else
                    return Nothing

    (mx, mmy) <- CSend client serverCheck (const Nothing) serverResponse
    case mmy of
        Nothing -> undefined -- can't happen
        Just my  -> case my of
            Nothing -> return Nothing -- Timeout occured
            Just y -> case mx of
                Nothing ->  undefined -- can't occur
                Just x ->  return $ Just (x,y) -- Server responded

instance Functor (Protocol ma mb) where
    f `fmap` mx = f <$> mx

instance Applicative (Protocol ma mb) where
    pure = return
    mf <*> mx = mx >>= (\x -> mf >>= (\f -> pure (f x)))

instance Monad (Protocol ma mb) where
    return = Preshared
    Preshared a >>= f = f a
    m >>= f = BindP m f


-- todo
-- - lit + checking + computation

  --SendA2BLit

-- And there will also be function to decompose values of ProtocolM into the actual algorithms for A and B:
algoA :: Monad ma => Protocol ma mb z -> ma z
algoA (Preshared a) = return a
algoA (SendA2B f) = do
    z <- f
    str <- send (show z)
    return $ read str
algoA (SendB2A _) = recv <&> read
algoA (BindP pz f) = do
    z <- algoA pz
    algoA (f z)
algoA (LiftA ma) = do ma
algoA (LiftB mb) = return ()
algoA (CSend la lb ra rb) = do
    maybeStrB <- maybeRecv
    case maybeStrB of
        Nothing -> do
            maybeX <- la
            case maybeX of
                Nothing -> algoA (CSend la lb ra rb)
                Just x -> do
                    strA <- send (show x)
                    -- check, whether B will answer
                    case ra $ read strA of
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



flipProt :: Protocol ma mb z -> Protocol mb ma z
flipProt (Preshared a) = Preshared a
flipProt (SendA2B f) = SendB2A f
flipProt (SendB2A f) = SendA2B f
flipProt (LiftA m) = LiftB m
flipProt (LiftB m) = LiftA m
flipProt p@(CSend sa sb ra rb) = do
    res <- CSend sb sa rb ra
    return (snd res, fst res)
flipProt (BindP ma f) = do
    a <- flipProt ma
    flipProt $ f a

algoB :: Monad mb => Protocol ma mb z -> mb z
algoB p = algoA $ flipProt p
