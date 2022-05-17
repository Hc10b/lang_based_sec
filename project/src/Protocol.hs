{-# LANGUAGE GADTs #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Protocol where

import qualified Control.Monad

-- * Protocol definition

-- The monad where everything will be happening
data Protocol ma mb z where
    Preshared :: z -> Protocol ma mb z
    BindP :: Protocol ma mb z -> (z -> Protocol ma mb x) -> Protocol ma mb x
    -- simple message passing
    SendA2B :: (Show z, Read z) => ma z -> Protocol ma mb z
    SendB2A :: (Show z, Read z) => mb z -> Protocol ma mb z
    -- inject data into monads
    LiftAC :: ma () -> Protocol ma mb ()
    --        L push public data into client monad
    LiftBC :: mb () -> Protocol ma mb ()
    -- possible sychronized concurrent messages of both sides
    CSend :: (Show x, Read x, Show y, Read y) => ma (Maybe x) -> mb (Maybe y) -> (y -> Maybe (ma x)) -> (x -> Maybe (mb y)) -> Protocol ma mb (Maybe x, Maybe y)
    --                                             ^               ^                      ^                        ^
    --                                             |               |                      |                        L PartyB's response to PartA's message if PartyB hasn't send by now. May refuse to answer.
    --                                             |               |                      L PartyA's response to PartB's message if PartyA hasn't send by now. May refuse to answer.
    --                                             |               L For asking whether (Maybe) and what (y) to send for partB.
    --                                             L For asking whether (Maybe) and what (y) to send for partA.
    --
    Async :: (Show ca, Read ca, Show cb, Read cb) => ma (Maybe ca) -> mb (Maybe cb) -> (ca -> mb ()) -> (cb -> ma ()) -> (ca -> Bool) -> (cb -> Bool) -> Protocol ma mb (ca, cb)
    --                                                      ^                 ^                      ^                ^                 ^                ^
    --                                                      |                 |                      |                |                 |                L Whether B's message is a sync request
    --                                                      |                 |                      |                |                 L Whether A's message is a sync request
    --                                                      |                 |                      |                L Processing a message from B on A's side
    --                                                      |                 |                      L Processing a message from A on B's side
    --                                                      |                 L Given whether party A want to sync, whether and what B want's to send.
    --                                                      L Given whether party B want to sync, whether and what A want's to send.
instance Functor (Protocol ma mb) where
    f `fmap` mx = f <$> mx

instance Applicative (Protocol ma mb) where
    pure = return
    mf <*> mx = mx >>= (\x -> mf >>= (\f -> pure (f x)))

instance Monad (Protocol ma mb) where
    return = Preshared
    Preshared a >>= f = f a
    m >>= f = BindP m f


flipProt :: Protocol ma mb z -> Protocol mb ma z
flipProt (Preshared a) = Preshared a
flipProt (SendA2B f) = SendB2A f
flipProt (SendB2A f) = SendA2B f
flipProt (LiftAC m) = LiftBC m
flipProt (LiftBC m) = LiftAC m
flipProt (CSend sa sb ra rb) = do
    res <- CSend sb sa rb ra
    return (snd res, fst res)
flipProt (Async txa txb rxa rxb sa sb) = do
    (ca,cb) <- Async txb txa rxb rxa sb sa
    return (cb, ca)
flipProt (BindP ma f) = do
    a <- flipProt ma
    flipProt $ f a
