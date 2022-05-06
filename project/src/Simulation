{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Simulation where

import Control.Monad.State
import Protocol
import ClientMonadClasses
import qualified Control.Monad.Identity as Monad
import Control.Monad.Identity (Identity(runIdentity))


-- * Types for simulation
data Input = Input {timeI:: Int, payloadI::String}
data Message = Message {timeM::Int, payloadM::String}
  deriving Show

--type DuplexStore cs = State ([String], [String], [String], cs, IO ())
type DuplexStore cs = StateT cs (StateT ([Message], [Message]) (StateT [Input] (StateT (IO ()) (StateT Int Monad.Identity))))
--                            ^              ^          ^                  ^              ^             ^
--                            |              |          |                  |              |             L current time at the client
--                            |              |          |                  |              L Final IO of this client. Not useful for interaction. Mostly for debug and output
--                            |              |          |                  L  IO inputs
--                            |              |          L Incomming messages from the other party
--                            |              L  Outgoing messages to the other party
--                            L   ClientState

getTime :: DuplexStore cs Int
getTime = lift $ lift $ lift $ lift get

spendTime :: Int -> DuplexStore cs ()
spendTime time = do
    curTime <- getTime
    lift $ lift $ lift $ lift $ put $ curTime + abs time

instance ClientState s (DuplexStore s) where
    putC = put
    getC = get

instance Interactive (DuplexStore cs) where
    readI = do
        msgs <- lift $ lift get
        lift $ lift $ put (tail msgs)
        let Input mTime payload = head msgs
        curTime <- getTime
        Monad.when (mTime > curTime) $
            spendTime (mTime - curTime)
        return payload
    outputI msg = do
        io <- lift $ lift $ lift get
        curTime <- getTime
        lift $ lift $ lift $ put (io >> putStrLn (show curTime ++ ": " ++ msg))
        return ()
    time = do
        spendTime 1
        getTime
    sleep time = spendTime $ abs time

instance Medium (DuplexStore cs) where
    send msg = do
        (txs, rxs) <- lift get
        spendTime 1
        curTime <- getTime
        lift $ put (txs++[Message curTime msg], rxs)
        outputI $ "sends " ++ msg
        return msg
    recv = do
        (txs, rxs) <- lift get
        lift $ put (txs, tail rxs)
        let Message mTime payload = head rxs
        curTime <- getTime
        let tts = max 0 (mTime-curTime)
        spendTime tts
        outputI $ "receives " ++ payload
        {- -- Why isn't this working?
        _ <- do if (mTime > curTime) then
                   return ()
                else
                   return ()
        
        --Monad.when (mTime > curTime) $
           --spendTime (mTime - curTime)
        -}
        return payload
    maybeRecv = do
        curTime <- getTime
        spendTime 1
        (txs, rxs) <- lift get
        let Message mTime payload = head rxs -- <- hier liegt das Problem
        if mTime <= curTime then
            Just <$> recv
        else 
            return Nothing

delayMessages :: [Message] -> (Message -> Int) -> [Message]
delayMessages msgs delay = map (\m-> m{timeM=timeM m + abs (delay m)}) msgs

simulateCommunication :: Protocol (DuplexStore sa) (DuplexStore sb) z -> [Input] -> [Input] -> sa -> sb -> (Message -> Int) -> (Message -> Int) -> IO (z,z)
simulateCommunication prot inpA inpB sa sb delayAB delayBA=
    let b1 = evalStateT (algoB prot) sb
        --b2 = runStateT b1 ([], delayMessages a2bs delayAB)
        b2 = runStateT b1 ([], delayMessages a2bs delayAB)
        b3 = evalStateT b2 inpB
        b4 = runStateT b3 (return ())
        b5 = evalStateT b4 0
        ((resB, (b2as, _)), iosB) = runIdentity b5
        a1 = evalStateT (algoA prot) sa
        a2 = runStateT a1 ([], delayMessages b2as delayBA)
        --a2 = trace "a2" $ runStateT a1 ([], b2as)
        a3 = evalStateT a2 inpA
        a4 = runStateT a3 (return ())
        a5 = evalStateT a4 0
        ((resA, (a2bs, _)), iosA) = runIdentity a5
    in do
        putStrLn "Messages A→B:"
        print a2bs
        putStrLn "Messages B→A:"
        print b2as
        putStrLn "IO A:"
        iosA
        putStrLn "IO B:"
        iosB
        return (resA, resB)