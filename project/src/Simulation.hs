{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Control.Monad.State
import Protocol
import ClientMonadClasses
import qualified Control.Monad.Identity as Monad
import Control.Monad.Identity (Identity(runIdentity))


-- TODO: Implement support for timinig
-- get time
-- timed messages
-- delayed messages

-- * Types for simulation
--type DuplexStore cs = State ([String], [String], [String], cs, IO ())
--                              ^         ^          ^     ^    ^
--                              |         |          |     |    L Final IO of this client. Not useful for interaction. Mostly for debug and output
--                              |         |          |     L   ClientState
--                              |         |          L  IO inputs
--                              |         L  Incomming messages from the other party
--                              L  Outgoing messages to the other party
--
type DuplexStore cs = StateT cs (StateT ([String], [String]) (StateT [String] (StateT (IO ()) Monad.Identity)))

instance ClientState s (DuplexStore s) where
    putC cs = do
        put cs
        --(txs, rxs, msgs, _, io) <- get
        --put (txs, rxs, msgs, cs, io)
    getC = do
        get
        --(txs, rxs, msgs, cs, io) <- get
        --return cs

instance Interactive (DuplexStore cs) where
    readI = do
        msgs <- lift $ lift get
        lift $ lift $ put (tail msgs)
        return $ head msgs
    outputI msg = do
        io <- lift $ lift $ lift get
        lift $ lift $ lift $ put (io >> putStrLn msg)

instance Medium (DuplexStore cs) where
    send msg = do
        (txs, rxs) <- lift get
        lift $ put (txs++[msg], rxs)
        outputI $ "A sends " ++ msg
        return msg
    recv = do
        (txs, rxs) <- lift get
        lift $ put (txs, tail rxs)
        outputI $ "A receives " ++ head rxs
        return $ head rxs
    maybeRecv = do
        Just <$> recv

simulateCommunication :: Protocol (DuplexStore sa) (DuplexStore sb) z -> [String] -> [String] -> sa -> sb -> IO (z,z)
simulateCommunication prot inpA inpB sa sb=
    let b1 = evalStateT (algoB prot) sb
        b2 = runStateT b1 ([], a2bs)
        b3 = evalStateT b2 inpB
        b4 = runStateT b3 (return ())
        ((resB, (b2as, _)), iosB) = runIdentity b4
        a1 = evalStateT (algoA prot) sa
        a2 = runStateT a1 ([], b2as)
        a3 = evalStateT a2 inpA
        a4 = runStateT a3 (return ())
        ((resA, (a2bs, _)), iosA) = runIdentity a4
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