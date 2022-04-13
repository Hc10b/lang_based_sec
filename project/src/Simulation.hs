{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Control.Monad.State
import Protocol
import ClientMonadClasses

-- ** Simulation of communication

-- * Types for simulation
type DuplexStore cs = State ([String], [String], [String], cs, IO ())
--                              ^         ^          ^     ^    ^
--                              |         |          |     |    L Final IO of this client. Not useful for interaction. Mostly for debug and output
--                              |         |          |     L   ClientState
--                              |         |          L  IO inputs
--                              |         L  Incomming messages from the other party
--                              L  Outgoing messages to the other party
--
--type DuplexStore cs = StateT cs (StateT ([String], [String]) (StateT [String] (StateT IO ())))))

instance ClientState s (DuplexStore s) where
    putC cs = do
        (txs, rxs, msgs, _, io) <- get
        put (txs, rxs, msgs, cs, io)
    getC = do
        (txs, rxs, msgs, cs, io) <- get
        return cs

instance Interactive (DuplexStore cs) where
    readI = do
        (txs, rxs, msgs, cs, io) <- get
        put (txs, txs, tail msgs, cs, io)
        return $ head msgs
    outputI msg = do
        (txs, rxs, msgs, cs, io) <- get
        put (txs, txs, msgs, cs, io >> putStrLn msg)

instance Medium (DuplexStore cs) where
    send msg = do
        (txs, rxs, msgs, cs, io) <- get
        put (txs++[msg], rxs, msgs, cs, io >> putStrLn ("A sends " ++ msg))
        return msg
    recv = do
        (txs, rxs, msgs, cs, io) <- get
        put (txs, tail rxs, msgs, cs, io >> putStrLn ("A receives " ++ head rxs))
        return $ head rxs
    maybeRecv = do
        Just <$> recv

simulateCommunication :: Protocol (DuplexStore sa) (DuplexStore sb) z -> [String] -> [String] -> sa -> sb -> IO (z,z)
simulateCommunication prot inp1 inp2 sa sb=
    let (resA, (a2bs, _, _, _, iosA)) = runState (algoA prot) ([]::[String],b2as::[String], inp1, sa, return ()::IO ())
        (resB, (b2as, _, _, _, iosB)) = runState (algoB prot) ([]::[String],a2bs::[String], inp2, sb, return ()::IO ())
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