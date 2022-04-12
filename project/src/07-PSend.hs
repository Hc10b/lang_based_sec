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
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
--import qualified Control.Monad
--import qualified Data.Functor
import Data.Functor

-- * Classes
class Interactive m where
    outputI :: String -> m ()
    readI :: m String

class ClientState s m where
    putC :: s -> m ()
    getC :: m s

class Monad m => Medium m where
    send :: String -> m String
    recv :: m String

-- * Protocol definition

-- The monad where everything will be happening
data Protocol ma mb z where
    Preshared :: z -> Protocol ma mb z
    BindP :: Protocol ma mb z -> (z -> Protocol ma mb x) -> Protocol ma mb x
    -- simple message passing
    SendA2B :: (Medium ma, Medium mb, Show z, Read z) => ma z -> Protocol ma mb z
    --                                                      L send result
    SendB2A :: (Medium ma, Medium mb, Show z, Read z) => mb z -> Protocol ma mb z
    -- inject data into monads
    LiftA :: (y -> ma ()) -> Protocol ma mb z
    --        L push public data into client monad
    LiftB :: (y -> mb ()) -> Protocol ma mb z
    -- possible sychronized concurrent messages of both sides
    PSend :: (Medium ma, Medium mb, Show x, Show y) => ma (Maybe x) -> mb (Maybe y) -> (y -> Maybe (ma x)) -> (x -> Maybe (mb y)) -> Protocol ma mb (x, y)
    --                                                  ^               ^                      ^                        ^
    --                                                  |               |                      |                        L PartyB's response to PartA's message if PartyB hasn't send by now. May refuse to answer.
    --                                                  |               |                      L PartyA's response to PartB's message if PartyA hasn't send by now. May refuse to answer.
        --                                              |               L For asking whether (Maybe) and what (y) to send for partB.
        --                                              L For asking whether (Maybe) and what (y) to send for partA.
    -- -- possible async concurrent messages of both side with possibility to return resync
    -- -- e.g. for live multiplayer game
    -- Async :: (Bool -> ma (Maybe ca)) -> (Bool -> mb (Maybe cb)) -> (ca -> mb ()) -> (cb -> ma ()) -> (ca -> Bool) -> (cb -> Bool) -> Protocol ma mb (ca, cb)

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


-- * Example protocols

-- An example program written in ProtoM could look like
--example :: Program Int [Int] Int ()
example :: Medium ma => Medium mb => ClientState Int ma => ClientState [Int] mb => Protocol ma mb (Int, ())
example = do
  iPub <- SendA2B $ do getC
  xPub <- SendB2A $ do
      ints <- getC
      return $ ints !! iPub
  return (xPub, ())

-- this is flawful because single-dots are recognized as end of message
type SmtpClientS = [[String]]
smtpWithFlaw :: Medium ma => Medium mb => ClientState SmtpClientS ma => Interactive ma => Protocol ma mb [[String]]
smtpWithFlaw = do
    greeting
    mails <- mail_exchange []
    quit
    return mails
    where
        greeting = do
            SendA2B (return "HELO")
            SendB2A (return "HELO")
            return ()
        mail_exchange collected_mails = do
            continue <- SendA2B $ do
                msgs <- getC
                let x = ["test"]:msgs -- type hint
                if length collected_mails /= length msgs
                              then do
                                   return "SEND"
                              else do
                                  return ""
            if continue=="SEND" then do
                next_mail <- transfer_mail (length collected_mails) []
                mail_exchange (next_mail:collected_mails)
            else
                return $ reverse collected_mails

        transfer_mail mailN collectedLines = do
            continue <- SendA2B (do
                msgs <- getC
                if length collectedLines /= length (msgs !! mailN) then 
                    return $ (msgs !! mailN) !! length collectedLines
                else
                    return "."
                )
            if continue == "." then
                return $ reverse collectedLines
            else do
                --nextLine <- SendA2B (\(AData msgs) -> (msgs !! mailN) !! length collectedLines)
                transfer_mail mailN (continue:collectedLines)

        quit = do
            return ()

-- https://sv.wikipedia.org/wiki/Simple_Mail_Transfer_Protocol
-- → dot stuffing problem
-- “You can't rely on all smtp servers doing the dot removal correctly.”
-- — https://stackoverflow.com/questions/15224224/smtp-dot-stuffing-when-and-where-to-do-it


-- * Simulation
-- ** Direct simulation
{-
simulateProgram :: Protocol a b IO z -> a -> b -> IO z
simulateProgram p ad bd = simulateProtocol p where
    simulateProtocol (Preshared a) = return a
    simulateProtocol (SendA2B fa) = let a = fa ad in putStrLn ("Sending A->B:" ++ show a) >> return a
    simulateProtocol (SendB2A fb) = let a = fb bd in putStrLn ("Sending B->A:" ++ show a) >> return a
    simulateProtocol (BindP m1 f) = do
        z <- simulateProgram m1 ad bd
        simulateProgram (f z) ad bd
        -}

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

-- And there will also be function to decompose values of ProtocolM into the actual algorithms for A and B:
algoA :: Monad ma => Protocol ma mb z -> ma z
algoA (Preshared a) = return a
{-algoA (SendA2B f) ad = do
    z <- f ad
    str <- send (show z)
    return $ read str-}
algoA (SendB2A _) = recv <&> read
algoA (BindP pz f) = do
    z <- algoA pz
    algoA (f z)

flipProt :: Protocol ma mb z -> Protocol mb ma z
flipProt (Preshared a) = Preshared a

algoB :: Monad mb => Protocol ma mb z -> mb z
algoB p = algoA $ flipProt p

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


smtpExampleShowingFlaw = simulateCommunication smtpWithFlaw [] [] [["hi", "", "this is just a short message"], ["the next line is only a dot",".","and this line is dropped symmetrically"]]

{-instance MonadIO m => Interactive m where
    readI = liftIO readLn 
    outputI = liftIO . putStrLn-}