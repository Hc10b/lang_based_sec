{-# LANGUAGE GADTs #-}
{-# LANGUAGE Safe #-}
--{-# LANGUAGE RankNTypes #-}
--{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
--{-# LANGUAGE DatatypeContexts #-}
--{-# LANGUAGE TypeFamilies #-}
--{-# LANGUAGE DataKinds #-}
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
--import qualified Control.Monad
--import qualified Data.Functor
import Data.Functor

-- The monad where everything will be happening
data Protocol a b ma mb z where
    Preshared :: z -> Protocol a b ma mb z
    SendA2B :: (Show z, Read z) => (a -> ma z) -> Protocol a b ma mb z
    SendB2A :: (Show z, Read z) => (b -> mb z) -> Protocol a b ma mb z
    BindP :: (Protocol a b ma mb z) -> (z -> Protocol a b ma mb x) -> Protocol a b ma mb x

instance Functor (Protocol a b ma mb) where
    f `fmap` mx = f <$> mx

instance Applicative (Protocol a b ma mb) where
    pure = return
    mf <*> mx = mx >>= (\x -> mf >>= (\f -> pure (f x)))

instance Monad (Protocol a b ma mb) where
    return = Preshared
    Preshared a >>= f = f a
    m >>= f = BindP m f

-- Some primitives to write programs inside ProtocolM

-- An example program written in ProtoM could look like
--example :: Program Int [Int] Int ()
example :: Medium ma => Medium mb => Protocol Int [Int] ma mb (Int, ())
example = do
  iPub <- SendA2B return
  xPub <- SendB2A (\l -> let x = l !! iPub in return x)
  return (xPub, ())


class Interactive m where
    outputI :: String -> m ()
    readI :: m String

class ClientState s m where
    putC :: s -> m ()
    getC :: m s

instance ClientState s (DuplexStore s) where
    putC cs = do
        (txs, rxs, msgs, _, io) <- get
        put (txs, rxs, msgs, cs, io)
    getC = do
        (txs, rxs, msgs, cs, io) <- get
        return cs



-- this is flawful because single-dots are recognized as end of message
type ClientS = [[String]]
smtpWithFlaw :: Medium ma => Medium mb => ClientState ClientS ma => Interactive ma => Protocol [[String]] () ma mb [[String]]
smtpWithFlaw = do
    greeting
    mails <- mail_exchange []
    quit
    return mails
    where
        greeting = do
            SendA2B (const $ return "HELO")
            SendB2A (const $ return "HELO")
            return ()
        mail_exchange collected_mails = do
            continue <- SendA2B (\msgs ->
                if length collected_mails /= length msgs
                              then do
                                   msg <- readI
                                   msg' <- getC
                                   putC ([msg]:msg')
                                   return "SEND"
                              else do
                                  outputI "end of messages detected"
                                  return ""
                    )
            if continue=="SEND" then do
                next_mail <- transfer_mail (length collected_mails) []
                mail_exchange (next_mail:collected_mails)
            else
                return $ reverse collected_mails

        transfer_mail mailN collectedLines = do
            continue <- SendA2B (\msgs ->
                if length collectedLines /= length (msgs !! mailN) then do
                    let msg = ""
                    msg' <- getC
                    putC ([msg]:msg')
                    putC msg'
                    outputI $ "Found sth in state:" ++ concat (concat msg') ++ "!"
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

-- todo
-- - interaction with enviorment
-- - lit + checking + computation

  --SendA2BLit
  -- repeat = (SendA2B msg >> repeat) <|> SendA2BLit "\n.\n"
  --(SendA2B "NextMessage" >> sMTP) <|> (SendA2B "QUIT" >> SENDB2A "QUIT")

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


-- >>> simulateProgram example (AData 1) (BData ([1,2,3]))
-- ProgressCancelledException

-- And there will also be function to decompose values of ProtocolM into the actual algorithms for A and B:

{-instance MonadIO m => Interactive m where
    readI = liftIO readLn 
    outputI = liftIO . putStrLn-}

class Monad m => Medium m where
    send :: String -> m String
    recv :: m String

type DuplexStore cs = State ([String], [String], [String], cs, IO ())
--type DuplexStore cs = StateT cs (StateT ([String], [String]) (StateT [String] (StateT IO ())))))

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

algoA :: Medium ma => Protocol a b ma mb z -> a -> ma z
algoA (Preshared a) _ = return a
algoA (SendA2B f) ad = do
    z <- f ad
    str <- send (show z)
    return $ read str
algoA (SendB2A _) ad = recv <&> read
algoA (BindP pz f) ad = do
    z <- algoA pz ad
    algoA (f z) ad

algoB :: Medium mb => Protocol a b ma mb z -> b -> mb z
algoB (Preshared a) _ = return a
algoB (SendB2A f) ad = do 
    z <- f ad
    str <- send (show z)
    return (read str)
algoB (SendA2B _) ad = recv <&> read
algoB (BindP pz f) ad = do
    z <- algoB pz ad
    algoB (f z) ad

-- And you can obtain the programs for our example by doing:
clientA d example = algoA example d

--clientB :: Channel ch m => BData [Int] -> ch -> m ()
clientB d example = algoB example d

simulateCommunication :: DuplexStore ClientS a -> DuplexStore ClientS b -> IO (a,b)
simulateCommunication clA clB =
    let (resA, (a2bs, _, _, _, iosA)) = runState clA ([]::[String],b2as::[String], ["some message", "another message","and on another leevel"], [], return ()::IO ())
        (resB, (b2as, _, _, _, iosB)) = runState clB ([]::[String],a2bs::[String], [], [[]],return ()::IO ())
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


--simulateExample = simulateCommunication (clientA 1 example) (clientB [1,2,3] example)

simulateCom prot ad bd = simulateCommunication (clientA ad prot) (clientB bd prot)
simulateSMTP msgs = simulateCom smtpWithFlaw msgs undefined

smtpExampleShowingFlaw = simulateSMTP [["hi", "", "this is just a short message"], ["the next line is only a dot",".","and this line is dropped symmetrically"]]
