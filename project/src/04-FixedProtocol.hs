{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import qualified Control.Monad
import qualified Data.Functor
import Data.Functor

-- A wrapper for values known by client A. This can not be in a computation with B's values since the values must be transferred before use.
newtype AData a = AData a

-- Current state: What does it actually mean?

{-
# Ideas:
- make channel availabel in monad
-}

instance Functor AData where
    f `fmap` x = pure f <*> x

instance Applicative AData where
    pure = AData
    AData f <*> AData x = AData (f x)

-- The same for B
newtype BData b = BData b

instance Functor BData where
    f `fmap` x = pure f <*> x

instance Applicative BData where
    pure = BData
    BData f <*> BData x = BData (f x)

-- A value known to both which can be used anywhere, i.e. mixed with both AData and BData.
type Public a = a

--data NextSender = ASends | BSends

data ASends = MkASends
data BSends = MkBSends

class Sender c where

instance Sender ASends where
instance Sender BSends where

--data SendData = SendDataC NextSender *

-- The monad where everything will be happening
data Protocol a b z where
    Preshared :: z -> Protocol a b z
    SendA2B :: (Show z, Read z) => (AData a -> z) -> Protocol a b z
    SendB2A :: (Show z, Read z) => (BData b -> z) -> Protocol a b z
    BindP :: (Protocol a b z) -> (z -> Protocol a b x) -> Protocol a b x

instance Functor (Protocol a b) where
    f `fmap` mx = f <$> mx

instance Applicative (Protocol a b) where
    pure = return
    mf <*> mx = mx >>= (\x -> mf >>= (\f -> pure (f x)))

instance Monad (Protocol a b) where
    return = Preshared
    Preshared a >>= f = f a
    m >>= f = BindP m f

-- Some primitives to write programs inside ProtocolM

-- An example program written in ProtoM could look like
--example :: Program Int [Int] Int ()
example :: Protocol Int [Int] (Int, ())
example = do
  iPub <- SendA2B (\(AData i) -> i)
  xPub <- SendB2A (\(BData l) -> let x = l !! iPub in x)
  return (xPub, ())

simulateProgram :: Protocol a b z -> AData a -> BData b -> IO z
simulateProgram p ad bd = simulateProtocol p where
    simulateProtocol (Preshared a) = return a
    simulateProtocol (SendA2B fa) = let a = fa ad in putStrLn ("Sending A->B:" ++ show a) >> return a
    simulateProtocol (SendB2A fb) = let a = fb bd in putStrLn ("Sending B->A:" ++ show a) >> return a
    simulateProtocol (BindP m1 f) = do
        z <- simulateProgram m1 ad bd
        simulateProgram (f z) ad bd


-- >>> simulateProgram example (AData 1) (BData ([1,2,3]))
-- ProgressCancelledException

-- And there will also be function to decompose values of ProtocolM into the actual algorithms for A and B:

type DuplexStore = ([String], [String], IO ())

data DuplexDir = AView | BView

instance Channel DuplexDir (State DuplexStore) where
    empty _ = return ()
    send msg AView = do
        (a2bs, b2as, io) <- get
        put (a2bs++[msg], b2as, io >> putStrLn ("A sends " ++ msg))
    send msg BView = do
        (a2bs, b2as, io) <- get
        put (a2bs, b2as++[msg], io >> putStrLn ("B sends " ++ msg))
    recv AView = do
        (a2bs, b2as, io) <- get
        put (a2bs, tail b2as, io >> putStrLn ("A receives " ++ head b2as))
        return $ head b2as
    recv BView = do
        (a2bs, b2as, io) <- get
        put (tail a2bs, b2as, io >> putStrLn ("B receives " ++ head a2bs))
        return $ head a2bs

class Monad m => Channel c m where
    empty :: c -> m ()
    send :: String -> c -> m ()
    recv :: c -> m String

algoA :: Monad m => Channel ch m => m () -> Protocol a b z -> AData a -> ch -> m z
algoA me (Preshared a) _ _ = return a
algoA me (SendA2B f) ad ch = send (show (f ad)) ch >> return (f ad)
algoA me (SendB2A _) ad ch = recv ch Data.Functor.<&> read
algoA me (BindP pz f) ad ch = do
    z <- algoA me pz ad ch
    algoA me (f z) ad ch

algoB :: Monad m => Channel ch m => m () -> Protocol a b z -> BData b -> ch -> m z
algoB me (Preshared a) _ _ = return a
algoB me (SendB2A f) ad ch = send (show (f ad)) ch >> return (f ad)
algoB me (SendA2B _) ad ch = recv ch Data.Functor.<&> read
algoB me (BindP pz f) ad ch = do
    z <- algoB me pz ad ch
    algoB me (f z) ad ch

-- And you can obtain the programs for our example by doing:
clientA :: Channel ch m => AData Int -> ch -> m Int
clientA d ch = algoA (empty ch) example d ch <&> fst

clientB :: Channel ch m => BData [Int] -> ch -> m ()
clientB d ch = algoB (empty ch) example d ch <&> snd

simulateCommunication :: (DuplexDir -> State DuplexStore a) -> (DuplexDir -> State DuplexStore b) -> IO (a,b)
simulateCommunication clA clB =
    let (resA, (a2bs, _, iosA)) = runState (clA AView) ([]::[String],b2as::[String], return ()::IO ())
        (resB, (_, b2as, iosB)) = runState (clB BView) (a2bs::[String],[]::[String], return ()::IO ())
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

simulateExample = simulateCommunication (clientA (AData 1)) (clientB (BData [1,2,3]))