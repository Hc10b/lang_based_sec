{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader

-- A wrapper for values known by client A. This can not be in a computation with B's values since the values must be transferred before use.
newtype AData a = AData a

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

-- The monad where everything will be happening
data Protocol a where
    BindP :: (Protocol b) -> (b -> Protocol a) -> Protocol a
    PureP :: a -> Protocol a
    SendA2B :: (Show a, Read a) => AData a -> Protocol a
    SendB2A :: (Show a, Read a) => BData a -> Protocol a

instance Functor Protocol where
    f `fmap` mx = f <$> mx

instance Applicative Protocol where
    pure = return
    mf <*> mx = BindP mx (\x -> BindP mf (\f -> pure (f x)))

instance Monad Protocol where
    return = PureP
    ma >>= f = BindP ma f

-- Some primitives to write programs inside ProtocolM

type Program a b c d
    =  AData a -- A's input
    -> BData b -- B's input
    -> Protocol (c, d) -- the action producing A and B's outputs

-- An example program written in ProtoM could look like
example :: Program Int [Int] Int ()
example i l = do
  iPub <- SendA2B i
  let x = (!! iPub) <$> l
  xPub <- SendB2A x
  return (xPub, ())

simulateProgram p ad bd = simulateProtocol (p ad bd) where
    simulateProtocol :: Protocol a -> IO a
    simulateProtocol (PureP a) = return a
    simulateProtocol (BindP ma f) = simulateProtocol ma >>= (simulateProtocol . f)
    simulateProtocol (SendA2B (AData a)) = putStrLn ("Sending A->B:" ++ show a) >> return a
    simulateProtocol (SendB2A (BData a)) = putStrLn ("Sending B->A:" ++ show a) >> return a

-- >>> simulateProgram example (AData 1) (BData ([1,2,3]))
-- ProgressCancelledException

-- And there will also be function to decompose values of ProtocolM into the actual algorithms for A and B:

type DuplexStore = ([String], [String])

data DuplexDir = AView | BView

data DuplexChannel = DuplexChannel DuplexDir DuplexStore

data SenderT m a = SenderT (m a)
data RecvT m a = RecvT (m a)

instance Monad m => Functor (SenderT m) where
    f `fmap` (SenderT a) = SenderT $ f `fmap` a

instance Channel DuplexDir (ReaderT String (WriterT String IO)) where
    empty _ = return ()
    send msg AView = do
        (a2bs, b2as, _) <- get
        put (a2bs++[msg], b2as, return ())
    send msg BView = do
        (a2bs, b2as, _) <- get
        put (a2bs, b2as++[msg], return ())
    recv AView = do
        (a2bs, b2as, _) <- get
        put (a2bs, tail b2as, return ())
        return $ head b2as
    recv BView = do
        (a2bs, b2as, _) <- get
        put (tail a2bs, b2as, return ())
        return $ head a2bs

class Monad m => Channel c m where
    empty :: c -> m ()
    send :: String -> c -> m ()
    recv :: c -> m String

algoA :: Monad m => Channel ch m => m () -> Program a b c d -> AData a -> ch -> m c
algoA me p ad ch = handleProtocol ch me (p ad undefined) >>= (return.fst) where
    handleProtocol :: Monad m => Channel ch m => ch -> m () -> Protocol a -> m a
    handleProtocol ch me (PureP a) = return a
    handleProtocol ch me (BindP mb f) = handleProtocol ch me mb >>= (\b -> handleProtocol ch me (f b))
    handleProtocol ch me (SendA2B (AData a)) = send (show a) ch >> return a
    handleProtocol ch me (SendB2A _) = recv ch >>= (return.read)

algoB :: forall m a b c d ch. Monad m => Channel ch m => m () -> Program a b c d -> BData b -> ch -> m d
algoB me p bd ch = handleProtocol ch me (p undefined bd) >>= (return.snd) where
    handleProtocol :: Monad m => Channel ch m => ch -> m () -> Protocol a -> m a
    handleProtocol ch me (PureP a) = return a
    handleProtocol ch me (BindP mb f) = handleProtocol ch me mb >>= (\b -> handleProtocol ch me (f b))
    handleProtocol ch me (SendB2A (BData a)) = send (show a) ch >> return a
    handleProtocol ch me (SendA2B _) = recv ch >>= (return.read)

-- And you can obtain the programs for our example by doing:
clientA :: Channel ch m => AData Int -> ch -> m Int
clientA d ch = algoA (empty ch) example d ch

clientB :: Channel ch m => BData [Int] -> ch -> m ()
clientB d ch = algoB (empty ch) example d ch

--simulateCommunication :: (DuplexChannel -> IO a) -> (DuplexChannel -> IO b) -> IO (a,b)
simulateCommunication clA clB = runStateT prog ([],[]) where
    prog = do
        a <- liftIO $ clA AView
        b <- liftIO $ clB BView
        return (a, b)

