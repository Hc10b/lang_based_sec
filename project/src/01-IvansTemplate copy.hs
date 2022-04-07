-- A wrapper for values known by client A. This can not be in a computation with B's values since the values must be transferred before use.
data AData a = AData a
  deriving Applicative

-- The same for B
data BData b = BData b
  deriving Applicative

-- A value known to both which can be used anywhere, i.e. mixed with both AData and BData.
type Public a = a

-- These applicative instances are for local computations that one client is doing with its data. For example, A can add values x :: AData Int and y :: AData Int by doing (+) <*> x <$> y.
--instance Applicative AData …
--instance Applicative BData …

-- The monad where everything will be happening
data ProtocolM a = MkProtocol a
  deriving Monad

--instance Monad ProtocolM …

-- Some primitives to write programs inside ProtocolM

-- If A sends a value only it knows to B, the value becomes public.
sendA2B :: AData a -> ProtocolM (Public a)
sendA2B (AData a) = MkProtocol a
sendB2A :: BData a -> ProtocolM (Public a)
sendB2A (BData a) = MkProtocol a

-- An example program written in ProtoM could look like

type Program a b c d
    =  AData a -- A's input
    -> BData [b] -- B's input
    -> ProtocolM (c, d) -- the action producing A and B's outputs

example :: Program Int [Int] Int ()
example i l = do
  iPub <- sendA2B i
  let x = (!! iPub) <$> l
  xPub <- sendB2A x
  return (pure xPub, ())

-- And there will also be function to decompose values of ProtocolM into the actual algorithms for A and B:

data Channel = Channel ([String], [String])

send :: String -> Channel -> Channel
send msg (Channel (rxs, txs)) = Channel (rxs, msg:txs)

recv :: Channel -> (String, Channel)
recv (Channel (msg:rxs,txs)) = (msg, Channel (rxs, txs))

algoA :: Program a b c d -> AData a -> Channel -> IO c
algoA = undefined

algoB :: Program a b c d -> BData b -> Channel -> IO d
algoB = undefined

-- And you can obtain the programs for our example by doing:
client_A = algoA example
client_B = algoB example