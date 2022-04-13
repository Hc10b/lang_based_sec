{-# LANGUAGE FlexibleContexts #-}
import Protocol
import ClientMonadClasses

-- An example program written in ProtoM could look like
--example :: Program Int [Int] Int ()
example :: Medium ma => Medium mb => ClientState Int ma => ClientState [Int] mb => Protocol ma mb (Int, ())
example = do
  iPub <- SendA2B $ do getC
  xPub <- SendB2A $ do
      ints <- getC
      return $ ints !! iPub
  return (xPub, ())
