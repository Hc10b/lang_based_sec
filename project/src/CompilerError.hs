-- This file crashes the ghc type checker
{-
{-# LANGUAGE GADTs #-}

module CompilerError where

class Monad m => Medium m where

data Protocol ma mb z where
    Preshared :: z -> Protocol ma mb z
    SendA2B :: (Show z, Read z) => ma z -> Protocol ma mb z
    SendB2A :: Protocol ma mb z
    PSend ::  Protocol ma mb (x, y)
    BindP ::  Protocol ma mb x


algoA :: Medium ma => Protocol ma mb z -> a -> ma z
algoA (SendA2B f) ad = do
    return ()
algoA (BindP pz f) ad = undefined
-}