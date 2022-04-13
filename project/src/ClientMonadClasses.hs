{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}

module ClientMonadClasses where

class Interactive m where
    outputI :: String -> m ()
    readI :: m String
    time :: m Int
    sleep :: Int -> m ()

class ClientState s m where
    putC :: s -> m ()
    getC :: m s
