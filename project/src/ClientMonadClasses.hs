{-# LANGUAGE MultiParamTypeClasses #-}

module ClientMonadClasses where

class Interactive m where
    outputI :: String -> m ()
    readI :: m String
    time :: m Int

class ClientState s m where
    putC :: s -> m ()
    getC :: m s
