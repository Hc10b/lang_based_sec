{-# LANGUAGE MultiParamTypeClasses #-}
module Medium where

class Monad m => Medium m c where
    send :: c -> String -> m String
    recv :: c -> m String
    generateRecv :: c -> m (IO String)
    maybeRecv :: c -> m (Maybe String)
