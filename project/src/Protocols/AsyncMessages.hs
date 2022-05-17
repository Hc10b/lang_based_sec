module Protocols.AsyncMessages where
import Protocol (Protocol(Async))
import ClientMonadClasses
import System.Timeout
import Control.Monad.IO.Class

handleInput :: (Monad ma, Interactive ma, MonadIO ma) => ma (Maybe String)
handleInput = do
    mStr <- liftIO $ timeout 100000 getLine
    let toSend = do
                str <- mStr
                if str == "" then
                    Nothing
                else
                    return str
    return (toSend::Maybe String)

handleReceive str = do
    outputI "Other:"
    outputI str

isSync "Sync!" = True
isSync _ = False

chat :: (Interactive ma, Interactive  mb, MonadIO ma, MonadIO mb) => Protocol ma mb ()
chat = do
    Async handleInput handleInput handleReceive handleReceive isSync isSync
    return ()
