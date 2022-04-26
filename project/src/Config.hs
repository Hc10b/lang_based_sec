module Config where
import Protocol
import RealConnector
import Nanomsg
import Protocols.Http


curProt = http testMiddelware::Protocol (RealConnector Pair ()) (RealConnector Pair (Maybe Int)) ()

serverState = Nothing
clientState = ()


type SS = Maybe Int
type CS = ()

{-
curProt :: Protocol (RealConnector Pair CS) (RealConnector Pair SS) ()
curProt = do
    SendA2B $ return "hi"
    return ()

type SS = ()
serverState = ()
type CS = ()
clientState = ()
-}