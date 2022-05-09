module Config where
import Protocol
import RealConnector
import Nanomsg
import Protocols.Http
import Protocols.AsyncMessages
import Protocols.Smtp

curProt = smtpWithFlaw::Protocol (RealConnector Pair CS) (RealConnector Pair SS) ()
type SS = ()
type CS = SmtpClientS
serverState = ()
clientState = [lines "Hello\nHow are you?\nBest,\nDaniel", lines "Here is\nanother message", lines "This message shows the flaw\n.\nbecause this line won't appear"]

{-
curProt = chat::Protocol (RealConnector Pair CS) (RealConnector Pair SS) ()
type SS = ()
type CS = ()
serverState = ()
clientState = ()
-}

{-
testMiddelware = HttpMiddleware id
curProt = http testMiddelware::Protocol (RealConnector Pair ()) (RealConnector Pair (Maybe Int)) ()

serverState = Nothing
clientState = ()


type SS = Maybe Int
type CS = ()
-}

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