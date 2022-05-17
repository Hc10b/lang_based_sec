module Config where
import Protocol
import RealConnector
import Nanomsg
import Protocols.Http
import Protocols.AsyncMessages
import Protocols.Smtp

data ProtocolInIO cs ss = MkProtoInIo {
    protocol :: Protocol (RealConnector Pair cs) (RealConnector Pair ss) (),
    initServerState :: ss,
    initClientState :: cs}

curProt = smtpProt

smtpProt = MkProtoInIo smtpWithFlaw () [lines "Hello\nHow are you?\nBest,\nDaniel", lines "Here is\nanother message", lines "This message shows the flaw\n.\nbecause this line won't appear"]

chatProt = MkProtoInIo chat () ()

testMiddelware = HttpMiddleware id

httpProt = MkProtoInIo (http testMiddelware) (Nothing :: Maybe Int) ()