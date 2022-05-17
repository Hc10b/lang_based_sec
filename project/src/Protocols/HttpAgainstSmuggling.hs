{-# LANGUAGE FlexibleContexts #-}
module Protocols.HttpAgainstSmuggling where
import Protocol
import Control.Monad.Writer
import Control.Monad.State
import Data.List

data HttpPayloadLength = ContentLength Int | Chunking

httpForForwardingToBackend :: (Monad ma, Monad mb, MonadState [String] ma, MonadWriter String mb) => Protocol ma mb ()
httpForForwardingToBackend = do
    LiftAC dropForbiddenMessages
    sendTopRequest
    httpForForwardingToBackend
   where
       sendTopRequest = do
           payloadLength <- sendHeader
           case payloadLength of
               ContentLength bytes -> sendNBytes bytes
               Chunking -> sendChunkedPayload
           return ()

       sendHeader = do
           nextLine <- sendNextLine
           if "'Content-Length:'" `isPrefixOf` nextLine then do
               let length = parseContentLength nextLine
               sendRemainingHeader
               return $ ContentLength length
           else if "'Transfer-Encoding: chunked'" `isPrefixOf` nextLine then do
               sendRemainingHeader
               return Chunking
            else
               sendHeader

       sendRemainingHeader = do
           nextLine <- sendNextLine
           unless (nextLine == "") sendRemainingHeader

       sendNextLine = do
            line <- SendA2B $ do
               lines <- get
               put $ tail lines
               return $ head lines
            LiftBC $ do
                tell line
            return line

       sendNBytes n = undefined -- sends the next n bytes
       sendChunkedPayload = undefined
       parseContentLength = undefined

dropForbiddenMessages :: MonadState [String] ma => ma ()
dropForbiddenMessages = do
    header <- gets header
    if isOK header then
        return ()
    else do
        dropTopMessage
        dropForbiddenMessages

header :: [String] -> a
header = undefined

isOK :: a -> bool
isOK = undefined

dropTopMessage :: MonadState [String] ma => ma ()
dropTopMessage = undefined