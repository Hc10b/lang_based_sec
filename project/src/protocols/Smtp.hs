{-# LANGUAGE FlexibleContexts #-}
import Protocol
import ClientMonadClasses

-- this is flawful because single-dots are recognized as end of message
type SmtpClientS = [[String]]
smtpWithFlaw :: Medium ma => Medium mb => ClientState SmtpClientS ma => Interactive ma => Protocol ma mb [[String]]
smtpWithFlaw = do
    greeting
    mails <- mail_exchange []
    quit
    return mails
    where
        greeting = do
            SendA2B (return "HELO")
            SendB2A (return "HELO")
            return ()
        mail_exchange collected_mails = do
            continue <- SendA2B $ do
                msgs <- getC
                let x = ["test"]:msgs -- type hint
                if length collected_mails /= length msgs
                              then do
                                   return "SEND"
                              else do
                                  return ""
            if continue=="SEND" then do
                next_mail <- transfer_mail (length collected_mails) []
                mail_exchange (next_mail:collected_mails)
            else
                return $ reverse collected_mails

        transfer_mail mailN collectedLines = do
            continue <- SendA2B (do
                msgs <- getC
                if length collectedLines /= length (msgs !! mailN) then
                    return $ (msgs !! mailN) !! length collectedLines
                else
                    return "."
                )
            if continue == "." then
                return $ reverse collectedLines
            else do
                --nextLine <- SendA2B (\(AData msgs) -> (msgs !! mailN) !! length collectedLines)
                transfer_mail mailN (continue:collectedLines)

        quit = do
            return ()

-- https://sv.wikipedia.org/wiki/Simple_Mail_Transfer_Protocol
-- → dot stuffing problem
-- “You can't rely on all smtp servers doing the dot removal correctly.”
-- — https://stackoverflow.com/questions/15224224/smtp-dot-stuffing-when-and-where-to-do-it
