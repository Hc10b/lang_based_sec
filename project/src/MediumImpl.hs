{-# LANGUAGE GADTs #-}
module MediumImpl where
import ClientMonadClasses
import Protocol
import Data.Functor

-- And there will also be function to decompose values of ProtocolM into the actual algorithms for A and B:
algoA :: Interactive ma => Monad ma => Protocol ma mb z -> ma z
algoA (Preshared a) = return a
algoA (SendA2B f) = do
    z <- f
    str <- send $ show z
    return $ read str
algoA (SendB2A _) = recv <&> read
algoA (BindP pz f) = do
    z <- algoA pz
    algoA (f z)
algoA (LiftAC ma) = do ma
algoA (LiftBC mb) = return ()
algoA (CSend la lb ra rb) = do
    maybeStrB <- maybeRecv
    case maybeStrB of
        Nothing -> do
            maybeX <- la
            case maybeX of
                Nothing -> algoA (CSend la lb ra rb)
                Just x -> do
                    strA <- send (show x)
                    -- check, whether B will answer
                    case rb $ read strA of
                        Nothing -> return (Just $ read strA, Nothing)
                        Just _ -> do
                            strB <- recv
                            return (Just $ read strA, Just $ read strB)
        Just strB -> case ra (read strB) of
            Nothing -> return (Nothing, Just $ read strB)
            Just mx -> do
                x <- mx
                strA <- send (show x)
                return (Just $ read strA, Just $ read strB)
algoA (Async txa txb rxa rxb sa sb) = loopT
    where
        loopR = do
                mb <- maybeRecv
                case mb of
                    Nothing -> loopT
                    Just strb ->
                        do
                            let cb = read strb
                            rxb cb
                            if sb cb then
                                wait_for_this cb
                            else
                                loopT
        loopT = do
            ma <- txa
            case ma of
              Nothing -> loopR
              Just ca ->
                  do
                      str <- send $ show ca
                      if sa $ read str then
                          wait_for_other (read str)
                      else
                          loopR
        wait_for_other this_sync = do
            mb <- maybeRecv
            case mb of
              Nothing -> wait_for_other this_sync
              Just strb ->
                  do
                      let cb = read strb
                      rxb cb
                      if sb cb then
                          return (this_sync, cb)
                      else wait_for_other this_sync
        wait_for_this other_sync = do
            ma <- txa
            case ma of
              Just ca ->
                  do
                      str <- send $ show ca
                      if sa $ read str then
                          return (read str, other_sync)
                      else
                          wait_for_this other_sync
              Nothing -> wait_for_this other_sync





algoB :: Interactive mb => Monad mb => Protocol ma mb z -> mb z
algoB p = algoA $ flipProt p