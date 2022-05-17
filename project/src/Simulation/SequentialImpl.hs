{-# LANGUAGE GADTs #-}
module Simulation.SequentialImpl where
import ClientMonadClasses
import Protocol
import Data.Functor
import Medium


-- And there will also be function to decompose values of ProtocolM into the actual algorithms for A and B:
algoA :: (Monad mb, Interactive ma) => Monad ma => Medium ma m => m -> Protocol ma mb z -> ma z
algoA m (Preshared a) = return a
algoA m (SendA2B f) = do
    z <- f
    str <- send m $ show z
    return $ read str
algoA m (SendB2A _) = recv m <&> read
algoA m (BindP pz f) = do
    z <- algoA m pz
    algoA m (f z)
algoA m (LiftAC ma) = do ma
algoA m (LiftBC mb) = return ()
algoA m (CSend la lb ra rb) = do
    maybeStrB <- maybeRecv m
    case maybeStrB of
        Nothing -> do
            maybeX <- la
            case maybeX of
                Nothing -> algoA m (CSend la lb ra rb)
                Just x -> do
                    strA <- send m (show x)
                    -- check, whether B will answer
                    case rb $ read strA of
                        Nothing -> return (Just $ read strA, Nothing)
                        Just _ -> do
                            strB <- recv m
                            return (Just $ read strA, Just $ read strB)
        Just strB -> case ra (read strB) of
            Nothing -> return (Nothing, Just $ read strB)
            Just mx -> do
                x <- mx
                strA <- send m (show x)
                return (Just $ read strA, Just $ read strB)
algoA m (Async txa txb rxa rxb sa sb) = loopT
    where
        loopR = do
                mb <- maybeRecv m
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
                      str <- send m $ show ca
                      if sa $ read str then
                          wait_for_other (read str)
                      else
                          loopR
        wait_for_other this_sync = do
            mb <- maybeRecv m
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
                      str <- send m $ show ca
                      if sa $ read str then
                          return (read str, other_sync)
                      else
                          wait_for_this other_sync
              Nothing -> wait_for_this other_sync





algoB :: (Monad ma, Interactive mb, Monad mb, Medium mb m) => m -> Protocol ma mb z -> mb z
algoB m p = algoA m $ flipProt p