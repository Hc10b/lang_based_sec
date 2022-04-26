{-# LANGUAGE FlexibleContexts #-}
module BlockingTimeout where
import Protocol
import ClientMonadClasses

timeoutB :: Medium ma => Medium mb => Interactive mb => Show x => Read x => Show y => Read y => ma (Maybe x) -> (x -> mb y) -> Int -> Protocol ma mb (Maybe (x,y))
timeoutB client response timeout =  do
    let serverResponse x = Just $ do
            y <- response x
            return $ Just y
    let serverCheck = do
            outputI "Restart timeout"
            sleep timeout
            return $ Just Nothing

    (mx, mmy) <- CSend client serverCheck (const Nothing) serverResponse
    case mmy of
        Nothing -> undefined -- can't happen
        Just my  -> case my of
            Nothing -> return Nothing -- Timeout occured
            Just y -> case mx of
                Nothing ->  undefined -- can't occur
                Just x ->  return $ Just (x,y) -- Server responded
