{-# LANGUAGE FlexibleContexts #-}
module YieldingTimeout where
import Protocol
import ClientMonadClasses


timeoutB :: Monad ma => Monad mb => Interactive mb => ClientState (Maybe Int) mb => Show x => Read x => Show y => Read y => ma (Maybe x) -> (x -> mb y) -> Int -> Protocol ma mb (x,y)
timeoutB client response timeout =  do
    let serverResponse x = Just $ do
            putC (Nothing::Maybe Int)
            y <- response x
            return $ Just y
    let serverCheck = do
            curTime <- time
            mTime <- getC
            case mTime of
                Nothing -> do
                    putC $ Just curTime
                    return Nothing
                Just startTime ->
                    if curTime - startTime > timeout then
                        return $ Just Nothing
                    else do
                        outputI $ "time spend" ++ show (curTime - startTime)
                        return Nothing

    (mx, mmy) <- CSend client serverCheck (const Nothing) serverResponse
    case mmy of
            Just y ->  return (mx,y) -- Server responded
            Nothing -> undefined -- Timeout occured. Thus Exception thrown and this will never be evaluated
