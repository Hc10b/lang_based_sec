{-# LANGUAGE FlexibleContexts #-}
module BlockingTimeout where
import Protocol
import ClientMonadClasses

timeoutB :: Monad ma => Monad mb => Interactive mb => Show x => Read x => Show y => Read y => ma (Maybe x) -> (x -> mb y) -> Int -> Protocol ma mb (x,y)
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
            Just y ->  return (mx,y) -- Server responded
            Nothing -> undefined -- Timeout occured. Thus exception thrown. So never evaluated.
