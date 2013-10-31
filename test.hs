import Data.Maybe

data Task = Target | Endo deriving (Eq, Show, Enum)


data CoreData a = CoreData
    { cd_sample :: String
    , cd_detector :: String
    , cd_task :: Task
    , cd_cts :: [a]
    } deriving (Eq, Show)

data MeanData a = MeanData
    { md_mean :: Maybe a
    , md_stdDev :: Maybe a
    } deriving (Eq, Show)

data TargetData a = TargetData
    { td_dct :: Maybe a
    , td_ddct :: Maybe a
    , td_ddctErr :: Maybe a
    , td_rq :: Maybe a
    , td_rqErr :: Maybe a
    } deriving (Eq, Show)

data Result a = Result
    { r_core :: CoreData a
    , r_mean :: Maybe (MeanData a)
    , r_target :: Maybe (TargetData a)
    } deriving (Eq, Show)


meanData :: (Floating a) => [a] -> Maybe (MeanData a)
meanData [] = Nothing
meanData xs = Just (MeanData (Just m) sd)
    where
        m  = mean xs
        sd = stdev m xs

-- samples
-- targetData (MeanData (Just 21.0) (Just 1.0)) (MeanData (Just 15.0) (Just 1.0)) (MeanData (Just 16.0) (Just 1.0)

targetData :: (Floating a) => MeanData a -> MeanData a -> MeanData a -> Maybe (TargetData a)
targetData s e uc = Just (TargetData dct ddct ddctErr rq rqErr)
    where
        dct = do
            sm <- (md_mean s)
            em <- (md_mean e)
            return (sm - em)
        ddct = do
            ucm <- (md_mean uc)
            em <- (md_mean e)
            dct' <- dct
            return (dct' - (ucm - em))
        ddctErr = do
            ssd <- (md_stdDev s)
            esd <- (md_stdDev e)
            return (sqrt $ ((esd * esd) + (ssd * ssd) + (ssd * ssd) + (esd * esd))
        rq = do
            ddct' <- ddct
            return (2^-ddct)
        rqErr = Nothing
            
            
mean :: (Floating a) => [a] -> a
mean xs = mean' 0 0 xs
    where
        mean' :: (Floating a) => a -> Int -> [a] -> a
        mean' sum len []     = sum / fromIntegral len
        mean' sum len (x:xs) = mean' (sum + x) (succ len) xs

stdev :: (Floating a) => a -> [a] -> Maybe a
stdev _ []      = Nothing
stdev m xs      = Just (sqrt $ variance m xs)

variance :: (Floating a) => a -> [a] -> a
variance m xs = variance' m 0 0 xs
    where
        variance' :: (Floating a) => a -> a -> Int -> [a] -> a
        variance' mean sum len []     = sum / fromIntegral (pred len)
        variance' mean sum len (x:xs) = variance' mean (sum + ((mean - x) * (mean - x))) (succ len) xs
