
import Data.Maybe

data Task = Target | Endo deriving(Eq, Show, Enum)

data Sample a = Sample
    { s_name :: String
    , s_detector :: String
    , s_task :: Task
    , s_cts :: [a]
    } deriving (Eq, Show)

data Result a = Result
    { r_sample :: Sample a
    , r_mean :: (Maybe a, Maybe a)
  --  , r_dct :: Maybe a
  --  , r_ddct :: (Maybe a, Maybe a)
  --  , r_rq :: (Maybe a, Maybe a)
    } deriving (Eq, Show)

samples = 
    [ Sample "D39 MOI 10" "DEFB3" Target [30.839, 31.222, 31.107]
    , Sample "D39 MOI 100" "DEFB3" Target [29.058, 29.091, 28.981]
    , Sample "Mock" "DEFB3" Target [33.315, 33.38, 33.019]
    , Sample "D39 MOI 10" "betaActin" Endo [18.725, 18.782, 18.925]
    , Sample "D39 MOI 100" "betaActin" Endo [19.499, 19.786, 19.627]
    , Sample "Mock" "betaActin" Endo [17.653, 17.717]
    , Sample "D39 MOI 10" "IL8" Target [18.494, 18.292, 18.401]
    , Sample "bad" "DEFB3" Target []
    ]


-- map (\x -> Result x (sampleCtsMean (s_cts x))) samples


search x xs = filter (\s -> (s_task s) == Target && (s_name s) == x) xs

sampleCtsMean [] = (Nothing, Nothing)
sampleCtsMean xs = (m, stdev m xs)
    where
        m = mean xs





mean :: (Floating a) => [a] -> Maybe a
mean [] = Nothing
mean xs = Just (mean' 0 0 xs)
    where
        mean' :: (Floating a) => a -> Int -> [a] -> a
        mean' sum len []     = sum / fromIntegral len
        mean' sum len (x:xs) = mean' (sum + x) (succ len) xs
 
stdev :: (Floating a) => Maybe a -> [a] -> Maybe a
stdev _ []      = Nothing
stdev Nothing _ = Nothing
stdev m xs      = Just (sqrt $ variance (fromJust m) xs)
 
variance :: (Floating a) => a -> [a] -> a
variance m xs = variance' m 0 0 xs
    where
        variance' :: (Floating a) => a -> a -> Int -> [a] -> a
        variance' m' s l []     = s / fromIntegral (pred l)
        variance' m' s l (x:xs) = variance' m' (s + ((m' - x) * (m' - x))) (succ l) xs

