
import Data.Maybe

data Task = Target | Endo 
    deriving (Eq, Show, Enum)

data Sample a = Sample 
    { s_name :: String
    , s_detector :: String
    , s_task :: Task
    , s_cts :: [a]
    , s_mean :: (Maybe a, Maybe a)
    } deriving (Eq, Show)

data SampleValues a = SampleValues
    { sv_cts :: [a]
    , sv_mean :: (Maybe a, Maybe a)
    } deriving (Eq, Show)

    
    
add :: String ->  a -> [Sample] -> [Sample]
add n d t v xs = 

    
    
    
  add "D39 MOI 10" "DEFB3" Target 30.839
    "D39 MOI 10" "DEFB3" Target 31.222
    "D39 MOI 10" "DEFB3" Target 31.107
    
    
-- Sample "D39 MOI 10" "DEFB3" Target [30.839, 31.222, 31.107]