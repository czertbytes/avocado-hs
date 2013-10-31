

diff :: (Floating a) => Maybe a -> Maybe a -> Maybe a
diff x y = do
    x' <- x
    y' <- y
    return (x' - y')

diffErr :: (Floating a) => Maybe a -> Maybe a -> Maybe a
diffErr x y = do
    x' <- x
    y' <- y
    return (sqrt $ 2 * (x' * x') + 2 * (y' * y'))

rq :: (Floating a) => Maybe a -> Maybe a
rq x = do
    x' <- x
    return (2.0 ** (negate x'))

rqErr :: (Floating a) => Maybe a -> Maybe a -> Maybe a
rqErr x y = do
    x' <- x
    y' <- y
    return (sqrt $ (x' * x') * 0.4804530139182014 * (y' * y')) 

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

