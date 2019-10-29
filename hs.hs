import System.IO

chase = 99222

ndistance :: Integer -> Integer -> Integer
ndistance x y = abs $ x - y

getmin :: Integer -> Integer -> Integer -> Integer
getmin x y z | y' < z'   = y
             | otherwise = z
               where y' = ndistance x y
                     z' = ndistance x z

challenge :: [[Integer]] -> Integer -> Maybe (Integer, Integer)
challenge [xs,ys] z = Just (foldl1 getmin' xs, foldl1 getmin' ys)
                      where getmin' = getmin z
challenge _ _ = Nothing

main = do
    handle <- openFile "logs.txt" ReadMode
    contents <- hGetContents handle
    --putStrLn $ show $ words contents -- Debug
    putStrLn $ show $ challenge (map (read . tail) $ words contents) chase
    hClose handle
