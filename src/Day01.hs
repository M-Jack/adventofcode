
module Day01 where 



day01 :: IO ()
day01 = do
    par <- readFile "input/day01"
    putStrLn . show $ elevator par
    putStrLn . show $ firstBasement par


elevator :: String -> Int
elevator = sum . map f
    where f '(' = 1
          f ')' = (-1)
          f c = error $ "Unexpected character : " ++ [c]

firstBasement :: String -> Int
firstBasement par = rec (0,0) par
    where rec :: (Int, Int) -> String -> Int
          rec (0,b) (')':_) = b + 1
          rec (a,b) (')':xs) = rec (a - 1, b + 1) xs
          rec (a,b) ('(':xs) = rec (a + 1, b + 1) xs
          rec _ (c:_) = error $ "Unexpected character : " ++ [c]
          rec _ [] = error "Never got in the basement"


