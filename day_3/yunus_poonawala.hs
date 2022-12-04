import Data.Char (ord)
import Data.List (intersect, nub)

processStr :: String -> [(String, String)]
processStr s =
  let slines = lines s
      c2 = map (\l -> drop (length l `div` 2) l) slines
      c1 = map (\l -> take (length l `div` 2) l) slines
   in zip c1 c2

toPriority :: [String] -> [Int]
toPriority strs =
  sum <$> res
  where
    res =
      map
        ( \x ->
            (if ord x - 64 > 26 then ord x - 64 - 32 else ord x - 64 + 26)
        )
        <$> strs

p1 :: String -> Int
p1 contents =
  let final = processStr contents
      inter = nub <$> zipWith intersect (fst <$> final) (snd <$> final)
   in sum <$> toPriority $ filter (/= "") inter

group3 :: [String] -> [(String, String, String)]
group3 [] = []
group3 (x : y : z : zs) = (x, y, z) : group3 zs
group3 _ = error "unreachable!"

intersect3 :: [(String, String, String)] -> [String]
intersect3 [] = []
intersect3 ((x, y, z) : xs) = x' `intersect` z : intersect3 xs
  where
    x' = x `intersect` y


p2 :: String -> Int
p2 contents = sum $ toPriority $  nub <$> intersect3 (group3 $ lines contents)


main :: IO ()
main = do
  contents <- readFile "./i1.txt"
  print $ p1 contents
  print $ p2 contents