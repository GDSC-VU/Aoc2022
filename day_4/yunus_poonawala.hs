import Data.List
import Data.List.Split (splitOn)

processStr :: String -> [(String, String)]
processStr str =
  let splits = map (splitOn ",") $ lines str
   in map (\l -> (head l, last l)) splits

toLists :: [(String, String)] -> [([Int], [Int])]
toLists =
  map
    ( \(x, y) ->
        let a : b : _ = read <$> splitOn "-" x
            e : f : _ = read <$> splitOn "-" y
         in ([a .. b], [e .. f])
    )

overlaps :: Eq a => [a] -> [a] -> Bool
overlaps x y = not (null (x `intersect` y))

subList :: Eq a => [a] -> [a] -> Bool
subList x y = intersect x y == x || intersect x y == y

p1 :: String -> Int
p1 ys = length $ filter (uncurry subList) xs
  where
    xs = toLists $ processStr ys

p2 :: String -> Int
p2 ys = length $ filter (uncurry overlaps) xs
  where
    xs = toLists $ processStr ys

main :: IO ()
main = do
  contents <- readFile "./inp.txt"
  print $ p1 contents
  print $ p2 contents