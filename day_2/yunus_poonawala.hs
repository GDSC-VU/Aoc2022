import Data.List.Split ( splitOn )

main :: IO ()
main = do
  s <- readFile "./inp.txt"
  let sLines = lines s
  let proc = map (splitOn " ") sLines
  let p1s = sum (map p1 proc)
  let p2s = sum (map p2 proc)
  print p1s
  print p2s


-- Gross code follows

p1 :: [String] -> Int
p1 move = case move of
  ["A", "Y"] -> 8
  ["B", "Z"] -> 9
  ["C", "X"] -> 7
  ["A", "X"] -> 4
  ["B", "Y"] -> 5
  ["C", "Z"] -> 6
  [_, "X"] -> 1
  [_, "Y"] -> 2
  [_, "Z"] -> 3
  _ -> 0

p2 :: [String] -> Int
p2 move = case move of
  ["A", "X"] -> 3
  ["B", "X"] -> 1
  ["C", "X"] -> 2
  ["A", "Y"] -> 4
  ["B", "Y"] -> 5
  ["C", "Y"] -> 6
  ["A", "Z"] -> 8
  ["B", "Z"] -> 9
  ["C", "Z"] -> 7
  _ -> 0