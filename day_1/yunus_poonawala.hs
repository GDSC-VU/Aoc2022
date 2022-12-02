import Data.Char (isSpace)
import Data.List (sort)
import Data.List.Split (splitOn)
import GHC.IO.IOMode ( IOMode(ReadMode) )
import System.IO (hGetContents, openFile)

group' :: [String] -> [[String]]
group' [] = []
group' line =
  let first = takeWhile (/= "") line
      rest = drop (length first + 1) line
   in first : group' rest

processFile :: String -> [[Int]]
processFile f = map (map read) $ group' $ splitOn "\n" f

p1 :: String -> Int
p1 s = maximum $ map sum $ processFile s

p2 :: String -> Int
p2 s = sum $ take 3 $ reverse $ sort $ map sum $ processFile s

main :: IO ()
main = do
  h <- openFile "./i1.txt" ReadMode
  c <- hGetContents h
  print $ p1 c
  print $ p2 c