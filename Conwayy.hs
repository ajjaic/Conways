module Conwayy (
  prntConwayb
 ,neigborF, neigborB, neigborU, neigborD
 ,isCellin
 ,forwrd ,bkwrd ,up ,down ,upf ,upb ,downf ,downb
 ,Cell (Alive, Empty)
 ,Conwayb
) where

import Data.List (groupBy)

type Loc = (Int, Int)
data Cell = Alive Loc | Empty Loc deriving (Show, Eq)
type Height = Int
type Direction = String
type Conwayb = ([Cell], Height)

(forwrd, bkwrd, up, down, upf, upb, downf, downb) = ("f", "b", "u", "d", "uf", "ub", "df", "db")
prntConwayb :: Conwayb -> IO ()
prntConwayb (brd, h) = putStrLn "" >> (mapM_ putStrLn $ map help grouped) >> putStrLn "" where
  grouped = groupBy samerow brd :: [[Cell]]
  samerow c1 c2 = let (x', _) = getLocation c1; (x'', _) = getLocation c2 in x' == x'' 
  help lst = foldl (\acc c -> if isAlive c then acc ++ " x" else acc ++ " _") [] lst :: String

isAlive :: Cell -> Bool
isAlive (Alive _) = True
isAlive _ = False

getLocation :: Cell -> Loc
getLocation (Alive l) = l
getLocation (Empty l) = l

getCell :: Conwayb -> Cell -> Direction -> Cell
getCell (b, h) c d = case d of 
                                    "f" -> let a = (x, y+1) in head $ filter ((== a) . getLocation) b
                                    "b" -> let a = (x, y-1) in head $ filter ((== a) . getLocation) b
                                    "u" -> let a = (x-1, y) in head $ filter ((==a ) . getLocation) b
                                    "d" -> let a = (x+1, y) in head $ filter ((== a) . getLocation) b
  where (x,y) = getLocation c

isCellin :: Conwayb -> Cell -> Direction -> Bool
isCellin (_, h) c d = case d of 
                                     "f" -> y < h 
                                     "b" -> y > 1
                                     "u" -> x > 1
                                     "d" -> x < h
  where (x, y) = getLocation c

neigborF :: Conwayb -> Cell -> Maybe Cell
neigborF cn c = if isCellin cn c forwrd then Just (getCell cn c forwrd) else Nothing

neigborB :: Conwayb -> Cell -> Maybe Cell
neigborB cn c = if isCellin cn c bkwrd then Just (getCell cn c bkwrd) else Nothing

neigborU :: Conwayb -> Cell -> Maybe Cell
neigborU cn c = if isCellin cn c up then Just (getCell cn c up) else Nothing

neigborD :: Conwayb -> Cell -> Maybe Cell
neigborD cn c = if isCellin cn c down then Just (getCell cn c down) else Nothing