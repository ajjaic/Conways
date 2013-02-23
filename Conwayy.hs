module Conwayy (
  prntConwayb
 ,cellAliveNeighbors
 ,nxtGen
 ,forwrd ,bkwrd ,up ,down ,upf ,upb ,downf ,downb
 ,Cell (Alive, Empty) ,Conwayb ,Loc, Height
) where

import Data.List (groupBy)
import Data.Maybe (catMaybes)

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
                                    "f" -> help (x, y+1) 
                                    "b" -> help (x, y-1) 
                                    "u" -> help (x-1, y) 
                                    "d" -> help (x+1, y)
                                    "uf" -> help (x-1, y+1) 
                                    "ub" -> help (x-1, y-1)
                                    "df" -> help (x+1, y+1)
                                    "db" -> help (x+1, y-1)
  where 
    (x,y) = getLocation c
    help loc = head $ filter ((== loc) . getLocation) b

isCellin :: Conwayb -> Cell -> Direction -> Bool
isCellin cn@(_, h) c d = case d of 
                                         "f" -> y < h 
                                         "b" -> y > 1
                                         "u" -> x > 1
                                         "d" -> x < h
                                         "uf" -> isCellin cn c forwrd && isCellin cn c up
                                         "ub" -> isCellin cn c bkwrd && isCellin cn c up
                                         "df" -> isCellin cn c forwrd && isCellin cn c down
                                         "db" -> isCellin cn c bkwrd && isCellin cn c down
  where (x, y) = getLocation c

neigborInDir :: Conwayb -> Cell -> Direction -> Maybe Cell
neigborInDir cn c d = if isCellin cn c d then Just (getCell cn c d) else Nothing

cellAliveNeighbors :: Conwayb -> Cell -> [Cell]
cellAliveNeighbors cn c = filter isAlive $ catMaybes $ map (neigborInDir cn c) directions where
  directions = [forwrd, bkwrd, up, down, upf, upb, downf, downb] 

nxtGen :: Conwayb -> Conwayb
nxtGen cn@(brd, h) = (help brd, h) where 
  help [] = []
  help (c:cs) =  (aoe):(help cs) where 
    aoe
      | isAlive c && (nc < 2 || nc > 3) = Empty $ getLocation c
      | (isAlive c && (nc == 2 || nc == 3)) || ((not $ isAlive c) && nc == 3) = Alive $ getLocation c
      | otherwise = Empty $ getLocation c
    nc = length $ cellAliveNeighbors cn c








