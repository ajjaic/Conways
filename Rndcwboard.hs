{-# LANGUAGE FlexibleInstances #-}

module Rndcwboard (cbrd) where

import Conwayy (Cell (Alive, Empty), Conwayb, Loc, Height)
import System.Random

instance Random (Loc -> Cell) where
  
  randomR _ r = random r
  random r = (if t then Alive else Empty, nr) where 
    (t, nr) = random r 

rndcell :: Loc -> IO Cell
rndcell l = (getStdRandom random) >>= (\f -> return (f l))

cbrd :: Height -> IO Conwayb
cbrd h = (mapM rndcell [(x,y) | x <- [1..h], y <- [1..h]]) >>= (\l -> return (l, h))