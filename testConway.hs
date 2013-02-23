import Conwayy
import Data.List (partition)

t1 = prntConwayb cbrd
t2 = prntConwayb brdc
t3 = mapM_ putStrLn $ map (show . cellAliveNeighbors cbrd) cbrdcells
t4 = mapM_ putStrLn $ map (show . cellAliveNeighbors brdc) brdccells
t5 = prntConwayb $ nxtGen cbrd
t6 = prntConwayb $ nxtGen brdc


cbrd :: Conwayb
cbrd = (concat [[Alive (1,1), Alive (1,2), Empty (1,3), Empty (1,4)],
                        [Empty (2,1), Alive (2,2), Empty (2,3), Empty (2,4)],
                        [Alive (3,1), Empty (3,2), Empty (3,3), Alive (3,4)],
                        [Empty (4,1), Empty (4,2), Alive (4,3), Empty (4,4)]], 4)

brdc :: Conwayb
brdc = (concat [[Alive (1,1), Alive (1,2), Empty (1,3), Empty (1,4), Empty (1,5), Alive (1,6)],
                         [Empty (2,1), Alive (2,2), Empty (2,3), Empty (2,4), Empty (2,5), Empty (2,6)],
                         [Alive (3,1), Empty (3,2), Empty (3,3), Alive (3,4), Alive (3,5), Alive (3,6)],
                         [Empty (4,1), Empty (4,2), Alive (4,3), Empty (4,4), Alive (4,5), Empty (4,6)],
                         [Empty (5,1), Alive (5,2), Empty (5,3), Empty (5,4), Empty (5,5), Alive (5,6)],
                         [Empty (6,1), Empty (6,2), Alive (6,3), Empty (6,4), Alive (6,5), Empty (6,6)]], 6)

tbrd1 = fst cbrd
tbrd2 = fst brdc
cbrdcells = [Alive (1,1), Alive (3,4), Empty (4,1), Empty (4,4), Empty (1,4), Alive (2,2), Alive (3,1), Empty (1,3)]
brdccells = [Alive (1,1), Empty (5,4), Empty (4,6), Empty (4,4), Empty (6,4), Empty (6,6), Empty (6,1)]