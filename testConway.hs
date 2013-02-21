import Conwayy
import Data.List (partition)

t1 = prntConwayb cbrd
t2 = prntConwayb brdc
t3 = let (f, s) = partition ((\fn -> fn forwrd) . (isCellin cbrd)) cbrdcells in mapM_ putStrLn $ (map (\c -> "Has front " ++ show c) f) ++ (map (\c -> "No front " ++ show c) s)
t4 = let (f, s) = partition ((\fn -> fn forwrd) . (isCellin brdc)) brdccells in mapM_ putStrLn $ (map (\c -> "Has front " ++ show c) f) ++ (map (\c -> "No front " ++ show c) s)
t5 = mapM_ putStrLn $ map (\(a,b) -> "Cell in front of " ++ show a ++ " is " ++ show b) $ zip cbrdcells (map (neigborF cbrd) cbrdcells) 
t6 = mapM_ putStrLn $ map (\(a,b) -> "Cell in front of " ++ show a ++ " is " ++ show b) $ zip brdccells $ map (neigborF brdc) brdccells
t7 = mapM_ putStrLn $ map (\(a,b) -> "Cell behind " ++ show a ++ " is " ++ show b) $ zip cbrdcells (map (neigborB cbrd) cbrdcells) 
t8 = mapM_ putStrLn $ map (\(a,b) -> "Cell behind " ++ show a ++ " is " ++ show b) $ zip brdccells $ map (neigborB brdc) brdccells
t9 = mapM_ putStrLn $ map (\(a,b) -> "Cell above " ++ show a ++ " is " ++ show b) $ zip cbrdcells (map (neigborU cbrd) cbrdcells) 
t10 = mapM_ putStrLn $ map (\(a,b) -> "Cell above " ++ show a ++ " is " ++ show b) $ zip brdccells $ map (neigborU brdc) brdccells
t11 = mapM_ putStrLn $ map (\(a,b) -> "Cell below " ++ show a ++ " is " ++ show b) $ zip cbrdcells (map (neigborD cbrd) cbrdcells) 
t12 = mapM_ putStrLn $ map (\(a,b) -> "Cell below " ++ show a ++ " is " ++ show b) $ zip brdccells $ map (neigborD brdc) brdccells


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
cbrdcells = [Alive (1,1), Alive (3,4), Empty (4,1), Empty (4,4), Empty (1,4), Alive (2,2), Alive (3,1)]
brdccells = [Alive (1,1), Empty (5,4), Empty (4,6), Empty (4,4), Empty (6,4), Empty (6,6), Empty (6,1)]