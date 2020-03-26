-- A Rubik's Cube Scrambler that provides random move directions.
-- Improved Scrambler that eliminates all duplicates.
-- Fixed a problem where the first argument to genRandIntList does not match the length of the output scramble.
-- A slightly more readable version with consInIO function turned into an infix operator.

import System.Random
import Data.Maybe


data Movetypes = Single | Twice | CCW

data Moves =
    R Movetypes |
    L Movetypes |
    F Movetypes |
    B Movetypes |
    U Movetypes |
    D Movetypes


mappingOfDirs :: [(Int, Movetypes -> Moves)]
mappingOfDirs = [(1, R), (2, L), (3, F), (4, B), (5, U), (6, D)]


mappingOfTypes :: [(Int, Movetypes)]
mappingOfTypes = [(1, Single), (2, Twice), (3, CCW)]


genRandIntList :: Int -> [(Int, Int)] -> IO [(Int, Int)]
genRandIntList 0 l = return l
genRandIntList n l = do
    i <- randomRIO ((1, 6) :: (Int, Int))
    j <- randomRIO ((1, 3) :: (Int, Int))
    genRandIntList (n-1) ((i, j):l)


randIntList :: IO [(Int, Int)]
randIntList = genRandIntList 20 []


-- The (:) operator for (Int, Int) and IO [(Int, Int)].
(<::>) :: (Int, Int) -> IO [(Int, Int)] -> IO [(Int, Int)]
e <::> l = l >>= \x -> return (e:x)


removeDups :: [(Int, Int)] -> IO [(Int, Int)]
removeDups l = (head l) <::> (removeDups_inner (fst (head l)) (tail l))


removeDups_inner :: Int -> [(Int, Int)] -> IO [(Int, Int)]
removeDups_inner current [] = return []
removeDups_inner current ((x,y):xys) =
    case even current of
        True -> if x == current-1 || x == current then do
                    a <- randomRIO ((1, 6) :: (Int, Int))
                    removeDups_inner current ((a,y):xys)
                else (x,y) <::> (removeDups_inner x xys)
        _    -> if x == current+1 || x == current then do
                    a <- randomRIO ((1, 6) :: (Int, Int))
                    removeDups_inner current ((a,y):xys)
                else (x,y) <::> (removeDups_inner x xys)


convert2move :: (Int, Int) -> Maybe Moves
convert2move (n, m) = do
    dir <- lookup n mappingOfDirs
    typ <- lookup m mappingOfTypes
    return (dir typ)


convert2moves :: [(Int, Int)] -> IO [Moves]
convert2moves l = return (catMaybes $ convert2move <$> l)


movetype2str :: Movetypes -> [Char]
movetype2str t =
    case t of
        Single -> ""
        Twice -> "2"
        CCW -> "\'"


printMoves :: [Moves] -> IO ()
printMoves [] = putStrLn ""
printMoves (x:xs) = do
    case x of
        (R t) -> putStr ("R" ++ movetype2str t ++ " ")
        (L t) -> putStr ("L" ++ movetype2str t ++ " ")
        (F t) -> putStr ("F" ++ movetype2str t ++ " ")
        (B t) -> putStr ("B" ++ movetype2str t ++ " ")
        (D t) -> putStr ("D" ++ movetype2str t ++ " ")
        (U t) -> putStr ("U" ++ movetype2str t ++ " ")
    printMoves xs


main :: IO ()
main = randIntList >>= removeDups >>= convert2moves >>= printMoves