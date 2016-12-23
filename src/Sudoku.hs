module Sudoku (Puzzle, printSudoku, displayPuzzle, sudoku) where

import Control.Monad
import Data.List (transpose)
import Lib


type Puzzle = [Int]

test :: Puzzle
test = [
    0, 0, 0, 0, 8, 0, 0, 0, 0,
    0, 0, 0, 1, 0, 6, 5, 0, 7,
    4, 0, 2, 7, 0, 0, 0, 0, 0,
    0, 8, 0, 3, 0, 0, 1, 0, 0,
    0, 0, 3, 0, 0, 0, 8, 0, 0,
    0, 0, 5, 0, 0, 9, 0, 7, 0,
    0, 5, 0, 0, 0, 8, 0, 0, 6,
    3, 0, 1, 2, 0, 4, 0, 0, 0,
    0, 0, 6, 0, 1, 0, 0, 0, 0 ]

displayPuzzle :: Puzzle -> String
displayPuzzle = unlines . map show . chunk 9

printSudoku :: Puzzle -> IO ()
printSudoku = putStr . unlines . map displayPuzzle . sudoku

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = ys : chunk n zs where
    (ys, zs) = splitAt n xs

sudoku :: Puzzle -> [Puzzle]
sudoku puzzle = runFD $ do
    vars <- newVars 81 [1..9]
    zipWithM_ (\x n -> when (n > 0) (x `hasValue` n)) vars puzzle
    mapM_ allDifferent (rows vars)
    mapM_ allDifferent (columns vars)
    mapM_ allDifferent (boxes vars)
    labelling vars

rows, columns, boxes :: [a] -> [[a]]
rows = chunk 9
columns = transpose . rows
boxes = concat . map (map concat . transpose) . chunk 3 . chunk 3 . chunk 3    
