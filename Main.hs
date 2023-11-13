import Data.List
import Data.Maybe

type Sudoku = [[Maybe Int]]

size :: Int
size = 9

exampleSudoku :: Sudoku -- Example Sudoku grid
exampleSudoku =
    [ [Just 5, Nothing, Nothing, Just 8, Nothing, Nothing, Nothing, Just 4, Just 9]
    , [Nothing, Just 9, Nothing, Just 7, Nothing, Just 1, Nothing, Just 8, Nothing]
    , [Nothing, Nothing, Just 3, Nothing, Just 5, Nothing, Just 6, Nothing, Nothing]
    , [Just 6, Nothing, Just 8, Nothing, Nothing, Nothing, Just 3, Nothing, Just 7]
    , [Nothing, Just 4, Nothing, Nothing, Nothing, Nothing, Nothing, Just 1, Nothing]
    , [Just 2, Nothing, Just 7, Nothing, Nothing, Nothing, Just 9, Nothing, Just 8]
    , [Nothing, Nothing, Just 1, Nothing, Just 3, Nothing, Just 8, Nothing, Nothing]
    , [Nothing, Just 3, Nothing, Just 5, Nothing, Just 9, Nothing, Just 2, Nothing]
    , [Just 7, Just 2, Nothing, Nothing, Nothing, Just 8, Nothing, Nothing, Just 4]
    ]


printSudoku :: Sudoku -> IO () -- Print the Sudoku grid
printSudoku = mapM_ (putStrLn . concatMap showRow)


showRow :: Maybe Int -> String -- Show a row in the Sudoku grid
showRow Nothing  = " ."
showRow (Just n) = ' ' : show n


getSubgrid :: Sudoku -> Int -> Int -> [Maybe Int] -- Get 3x3 subgrid
getSubgrid grid row col =
    [grid !! r !! c | r <- [startRow..endRow], c <- [startCol..endCol]]
    where
        startRow = (row `div` 3) * 3
        endRow = startRow + 2
        startCol = (col `div` 3) * 3
        endCol = startCol + 2


isValid :: Sudoku -> Int -> Int -> Maybe Int -> Bool -- Check if a value is valid in a given position
isValid grid row col num =
    notElem num (grid !! row) &&
    notElem num (transpose grid !! col) &&
    notElem num (getSubgrid grid row col)


findEmpty :: Sudoku -> Maybe (Int, Int) -- Find the first empty position in the Sudoku grid
findEmpty grid = listToMaybe [(r, c) | r <- [0..size-1], c <- [0..size-1], isNothing (grid !! r !! c)]


main :: IO ()
main = do
  putStrLn "Original Sudoku:"
  printSudoku exampleSudoku





