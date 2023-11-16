import Data.List
import Data.Maybe

type Sudoku = [[Maybe Int]]

size :: Int
size = 9

-- Example Sudoku grid
exampleSudoku :: Sudoku 
exampleSudoku =
   [ [Nothing, Nothing, Nothing, Just 6, Nothing, Nothing,  Just 4, Nothing, Nothing]
    , [Just 7, Nothing, Nothing, Nothing, Nothing, Just 3, Just 6, Nothing, Nothing]
    , [Nothing, Nothing, Nothing, Nothing, Just 9, Just 1, Nothing, Just 8, Nothing]
    , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
    , [Nothing, Just 5, Nothing, Just 1, Just 8, Nothing, Nothing, Nothing, Just 3]
    , [Nothing, Nothing, Nothing, Just 3, Nothing, Just 6, Nothing, Just 4, Just 5]
    , [Nothing, Just 4, Nothing, Just 2, Nothing, Nothing, Nothing, Just 6, Nothing]
    , [Just 9, Nothing, Just 3, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
    , [Nothing, Just 2, Nothing, Nothing, Nothing,Nothing, Just 1, Nothing, Nothing]
    ]


-- Print the Sudoku grid
printSudoku :: Sudoku -> IO () 
printSudoku = mapM_ (putStrLn . concatMap showRow)


-- Show a row in the Sudoku grid
showRow :: Maybe Int -> String 
showRow Nothing  = " ."
showRow (Just n) = ' ' : show n


-- Get 3x3 subgrid
getSubgrid :: Sudoku -> Int -> Int -> [Maybe Int] 
getSubgrid grid row col =
    [grid !! r !! c | r <- [startRow..endRow], c <- [startCol..endCol]]
    where
        startRow = (row `div` 3) * 3
        endRow = startRow + 2
        startCol = (col `div` 3) * 3
        endCol = startCol + 2


-- Check if a value is valid in a given position
isValid :: Sudoku -> Int -> Int -> Maybe Int -> Bool 
isValid grid row col num =
    notElem num (grid !! row) &&
    notElem num (transpose grid !! col) &&
    notElem num (getSubgrid grid row col)


-- Find the first empty position in the Sudoku grid
findEmpty :: Sudoku -> Maybe (Int, Int) 
findEmpty grid = listToMaybe [(r, c) | r <- [0..size-1], c <- [0..size-1], isNothing (grid !! r !! c)]


-- Set a value in a given position in the Sudoku grid
setElem :: Sudoku -> Int -> Int -> Maybe Int -> Sudoku 
setElem xs x y val = take x xs ++ [take y (xs !! x) ++ [val] ++ drop (y + 1) (xs !! x)] ++ drop (x + 1) xs


-- Solve the Sudoku by backtracking
solveSudoku :: Sudoku -> Maybe Sudoku 
solveSudoku grid = case findEmpty grid of
    Nothing -> Just grid
    Just (row, col) -> solveNext grid row col


-- Try values for an empty cell
solveNext :: Sudoku -> Int -> Int -> Maybe Sudoku 
solveNext grid row col = case [result | num <- [1..size], 
                                        isValid grid row col (Just num),
                                        result <- [solveSudoku (setElem grid row col (Just num))],
                                        isJust result] of
                            [] -> Nothing
                            (x:_) -> x


main :: IO ()
main = do
    putStrLn "Sudoku to solve:"
    printSudoku exampleSudoku
    putStrLn "\nSolved Sudoku:"
    case solveSudoku exampleSudoku of
        Just solution -> printSudoku solution
        Nothing -> putStrLn "No solution found."
