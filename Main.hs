type Sudoku = [[Maybe Int]]

size :: Int
size = 9

main :: IO ()
main = do
  putStrLn "Original Sudoku:"
  printSudoku exampleSudoku

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
