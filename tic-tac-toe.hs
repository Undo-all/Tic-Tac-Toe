import Data.List (intersperse)
import Text.Read (readMaybe)

data Square = X | O | Empty deriving Eq

oppSquare :: Square -> Square
oppSquare X = O
oppSquare O = X

instance Show Square where
    show X     = "X"
    show O     = "O"
    show Empty = " "

type Row = [Square]

showRow :: Int -> Row -> String
showRow n xs = 
    (show n) ++ " | " ++ (concat $ intersperse " | " (map show xs)) ++ " |"

type Board = [Row]

showBoard :: Board -> String
showBoard xs =
    "    1   2   3\n" ++ 
    "   ------------\n" ++ 
    (concat $ break $ map (\n -> showRow n (xs !! (n-1))) [1..3]) ++ 
    "\n   ------------"
    where break = intersperse "\n   ------------\n"

-- Determines if a piece is the winner on a board.

horizontalWin :: Square -> Board -> Bool
horizontalWin s xs =
    any (\ys -> filter (==s) ys == ys) xs

verticalWin :: Square -> Board -> Bool
verticalWin s xs = horizontalWin s newBoard
    where newBoard = map (\n -> foldl (\acc ys -> acc ++ [ys !! n]) [] xs) [0..2]

-- I brute-forced this one, it's more efficient
diagonalWin :: Square -> Board -> Bool
diagonalWin s xs = 
    ((xs !! 0 !! 0 == s) && (xs !! 1 !! 1 == s) && (xs !! 2 !! 2 == s)) ||
    ((xs !! 2 !! 0 == s) && (xs !! 1 !! 1 == s) && (xs !! 0 !! 2 == s))

win :: Square -> Board -> Bool
win s xs = (horizontalWin s xs) || 
           (diagonalWin s xs)   || 
           (verticalWin s xs)

-- Puts an X or an O on the board.
-- If the space isn't empty, it 
-- returns an error message. If
-- the index is out of range, it
-- returns a different message.
-- Yes, I know this function is a bit messy.
move :: Square -> (Int, Int) -> Board -> Either String Board
move s (x,y) xs
    | x > 2 || y > 2        = 
        Left "That square doesn't exist."
    | xs !! y !! x /= Empty = 
        Left $ "That square is already taken by " ++ (show $ xs !! y !! x) ++ "."
    | otherwise = Right $ replace y newRow xs
    where newRow = replace x s (xs !! y)
          replace n y (x:xs) 
              | n == 0    = y : xs
              | otherwise = x : replace (n-1) y xs

turn :: Square -> Board -> IO ()
turn s xs = do
    putStr "Enter the square you want to move to: "
    line <- getLine
    let foo = sequence $ map (\x -> readMaybe x :: Maybe Int) $ words line
    let coor =  fmap (\[y,x] -> (x-1,y-1)) foo
    let board = maybe (Left "That's not a square.") (flip (move s) xs) coor
    either (\x -> putStrLn x >> turn s xs) (play (oppSquare s)) board

play :: Square -> Board -> IO ()
play s xs
    | win X xs  = putStrLn "X wins!"
    | win O xs  = putStrLn "O wins!"
    | otherwise = do
        putStrLn $ "\n" ++ (show s) ++ "'s turn.\n"
        putStrLn (showBoard xs)
        turn s xs 

