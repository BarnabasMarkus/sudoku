{- SUDOKU SOLVER -}

module Sudoku
  (
  solve
  ) where

import Data.List


board = [ [5,1,7,6,0,0,0,3,4]
        , [2,8,9,0,0,4,0,0,0]
        , [3,4,6,2,0,5,0,9,0]
        , [6,0,2,0,0,0,0,1,0]
        , [0,3,8,0,0,6,0,4,7]
        , [0,0,0,0,0,0,0,0,0]
        , [0,9,0,0,0,0,0,7,8]
        , [7,0,3,4,0,0,5,6,0]
        , [0,0,0,0,0,0,0,0,0]
        ]

type Colx = Int
type Rowx = Int
type Posx = Int
type Board = [[Integer]]

getCol :: Board -> Colx -> [Integer]
getCol board colx = map (!! colx) board

getRow :: Board -> Rowx -> [Integer]
getRow board rowx = board !! rowx

getRange :: Posx -> [Posx]
getRange pos
  | pos < 3 = [0,1,2]
  | pos < 6 = [3,4,5]
  | pos < 9 = [6,7,8]

getBox :: Board -> Colx -> Rowx -> [Integer]
getBox board colx rowx = concat [map (row !!) colRange | row <- rows] 
  where
    rowRange = getRange rowx
    colRange = getRange colx
    rows = map (board !!) rowRange

-- Find possible values [1..9] on a board 
-- at given position (colx,rowx)
possible :: Board -> Colx -> Rowx -> [Integer]
possible board colx rowx = 
  let col = getCol board colx
      row = getRow board rowx
      box = getBox board colx rowx
      taken = nub $ filter (/= 0) $ col ++ row ++ box
  in  [x | x <- [1..9], x `notElem` taken]

-- Find possible values [1..9] on a board
-- at all unfilled position (==0)
allPossible :: Board -> [(Colx, Rowx, [Integer])]
allPossible board = [(colx, rowx, possible board colx rowx)
                      | colx <- [0..8]
                      , rowx <- [0..8]
                      , board !! rowx !! colx == 0]
                      
-- Find all position on board where there is only one
-- exact possible value [1..9]
onePossible :: Board -> [(Colx, Rowx, [Integer])]
onePossible board = filter (\(_,_,options) -> length options == 1) 
                           $ allPossible board

-- True if board is already solved
isSolved :: Board -> Bool
isSolved = not . any (==0) . concat

-- Solve Sudoku board
solve :: Board -> Board
solve board = if isSolved board
              then board
              else solve $ foldl (\board (c,r,[x]) -> update board c r x) board $ onePossible board

-- Update element of board at given position (colx,rowx)
update :: Board -> Colx -> Rowx -> Integer -> Board
update board colx rowx x = changeItem board rowx newRow
  where newRow = changeItem (board !! rowx) colx x

-- Change element (x) in list (xs) at given position (n)
changeItem :: [a] -> Int -> a -> [a]
changeItem xs n x = take n xs ++ [x] ++ drop (n + 1) xs
              
-- TODO:
-- Pretty print Sudoku board
pretty :: Board -> IO ()
pretty board = undefined
