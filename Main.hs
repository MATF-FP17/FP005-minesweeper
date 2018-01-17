module Main(main) where

import Graphics.Gloss
import System.Random
import Data.List
import Graphics.Gloss.Interface.Pure.Game
import Data.Fixed

data Board = Board
  { clicked :: [Int]
  , unclicked :: [Int]
}

-- pocetno stanje table - nijedno polje nije kliknuto
initialState :: Board
initialState = Board [] [0..80]

-- funkcija za iscrtavanje table - trebalo bi proci kroz listu "clicked" i iscrtati odgovarajuce slike u zavisnosti od numbers
drawBoard :: Board -> Picture
drawBoard board = Pictures $ grid : tiles where
   tiles = map (drawTile) $ clicked board
   grid = pictures [translate i j $ color blue $ rectangleWire 50 50 | i <- [-200,-150..200], j <- [-200,-150..200]]

drawTile :: Int -> Picture
drawTile index = pictures [translate (-200.0 + 50.0 * fromIntegral(index - (index `div` 9)*9)) (-200.0 + 50.0 * fromIntegral(index `div` 9)) $ color red $ rectangleSolid 50 50]
  
-- funkcija koja na osnovu koordinata racuna odgovarajuci indeks u listi polja
coordinatesToIndex :: Float -> Float -> Int
coordinatesToIndex x y = let k = (x+200) `div'` 50
			     l = (y+200) `div'` 50
			 in l*9+k
  
-- funkcija za obradu levog klika misa
handleKeys :: Event -> Board -> Board
handleKeys (EventKey (MouseButton LeftButton) Down _ (x,y)) board = changeState (coordinatesToIndex x y) board
handleKeys _ board = board

-- promena stanja kada dodje do klika
changeState :: Int -> Board -> Board
changeState tile board = board { clicked = tile : clicked board, unclicked = delete tile $ unclicked board}
  
window :: Display
window = InWindow "Minesweeper" (450, 450) (10, 10)

fps :: Int
fps = 60

background :: Color
background = light (light blue)

main :: IO ()
main = do
  g <- newStdGen
  one <- loadBMP "1.bmp"
  print "Indeksi polja gde su mine:"
  print $ mineTiles 0 81 g
  print "Matrica:"
  print $ numbers g
  --print $ coordinatesToIndex (2.00) (2.00)
  play window background fps initialState drawBoard handleKeys (flip const)
  
  
-- generisanje 10 nasumicnih brojeva (indeksa gde ce biti smestene mine)
mineTiles :: RandomGen g => Int -> Int -> g -> [Int]
mineTiles a b g = take 10 $ nub (randomRs (a, b) g)

-- provera da li je broj element liste
checkIfElem :: Int -> [Int] -> Bool
checkIfElem x l 
         | x `elem` l = True
         | otherwise = False

-- indeksi susednih polja
adjacentTilesIndices :: Int -> [Int]
adjacentTilesIndices index
  | index == 0 = [1, 9, 10]
  | index == 8 = [7, 16, 17]
  | index == 72 = [63, 64, 73]
  | index == 80 = [70, 71, 79]
  | index < 9 = [index - 1, index + 1, index + 8, index + 9, index + 10]
  | index `mod` 9 == 0 = [index - 9, index - 8, index + 1, index + 9, index + 10]
  | index `mod` 9 == 8 = [index - 10, index - 9, index - 1, index + 8, index + 9]
  | index > 72 = [index - 10, index - 9, index - 8, index - 1, index + 1]
  | otherwise = [index - 10, index - 9, index - 8, index - 1, index + 1, index + 8, index + 9, index + 10]

-- lista indeksa susednih poljima gde su mine
indicesAdjacentToMines :: RandomGen g => g -> [Int]
indicesAdjacentToMines g = concat (map (adjacentTilesIndices) (mineTiles 0 81 g))

-- broj pojavljivanja elementa u listi
count :: Eq a => a -> [a] -> Int
count x xs = foldl (\acc y -> if x == y then acc+1 else acc) 0 xs

-- lista sa rasporedjenim odgovarajucim brojevima i brojem 9 na mestima gde su mine
numbers :: RandomGen g => g -> [(Int, Int)]
numbers g = sort ((zip ([0..] \\ (mineTiles 0 81 g)) [count x (indicesAdjacentToMines g) | x <- ([0..80] \\ (mineTiles 0 81 g))]) `union` (zip (mineTiles 0 81 g) [9,9..]))


-- 0  1  2  3  4  5  6  7  8
-- 9 10 11 12 13 14 15 16 17
--18 19 20 21 22 23 24 25 26
--27 28 29 30 31 32 33 34 35
--36 37 38 39 40 41 42 43 44
--45 46 47 48 49 50 51 52 53
--54 55 56 57 58 59 60 61 62
--63 64 65 66 67 68 69 70 71
--72 73 74 75 76 77 78 79 80