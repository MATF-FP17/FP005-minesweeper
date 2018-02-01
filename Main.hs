module Main(main) where

import Graphics.Gloss
import System.Random
import Data.List as L
import Graphics.Gloss.Interface.Pure.Game as G
import Data.Fixed
import Graphics.Image


-- stanje table
data Board = Board
  { clicked :: [Int]
  , unclicked :: [Int]
  , gameState :: Game
  }
  
-- stanje igre  
data Game = Playing
	  | Won
	  | Lost
	  | Ended
	  deriving Eq
	  
-- pocetno stanje table - nijedno polje nije kliknuto
initialState :: Board
initialState = Board [] [0..80] Playing

-- iscrtavanje table i polja - za svaki element liste "clicked" iscrtava se odgovarajuca slika u zavisnosti od numbers
drawBoard :: RandomGen g => g -> [Picture] -> Board -> Picture
drawBoard g images board
    | ((gameState board) == Won) = Pictures $ grid : tiles ++ [images !! 9]
    | ((gameState board) == Lost) = Pictures $ grid : tiles ++ [images !! 10]
    | otherwise = Pictures $ grid : tiles
  where
   tiles = L.map (drawTile g images) $ clicked board
   grid = pictures [G.translate i j $ color orange $ rectangleWire 50 50 | i <- [-200,-150..200], j <- [-200,-150..200]]

drawTile :: RandomGen g => g -> [Picture] -> Int -> Picture
drawTile g images index
  | image == 0 = pictures [G.translate (-200.0 + 50.0 * fromIntegral(index - (index `div` 9)*9)) (-200.0 + 50.0 * fromIntegral(index `div` 9)) $ color (light (light orange)) $ rectangleSolid 50 50]
  | otherwise = pictures [G.translate (-200.0 + 50.0 * fromIntegral(index - (index `div` 9)*9)) (-200.0 + 50.0 * fromIntegral(index `div` 9)) $ (images !! (image-1))]   
  where image = head [number | (ind, number) <- (numbers g), (ind == index)]
    
-- funkcija koja na osnovu koordinata racuna odgovarajuci indeks u listi polja
coordinatesToIndex :: Float -> Float -> Int
coordinatesToIndex x y = let k = (x+225) `div'` 50
			     l = (y+225) `div'` 50
			 in l*9+k
  
-- funkcija za obradu levog klika misa
handleKeys :: RandomGen g => g -> Event -> Board -> Board
handleKeys g (EventKey (MouseButton LeftButton) Down _ (x,y)) board = changeState g (coordinatesToIndex x y) board
handleKeys g _ board = board

-- promena stanja kada dodje do klika
changeState :: RandomGen g => g -> Int -> Board -> Board
changeState g tile board 
  | checkIfElem tile (mineTiles 0 80 g) && (gameState board /= Won) == True = board { clicked = [0..80], unclicked = [], gameState = Lost }
  | (length $ unclicked board) == 11 = board { clicked = [0..80], unclicked = [], gameState = Won }
  | checkIfElem tile (unclicked board) == True = board { clicked = tile : clicked board, unclicked = delete tile $ unclicked board, gameState = Playing }
  | otherwise = board { clicked = clicked board, unclicked = unclicked board, gameState = gameState board }
  
window :: Display
window = InWindow "Minesweeper" (450, 450) (10, 10)

fps :: Int
fps = 60

background :: Color
background = light orange
  
  
main :: IO ()
main = do
  g <- getStdGen
  print "Indeksi polja gde su mine:"
  print $ mineTiles 0 80 g
  print "Matrica:"
  print $ numbers g
  img1 <- loadBMP "images/1.bmp"
  img2 <- loadBMP "images/2.bmp"
  img3 <- loadBMP "images/3.bmp"
  img4 <- loadBMP "images/4.bmp"
  img5 <- loadBMP "images/5.bmp"
  img6 <- loadBMP "images/6.bmp"
  img7 <- loadBMP "images/7.bmp"
  img8 <- loadBMP "images/8.bmp"
  img9 <- loadBMP "images/9.bmp"
  won <- loadBMP "images/won.bmp"
  lost <- loadBMP "images/lost.bmp"
  play window background fps initialState (drawBoard g [img1, img2, img3, img4, img5, img6, img7, img8, img9, won, lost]) (handleKeys g) (flip const)
  
  
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
indicesAdjacentToMines g = concat (L.map (adjacentTilesIndices) (mineTiles 0 80 g))

-- broj pojavljivanja elementa u listi
count :: Eq a => a -> [a] -> Int
count x xs = foldl (\acc y -> if x == y then acc+1 else acc) 0 xs

-- lista sa rasporedjenim odgovarajucim brojevima i brojem 9 na mestima gde su mine
numbers :: RandomGen g => g -> [(Int, Int)]
numbers g = sort ((zip ([0..] \\ (mineTiles 0 80 g)) [count x (indicesAdjacentToMines g) | x <- ([0..80] \\ (mineTiles 0 80 g))]) `union` (zip (mineTiles 0 80 g) [9,9..]))
