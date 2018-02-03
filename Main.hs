module Main(main) where

import Graphics.Gloss
import System.Random
import Data.List
import Graphics.Gloss.Interface.Pure.Game
import Data.Fixed


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
	  deriving Eq
	  

-- dimenzija table
dimension :: Int
dimension = 9

-- indeks poslednjeg polja
lastTileIndex :: Int
lastTileIndex = dimension * dimension - 1

-- broj mina
numOfMines :: Int
numOfMines = 10

-- dimenzija prozora
dimOfWindow :: Int
dimOfWindow = dimension * 50

-- pocetno stanje table - nijedno polje nije kliknuto
initialState :: Board
initialState = Board [] [0..lastTileIndex] Playing

-- iscrtavanje table i polja - za svaki element liste "clicked" iscrtava se odgovarajuca slika u zavisnosti od numbers
drawBoard :: RandomGen g => g -> [Picture] -> Board -> Picture
drawBoard g images board
    | ((gameState board) == Won) = Pictures $ grid : tiles ++ [images !! 9]
    | ((gameState board) == Lost) = Pictures $ grid : tiles ++ [images !! 10]
    | otherwise = Pictures $ grid : tiles
  where
   tiles = map (drawTile g images) $ clicked board
   grid = pictures [translate i j $ color orange $ rectangleWire 50 50 | i <- [-fromIntegral((dimOfWindow - 50) `div` 2),-fromIntegral((dimOfWindow - 50) `div` 2) + 50..fromIntegral((dimOfWindow - 50) `div` 2)], j <- [-fromIntegral((dimOfWindow - 50) `div` 2),-fromIntegral((dimOfWindow - 50) `div` 2) + 50..fromIntegral((dimOfWindow - 50) `div` 2)]]

drawTile :: RandomGen g => g -> [Picture] -> Int -> Picture
drawTile g images index
  | image == 0 = pictures [translate (-fromIntegral((dimOfWindow - 50) `div` 2) + 50.0 * fromIntegral(index - (index `div` dimension) * dimension)) (-fromIntegral((dimOfWindow - 50) `div` 2) + 50.0 * fromIntegral(index `div` dimension)) $ color (light (light orange)) $ rectangleSolid 50 50]
  | otherwise = pictures [translate (-fromIntegral((dimOfWindow - 50) `div` 2) + 50.0 * fromIntegral(index - (index `div` dimension) * dimension)) (-fromIntegral((dimOfWindow - 50) `div` 2) + 50.0 * fromIntegral(index `div` dimension)) $ (images !! (image - 1))]   
  where image = head [number | (ind, number) <- (numbers g), (ind == index)]
    
-- funkcija koja na osnovu koordinata racuna odgovarajuci indeks u listi polja
coordinatesToIndex :: Float -> Float -> Int
coordinatesToIndex x y = let k = (x + fromIntegral(dimOfWindow `div` 2)) `div'` 50
			     l = (y + fromIntegral(dimOfWindow `div` 2)) `div'` 50
			 in l * dimension + k
  
-- funkcija za obradu levog klika misa
handleKeys :: RandomGen g => g -> Event -> Board -> Board
handleKeys g (EventKey (MouseButton LeftButton) Down _ (x,y)) board = changeState g (coordinatesToIndex x y) board
handleKeys g _ board = board

-- promena stanja kada dodje do klika
changeState :: RandomGen g => g -> Int -> Board -> Board
changeState g tile board 
  -- slucaj da je kliknuto polje na kome je mina (sva polja prelaze u "clicked" da bi se prikazala cela tabla)
  | checkIfElem tile (mineTiles 0 lastTileIndex g) && (gameState board /= Won) == True = board { clicked = [0..lastTileIndex], unclicked = [], gameState = Lost }
  -- slucaj da su ostala samo polja s minama (pobeda)
  | (length $ unclicked board) == numOfMines + 1 = board { clicked = [0..lastTileIndex], unclicked = [], gameState = Won }
  -- "obican" slucaj
  | checkIfElem tile (unclicked board) == True = board { clicked = tile : clicked board, unclicked = delete tile $ unclicked board, gameState = Playing }
  -- inace, nista se ne menja (kraj igre)
  | otherwise = board { clicked = clicked board, unclicked = unclicked board, gameState = gameState board }
  
window :: Display
window = InWindow "Minesweeper" (dimOfWindow, dimOfWindow) (10, 10)

fps :: Int
fps = 60

background :: Color
background = light orange
  
  
main :: IO ()
main = do
  g <- getStdGen
  print "Indeksi polja gde su mine:"
  print $ mineTiles 0 lastTileIndex g
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
  
-- generisanje nasumicnih brojeva (indeksa gde ce biti smestene mine)
mineTiles :: RandomGen g => Int -> Int -> g -> [Int]
mineTiles a b g = take numOfMines $ nub (randomRs (a, b) g)

-- provera da li je broj element liste
checkIfElem :: Int -> [Int] -> Bool
checkIfElem x l 
         | x `elem` l = True
         | otherwise = False         

-- indeksi susednih polja
adjacentTilesIndices :: Int -> [Int]
adjacentTilesIndices index
  | index == 0 = [1, dimension, dimension + 1]
  | index == dimension - 1 = [dimension - 2, 2 * dimension - 2, 2 * dimension - 1]
  | index == (dimension - 1) * dimension = [(dimension - 2) * dimension, (dimension - 2) * dimension + 1, (dimension - 1) * dimension + 1]
  | index == dimension * dimension - 1 = [dimension * dimension - dimension - 2, dimension * dimension - dimension - 1, dimension * dimension - 2]
  | index < dimension = [index - 1, index + 1, index + dimension - 1, index + dimension, index + dimension + 1]
  | index `mod` dimension == 0 = [index - dimension, index - dimension + 1, index + 1, index + dimension, index + dimension + 1]
  | index `mod` dimension == dimension - 1 = [index - dimension - 1, index - dimension, index - 1, index + dimension - 1, index + dimension]
  | index > (dimension-1)*dimension = [index - dimension - 1, index - dimension, index - dimension + 1, index - 1, index + 1]
  | otherwise = [index - dimension - 1, index - dimension, index - dimension + 1, index - 1, index + 1, index + dimension - 1, index + dimension, index + dimension + 1]

-- lista indeksa susednih poljima gde su mine
indicesAdjacentToMines :: RandomGen g => g -> [Int]
indicesAdjacentToMines g = concat (map (adjacentTilesIndices) (mineTiles 0 lastTileIndex g))

-- broj pojavljivanja elementa u listi
count :: Eq a => a -> [a] -> Int
count x xs = foldl (\acc y -> if x == y then acc + 1 else acc) 0 xs

-- lista sa rasporedjenim odgovarajucim brojevima i brojem 9 na mestima gde su mine
numbers :: RandomGen g => g -> [(Int, Int)]
numbers g = sort ((zip ([0..] \\ (mineTiles 0 lastTileIndex g)) [count x (indicesAdjacentToMines g) | x <- ([0..lastTileIndex] \\ (mineTiles 0 lastTileIndex g))]) `union` (zip (mineTiles 0 lastTileIndex g) [9,9..]))
