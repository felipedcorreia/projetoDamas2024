module Main where

import Data.List
import Data.Maybe (fromJust, isNothing)

data Content = Black | White | Empty | WhiteDama | BlackDama deriving (Eq, Read)

data Player = Player1 | Player2 deriving (Eq, Show)

instance Show Content where
    show Black = " ● "
    show White = " ○ "
    show Empty = "   "
    show BlackDama = " ⦿ "
    show WhiteDama = " ⓞ "

type Coord = (Int, Int)
type Cell = (Content, String, Coord)
type Board = [[Cell]]

-- Função para criar uma célula do tabuleiro
createCell :: Int -> Int -> Content -> Cell
createCell r c content = (content, if even (r + c) then "\x1b[41m" else "\x1b[44m", (r, c))

-- Função para criar uma célula vazia
emptyCell :: Int -> Int -> Cell
emptyCell r c = createCell r c Empty

-- Função para inicializar as células do tabuleiro com as peças
initialSetCell :: Int -> Int -> Cell
initialSetCell r c
    | r < 3 && even (r + c) = createCell r c Black
    | r > 4 && even (r + c) = createCell r c White
    | otherwise = emptyCell r c

-- Função para criar o tabuleiro inicial
createBoard :: Int -> Board
createBoard size = [[initialSetCell r c | c <- [0..size-1]] | r <- [0..size-1]]

-- Função para mostrar uma célula do tabuleiro
showCell :: Cell -> String
showCell (content, color, _) = color ++ show content ++ "\x1b[0m"  -- Reset da cor

-- Função para mostrar o tabuleiro
showBoard :: Board -> IO ()
showBoard board = do
    let letters = "01234567"
    let numberedRows = zip [1..] board
    putStrLn "   0  1  2  3  4  5  6  7"
    putStr (unlines (map (showRow letters) numberedRows))
  where
    showRow letters (rowNum, row) = letters !! (rowNum - 1) : " " ++ concatMap (\cell -> showCell cell ++ "") row

-- Função para mover uma peça
movePiece :: Board -> Coord -> Coord -> Board
movePiece board (fromX, fromY) (toX, toY) = 
    let piece = fst3 (board !! fromX !! fromY)
        board' = replaceElemAt board (fromX, fromY) Empty
    in replaceElemAt board' (toX, toY) piece
  where
    fst3 (x, _, _) = x

-- Função para substituir um elemento no tabuleiro
replaceElemAt :: Board -> Coord -> Content -> Board
replaceElemAt board (r, c) newContent = 
    let (beforeRow, row:afterRow) = splitAt r board
        (beforeCell, cell:afterCell) = splitAt c row
    in beforeRow ++ [beforeCell ++ [newContent, snd3 cell, thd3 cell] : afterCell] ++ afterRow
  where
    snd3 (_, y, _) = y
    thd3 (_, _, z) = z

-- Função para obter a jogada dos jogadores
getPlayerMove :: Player -> IO (Coord, Coord)
getPlayerMove player = do
    putStrLn $ show player ++ ", insira a posição da peça que deseja mover (linha coluna):"
    from <- getLine
    putStrLn "Insira a posição para onde deseja mover (linha coluna):"
    to <- getLine
    return (readCoord from, readCoord to)
  where
    readCoord :: String -> Coord
    readCoord input = let [r, c] = map read (words input) in (r, c)

-- Função principal para iniciar o jogo
gameLoop :: Board -> Player -> IO ()
gameLoop board player = do
    showBoard board
    (from, to) <- getPlayerMove player
    let board' = movePiece board from to
    gameLoop board' (nextPlayer player)
  where
    nextPlayer Player1 = Player2
    nextPlayer Player2 = Player1

-- Função inicial
main :: IO ()
main = do
    let board = createBoard 8
    gameLoop board Player1

