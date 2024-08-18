module Lib (gameLoop, createBoard, Player(..)) where

import Data.List
import Data.Maybe (fromJust, isNothing)

data Content = Black | White | Empty | WhiteDama | BlackDama deriving (Eq, Read)
instance Show Content where
    show Black = " ● "
    show White = " ○ "
    show Empty = "   "
    show BlackDama = " ⦿ "
    show WhiteDama = " ⓞ "

type Coord = (Int, Int)
type Cell = (Content, String, Coord)
type Board = [[Cell]]

-- Estrutura Jogador
data Player = Player1 | Player2 deriving (Eq, Show)

-- Função para criar uma célula do tabuleiro
createCell :: Int -> Int -> Content -> Cell
createCell row column content = (content, if even (row + column) then "\x1b[41m" else "\x1b[44m", (row, column))

-- Função para criar uma célula vazia
emptyCell :: Int -> Int -> Cell
emptyCell row column = createCell row column Empty

-- Função para inicializar as células do tabuleiro com as peças
initialSetCell :: Int -> Int -> Cell
initialSetCell row column
    | row < 3 && even (row + column) = createCell row column Black
    | row > 4 && even (row + column) = createCell row column White
    | otherwise = emptyCell row column

-- Função para criar o tabuleiro inicial
createBoard :: Int -> Board
createBoard size = [[initialSetCell row column | column <- [0..size-1]] | row <- [0..size-1]]

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
        newCell = (newContent, snd3 cell, thd3 cell)
    in beforeRow ++ [beforeCell ++ [newCell] ++ afterCell] ++ afterRow
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
    readCoord input = let [row, column] = map read (words input) in (row, column)

-- Função para verificar se a jogada é válida
isValidMove :: Board -> Player -> Coord -> Coord -> Bool
isValidMove board player (fromX, fromY) (toX, toY) =
    let (content, _, _) = board !! fromX !! fromY
        targetContent = fst3 (board !! toX !! toY)
        isForward = case player of
            Player1 -> fromX > toX  -- Player1 (peças brancas) deve se mover "para cima" no tabuleiro
            Player2 -> fromX < toX  -- Player2 (peças pretas) deve se mover "para baixo" no tabuleiro
        isOneStepMove = abs (fromX - toX) == 1 && abs (fromY - toY) == 1
        isCaptureMove = abs (fromX - toX) == 2 && abs (fromY - toY) == 2
        middlePiece = if isCaptureMove then fst3 (board !! ((fromX + toX) `div` 2) !! ((fromY + toY) `div` 2)) else Empty
        isOpponentPiece = case player of
            Player1 -> middlePiece == Black || middlePiece == BlackDama
            Player2 -> middlePiece == White || middlePiece == WhiteDama
    in case content of
        Empty -> False  -- Não há peça para mover
        White -> isForward && (isOneStepMove && targetContent == Empty || isCaptureMove && targetContent == Empty && isOpponentPiece)
        Black -> isForward && (isOneStepMove && targetContent == Empty || isCaptureMove && targetContent == Empty && isOpponentPiece)
        WhiteDama -> (isOneStepMove || isCaptureMove && isOpponentPiece) && targetContent == Empty
        BlackDama -> (isOneStepMove || isCaptureMove && isOpponentPiece) && targetContent == Empty
  where
    fst3 (x, _, _) = x


-- Função principal para iniciar o jogo
gameLoop :: Board -> Player -> IO ()
gameLoop board player = do
    showBoard board
    (from, to) <- getPlayerMove player
    if isValidMove board player from to
        then do
            let board' = movePiece board from to
            gameLoop board' (nextPlayer player)
        else do
            putStrLn "Movimento inválido! Tente novamente."
            gameLoop board player
  where
    nextPlayer Player1 = Player2
    nextPlayer Player2 = Player1

-- Função inicial
main :: IO ()
main = do
    let board = createBoard 8
    gameLoop board Player1
