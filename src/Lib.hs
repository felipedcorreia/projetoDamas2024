module Lib
    ( someFunc
    ) where
import Data.List

data Content = Black | White | Empty | WhiteDama | BlackDama deriving (Eq, Read)

instance Show Content where
    show Black = " ● "
    show White = " ○ "
    show Empty = "   "
    show BlackDama = " ⦿ "
    show WhiteDama = " ⓞ "

type Coord = (Int, Int)
type Cell = (Content, String, Coord)  -- (Conteudo, Cor, Coordenada)

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
showCell (content, color, _) = color ++ show content ++ "\x1b[0m"

-- Função para mostrar o tabuleiro
showBoard :: Board -> IO ()
showBoard board = do
    let letters = "01234567"
    let numberedRows = zip [1..] board
    putStrLn "   0  1  2  3  4  5  6  7"
    putStr (unlines (map (showRow letters) numberedRows))
  where
    showRow letters (rowNum, row) = letters !! (rowNum - 1) : " " ++ concatMap (\cell -> showCell cell ++ "") row

-- Função para capturar as coordenadas
getCoord :: String -> IO Coord
getCoord prompt = do
    putStrLn prompt
    input <- getLine
    let [r, c] = map read (words input) :: [Int]
    return (r, c)

-- Função para mover uma peça
movePiece :: Board -> Coord -> Coord -> Board
movePiece board (r1, c1) (r2, c2) =
    if isValidMove (r1, c1) (r2, c2)
    then [ [ updateCell r c | c <- [0..7] ] | r <- [0..7] ]
    else board
  where
    (content, color, coord) = board !! r1 !! c1
    updateCell r c
        | (r, c) == (r1, c1) = (Empty, color, coord)
        | (r, c) == (r2, c2) = (content, color, (r2, c2))
        | otherwise = board !! r !! c

isValidMove :: Coord -> Coord -> Bool
isValidMove (r1, c1) (r2, c2) =
    r2 >= 0 && r2 < 8 && c2 >= 0 && c2 < 8 

someFunc :: IO ()
someFunc = do
    let newBoard = createBoard 8
    gameLoop newBoard

gameLoop :: Board -> IO ()
gameLoop board = do
    showBoard board
    start <- getCoord "Digite as coordenadas da peça que deseja mover (linha coluna):"
    end <- getCoord "Digite as coordenadas para onde deseja mover (linha coluna):"
    let newBoard = movePiece board start end
    gameLoop newBoard
