module Board (Coord, Cell, Board, createBoard, showBoard, movePiece, replaceElemAt) where

import Content (Content(..))

-- | Coordenadas de uma célula no tabuleiro.
type Coord = (Int, Int)

-- | Uma célula é composta pelo conteúdo (peça), cor de fundo e suas coordenadas.
type Cell = (Content, String, Coord)

-- | O tabuleiro é uma matriz bidimensional de células.
type Board = [[Cell]]

-- | Cria uma célula do tabuleiro.
createCell :: Int -> Int -> Content -> Cell
createCell row column content = (content, if even (row + column) then "\x1b[41m" else "\x1b[44m", (row, column))

-- | Cria uma célula vazia.
emptyCell :: Int -> Int -> Cell
emptyCell row column = createCell row column Empty

-- | Inicializa as células do tabuleiro com as peças na posição inicial.
initialSetCell :: Int -> Int -> Cell
initialSetCell row column
    | row < 3 && even (row + column) = createCell row column Black
    | row > 4 && even (row + column) = createCell row column White
    | otherwise = emptyCell row column

-- | Cria o tabuleiro inicial com o tamanho especificado.
createBoard :: Int -> Board
createBoard size = [[initialSetCell row column | column <- [0..size-1]] | row <- [0..size-1]]

-- | Exibe uma célula do tabuleiro.
showCell :: Cell -> String
showCell (content, color, _) = color ++ show content ++ "\x1b[0m"  -- Reseta a cor após mostrar a célula.

-- | Exibe o tabuleiro no terminal.
showBoard :: Board -> IO ()
showBoard board = do
    let letters = "01234567"
    let numberedRows = zip [1..] board
    putStrLn "   0  1  2  3  4  5  6  7"
    putStr (unlines (map (showRow letters) numberedRows))
  where
    showRow letters (rowNum, row) = letters !! (rowNum - 1) : " " ++ concatMap (\cell -> showCell cell ++ "") row

-- | Move uma peça de uma coordenada para outra no tabuleiro.
movePiece :: Board -> Coord -> Coord -> Board
movePiece board (fromX, fromY) (toX, toY) =
    let piece = fst3 (board !! fromX !! fromY)  -- Obtém a peça na coordenada inicial.
        board' = replaceElemAt board (fromX, fromY) Empty  -- Limpa a célula de origem.
        board'' = if isCaptureMove then capturePiece board' (fromX, fromY) (toX, toY) else board'
    in replaceElemAt board'' (toX, toY) piece  -- Coloca a peça na célula de destino.
  where
    isCaptureMove = abs (fromX - toX) == 2 && abs (fromY - toY) == 2

    -- Função para capturar uma peça do oponente.
    capturePiece :: Board -> Coord -> Coord -> Board
    capturePiece board (fromX, fromY) (toX, toY) =
        let midX = (fromX + toX) `div` 2
            midY = (fromY + toY) `div` 2
        in replaceElemAt board (midX, midY) Empty  -- Remove a peça do oponente da célula intermediária.

-- | Substitui um elemento no tabuleiro em uma coordenada específica.
replaceElemAt :: Board -> Coord -> Content -> Board
replaceElemAt board (r, c) newContent = 
    let (beforeRow, row:afterRow) = splitAt r board
        (beforeCell, cell:afterCell) = splitAt c row
        newCell = (newContent, snd3 cell, thd3 cell)
    in beforeRow ++ [beforeCell ++ [newCell] ++ afterCell] ++ afterRow
  where
    snd3 (_, y, _) = y
    thd3 (_, _, z) = z

-- | Funções auxiliares para trabalhar com tuplas triplas.
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

thd3 :: (a, b, c) -> c
thd3 (_, _, z) = z
