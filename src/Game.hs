module Game (gameLoop) where

import Board (Board, Coord, showBoard, movePiece)
import Player (Player(..), nextPlayer)
import Content (Content(..))

-- | Solicita a jogada do jogador.
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

-- | Verifica se a jogada é válida de acordo com as regras do jogo.
isValidMove :: Board -> Player -> Coord -> Coord -> Bool
isValidMove board player (fromX, fromY) (toX, toY) =
    let (content, _, _) = board !! fromX !! fromY
        targetContent = fst3 (board !! toX !! toY)
        isForward = case player of
            PlayerOne -> fromX > toX  -- PlayerOne (peças brancas) deve se mover "para cima" no tabuleiro.
            PlayerTwo -> fromX < toX  -- PlayerTwo (peças pretas) deve se mover "para baixo" no tabuleiro.
        isOneStepMove = abs (fromX - toX) == 1 && abs (fromY - toY) == 1
        isCaptureMove = abs (fromX - toX) == 2 && abs (fromY - toY) == 2
        middlePiece = if isCaptureMove then fst3 (board !! ((fromX + toX) `div` 2) !! ((fromY + toY) `div` 2)) else Empty
        isOpponentPiece = case player of
            PlayerOne -> middlePiece == Black || middlePiece == BlackDama
            PlayerTwo -> middlePiece == White || middlePiece == WhiteDama
    in case content of
        Empty -> False  -- Não há peça para mover.
        White -> isForward && (isOneStepMove && targetContent == Empty || isCaptureMove && targetContent == Empty && isOpponentPiece)
        Black -> isForward && (isOneStepMove && targetContent == Empty || isCaptureMove && targetContent == Empty && isOpponentPiece)
        WhiteDama -> (isOneStepMove || (isCaptureMove && isOpponentPiece)) && targetContent == Empty
        BlackDama -> (isOneStepMove || (isCaptureMove && isOpponentPiece)) && targetContent == Empty
  where
    fst3 (x, _, _) = x

-- | Função principal que controla o loop do jogo.
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
