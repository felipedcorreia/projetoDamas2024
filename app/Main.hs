module Main where

import Board (createBoard)
import Player (Player(..))
import Game (gameLoop)

main :: IO ()
main = do
    let board = createBoard 8
    gameLoop board PlayerOne