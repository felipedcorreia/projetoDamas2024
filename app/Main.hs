module Main (main) where

import Lib

main :: IO ()
main = do
    let board = createBoard 8
    gameLoop board Player1
