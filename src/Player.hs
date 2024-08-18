module Player (Player(..), nextPlayer) where

-- | Representa os jogadores do jogo.
data Player = PlayerOne  -- ^ Primeiro jogador (joga com as peças brancas).
            | PlayerTwo  -- ^ Segundo jogador (joga com as peças pretas).
            deriving (Eq, Show)

-- | Alterna entre os jogadores.
nextPlayer :: Player -> Player
nextPlayer PlayerOne = PlayerTwo
nextPlayer PlayerTwo = PlayerOne
