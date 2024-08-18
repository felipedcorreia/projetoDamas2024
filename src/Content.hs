module Content (Content(..)) where

-- | Representa o conteúdo de uma célula no tabuleiro.
data Content = Black      -- ^ Representa uma peça preta.
             | White      -- ^ Representa uma peça branca.
             | Empty      -- ^ Representa uma célula vazia.
             | WhiteDama  -- ^ Representa uma Dama branca.
             | BlackDama  -- ^ Representa uma Dama preta.
             deriving (Eq, Read)

-- | Instância de 'Show' para exibir as peças no terminal.
instance Show Content where
    show Black = " ● "
    show White = " ○ "
    show Empty = "   "
    show BlackDama = " ⦿ "
    show WhiteDama = " ⓞ "