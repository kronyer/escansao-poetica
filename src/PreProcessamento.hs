module PreProcessamento (tokenizarVerso) where

import Data.Char (toLower, isAlpha)


tokenizarVerso :: String -> [String]
tokenizarVerso verso = words (map tratarCaracter verso)
  where
    tratarCaracter c
      | isAlpha c = toLower c
      | c `elem` "-–" = ' '       -- Hífen vira espaço
      | c == ','      = ' '       -- VÍRGULA VIRA ESPAÇO (Permite sinalefa!)
      | c `elem` ".!?;:" = c      -- PONTUAÇÃO FORTE FICA (Cria barreira)
      | otherwise = ' '