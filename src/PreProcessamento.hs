module PreProcessamento (tokenizarVerso) where

import Data.Char (toLower, isAlpha)


tokenizarVerso :: String -> [String]
tokenizarVerso verso = words (map tratarCaracter verso)
  where
    tratarCaracter c
      | isAlpha c = toLower c
      -- MUDANÇA AQUI: Hífen agora vira espaço para separar clíticos (canta-me -> canta me)
      | c == '-'  = ' ' 
      | otherwise = ' '