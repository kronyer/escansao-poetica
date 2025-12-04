module AnaliseLexical (classificar, Som(..)) where
    -- aqui estou dizendo que classificar é publico, assim como o tipo Som e seus construtores

import Tipos

vogais :: String
vogais = "aeiouAEIOUáéíóúÁÉÍÓÚàèìòùÀÈÌÒÙãẽĩõũÃẼĨÕŨâêîôûÂÊÎÔÛ"

consoantes :: String
consoantes = "bcdfghjklmnpqrstvwxyzçBCDFGHJKLMNPQRSTVWXYZÇ"


classificar :: Char -> Som
classificar c
  | c `elem` vogais     = Vogal
  | c `elem` consoantes = Consoante
  | otherwise           = Outro