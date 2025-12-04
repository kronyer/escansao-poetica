module Escansao (escandirVerso, VersoEscandido) where

import Tipos (Som(..))
import AnaliseLexical (classificar)
import PreProcessamento (tokenizarVerso)
import Silabacao (separarSilabas)
import Acentuacao (identificarIndiceTonica)
import Data.Char (toLower)
import Data.List (isSuffixOf) -- Import necessário para detectar o final "à*"

-- O resultado final é uma lista de sílabas poéticas (strings)
type VersoEscandido = [String]

-- Estrutura auxiliar para guardar a palavra processada
data PalavraProcessada = PalavraProcessada
    { silabas :: [String]
    , idxTonica :: Int
    } deriving (Show)

-- | Função Principal
escandirVerso :: String -> VersoEscandido
escandirVerso versoRaw =
    let
        -- 1. Limpa e separa palavras
        listaPalavrasStr = tokenizarVerso versoRaw
        
        -- 2. Analisa cada palavra individualmente (silabação + tônica)
        palavrasAnalisadas = map analisarPalavra listaPalavrasStr
        
        -- 3. Aplica o CORTE (descarta sílabas após a tônica da última palavra)
        palavrasCortadas = aplicarCorteFinal palavrasAnalisadas
        
        -- 4. Aplica SINALEFA (une final de uma com começo da outra)
        versoComSinalefa = aplicarSinalefa palavrasCortadas

        -- 5. resolver choques tonicos
        versoFinal = resolverChoqueAcentos versoComSinalefa
    in
        versoFinal

-- | Auxiliar: Transforma string "casa" em PalavraProcessada
analisarPalavra :: String -> PalavraProcessada
analisarPalavra s = 
    let sils = separarSilabas s
        idx  = identificarIndiceTonica sils
        
        -- Insere o '*' na sílaba tônica
        silsMarcadas = zipWith (\i sil -> if i == idx then sil ++ "*" else sil) [0..] sils
        
    in PalavraProcessada { silabas = silsMarcadas, idxTonica = idx }


-- | Remove o acento da sílaba anterior se houver duas tônicas seguidas
resolverChoqueAcentos :: [String] -> [String]
resolverChoqueAcentos [] = []
resolverChoqueAcentos [x] = [x]
resolverChoqueAcentos (s1:s2:resto)
    | temEstrela s1 && temEstrela s2 = removerEstrela s1 : resolverChoqueAcentos (s2:resto)
    | otherwise                      = s1 : resolverChoqueAcentos (s2:resto)
    where
        temEstrela s = not (null s) && last s == '*'
        removerEstrela s = init s -- Remove o '*' do final

-- | Regra do Corte: Remove as sílabas pós-tônicas da ÚLTIMA palavra
aplicarCorteFinal :: [PalavraProcessada] -> [PalavraProcessada]
aplicarCorteFinal [] = []
aplicarCorteFinal [ultima] = 
    let sils = silabas ultima
        tonica = idxTonica ultima
        -- Mantém apenas até a tônica (take (tonica + 1))
        novasSilabas = take (tonica + 1) sils
        -- Nota: Se cortamos, a tônica passa a ser a última sílaba desta palavra.
    in [ultima { silabas = novasSilabas }]
aplicarCorteFinal (p:ps) = p : aplicarCorteFinal ps

-- | LÓGICA DE SINALEFA REFINADA
aplicarSinalefa :: [PalavraProcessada] -> [String]
aplicarSinalefa [] = []
aplicarSinalefa [p] = silabas p
aplicarSinalefa (p1:p2:resto) =
    let 
        sils1 = silabas p1
        sils2 = silabas p2
        ultimaSilabaP1 = last sils1
        primeiraSilabaP2 = head sils2
        
        -- Verifica se P1 é átona (não tem '*')
        p1EhAtona = not (null ultimaSilabaP1) && last ultimaSilabaP1 /= '*'
        
        -- Verifica se é CRASE (à* ou ás*) ou termina em crase (do-à*)
        -- Se for crase, permitimos a fusão mesmo sendo tônica!
        ehCrase = "à*" `isSuffixOf` ultimaSilabaP1 || "ás*" `isSuffixOf` ultimaSilabaP1
        
        -- Encontros vocálicos
        terminaVogal = classificar (last (filter (/= '*') ultimaSilabaP1)) == Vogal
        comecaVogal  = classificar (head primeiraSilabaP2) == Vogal
        
        -- Regra de União: Átona+Vogal OU Crase+Vogal
        deveUnir = (p1EhAtona || ehCrase) && terminaVogal && comecaVogal
    in
        if deveUnir then
            let 
                inicioP1 = init sils1
                
                -- TRUQUE DE MESTRE:
                -- Se estamos unindo, removemos o acento da anterior (mesmo se for crase).
                -- Isso evita o "Stress Clash" com a próxima palavra e permite tripla sinalefa (do-à-e).
                p1Limpa = filter (/= '*') ultimaSilabaP1
                fusao = p1Limpa ++ "-" ++ primeiraSilabaP2
                
                -- A nova palavra P2 herda a fusão na cabeça
                novaP2 = p2 { silabas = fusao : tail sils2 } 
            in
                inicioP1 ++ aplicarSinalefa (novaP2 : resto)
        else
            sils1 ++ aplicarSinalefa (p2 : resto)