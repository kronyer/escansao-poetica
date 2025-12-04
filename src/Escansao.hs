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

-- | Analisa palavra e marca Tônica (*) e Pausa Forte (#)
analisarPalavra :: String -> PalavraProcessada
analisarPalavra sRaw =
    let
        -- 1. Pausa Forte: Só ! ? . ; : (A vírgula já sumiu no PreProcessamento)
        sinaisFortes = ".!?;:"
        temPausaForte = any (`elem` sinaisFortes) sRaw

        -- 2. Limpa a pontuação para o silabador
        sLimpa = filter (\c -> c /= '#' && not (c `elem` sinaisFortes)) sRaw

        -- 3. Silabação e Tônica
        sils = separarSilabas sLimpa
        idx  = identificarIndiceTonica sils

        -- 4. Marcação (* e #)
        silsMarcadas = zipWith (\i sil ->
            let comEstrela = if i == idx then sil ++ "*" else sil
            in if temPausaForte && i == (length sils - 1) then comEstrela ++ "#" else comEstrela
            ) [0..] sils

    in PalavraProcessada { silabas = silsMarcadas, idxTonica = idx }

-- | Resolve Choque de Acentos (Stress Clash)
resolverChoqueAcentos :: [String] -> [String]
resolverChoqueAcentos [] = []
resolverChoqueAcentos [x] = [limpar x]
resolverChoqueAcentos (s1:s2:resto)
    -- CASO 1: Choque de Acentos (* seguido de *)
    | temEstrela s1 && temEstrela s2 =
        if temBarreira s1
        then 
            -- Com Barreira (!): A pausa reforça a 1ª (6ª sílaba), então a 2ª perde força.
            -- Mantém s1 tônica, torna s2 átona.
            limpar s1 : resolverChoqueAcentos (removerEstrela s2 : resto)
        else 
            -- Sem Barreira: O fluxo corre para a frente. A 1ª perde força.
            -- Torna s1 átona, mantém s2 tônica.
            removerEstrela (limpar s1) : resolverChoqueAcentos (s2:resto)

    -- CASO 2: Apenas Barreira (sem choque)
    | temBarreira s1 = limpar s1 : resolverChoqueAcentos (s2:resto) -- Barreira protege

    -- CASO 3: Normal
    | otherwise = limpar s1 : resolverChoqueAcentos (s2:resto)

-- Funções Auxiliares
temBarreira :: String -> Bool
temBarreira s = '#' `elem` s

temEstrela :: String -> Bool
temEstrela s = '*' `elem` s

limpar :: String -> String
limpar s = filter (/= '#') s 

-- Melhorada para usar filter (mais seguro que init se tiver # e * juntos)
removerEstrela :: String -> String
removerEstrela s = filter (/= '*') s
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

        -- Verifica Barreira (#)
        temBarreira = '#' `elem` ultimaSilabaP1

        -- Verifica se P1 é átona (não tem '*')
        p1EhAtona = not (null ultimaSilabaP1) && last ultimaSilabaP1 /= '*'

        -- Verifica se é CRASE (à* ou ás*) ou termina em crase (do-à*)
        -- Se for crase, permitimos a fusão mesmo sendo tônica!
        ehCrase = "à*" `isSuffixOf` ultimaSilabaP1 || "ás*" `isSuffixOf` ultimaSilabaP1

        -- Encontros vocálicos
        terminaVogal = classificar (last (filter (/= '*') ultimaSilabaP1)) == Vogal
        comecaVogal  = classificar (head primeiraSilabaP2) == Vogal

        -- Regra de União: Átona+Vogal OU Crase+Vogal
        deveUnir = not temBarreira && (p1EhAtona || ehCrase) && terminaVogal && comecaVogal
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