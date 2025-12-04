module Escansao (escandirVerso, VersoEscandido) where

import Tipos (Som(..))
import AnaliseLexical (classificar)
import PreProcessamento (tokenizarVerso)
import Silabacao (separarSilabas)
import Acentuacao (identificarIndiceTonica)
import Data.Char (toLower)

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
        versoFinal = aplicarSinalefa palavrasCortadas
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

-- | Regra da Sinalefa: Percorre as palavras e une se necessário
aplicarSinalefa :: [PalavraProcessada] -> [String]
aplicarSinalefa [] = []
aplicarSinalefa [p] = silabas p -- Só uma palavra? Retorna suas sílabas.
aplicarSinalefa (p1:p2:resto) =
    let 
        sils1 = silabas p1
        sils2 = silabas p2
        
        ultimaSilabaP1 = last sils1
        primeiraSilabaP2 = head sils2
        
        -- Verificações para Sinalefa
        -- 1. A última da P1 é átona? (Índice da última != Índice da Tônica)
        p1TerminaAtona = (length sils1 - 1) /= idxTonica p1
        
        -- 2. Encontro vocálico? (Termina com Vogal + Começa com Vogal)
        -- Nota: Usamos a função 'classificar' do seu léxico
        terminaVogal = classificar (last ultimaSilabaP1) == Vogal
        comecaVogal  = classificar (head primeiraSilabaP2) == Vogal
        
        deveUnir = p1TerminaAtona && terminaVogal && comecaVogal
    in
        if deveUnir then
            -- UNE: Pega todas as sílabas de P1 menos a última
            -- E cria uma nova sílaba fundida (ultimaP1 + "-" + primeiraP2)
            -- E continua recursivamente tratando a P2 modificada como a nova "cabeça"
            let 
                inicioP1 = init sils1
                fusao = ultimaSilabaP1 ++ "-" ++ primeiraSilabaP2 -- Usamos '-' para marcar visualmente
                
                -- Criamos uma nova "P2" artificial que começa com a fusão
                novaP2 = p2 { silabas = fusao : tail sils2 } 
            in
                inicioP1 ++ aplicarSinalefa (novaP2 : resto)
        else
            -- NÃO UNE: Segue normal
            sils1 ++ aplicarSinalefa (p2 : resto)