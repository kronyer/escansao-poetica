module Acentuacao (identificarIndiceTonica) where

import Tipos (Som(..))
import AnaliseLexical (classificar)
import Data.List (findIndex)
import Data.Maybe (fromMaybe)


-- | Verifica se um caractere tem acento gráfico indicador de tonicidade
-- (O til '~' em 'ã'/'õ' conta como tônica se não houver outro acento agudo/circunflexo, 
-- mas 'órfão' tem acento no ó. Para simplificar, assumimos que se tem acento, é tônica).
temAcentoGrafico :: Char -> Bool
temAcentoGrafico c = c `elem` "áâãéêíóôõú"
-- Nota: 'ã' e 'õ' muitas vezes são tônicos (imã), mas às vezes não (órfão). 
-- Vamos tratar 'ã' e 'õ' como indicativos de tônica se não acharmos agudos/circunflexos antes.


-- | Função Principal
-- Recebe a lista de sílabas (ex: ["ca", "sa"]) e retorna o ÍNDICE da tônica (0, 1...)
-- Retorna o índice contando do INÍCIO (0 é a primeira sílaba)
identificarIndiceTonica :: [String] -> Int
identificarIndiceTonica silabas = 
    case encontrarPorAcento silabas of
        Just i  -> i -- Achou acento gráfico, manda bala.
        Nothing -> aplicarRegrasTerminacao silabas


-- | Procura acento gráfico na lista de sílabas
encontrarPorAcento :: [String] -> Maybe Int
encontrarPorAcento =
     findIndex (any temAcentoGrafico)
    -- findIndex retorna Just indice ou Nothing


-- | Aplica regras para palavras sem acento gráfico
aplicarRegrasTerminacao :: [String] -> Int
aplicarRegrasTerminacao silabas
    -- 1. Regra dos Monossílabos Átonos (NOVO!)
    -- Se tem 1 sílaba, termina em a/e/o e NÃO teve acento (já testado antes)... é átona (-1).
    -- Isso cobre: a, o, me, te, se, que, de, da...
    | length silabas == 1 && ehMonossilaboAtono (head silabas) = -1    
    -- 2. Regras Normais
    | null silabas = 0
    | terminaEmParoxitona ultimaPalavra = indicePenultima
    | otherwise                         = indiceUltima
    where
        n = length silabas
        indiceUltima = n - 1
        indicePenultima = if n > 1 then n - 2 else 0
        ultimaPalavra = last silabas

-- | Verifica se um monossílabo é átono (artigos, preposições, conjunções)
-- Cobre: o, a, os, as, de, da, do, das, dos, me, te, se, nos, vos...
ehMonossilaboAtono :: String -> Bool
ehMonossilaboAtono s =
    let 
        -- Remove o 's' final se houver (para tratar 'das', 'os', 'nos')
        sSemS = if last s == 's' && length s > 1 then init s else s
        ultimaLetra = last sSemS
    in 
        -- Se, ao tirar o 's', termina em a/e/o... é átono!
        ultimaLetra `elem` "aeo" || 
        s == "em" || s == "por" || s == "mas" || s == "com"
        -- "em", "por", "mas", "com" são exceções comuns de palavras sem acento e átonas.

-- | Regra: Palavras terminadas em A, E, O, AM, EM, ENS são Paroxítonas
terminaEmParoxitona :: String -> Bool
terminaEmParoxitona s = 
    let 
        -- Remove o 's' final para análise (casas -> casa, imagens -> imagen)
        sLimpa = reverse (dropWhile (== 's') (reverse s))
        
        -- Pega as 2 últimas letras INVERTIDAS
        -- Ex: "souberam" -> sLimpa="souberam" -> reverse="marebuos" -> fim="ma"
        -- Ex: "homem"    -> sLimpa="homem"    -> reverse="memoh"    -> fim="me"
        -- Ex: "hifens"   -> sLimpa="hifen"    -> reverse="nefih"    -> fim="ne"
        fim = if length sLimpa >= 2 then take 2 (reverse sLimpa) else ""
        
        ultimaLetra = last sLimpa
    in 
        ultimaLetra `elem` "aeo" ||   -- Termina em vogal baixa (ca-sa)
        fim == "ma" ||                -- Termina em "am" (sou-be-ram) -> NOVA REGRA
        fim == "me" ||                -- Termina em "em" (ho-mem, nu-vem) -> CORREÇÃO
        fim == "ne"                   -- Termina em "en(s)" (jo-vens)