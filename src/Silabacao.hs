module Silabacao (separarSilabas, Dicionario, carregarDicionario) where

import Tipos (Som(..))
import AnaliseLexical (classificar)
import qualified Data.Map as Map
import System.IO (readFile)
import Data.List.Split (splitOn) -- Adicione 'split' ao .cabal se não tiver

-- Um par contendo a Letra e o seu Som correspondente
type LetraSom = (Char, Som)

type Dicionario = Map.Map String [String]


-- | Carrega o arquivo txt para a memória
carregarDicionario :: FilePath -> IO Dicionario
carregarDicionario path = do
    conteudo <- readFile path
    let linhas = lines conteudo
    let pares = map parseLinha linhas
    return $ Map.fromList pares
  where
    parseLinha :: String -> (String, [String])
    parseLinha linha = 
        let [palavra, silabada] = splitOn "=" linha
        in (palavra, splitOn "-" silabada)

-- | Função Principal: Recebe uma palavra limpa e devolve lista de sílabas
separarSilabas :: Dicionario -> String -> [String]
separarSilabas dic palavra = 
    -- 1. Tenta buscar no Dicionário
    case Map.lookup palavra dic of
        Just silabasProntas -> silabasProntas -- ACHOU! Retorna o que está no arquivo.
        Nothing -> 
            -- 2. NÃO ACHOU: Roda o algoritmo algorítmico (Fallback)
            let 
                pares = zip palavra (map classificar palavra)
                silabasPares = quebrar pares
            in 
                map (map fst) silabasPares


-- | Lista de Encontros Consonantais Perfeitos (Inseparáveis)
-- Nota: 'vr', 'tl', 'pn', etc.
clusters :: [String]
clusters = ["bl", "cl", "fl", "gl", "pl", "tl", 
            "br", "cr", "dr", "fr", "gr", "pr", "tr", "vr",
            "ch", "lh", "nh"] -- Já incluímos os dígrafos aqui para facilitar

-- | Verifica se duas letras formam um cluster inseparável
ehCluster :: Char -> Char -> Bool
ehCluster c1 c2 = [c1, c2] `elem` clusters


-- | Define vogais "Fortes" (Baixas/Médias)
-- A, E, O são sempre fortes.
ehForte :: Char -> Bool
ehForte c = c `elem` "aáàãâeéêoóõô" 


-- | Define vogais "Fracas" (Altas) QUE FORMAM DITONGOS
-- I e U são fracas apenas SE NÃO ESTIVEREM ACENTUADAS.
-- Se 'í' ou 'ú' tem acento, elas viram o núcleo da sílaba e se separam.
ehFraca :: Char -> Bool
ehFraca c = c `elem` "iuü"

-- Nova função auxiliar: qualquer vogal com acento tônico (agudo/circunflexo)
ehAcentuada :: Char -> Bool
ehAcentuada c = c `elem` "áéíóúâêô"

ehNasal :: Char -> Bool
ehNasal c = c `elem` "ãõ"


deveSepararVogais :: Char -> Char -> Bool
deveSepararVogais v1 v2
    | v1 == v2 = True                     
    
    -- [NOVA REGRA CRÍTICA]
    -- Se a primeira é nasal (ã, õ) e a segunda é 'e' ou 'o', NÃO SEPARA.
    -- Resolve: ser-tões, pães, pão, mão.
    | ehNasal v1 && v2 `elem` "eo" = False 

    | ehForte v1 && ehForte v2 = True     -- Forte + Forte (co-e-lho)
    | ehAcentuada v1 && ehForte v2 = True -- Í/Ú Tônico + Forte (sa-ú-de)
    | ehForte v1 && ehAcentuada v2 = True -- Forte + Í/Ú Tônico (pa-ís)
    | ehFraca v1 && ehAcentuada v2 = True -- Fraca + Acentuada (sa-bi-á)
    | ehFraca v1 && ehForte v2 = True
    | otherwise = False                   -- Ditongos



-- | MOTOR DE PATTERN MATCHING
quebrar :: [LetraSom] -> [[LetraSom]]
quebrar [] = []

-- =========================================================================
-- GRUPO 1: PREFIXOS E EXCEÇÕES (Prioridade Máxima)
-- =========================================================================

-- REGRA: 4 Consoantes (Vogal-C-C-C-C-Vogal) [NOVogalA]
-- Ex: tungs-tê-nio (n-g-s-t). 'st' não é cluster no inicio de sílaba PT.
-- Ex: mons-truo-so (n-s-t-r). 'tr' É cluster.
quebrar ((v,Vogal):(c1,Consoante):(c2,Consoante):(c3,Consoante):(c4,Consoante):resto)
    -- Se C3+C4 formam cluster (ex: ns-tr), separa antes do C3 (mons-tru)
    | ehCluster c3 c4 = [(v,Vogal),(c1,Consoante),(c2,Consoante)] : quebrar ((c3,Consoante):(c4,Consoante):resto)
    -- Se C3+C4 NÃO formam cluster (ex: ng-st), separa depois do C3 (tungs-tê)
    | otherwise       = [(v,Vogal),(c1,Consoante),(c2,Consoante),(c3,Consoante)] : quebrar ((c4,Consoante):resto)

-- REGRA: 3 Consoantes (V-C-C-C-V)
-- Ex: ist-mo, sols-tí-cio.
quebrar ((v,Vogal):(c1,Consoante):(c2,Consoante):(c3,Consoante):resto)
    | not (ehCluster c2 c3) = [(v,Vogal),(c1,Consoante),(c2,Consoante)] : quebrar ((c3,Consoante):resto)

-- =========================================================================
-- GRUPO 1: PREFIXOS E MORFOLOGIA ESPECÍFICA
-- =========================================================================

-- REGRA: Prefixo 'sub' seguido de consoante líquida (l, r)
-- Função: Garante que o prefixo se mantenha intacto e não forme cluster com a próxima letra.
-- Ex: sub-li-nhar (certo) vs su-bli-nhar (errado).
quebrar (('s',Consoante):('u',Vogal):('b',Consoante):(l,Consoante):resto)
    | l `elem` "lr" = [('s',Consoante),('u',Vogal),('b',Consoante)] : quebrar ((l,Consoante):resto)

-- =========================================================================
-- GRUPO 2: PROTEÇÃO DE QU/GU (Dígrafos e Tritongos)
-- Função: O 'u' depois de q/g geralmente não conta como vogal cheia, ele
-- "gruda" na anterior ou forma ditongo/tritongo com as seguintes.
-- =========================================================================

-- Caso Especial: QU/GU + Vogal + Consoante de Travamento + Consoante
-- Função: Resolve sílabas travadas que começam com QU/GU. O 1º C fica na sílaba.
-- Ex: guer-ra (o 'r' fica com 'gue'), qual-quer (o 'l' fica com 'qua').
quebrar ((c,Consoante):(u,Vogal):(v,Vogal):(c1,Consoante):(c2,Consoante):resto)
    | c `elem` "qgQG" && u `elem` "uU" && not (ehCluster c1 c2) = 
        [(c,Consoante),(u,Vogal),(v,Vogal),(c1,Consoante)] : quebrar ((c2,Consoante):resto)

-- Caso A.1: Tritongo/Ditongo + S/M final (Plurais/Nasais)
-- Função: Captura tritongos plurais ou nasais.
-- Ex: Uru-guais, sa-guões, en-xa-guem.
quebrar ((c, Consoante) : (u, Vogal) : (v1, Vogal) : (v2, Vogal) : (vf, Consoante) : resto)
    | c `elem` "qgQG" && u `elem` "uU" && (ehFraca v2 || (ehNasal v1 && v2 `elem` "eo")) && vf `elem` "sm" = 
        [(c, Consoante), (u, Vogal), (v1, Vogal), (v2, Vogal), (vf, Consoante)] : quebrar resto

-- Caso A.2: Tritongo ou Ditongo após QU/GU (sem consoante final)
-- Função: Captura tritongos abertos ou ditongos nasais.
-- Ex: Uru-guai, sa-guão.
quebrar ((c, Consoante) : (u, Vogal) : (v1, Vogal) : (v2, Vogal) : resto)
    | c `elem` "qgQG" && u `elem` "uU" && (ehFraca v2 || (ehNasal v1 && v2 `elem` "eo")) = 
        [(c, Consoante), (u, Vogal), (v1, Vogal), (v2, Vogal)] : quebrar resto

-- Caso B.1: Ditongo Crescente + S/M
-- Função: Captura sílabas como 'quas' ou 'guem'.
-- Ex: a-de-quas, a-lí-quo-ta (se tivesse s).
quebrar ((c, Consoante) : (u, Vogal) : (v1, Vogal) : (vf, Consoante) : resto)
    | c `elem` "qgQG" && u `elem` "uU" && vf `elem` "sm" = 
        [(c, Consoante), (u, Vogal), (v1, Vogal), (vf, Consoante)] : quebrar resto

-- Caso B.2: Apenas QU/GU + Vogal
-- Função: O caso mais comum. Junta Q+U+V.
-- Ex: á-gua, que-i-jo, qui-lo.
quebrar ((c, Consoante) : (u, Vogal) : (v1, Vogal) : resto)
    | c `elem` "qgQG" && u `elem` "uU" = 
        [(c, Consoante), (u, Vogal), (v1, Vogal)] : quebrar resto

-- =========================================================================
-- GRUPO 3: ENCONTROS VOCÁLICOS (Hiatos, Ditongos e Exceções)
-- =========================================================================

-- REGRA 0.5: Exceção para Paroxítonas Acentuadas terminadas em Ditongo
-- Função: Mantém o ditongo crescente final unido em palavras acentuadas.
-- Ex: ré-gia (e não ré-gi-a), gló-ria, sé-rie.
quebrar ((va, Vogal) : (c, Consoante) : (vf, Vogal) : (vF, Vogal) : resto)
    | ehAcentuada va && ehFraca vf && ehForte vF =
        [(va, Vogal)] : [(c, Consoante), (vf, Vogal), (vF, Vogal)] : quebrar resto

-- REGRA 1: Hiatos Simples (V-V)
-- Função: Separa vogais que não devem ficar juntas (Fortes+Fortes, Acentuadas).
-- Ex: sa-ú-de, ca-a-tin-ga, co-e-lho.
quebrar ((l1, Vogal) : (l2, Vogal) : resto)
    | deveSepararVogais l1 l2 = [(l1, Vogal)] : quebrar ((l2, Vogal) : resto)

-- REGRA 1.5: Hiato após Ditongo Decrescente
-- Função: Impede que a última vogal de um ditongo "roube" a próxima vogal.
-- Ex: idei-a (separa 'i' de 'a'), joi-a, mei-a.
quebrar ((l1, Vogal) : (l2, Vogal) : (l3, Vogal) : resto)
    | ehForte l1 && ehFraca l2 = [(l1, Vogal), (l2, Vogal)] : quebrar ((l3, Vogal) : resto)

-- =========================================================================
-- GRUPO 4: CONSOANTES COMPLEXAS (Essa estava faltando no seu snippet!)
-- =========================================================================

-- REGRA EXTRA: Encontro de 3 Consoantes (V-C-C-C-V)
-- Função: Resolve colisões de 3 consoantes. Se as duas últimas não formam cluster,
-- cortamos depois da primeira consoante complexa.
-- Ex: ist-mo, tungs-tê-nio, sols-tí-cio.
quebrar ((v,Vogal):(c1,Consoante):(c2,Consoante):(c3,Consoante):resto)
    | not (ehCluster c2 c3) = [(v,Vogal),(c1,Consoante),(c2,Consoante)] : quebrar ((c3,Consoante):resto)

-- =========================================================================
-- GRUPO 5: PADRÕES BÁSICOS (Clusters e Silabação Simples)
-- =========================================================================

-- REGRA 2: Cluster (V-CCV)
-- Função: Consoante + R ou L formam grupo inseparável.
-- Ex: pe-dra, a-tlân-ti-co, li-vro.
quebrar ((l1, Vogal) : (c1, Consoante) : (c2, Consoante) : (l2, Vogal) : resto)
    | ehCluster c1 c2 = [(l1, Vogal)] : quebrar ((c1, Consoante) : (c2, Consoante) : (l2, Vogal) : resto)

-- REGRA 3: V-CV (Padrão Ouro)
-- Função: Uma consoante entre vogais sempre fica com a segunda vogal.
-- Ex: ca-sa, a-mo, be-lo.
quebrar ((l1, Vogal) : (l2, Consoante) : (l3, Vogal) : resto) = 
    [(l1, Vogal)] : quebrar ((l2, Consoante) : (l3, Vogal) : resto)

-- REGRA 4: VC-CV (Travamento Simples)
-- Função: Duas consoantes (que não são cluster) se separam.
-- Ex: can-to, pac-to, ob-je-to.
quebrar ((l1, Vogal) : (l2, Consoante) : (l3, Consoante) : (l4, Vogal) : resto) =
    [(l1, Vogal), (l2, Consoante)] : quebrar ((l3, Consoante) : (l4, Vogal) : resto)

-- =========================================================================
-- CASO BASE (Fim da Linha)
-- =========================================================================
quebrar (x:xs) = 
    case quebrar xs of
        []       -> [[x]]
        (s:resto) -> (x:s) : resto