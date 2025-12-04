-- src/Silabacao.hs
module Silabacao (separarSilabas) where

import Tipos (Som(..))
import AnaliseLexical (classificar)

-- Um par contendo a Letra e o seu Som correspondente
type LetraSom = (Char, Som)

-- | Função Principal: Recebe uma palavra limpa e devolve lista de sílabas
separarSilabas :: String -> [String]
separarSilabas palavra = 
    let 
        -- 1. Cria pares: "casa" virou [('c',Consoante), ('a',Vogal)...]
        pares = zip palavra (map classificar palavra)
        
        -- 2. Aplica as regras de quebra
        silabasPares = quebrar pares
    in 
        -- 3. Reconstrói a String a partir dos pares
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
    | otherwise = False                   -- Ditongos
    
-- | O Motor de Pattern Matching
quebrar :: [LetraSom] -> [[LetraSom]]
quebrar [] = []

-- REGRA 1: Hiatos (Vogal-Vogal)
-- Se temos duas vogais seguidas, verificamos se elas se separam.
quebrar ((l1, Vogal) : (l2, Vogal) : resto)
    | deveSepararVogais l1 l2 = [(l1, Vogal)] : quebrar ((l2, Vogal) : resto)
    -- Se não separa (Ditongo), deixa cair no Caso Base (que agrupa)

-- REGRA 2: Encontros Consonantais Inseparáveis (V-CCV)
-- Ex: pe-dra, li-vro, fi-lho.
-- Se temos Vogal, depois C, depois C, depois Vogal...
-- E as duas consoantes formam um cluster... quebramos ANTES da primeira consoante.
quebrar ((l1, Vogal) : (c1, Consoante) : (c2, Consoante) : (l2, Vogal) : resto)
    | ehCluster c1 c2 = [(l1, Vogal)] : quebrar ((c1, Consoante) : (c2, Consoante) : (l2, Vogal) : resto)

-- REGRA 1: V - CV (Ex: a-mo, ca-sa)
-- Se temos Vogal, depois Consoante, depois Vogal... quebramos antes da Consoante.
quebrar ((l1, Vogal) : (l2, Consoante) : (l3, Vogal) : resto) = 
    [(l1, Vogal)] : quebrar ((l2, Consoante) : (l3, Vogal) : resto)

    -- REGRA 0: Dígrafos Inseparáveis (lh, nh, ch) [NOVA REGRA]
-- Se temos Vogal, depois uma consoante (l, n ou c), depois 'h', depois Vogal...
-- Nós NÃO separamos. Jogamos tudo para a próxima sílaba (comportamento V-CV).
quebrar ((l1, Vogal) : (c1, Consoante) : ('h', Consoante) : (l3, Vogal) : resto)
    | c1 `elem` "lnc" = [(l1, Vogal)] : quebrar ((c1, Consoante) : ('h', Consoante) : (l3, Vogal) : resto)

-- REGRA 2: VC - CV (Ex: can-to, res-to)
-- Se temos Vogal, Consoante, Consoante, Vogal... quebramos no meio das Consoantes.
quebrar ((l1, Vogal) : (l2, Consoante) : (l3, Consoante) : (l4, Vogal) : resto) =
    [(l1, Vogal), (l2, Consoante)] : quebrar ((l3, Consoante) : (l4, Vogal) : resto)

-- CASO BASE (Default):
-- Se nenhuma regra acima casou, pegamos a letra atual e tentamos agrupar com a próxima
-- (Isso vai "arrastar" as letras até encontrar um ponto de quebra ou acabar a palavra)
quebrar (x:xs) = 
    case quebrar xs of
        []       -> [[x]]            -- Se acabou, retorna só x
        (s:resto) -> (x:s) : resto