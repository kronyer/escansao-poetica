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

-- | MOTOR DE PATTERN MATCHING
quebrar :: [LetraSom] -> [[LetraSom]]
quebrar [] = []

-- REGRA 1: Hiatos Simples (V-V)
quebrar ((l1, Vogal) : (l2, Vogal) : resto)
    | deveSepararVogais l1 l2 = [(l1, Vogal)] : quebrar ((l2, Vogal) : resto)

-- REGRA 1.5 [NOVA]: Hiato após Ditongo Decrescente (Tritongo falso)
-- Ex: idei-a, mei-a, joi-a. (Forte + Fraca + Vogal)
-- Se não fizermos isso, ele agrupa tudo (ideia) ou separa errado.
-- Ação: Agrupa (Forte, Fraca) e separa da próxima Vogal.
quebrar ((l1, Vogal) : (l2, Vogal) : (l3, Vogal) : resto)
    | ehForte l1 && ehFraca l2 = [(l1, Vogal), (l2, Vogal)] : quebrar ((l3, Vogal) : resto)

-- REGRA 2: Cluster (V-CCV)
quebrar ((l1, Vogal) : (c1, Consoante) : (c2, Consoante) : (l2, Vogal) : resto)
    | ehCluster c1 c2 = [(l1, Vogal)] : quebrar ((c1, Consoante) : (c2, Consoante) : (l2, Vogal) : resto)

-- REGRA 3: V-CV
quebrar ((l1, Vogal) : (l2, Consoante) : (l3, Vogal) : resto) = 
    [(l1, Vogal)] : quebrar ((l2, Consoante) : (l3, Vogal) : resto)

-- REGRA 4: VC-CV
quebrar ((l1, Vogal) : (l2, Consoante) : (l3, Consoante) : (l4, Vogal) : resto) =
    [(l1, Vogal), (l2, Consoante)] : quebrar ((l3, Consoante) : (l4, Vogal) : resto)

-- CASO BASE
quebrar (x:xs) = 
    case quebrar xs of
        []       -> [[x]]
        (s:resto) -> (x:s) : resto