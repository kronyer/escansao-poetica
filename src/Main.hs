module Main (main) where

import Escansao
import Silabacao 
import Data.List (elemIndices, intercalate)

main :: IO ()
main = do
    -- Exemplo de Poema (Input Multilinha)
    let poema = unlines [
            "Musa, canta-me a régia poranduba",
            "das bandeiras, os feitos sublimados",
            "dos heróis que o Brasil plasmar souberam",
            "través do Pindorama, demarcando",
            "nos sertões a conquista e as esperanças."
            ]

    putStrLn "=== ANÁLISE MÉTRICA DO POEMA ==="
    putStrLn ""
    
    -- Processa linha por linha
    mapM_ processarVerso (lines poema)


processarVerso :: String -> IO ()
processarVerso "" = putStrLn "" -- Linha vazia
processarVerso versoOriginal = do
    let escandido = escandirVerso versoOriginal
    
    -- Calcula estatísticas
    let contagem = length escandido
    
    -- Encontra os índices onde há '*' (Soma 1 para ser base-1, estilo humano)
    let indicesTonicas = map (+1) $ findIndicesComTonica escandido
    let strTonicas = intercalate "-" (map show indicesTonicas)
    
    -- Formatação Visual
    let visualizacao = intercalate " | " escandido
    
    putStrLn $ "Verso: " ++ versoOriginal
    putStrLn $ "Escansão: [" ++ visualizacao ++ "]"
    putStrLn $ "Resumo: contagem poetica: " ++ show contagem ++ ", tonicas: " ++ strTonicas
    putStrLn "---------------------------------------------------"

-- | Função auxiliar para achar onde estão os '*'
findIndicesComTonica :: [String] -> [Int]
findIndicesComTonica lista = 
    [i | (i, silaba) <- zip [0..] lista, '*' `elem` silaba]