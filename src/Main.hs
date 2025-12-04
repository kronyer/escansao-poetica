{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Web.Scotty
import Data.Aeson (ToJSON)
import GHC.Generics
import Control.Monad.IO.Class (liftIO)

-- Middleware CORS para o Front-end não reclamar
import Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy, corsRequestHeaders, corsMethods)

import Escansao (escandirVerso)
import TiposJSON 

main :: IO ()
main = scotty 3000 $ do
    -- SUBSTITUIÇÃO DO CORS AQUI:
    -- Em vez de 'middleware simpleCors', usamos uma política customizada
    middleware $ cors (const $ Just policy)

    post "/escandir" $ do
        request <- jsonData :: ActionM PoemaRequest
        let textoPoema = poema request
        let linhas = filter (not . null) (lines textoPoema)
        let versosProcessados = map processarVerso linhas
        
        let resposta = PoemaResultado
                { versos = versosProcessados
                , totalVersos = length versosProcessados
                , resumoGeral = "Análise concluída com sucesso."
                }
        
        json resposta

-- | Definição da Política de CORS
policy = simpleCorsResourcePolicy
    { corsMethods = ["GET", "POST", "OPTIONS"] -- Permite os verbos
    , corsRequestHeaders = ["Content-Type"]    -- Permite enviar JSON
    }

-- | Converte a string bruta e a análise em um Objeto Rico
processarVerso :: String -> VersoResultado
processarVerso v = 
    let 
        -- O motor devolve: ["mi", "nha-ter", "ra", "tem*", ...]
        escansaoBruta = escandirVerso v
        
        -- Converte cada string em um objeto SilabaDetalhada
        objsSilabas = map criarObjetoSilaba escansaoBruta
        
        -- Calcula metadados
        contagem = length escansaoBruta
        tonicas  = [i + 1 | (i, s) <- zip [0..] objsSilabas, isTonica s]
    in
        VersoResultado 
            { versoOriginal = v
            , silabas = objsSilabas
            , contagemPoetica = contagem
            , indicesTonicas = tonicas
            }

-- | Helper: Remove o '*' da string e seta o booleano
criarObjetoSilaba :: String -> SilabaDetalhada
criarObjetoSilaba s = 
    if not (null s) && last s == '*' 
        then SilabaDetalhada { texto = init s, isTonica = True } -- 'init' remove o ultimo char
        else SilabaDetalhada { texto = s,      isTonica = False }