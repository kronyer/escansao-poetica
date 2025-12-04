{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Web.Scotty
import Data.Aeson (ToJSON)
import GHC.Generics
import Control.Monad.IO.Class (liftIO)

-- Middleware CORS para o Front-end não reclamar
import Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy, simpleCors, corsRequestHeaders, corsMethods)

import Escansao (escandirVerso)
import TiposJSON 
import Silabacao (carregarDicionario, Dicionario) -- Importe o carregador

main = do
    -- 1. Carrega o dicionário ANTES de subir o servidor
    putStrLn "Carregando dicionário de exceções..."
    dicionario <- carregarDicionario "assets/excecoes.txt"
    putStrLn "Dicionário carregado!"

    scotty 3000 $ do
        middleware (cors (const $ Just policy))

        post "/escandir" $ do
            request <- jsonData :: ActionM PoemaRequest
            let textoPoema = poema request
            let linhas = filter (not . null) (lines textoPoema)
            
            -- 2. Passa o 'dicionario' carregado para a função de escansão
            let versosProcessados = map (processarVerso dicionario) linhas
            
            let resposta = PoemaResultado
                    { versos = versosProcessados
                    , totalVersos = length versosProcessados
                    , resumoGeral = "OK"
                    }
            json resposta

-- | Definição da Política de CORS
policy = simpleCorsResourcePolicy
    { corsMethods = ["GET", "POST", "OPTIONS"] -- Permite os verbos
    , corsRequestHeaders = ["Content-Type"]    -- Permite enviar JSON
    }

-- | Converte a string bruta e a análise em um Objeto Rico
processarVerso :: Dicionario -> String -> VersoResultado
processarVerso dic v =
      let 
        -- O motor devolve: ["mi", "nha-ter", "ra", "tem*", ...]
        escansaoBruta = escandirVerso dic v -- Passa o dic aqui
        
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