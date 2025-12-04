{-# LANGUAGE DeriveGeneric #-}

module TiposJSON where

import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)

-- | Entrada (Request)
data PoemaRequest = PoemaRequest
    { poema :: String
    } deriving (Show, Generic)

instance FromJSON PoemaRequest

-- | NÍVEL 3: Objeto da Sílaba Individual
data SilabaDetalhada = SilabaDetalhada
    { texto    :: String -- Ex: "nha-ter" (sem o *)
    , isTonica :: Bool   -- Ex: true ou false
    } deriving (Show, Generic)

instance ToJSON SilabaDetalhada

-- | NÍVEL 2: Objeto do Verso
data VersoResultado = VersoResultado
    { versoOriginal   :: String
    , silabas         :: [SilabaDetalhada] -- Lista de objetos, não strings!
    , contagemPoetica :: Int
    , indicesTonicas  :: [Int]             -- Ex: [2, 6, 10]
    } deriving (Show, Generic)

instance ToJSON VersoResultado

-- | NÍVEL 1: Objeto Raiz (O Poema)
data PoemaResultado = PoemaResultado
    { versos      :: [VersoResultado]
    , totalVersos :: Int
    , resumoGeral :: String -- Um texto ou estatística extra (opcional)
    } deriving (Show, Generic)

instance ToJSON PoemaResultado