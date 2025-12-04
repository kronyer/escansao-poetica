import Test.Hspec
import Silabacao (separarSilabas, carregarDicionario) -- Importe carregarDicionario
import Escansao (escandirVerso)
import System.IO.Error (catchIOError)

main :: IO ()
main = do
  -- 1. Carrega o dicionário real antes dos testes
  -- Se o arquivo não existir, usamos um dicionário vazio para não quebrar tudo,
  -- mas avisamos no console.
  dic <- carregarDicionario "assets/excecoes.txt" `catchIOError` \_ -> do
    putStrLn "AVISO: 'assets/excecoes.txt' não encontrado. Usando dicionário vazio."
    return mempty -- Dicionário vazio

  hspec $ do
  
    describe "Silabacao.separarSilabas" $ do
      -- ==========================================
      -- 1. Casos Básicos e Simples (CV, V)
      -- ==========================================
      it "separa sílabas simples (CV-CV)" $ do
        separarSilabas dic "casa" `shouldBe` ["ca", "sa"]
        separarSilabas dic "bola" `shouldBe` ["bo", "la"]
        separarSilabas dic "abraço" `shouldBe` ["a", "bra", "ço"]
        separarSilabas dic "subir" `shouldBe` ["su", "bir"]
        separarSilabas dic "subterrâneo" `shouldBe` ["sub", "ter" , "râ", "ne" , "o"]
        
        -- Este caso depende do dicionário (se estiver no arquivo) ou da regra de exceção
        separarSilabas dic "sublime" `shouldBe` ["su", "bli", "me"]

      it "isola vogais iniciais (V-CV)" $ do
        separarSilabas dic "amor" `shouldBe` ["a", "mor"]
        separarSilabas dic "unico" `shouldBe` ["u", "ni", "co"]

      -- ==========================================
      -- 2. Dígrafos e Encontros Consonantais
      -- ==========================================
      it "mantém dígrafos inseparáveis juntos (ch, lh, nh)" $ do
        separarSilabas dic "chuva" `shouldBe` ["chu", "va"]
        separarSilabas dic "velho" `shouldBe` ["ve", "lho"]
        separarSilabas dic "manha" `shouldBe` ["ma", "nha"]

      it "separa dígrafos separáveis (rr, ss, sc, sç, xc)" $ do
        separarSilabas dic "carro" `shouldBe` ["car", "ro"]
        separarSilabas dic "pássaro" `shouldBe` ["pás", "sa", "ro"]
        separarSilabas dic "nascer" `shouldBe` ["nas", "cer"]
        separarSilabas dic "desça" `shouldBe` ["des", "ça"]
        separarSilabas dic "exceto" `shouldBe` ["ex", "ce", "to"]

      it "mantém encontros consonantais perfeitos juntos (bl, pr, tr, etc)" $ do
        separarSilabas dic "brasil" `shouldBe` ["bra", "sil"]
        separarSilabas dic "planta" `shouldBe` ["plan", "ta"]
        separarSilabas dic "atleta" `shouldBe` ["a", "tle", "ta"]

      it "separa encontros consonantais imperfeitos (VC-CV)" $ do
        separarSilabas dic "apto" `shouldBe` ["ap", "to"]
        separarSilabas dic "admitir" `shouldBe` ["ad", "mi", "tir"]
        separarSilabas dic "confecção" `shouldBe` ["con", "fec", "ção"]

      -- ==========================================
      -- 3. Hiatos e Ditongos
      -- ==========================================
      it "separa hiatos (V-V)" $ do
        separarSilabas dic "coelho" `shouldBe` ["co", "e", "lho"]
        separarSilabas dic "saúde" `shouldBe` ["sa", "ú", "de"]
        separarSilabas dic "país" `shouldBe` ["pa", "ís"]
        separarSilabas dic "dia" `shouldBe` ["di", "a"]

      it "não separa ditongos crescentes e decrescentes" $ do
        separarSilabas dic "peixe" `shouldBe` ["pei", "xe"]
        separarSilabas dic "caixa" `shouldBe` ["cai", "xa"]
        separarSilabas dic "série" `shouldBe` ["sé", "rie"]
        separarSilabas dic "história" `shouldBe` ["his", "tó", "ria"]

      it "separa hiato após ditongo (Regra 1.5)" $ do
        separarSilabas dic "ideia" `shouldBe` ["i", "dei", "a"]
        separarSilabas dic "praia" `shouldBe` ["prai", "a"]

      it "trata regra especial de ditongos nasais (mão, pães)" $ do
        separarSilabas dic "mão" `shouldBe` ["mão"]
        separarSilabas dic "mãos" `shouldBe` ["mãos"]
        separarSilabas dic "sertões" `shouldBe` ["ser", "tões"]

      -- ==========================================
      -- 4. O "Inferno" do QU e GU
      -- ==========================================
      it "identifica ditongos em QU/GU seguidos de vogal" $ do
        separarSilabas dic "queijo" `shouldBe` ["quei", "jo"]
        separarSilabas dic "guerra" `shouldBe` ["guer", "ra"]
        separarSilabas dic "água" `shouldBe` ["á", "gua"]
        separarSilabas dic "sague" `shouldBe` ["sa", "gue"]

      it "identifica tritongos em QU/GU" $ do
        separarSilabas dic "uruguai" `shouldBe` ["u", "ru", "guai"]
        separarSilabas dic "quão" `shouldBe` ["quão"]
        separarSilabas dic "saguão" `shouldBe` ["sa", "guão"]
      
      it "trata plurais de tritongos/ditongos nasais (guais, guões)" $ do
        separarSilabas dic "iguais" `shouldBe` ["i", "guais"]
        separarSilabas dic "saguões" `shouldBe` ["sa", "guões"]

      -- ==========================================
      -- 5. Casos de Fronteira / Difíceis
      -- ==========================================
      it "mantém consoantes mudas iniciais juntas" $ do
        separarSilabas dic "pneu" `shouldBe` ["pneu"]
        separarSilabas dic "psicologia" `shouldBe` ["psi", "co", "lo", "gi", "a"]
        separarSilabas dic "gnomo" `shouldBe` ["gno", "mo"]

      it "separa encontros de 3 consoantes (V-C-CC-V ou V-CC-C-V)" $ do
        separarSilabas dic "istmo" `shouldBe` ["ist", "mo"]
        separarSilabas dic "solstício" `shouldBe` ["sols", "tí", "cio"]
        separarSilabas dic "tungstênio" `shouldBe` ["tungs", "tê", "nio"]

      it "trata prefixos com cluster fonético (sublinhar)" $ do
        separarSilabas dic "sublinhar" `shouldBe` ["sub", "li", "nhar"]
        -- Este depende do dicionário ou regra especial
        separarSilabas dic "abrupto" `shouldBe` ["ab", "rup", "to"]

    describe "Escansao.escandirVerso (Casos Difíceis)" $ do
      
      it "Resolve 'Minha terra tem palmeiras' (Sinalefa simples)" $ do
        let res = escandirVerso dic "Minha terra tem palmeiras"
        length res `shouldBe` 7 
        last res `shouldBe` "mei*"

      it "Resolve 'Sobe, imaginação! Abre...' (Pontuação e Choque)" $ do
        let res = escandirVerso dic "Sobe, imaginação! Abre os arcanos"
        length res `shouldBe` 10
        (res !! 5) `shouldContain` "*"
        (res !! 6) `shouldNotContain` "*"

      it "Resolve 'Dá que em versos eu fixe...' (Choque sem pausa)" $ do
        let res = escandirVerso dic "Dá que em versos eu fixe os fundamentos"
        (res !! 4) `shouldNotContain` "*"
        (res !! 5) `shouldContain` "*"

      it "Resolve 'forçando à eternidade' (Crase e Sinalefa)" $ do
        let res = escandirVerso dic "de grandeza forçando à eternidade."
        length res `shouldBe` 10