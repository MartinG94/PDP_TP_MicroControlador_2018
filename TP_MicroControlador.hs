{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Show.Functions
import Data.List
import Data.Maybe
import Test.Hspec

-- 1ra Parte

type Valor = Int
type Memoria = [Valor]
type Leyenda = String
type Instrucción = MicroControlador -> MicroControlador
type Programa = [Instrucción]

data MicroControlador = MicroControlador {
  memoria :: Memoria,
  acumulador_A :: Valor,
  acumulador_B :: Valor,
  programCounter :: Valor,
  mensajeError :: Leyenda,
  programa :: Programa
} deriving(Show)

nuevaMemoria otraMemoria micro = micro {memoria = otraMemoria}
nuevoAcum_A otroAcum_A micro = micro {acumulador_A = otroAcum_A}
nuevoAcum_B otroAcum_B micro = micro {acumulador_B = otroAcum_B}
nuevoProgramCounter otroPC micro = micro {programCounter = otroPC}
nuevaEtiqueta otraEtiqueta micro = micro {mensajeError = otraEtiqueta}

xt8088 = MicroControlador [] 0 0 0 "" []

type Posición = Int

nop :: Instrucción
nop micro = nuevoProgramCounter (programCounter micro + 1) micro

lodv :: Valor -> Instrucción
lodv unValor micro = nuevoAcum_A unValor micro

swap :: Instrucción
swap micro = (nuevoAcum_A (acumulador_B micro) . nuevoAcum_B (acumulador_A micro)) micro

add :: Instrucción
add micro = (nuevoAcum_B 0 . nuevoAcum_A (acumulador_A micro + acumulador_B micro)) micro

agregar :: Memoria -> Posición -> Valor -> Memoria
agregar lista posición unValor
  | (posición - 1) >= 0 = (take (posición -1) lista) ++ [unValor] ++ (drop posición lista)
  | otherwise = lista

str :: Posición -> Valor -> Instrucción
str unaPosición unValor micro = nuevaMemoria (agregar (memoria micro) unaPosición unValor) micro

obtenerElemento :: Posición -> Memoria -> Valor
obtenerElemento posición lista
  | (>= 1) posición && (<=(length lista)) posición = (flip (!!)) (posición - 1) lista
  | otherwise = error "No existe la posición de memoria solicitada"

lod :: Posición -> Instrucción
lod unaPosición micro = nuevoAcum_A (obtenerElemento unaPosición (memoria micro)) micro

divide :: Instrucción
divide micro
  | (not . (==0) . acumulador_B) micro = (nuevoAcum_B 0 . nuevoAcum_A (acumulador_A micro `div` acumulador_B micro)) micro
  | otherwise = (nuevaEtiqueta "DIVISION BY ZERO") micro

fp20 = MicroControlador [] 7 24 0 "" []
at8086 = MicroControlador [1.. 20] 0 0 0 "" []

pruebasConInstrucciones = hspec $ do
  describe "Se realizan pruebas con las intrucciones" $ do
    it "Se ejecuta un nop 3 veces en xt8088, se espera que el program counter sea 3" $
      (programCounter . nop . nop . nop) xt8088 `shouldBe` 3
    it "Se ejecuta un lodv 5 en xt8088, se espera que el acumulador A sea 5" $
      (acumulador_A . lodv 5) xt8088 `shouldBe` 5
    it "Se ejecuta un swap en fp20, se espera que el acumulador A sea 24" $
      (acumulador_A . swap) fp20 `shouldBe` 24
    it "Se ejecuta un swap en fp20, se espera que el acumulador B sea 7" $
      (acumulador_B . swap) fp20 `shouldBe` 7
    it "Se ejecuta un str 2 5 en at8086, se espera que el elemento 2 de su memoria sea 5" $
      (obtenerElemento 2 . memoria . str 2 5) at8086 `shouldBe` 5
    it "Se ejecuta un lod 2 en xt8088 con una memoria de 1024 posiciones con valor 0. Su acumulador A debería ser 0" $
      (acumulador_A . lod 2. nuevaMemoria (take 1024 [0, 0..])) xt8088 `shouldBe` 0

avanzarTresPosiciones :: Programa
avanzarTresPosiciones = [nop, nop, nop]

sumar10Y22 :: Programa
sumar10Y22 = [lodv 10 , swap , lodv 22 , add]

divisiónDe2Por0 :: Programa
divisiónDe2Por0 = [str 1 2 , str 2 0 , lod 2 , swap , lod 1 , divide]

divisiónDe12Por4 :: Programa
divisiónDe12Por4 = [str 1 12, str 2 4 , lod 2 , swap , lod 1 , divide]

-- 2da Parte

nuevoPrograma unPrograma micro = micro {programa = unPrograma}

cargar :: Programa -> MicroControlador -> MicroControlador
cargar unPrograma = nuevoPrograma unPrograma

ejecutar :: MicroControlador -> Instrucción -> MicroControlador
ejecutar enMicro laInstrucción = (nop . laInstrucción) enMicro

ejecutarPrograma :: MicroControlador -> MicroControlador
ejecutarPrograma unMicro = foldl ejecutar unMicro (programa unMicro)

pruebasConProgramas = hspec $ do
  describe "Se realizan pruebas con los programas" $ do
    it "Cargar y ejecutar el programa sumar10Y22 a xt8088 genera que su acumulador A sea 32" $
      (acumulador_A . ejecutarPrograma . cargar sumar10Y22) xt8088 `shouldBe` 32
    it "Cargar y ejecutar el programa sumar10Y22 a xt8088 genera que su acumulador B sea 32" $
      (acumulador_B . ejecutarPrograma . cargar sumar10Y22) xt8088 `shouldBe` 0
    it "Cargar y ejecutar el programa sumar10Y22 a xt8088 genera que su program counter sea 32" $
      (programCounter . ejecutarPrograma . cargar sumar10Y22) xt8088 `shouldBe` 4
    it "Cargar y ejecutar el programa divisiónDe2Por0 a xt8088 genera que su acumulador A sea 2" $
      (acumulador_A . ejecutarPrograma . cargar divisiónDe2Por0) xt8088 `shouldBe` 2
    it "Cargar y ejecutar el programa divisiónDe2Por0 a xt8088 genera que su acumulador B sea 0" $
      (acumulador_B . ejecutarPrograma . cargar divisiónDe2Por0) xt8088 `shouldBe` 0
    it "Cargar y ejecutar el programa divisiónDe2Por0 a xt8088 genera que su mensaje de error sea DIVISION BY ZERO" $
      (mensajeError . ejecutarPrograma . cargar divisiónDe2Por0) xt8088 `shouldBe` "DIVISION BY ZERO"
    it "Cargar y ejecutar el programa divisiónDe2Por0 a xt8088 genera que su program counter sea 6" $
      (programCounter . ejecutarPrograma . cargar divisiónDe2Por0) xt8088 `shouldBe` 6

ifnz :: Programa -> MicroControlador -> MicroControlador
ifnz [] micro = micro
ifnz (unaInstrucción : otraInstrucción) micro
  | (/=0) (acumulador_A micro) = ifnz otraInstrucción (ejecutar micro unaInstrucción)
  | otherwise = ifnz otraInstrucción micro

pruebasConIfnz = hspec $ do
  describe "Se realizan pruebas con la función ifnz" $ do
    it "Ejecutar ifnz en las instrucciones lodv 3 y swap sobre fp20 genera que su acumulador A sea 24" $
      (acumulador_A . ifnz [lodv 3, swap]) fp20 `shouldBe` 24
    it "Ejecutar ifnz en las instrucciones lodv 3 y swap sobre fp20 genera que su acumulador B sea 3" $
      (acumulador_B . ifnz [lodv 3, swap]) fp20 `shouldBe` 3
    it "Ejecutar ifnz en las instrucciones lodv 3 y swap sobre xt8088 genera que su acumulador A sea 0" $
      (acumulador_A . ifnz [lodv 3, swap]) xt8088 `shouldBe` 0
    it "Ejecutar ifnz en las instrucciones lodv 3 y swap sobre xt8088 genera que su acumulador B sea 0" $
      (acumulador_B . ifnz [lodv 3, swap]) xt8088 `shouldBe` 0

esInnecesariaPara micro instrucción =
  ((==0) . acumulador_A) (ejecutar micro instrucción) &&
  ((==0) . acumulador_B) (ejecutar micro instrucción) &&
  ((==0) . sum . memoria) (ejecutar micro instrucción)

programaADepurar = [swap, nop, lodv 133, lodv 0, str 1 3, str 2 0]

depurar :: Programa -> MicroControlador -> Programa
depurar unPrograma micro = filter (not . esInnecesariaPara micro) unPrograma

pruebaConDepurar = hspec $ do
  describe "Se depura un programa y se utiliza xt8088 para la prueba" $ do
    it "Depurar el las instrucciones swap, nop, lodv 133, lodv 0, str 1 3, str 2 0. Sólo quedan 2 instrucciones" $
      (length . depurar programaADepurar) xt8088 `shouldBe` 2

type Criterio = Valor -> Valor -> Bool

menorAMayor :: Criterio
menorAMayor = (<=)

mayorAMenor :: Criterio
mayorAMenor = (>=)

listaOrdenadaDe :: Criterio -> Memoria -> Bool
listaOrdenadaDe _ [] = True
listaOrdenadaDe _ [x] = True
listaOrdenadaDe criterio (cab1 : cab2 : cola) = criterio cab1 cab2 && listaOrdenadaDe criterio (cab2 : cola)

memoriaOrdenadaSegún :: Criterio -> MicroControlador -> Bool
memoriaOrdenadaSegún criterio micro = listaOrdenadaDe criterio (memoria micro)

microDesorden = MicroControlador [2,5,1,0,6,9] 0 0 0 "" []

pruebasConLaMemoria = hspec $ do
  describe "Se realizan pruebas con el orden de la memoria" $ do
    it "La memoria de at8086 está ordenada" $
      memoriaOrdenadaSegún menorAMayor at8086 `shouldBe` True
    it "La memoria de microDesorden no está ordenada" $
      memoriaOrdenadaSegún menorAMayor microDesorden `shouldBe` False

ejecutarTests = do
  pruebasConInstrucciones
  pruebasConProgramas
  pruebasConIfnz
  pruebaConDepurar
  pruebasConLaMemoria
