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
nuevoAcumA otroAcum_A micro = micro {acumulador_A = otroAcum_A}
nuevoAcumB otroAcum_B micro = micro {acumulador_B = otroAcum_B}
nuevoProgramCounter otroPC micro = micro {programCounter = otroPC}
nuevaEtiqueta otraEtiqueta micro = micro {mensajeError = otraEtiqueta}

xt8088 = MicroControlador [] 0 0 0 "" []

type Posición = Int

nop :: Instrucción
nop micro = nuevoProgramCounter (programCounter micro + 1) micro

pruebasNOP = hspec $ do
  describe "Tests Punto 2 - Tests de NOP." $ do
    it "NOP incrementa el program counter." $
      (programCounter . nop) xt8088 `shouldBe` 1
    it "NOP no cambia el acumulador." $
      (acumulador_A . nop) xt8088 `shouldBe` 0
    it "Programa con tres nop, avanza tres veces el program counter." $
      (programCounter . nop . nop . nop) xt8088 `shouldBe` 3

lodv :: Valor -> Instrucción
lodv unValor micro = nuevoAcumA unValor micro

swap :: Instrucción
swap micro = (nuevoAcumA (acumulador_B micro) . nuevoAcumB (acumulador_A micro)) micro

add :: Instrucción
add micro = (nuevoAcumB 0 . nuevoAcumA (acumulador_A micro + acumulador_B micro)) micro

pruebasConInstrucciones1 = hspec $ do
  describe "Tests Punto 3 - Tests de programa Sumar." $ do
    it "Se ejecuta un LODV 5 en xt8088, se espera que el acumulador A sea 5." $
      (acumulador_A . lodv 5) xt8088 `shouldBe` 5
    it "Se ejecuta un SWAP en fp20, se espera que el acumulador A sea 24." $
      (acumulador_A . swap) fp20 `shouldBe` 24
    it "Se ejecuta un SWAP en fp20, se espera que el acumulador B sea 7." $
      (acumulador_B . swap) fp20 `shouldBe` 7
    it "Suma 10 y 22 da 32 en el acumulador A." $
      (acumulador_A . add . lodv 22 . swap . lodv 10) xt8088 `shouldBe` 32
    it "Suma 10 y 22 da 0 en el acumulador B." $
      (acumulador_B . add . lodv 22 . swap . lodv 10) xt8088 `shouldBe` 0

agregar :: Memoria -> Posición -> Valor -> Memoria
agregar lista posición unValor
  | (posición - 1) >= 0 = (take (posición -1) lista) ++ [unValor] ++ (drop posición lista)
  | otherwise = lista

str :: Posición -> Valor -> Instrucción
str unaPosición unValor micro = nuevaMemoria (agregar (memoria micro) unaPosición unValor) micro

obtenerElemento :: Posición -> Memoria -> Valor
obtenerElemento posición lista
  | (>= 1) posición && (<=(length lista)) posición = (flip (!!)) (posición - 1) lista
  | otherwise = 0

lod :: Posición -> Instrucción
lod unaPosición micro = nuevoAcumA (obtenerElemento unaPosición (memoria micro)) micro

divide :: Instrucción
divide micro
  | (not . (==0) . acumulador_B) micro = (nuevoAcumB 0 . nuevoAcumA (acumulador_A micro `div` acumulador_B micro)) micro
  | otherwise = nuevaEtiqueta "DIVISION BY ZERO" micro

fp20 = MicroControlador [] 7 24 0 "" []
at8086 = MicroControlador [1.. 20] 0 0 0 "" []

avanzarTresPosiciones :: Programa
avanzarTresPosiciones = [nop, nop, nop]

sumar10Y22 :: Programa
sumar10Y22 = [lodv 10 , swap , lodv 22 , add]

divisiónDe2Por0 :: Programa
divisiónDe2Por0 = [str 1 2 , str 2 0 , lod 2 , swap , lod 1 , divide]

divisiónDe12Por4 :: Programa
divisiónDe12Por4 = [str 1 12, str 2 4 , lod 2 , swap , lod 1 , divide]

pruebasConInstrucciones2 = hspec $ do
  describe "Tests Punto 4 - Tests de programa División." $ do
    it "Se ejecuta un STR 2 5 en at8086, se espera que el elemento 2 de su memoria sea 5." $
      (obtenerElemento 2 . memoria . str 2 5) at8086 `shouldBe` 5
    it "LOD 2 de una memoria vacía debe dar 0." $
      (acumulador_A . lod 2) xt8088 `shouldBe` 0
    it "División por cero da error." $
      (mensajeError . divide) xt8088 `shouldBe` "DIVISION BY ZERO"
    it "División de 12 por 4 se resuelve en el acumulador A." $
      (acumulador_A . divide . lod 1 . swap . lod 2 . str 2 4 . str 1 12) xt8088 `shouldBe` 3
    it "División de 12 por 4 deja en 0 el acumulador B." $
      (acumulador_B . divide . lod 1 . swap . lod 2 . str 2 4 . str 1 12) xt8088 `shouldBe` 0
    it "Se ejecuta un lod 2 en xt8088 con una memoria de 1024 posiciones con valor 0. Su acumulador A debería ser 0." $
      (acumulador_A . lod 2. nuevaMemoria (take 1024 [0, 0..])) xt8088 `shouldBe` 0

-- 2da Parte

nuevoPrograma unPrograma micro = micro {programa = unPrograma}

cargar :: Programa -> MicroControlador -> MicroControlador
cargar unPrograma = nuevoPrograma unPrograma

ejecutar :: MicroControlador -> Instrucción -> MicroControlador
ejecutar enMicro laInstrucción = (nop . laInstrucción) enMicro

ejecutarPrograma :: MicroControlador -> MicroControlador
ejecutarPrograma unMicro = foldl ejecutar unMicro (programa unMicro)

pruebasConProgramas = hspec $ do
  describe "Tests Punto 2.2 - Tests de programas Sumar y División." $ do
    it "Cargar y ejecutar el programa sumar10Y22 a xt8088 genera que su acumulador A sea 32." $
      (acumulador_A . ejecutarPrograma . cargar sumar10Y22) xt8088 `shouldBe` 32
    it "Cargar y ejecutar el programa sumar10Y22 a xt8088 genera que su acumulador B sea 32." $
      (acumulador_B . ejecutarPrograma . cargar sumar10Y22) xt8088 `shouldBe` 0
    it "Cargar y ejecutar el programa sumar10Y22 a xt8088 genera que su program counter sea 32." $
      (programCounter . ejecutarPrograma . cargar sumar10Y22) xt8088 `shouldBe` 4
    it "Cargar y ejecutar el programa divisiónDe2Por0 a xt8088 genera que su acumulador A sea 2." $
      (acumulador_A . ejecutarPrograma . cargar divisiónDe2Por0) xt8088 `shouldBe` 2
    it "Cargar y ejecutar el programa divisiónDe2Por0 a xt8088 genera que su acumulador B sea 0." $
      (acumulador_B . ejecutarPrograma . cargar divisiónDe2Por0) xt8088 `shouldBe` 0
    it "Cargar y ejecutar el programa divisiónDe2Por0 a xt8088 genera que su mensaje de error sea DIVISION BY ZERO." $
      (mensajeError . ejecutarPrograma . cargar divisiónDe2Por0) xt8088 `shouldBe` "DIVISION BY ZERO"
    it "Cargar y ejecutar el programa divisiónDe2Por0 a xt8088 genera que su program counter sea 6." $
      (programCounter . ejecutarPrograma . cargar divisiónDe2Por0) xt8088 `shouldBe` 6
    it "Cargar y ejecutar el programa divisiónDe2Por0 a xt8088 deja la memoria con valores." $
      (memoria . ejecutarPrograma . cargar divisiónDe2Por0) xt8088 `shouldBe` [2,0]

ifnz :: Programa -> MicroControlador -> MicroControlador
ifnz [] micro = micro
ifnz (unaInstrucción : otraInstrucción) micro
  | (/=0) (acumulador_A micro) = ifnz otraInstrucción (ejecutar micro unaInstrucción)
  | otherwise = ifnz otraInstrucción micro

pruebasConIfnz = hspec $ do
  describe "Tests Punto 2.3 - Tests de IFNZ." $ do
    it "Ejecutar IFNZ en las instrucciones LODV 3 y SWAP sobre fp20 genera que su acumulador A sea 24." $
      (acumulador_A . ifnz [lodv 3, swap]) fp20 `shouldBe` 24
    it "Ejecutar IFNZ en las instrucciones LODV 3 y SWAP sobre fp20 genera que su acumulador B sea 3." $
      (acumulador_B . ifnz [lodv 3, swap]) fp20 `shouldBe` 3
    it "Ejecutar IFNZ en las instrucciones LODV 3 y SWAP sobre xt8088 genera que su acumulador A sea 0." $
      (acumulador_A . ifnz [lodv 3, swap]) xt8088 `shouldBe` 0
    it "Ejecutar IFNZ en las instrucciones LODV 3 y SWAP sobre xt8088 genera que su acumulador B sea 0." $
      (acumulador_B . ifnz [lodv 3, swap]) xt8088 `shouldBe` 0

esInnecesariaPara micro instrucción =
  ((==0) . acumulador_A) (ejecutar micro instrucción) &&
  ((==0) . acumulador_B) (ejecutar micro instrucción) &&
  ((==0) . sum . memoria) (ejecutar micro instrucción)

programaADepurar = [swap, nop, lodv 133, lodv 0, str 1 3, str 2 0]

depurar :: Programa -> MicroControlador -> Programa
depurar unPrograma micro = filter (not . esInnecesariaPara micro) unPrograma

pruebasConDepurar = hspec $ do
  describe "Tests Punto 2.4 - Depuración de un Programa." $ do
    it "Depuración de un programa - Deben quedar sólo las intrucciones que modifican el estado interno del Micro Controlador." $
      (length . depurar programaADepurar) xt8088 `shouldBe` 2
    it "Depuración de un programa - Chequeo primera instrucción." $
      ( acumulador_A . ejecutar xt8088 . head . depurar programaADepurar) xt8088 `shouldBe` 133
    it "Depuración de un programa - Chequeo segunda instrucción." $
      ( obtenerElemento 1 . memoria . ejecutar xt8088 . flip (!!) 1 . depurar programaADepurar) xt8088 `shouldBe` 3

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
microInfinito = MicroControlador [0..] 0 0 0 "" []

pruebasConLaMemoria = hspec $ do
  describe "Tests Punto 2.5 - Orden de la Memoria." $ do
    it "La memoria de at8086 está ordenada." $
      memoriaOrdenadaSegún menorAMayor at8086 `shouldBe` True
    it "La memoria de microDesorden no está ordenada." $
      memoriaOrdenadaSegún menorAMayor microDesorden `shouldBe` False
    it "Ejecutar el programa sumar10Y22 en el microInfinito genera que su acumulador A sea 32." $
      (acumulador_A . ejecutarPrograma . cargar sumar10Y22) microInfinito `shouldBe` 32
    it "Ejecutar el programa sumar10Y22 en el microInfinito genera que su acumulador B sea 0." $
      (acumulador_B . ejecutarPrograma . cargar sumar10Y22) microInfinito `shouldBe` 0

ejecutarTests = do
  pruebasConInstrucciones1
  pruebasConInstrucciones2
  pruebasConProgramas
  pruebasConIfnz
  pruebasConDepurar
  pruebasConLaMemoria
