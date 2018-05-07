{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Show.Functions
import Data.List
import Data.Maybe
import Test.Hspec

data MicroControlador = MicroControlador {
  memoria :: [Int],
  acumulador_A :: Int,
  acumulador_B :: Int,
  programCounter :: Int,
  mensajeError :: String
} deriving(Show,Eq)

nuevaMemoria otraMemoria microControlador = microControlador {memoria = otraMemoria}
nuevoAcum_A otroAcum_A microControlador = microControlador {acumulador_A = otroAcum_A}
nuevoAcum_B otroAcum_B microControlador = microControlador {acumulador_B = otroAcum_B}
nuevoProgramCounter otroPC microControlador = microControlador {programCounter = otroPC}
nuevaEtiqueta otraEtiqueta microControlador = microControlador {mensajeError = otraEtiqueta}

xt8088 = MicroControlador [] 0 0 0 ""

type Instrucción = MicroControlador -> MicroControlador

nop :: Instrucción
nop micro = nuevoProgramCounter (programCounter micro + 1) micro

lodv :: Int -> Instrucción
lodv unValor micro = (nop . nuevoAcum_A unValor) micro

swap :: Instrucción
swap micro = (nop . nuevoAcum_A (acumulador_B micro) . nuevoAcum_B (acumulador_A micro)) micro

add :: Instrucción
add micro = (nop . nuevoAcum_B 0 . nuevoAcum_A (acumulador_A micro + acumulador_B micro)) micro

type Posición = Int

str :: Posición -> Int -> Instrucción
str unaPosición unValor micro = (nop . nuevaMemoria ((take (unaPosición - 1) (memoria micro)) ++ [unValor] ++ (drop unaPosición (memoria micro)))) micro

lod :: Posición -> Instrucción
lod unaPosición micro = (nop . nuevoAcum_A ((!!) (memoria micro) (unaPosición - 1))) micro

divide :: Instrucción
divide micro
  | (not . (==0) . acumulador_B) micro = (nop . nuevoAcum_B 0 . nuevoAcum_A (acumulador_A micro `div` acumulador_B micro)) micro
  | otherwise = (nop . nuevaEtiqueta "DIVISION BY ZERO") micro

fp20 = MicroControlador [] 7 24 0 ""
at8086 = MicroControlador [1.. 20] 0 0 0 ""

programa1 = nop.nop.nop
programa2 = add . lodv 22 . swap . lodv 10
programa3 = divide . lod 1 . swap . lod 2 . str 2 0 . str 1 2
programa3b = divide . lod 1 . swap . lod 2 . str 2 4 . str 1 12

testsPrimeraEntrega = hspec $ do
  describe "Probando las instrucciones." $ do
    it "Se modela un programa que haga avanzar tres posiciones el program counter." $
      (programCounter . programa1) xt8088 `shouldBe` 3
    it "Se modela un programa que permite sumar 10 + 22." $
      (acumulador_A . programa2) xt8088 `shouldBe` 32
    it "Se modela un programa que intenta dividir 2 por 0." $
      (mensajeError . programa3) xt8088 `shouldBe` "DIVISION BY ZERO"
    it "Dado un procesador fp20 que tiene acumulador A con 7 y acumulador B con 24, al ejecutar SWAP el acumulador A debe quedar con 24 y el B con 7." $
      swap fp20 `shouldBe` MicroControlador [] 24 7 1 ""
    it "Ejecutamos la instrucción STR 2 5 en at8086. Entonces el procesador at8086 debe quedar con un 5 en la posición 2: [1, 5, 3, 4, 5,... ]" $
      (flip (!!) 1 . memoria . str 2 5) at8086 `shouldBe` 5
    it "LOD 2 de un procesador xt8088 con la memoria vacía (1024 posiciones con valores cero) debe dejar con cero el acumulador A (cero = ausencia de información)" $
      (acumulador_A . lod 2 . nuevaMemoria [0,0..]) xt8088  `shouldBe` 0
    it "Ejecutar por consola la división 2 por 0 para el procesador xt8088 según el programa escrito arriba, esperamos el mensaje de error “DIVISION BY ZERO”, y un 6 en el program counter." $
      programa3 xt8088 `shouldBe` MicroControlador [2,0] 2 0 6 "DIVISION BY ZERO"
    it "Ejecutar la división de 12 por 4 para el procesador xt8088 (cambiando los valores del programa anterior), que debe dar 3 y no tirar ningún mensaje de error" $
      programa3b xt8088 `shouldBe` MicroControlador [12,4] 3 0 6 ""
