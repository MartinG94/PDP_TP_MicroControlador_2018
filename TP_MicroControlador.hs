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
lod unaPosición micro = (nop . nuevoAcum_A ( (!!) (memoria micro) (unaPosición - 1) )) micro

divide :: Instrucción
divide micro
  | (not . (==0) . acumulador_B) micro = (nop . nuevoAcum_B 0 . nuevoAcum_A (acumulador_A micro `div` acumulador_B micro)) micro
  | otherwise = nuevaEtiqueta "DIVISION BY ZERO" micro

fp20 = MicroControlador [] 7 24 0 ""
at8086 = MicroControlador [1.. 20] 0 0 0 ""

testsPrimeraEntrega = hspec $ do
  describe "Probando las instrucciones." $ do
    it "Se modela un programa que haga avanzar tres posiciones el program counter." $
      (programCounter.nop.nop.nop) xt8088 `shouldBe` 3
    it "Se modela un programa que permite sumar 10 + 22." $
      (acumulador_A . add . lodv 22 . swap . lodv 10) xt8088 `shouldBe` 32
    it "Se modela un programa que intenta dividir 2 por 0." $
      (mensajeError . divide . lod 1 . swap . lod 2 . str 2 0 . str 1 2) xt8088 `shouldBe` "DIVISION BY ZERO"
