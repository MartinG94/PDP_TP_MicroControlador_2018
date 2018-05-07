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
  etiqueta :: String
} deriving(Show,Eq)

nuevaMemoria otraMemoria microControlador = microControlador {memoria = otraMemoria}
nuevoAcum_A otroAcum_A microControlador = microControlador {acumulador_A = otroAcum_A}
nuevoAcum_B otroAcum_B microControlador = microControlador {acumulador_B = otroAcum_B}
nuevoProgramCounter otroPC microControlador = microControlador {programCounter = otroPC}
nuevaEtiqueta otraEtiqueta microControlador = microControlador {etiqueta = otraEtiqueta}

xT8088 = MicroControlador [] 0 0 0 ""

type Instrucción = MicroControlador -> MicroControlador

nop :: Instrucción
nop micro = nuevoProgramCounter (programCounter micro + 1) micro

lovd :: Int -> Instrucción
lovd unValor micro = (nop . nuevoAcum_A unValor) micro

swap :: Instrucción
swap micro = (nop . nuevoAcum_A (acumulador_B micro) . nuevoAcum_B (acumulador_A micro)) micro

add :: Instrucción
add micro = (nop . nuevoAcum_B 0 . nuevoAcum_A (acumulador_A micro + acumulador_B micro)) micro
