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
