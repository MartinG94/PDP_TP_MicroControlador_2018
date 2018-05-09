{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Show.Functions
import Data.List
import Data.Maybe
import Test.Hspec

-- 1ra Parte

data MicroControlador = MicroControlador {
  memoria :: [Int],
  acumulador_A :: Int,
  acumulador_B :: Int,
  programCounter :: Int,
  mensajeError :: String,
  programas :: [Programa]
} deriving(Show)

nuevaMemoria otraMemoria micro = micro {memoria = otraMemoria}
nuevoAcum_A otroAcum_A micro = micro {acumulador_A = otroAcum_A}
nuevoAcum_B otroAcum_B micro = micro {acumulador_B = otroAcum_B}
nuevoProgramCounter otroPC micro = micro {programCounter = otroPC}
nuevaEtiqueta otraEtiqueta micro = micro {mensajeError = otraEtiqueta}

xt8088 = MicroControlador [] 0 0 0 "" []

type Instrucción = MicroControlador -> MicroControlador
type Programa = MicroControlador -> MicroControlador

nop :: Instrucción
nop micro = nuevoProgramCounter (programCounter micro + 1) micro

lodv :: Int -> Instrucción
lodv unValor micro = nuevoAcum_A unValor micro

swap :: Instrucción
swap micro = (nuevoAcum_A (acumulador_B micro) . nuevoAcum_B (acumulador_A micro)) micro

add :: Instrucción
add micro = (nuevoAcum_B 0 . nuevoAcum_A (acumulador_A micro + acumulador_B micro)) micro

type Posición = Int

str :: Posición -> Int -> Instrucción
str unaPosición unValor micro = (nuevaMemoria ((take (unaPosición - 1) (memoria micro)) ++ [unValor] ++ (drop unaPosición (memoria micro)))) micro

lod :: Posición -> Instrucción
lod unaPosición micro = (nuevoAcum_A ((!!) (memoria micro) (unaPosición - 1))) micro

divide :: Instrucción
divide micro
  | (not . (==0) . acumulador_B) micro = (nuevoAcum_B 0 . nuevoAcum_A (acumulador_A micro `div` acumulador_B micro)) micro
  | otherwise = (nuevaEtiqueta "DIVISION BY ZERO") micro

fp20 = MicroControlador [] 7 24 0 "" []
at8086 = MicroControlador [1.. 20] 0 0 0 "" []

avanzarTresPosiciones :: Programa
avanzarTresPosiciones = nop.nop.nop

sumar10Y22 :: Programa
sumar10Y22 = add . lodv 22 . swap . lodv 10

diviciónDe2Por0 :: Programa
diviciónDe2Por0 = divide . lod 1 . swap . lod 2 . str 2 0 . str 1 2

diviciónDe12Por4 :: Programa
diviciónDe12Por4 = divide . lod 1 . swap . lod 2 . str 2 4 . str 1 12

-- 2da Parte

agregarPrograma unPrograma micro = micro {programas = programas micro ++ [unPrograma]}

cargar unPrograma = agregarPrograma unPrograma
