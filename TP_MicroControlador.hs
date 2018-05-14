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
  programa :: Programa
} deriving(Show)

nuevaMemoria otraMemoria micro = micro {memoria = otraMemoria}
nuevoAcum_A otroAcum_A micro = micro {acumulador_A = otroAcum_A}
nuevoAcum_B otroAcum_B micro = micro {acumulador_B = otroAcum_B}
nuevoProgramCounter otroPC micro = micro {programCounter = otroPC}
nuevaEtiqueta otraEtiqueta micro = micro {mensajeError = otraEtiqueta}

xt8088 = MicroControlador [] 0 0 0 "" []

type Instrucción = MicroControlador -> MicroControlador
type Programa = [Instrucción]

nop :: Instrucción
nop micro = nuevoProgramCounter (programCounter micro + 1) micro

lodv :: Int -> Instrucción
lodv unValor micro = nuevoAcum_A unValor micro

swap :: Instrucción
swap micro = (nuevoAcum_A (acumulador_B micro) . nuevoAcum_B (acumulador_A micro)) micro

add :: Instrucción
add micro = (nuevoAcum_B 0 . nuevoAcum_A (acumulador_A micro + acumulador_B micro)) micro

type Posición = Int
type Valor = Int

agregar :: [Int] -> Posición -> Valor -> [Int]
agregar lista posición unValor
  | (posición - 1) >= 0 = (take (posición -1) lista) ++ [unValor] ++ (drop posición lista)
  | otherwise = lista

str :: Posición -> Int -> Instrucción
str unaPosición unValor micro = nuevaMemoria (agregar (memoria micro) unaPosición unValor) micro

obtenerElemento :: Posición -> [Int] -> Int
obtenerElemento posición lista
  | (>= 1) posición && (>=(length lista)) posición = (flip (!!)) (posición - 1) lista
  | otherwise = error "No existe la posición de memoria solicitada"

lod :: Posición -> Instrucción
lod unaPosición micro = nuevoAcum_A (obtenerElemento unaPosición (memoria micro)) micro

divide :: Instrucción
divide micro
  | (not . (==0) . acumulador_B) micro = (nuevoAcum_B 0 . nuevoAcum_A (acumulador_A micro `div` acumulador_B micro)) micro
  | otherwise = (nuevaEtiqueta "DIVISION BY ZERO") micro

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

-- 2da Parte

nuevoPrograma unPrograma micro = micro {programa = unPrograma}

cargar :: Programa -> MicroControlador -> MicroControlador
cargar unPrograma = nuevoPrograma unPrograma

ejecutar :: MicroControlador -> Instrucción -> MicroControlador
ejecutar enMicro laInstrucción = (nop . laInstrucción) enMicro

ejecutarPrograma :: MicroControlador -> MicroControlador
ejecutarPrograma unMicro = foldl ejecutar unMicro (programa unMicro)

ifnz :: Programa -> MicroControlador -> MicroControlador
ifnz [] micro = micro
ifnz (unaInstrucción : otraInstrucción) micro
  | (/=0) (acumulador_A micro) = ifnz otraInstrucción (ejecutar micro unaInstrucción)
  | otherwise = ifnz otraInstrucción micro

esInnecesariaPara micro instrucción =
  ((==0) . acumulador_A) (ejecutar micro instrucción) &&
  ((==0) . acumulador_B) (ejecutar micro instrucción) &&
  ((==0) . sum . memoria) (ejecutar micro instrucción)

pruebaDepurar = [swap, nop, lodv 133, lodv 0, str 1 3, str 2 0]

depurar :: Programa -> MicroControlador -> Programa
depurar unPrograma micro = filter (not . esInnecesariaPara micro) unPrograma

listaOrdenada [] = True
listaOrdenada [x] = True
listaOrdenada (cab1 : cab2 : cola) = cab1 <= cab2 && listaOrdenada (cab2:cola)

laMemoriaEstáOrdenada :: MicroControlador -> Bool
laMemoriaEstáOrdenada micro = listaOrdenada (memoria micro)
