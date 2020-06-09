module Lib where
import Text.Show.Functions

laVerdad = True

data Jugador = UnJugador {
nombre :: String,
padre :: String,
habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
fuerzaJugador :: Int,
precisionJugador :: Int
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = Tiro {
velocidad :: Int,
precision :: Int,
altura :: Int
} deriving (Eq, Show)
type Puntos = Int

-- Funciones útiles
between n m x = elem x [n .. m]
{- between 5 6 7
False
*Lib Lib> between 5 8 7
True
-}

maximoSegun f = foldl1 (mayorSegun f)

mayorSegun f a b
    | f a > f b = a
    | otherwise = b

--MODELAR LOS PALOS DE GOLF (punto 1)
{- Sabemos que cada palo genera un efecto diferente, por lo tanto elegir el palo correcto puede ser la
diferencia entre ganar o perder el torneo.

a. Modelar los palos usados en el juego que a partir de una determinada habilidad generan un tiro
que se compone por velocidad, precisión y altura.
    i. El putter genera un tiro con velocidad igual a 10, el doble de la precisión recibida y altura 0.
    ii. La madera genera uno de velocidad igual a 100, altura igual a 5 y la mitad de la precisión.
    iii. Los hierros , que varían del 1 al 10 (número al que denominaremos n), generan un tiro de
    velocidad igual a la fuerza multiplicada por n, la precisión dividida por n y una altura de n-3
    (con mínimo 0). Modelarlos de la forma más genérica posible.
b. Definir una constante palos que sea una lista con todos los palos que se pueden usar en el juego. -}

--a
type Palo = Habilidad -> Tiro

putter :: Palo
putter habilidad = Tiro {velocidad = 10, precision = precisionJugador habilidad * 2, altura = 0}

madera :: Palo
madera habilidad = Tiro {velocidad = 100, precision = precisionJugador habilidad `div` 2, altura = 5}

hierro :: Int -> Palo
hierro n habilidad = Tiro {velocidad = fuerzaJugador habilidad * n , precision = precisionJugador habilidad `div` n, altura = max 0 (n-3)}


--b
palos :: [Palo]
palos = [putter, madera] ++ map hierro [1 .. 10]

--PUNTO 2
{-Definir la función golpe que dados una persona y un palo, obtiene el tiro resultante de usar ese palo con
las habilidades de la persona.
Por ejemplo si Bart usa un putter, se genera un tiro de velocidad = 10, precisión = 120 y altura = 0. -}


golpe :: Jugador -> Palo -> Tiro
golpe persona palo = (palo.habilidad) persona

{-golpe bart putter
Tiro {velocidad = 10, precision = 120, altura = 0}-}

--PUNTO 3
{-Lo que nos interesa de los distintos obstáculos es si un tiro puede superarlo, y en el caso de poder
superarlo, cómo se ve afectado dicho tiro por el obstáculo. En principio necesitamos representar los
siguientes obstáculos:
    a. Un túnel con rampita sólo es superado si la precisión es mayor a 90 yendo al ras del suelo,
        independientemente de la velocidad del tiro. Al salir del túnel la velocidad del tiro se duplica, la precision
        pasa a ser 100 y la altura 0.
    b. Una laguna es superada si la velocidad del tiro es mayor a 80 y tiene una altura de entre 1 y 5
        metros. Luego de superar una laguna el tiro llega con la misma velocidad y precisión, pero una
        altura equivalente a la altura original dividida por el largo de la laguna .
    c. Un hoyo se supera si la velocidad del tiro está entre 5 y 20 m/s yendo al ras del suelo con una
        precisión mayor a 95. Al superar el hoyo, el tiro se detiene, quedando con todos sus componentes en 0.

Se desea saber cómo queda un tiro luego de intentar superar un obstáculo, teniendo en cuenta que en
caso de no superarlo, se detiene, quedando con todos sus componentes en 0. -}



--LOGICA GENERALIZADA
superaObstaculo :: (Tiro -> Bool) -> (Tiro -> Tiro) -> Tiro -> Tiro
superaObstaculo condicion efecto tiro 
    | condicion tiro = efecto tiro
    | otherwise = tiroNoSuperado

tiroNoSuperado :: Tiro
tiroNoSuperado = Tiro 0 0 0

--REPRESENTAR OBSTACULOS
type LargoDeLaguna = Int
type Obstaculo = Tiro -> Tiro

tunerConRampita :: Obstaculo
tunerConRampita  = superaObstaculo superaTunel aplicarEfectoTunelConRampita

laguna :: LargoDeLaguna -> Obstaculo
laguna largo = superaObstaculo superaLaguna (aplicarEfectoLaguna largo)

hoyo :: Obstaculo
hoyo = superaObstaculo superaHoyo aplicarEfectoHoyo

--CONDICIONALES

superaLaguna :: Tiro -> Bool
superaLaguna tiro = velocidad tiro > 80 && (between 1 5.altura) tiro

superaTunel :: Tiro -> Bool
superaTunel tiro = precisionDelTiroEsMayor (precision tiro) 90 && vaAlRas tiro

superaHoyo :: Tiro -> Bool
superaHoyo tiro = (between 5 20.velocidad) tiro && vaAlRas tiro && precisionDelTiroEsMayor (precision tiro ) 95

precisionDelTiroEsMayor :: Int -> Int -> Bool
precisionDelTiroEsMayor numero cond = numero > cond

vaAlRas :: Tiro -> Bool
vaAlRas = (==0).altura

-- EFECTOS

aplicarEfectoTunelConRampita :: Tiro -> Tiro
aplicarEfectoTunelConRampita tiro = tiro {velocidad = velocidad tiro * 2 , precision = 100, altura = 0 }

aplicarEfectoLaguna :: LargoDeLaguna -> Tiro -> Tiro
aplicarEfectoLaguna largo tiro = tiro {altura = altura tiro `div` largo}

aplicarEfectoHoyo :: Tiro -> Tiro
aplicarEfectoHoyo  _ = tiroNoSuperado

