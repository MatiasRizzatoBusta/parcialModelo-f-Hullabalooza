module Lib where
import Text.Show.Functions
laVerdad = True
-------------------------- Punto 1 -------------------------- 
type Estado = String
data Festival = UnFestival{
    lugar :: String,
    publico :: Float,
    estadoPublico :: [Estado],
    bandas :: [Banda]
}deriving (Show)

data Banda = UnaBanda{
    nombre :: String,
    descripciones :: [String],
    decibeles :: Int,
    genero :: Genero
}deriving (Show)

aumentarPublico :: (Float->Float)->Festival->Festival
aumentarPublico aumento festival = festival{publico=aumento (publico festival) }

cambiarEstado :: String->Festival->Festival
cambiarEstado nuevoEstado festival = festival{estadoPublico=[nuevoEstado]}

agregoEstadoAlPublico :: String->Festival->Festival
agregoEstadoAlPublico nuevoEstado festival = festival{estadoPublico=(estadoPublico festival) ++[nuevoEstado]}
-------------------------- Bandas -------------------------- 

tocar :: Banda->Festival->Festival
tocar banda  = (genero banda)

------------------------------------------------------------- Punto 2 -------------------------------------------------------------
------------------------------------------------------------- Generos -------------------------------------------------------------
type Genero = Festival->Festival
rockNacional :: Genero
rockNacional = aumentarPublico (+100)

pop :: Genero
pop festival |(any (=="indiferente").estadoPublico) festival = (aumentarPublico (*2) .cambiarEstado "euforico") festival
             |otherwise = festival

heavyMetal :: Genero
heavyMetal = aumentarPublico (*1.1).agregoEstadoAlPublico "Pesado"

trashMetal :: Genero
trashMetal = aumentarPublico (*1.1).agregoEstadoAlPublico "basura"

nuMetal :: Genero
nuMetal = aumentarPublico (*1.1).agregoEstadoAlPublico "manija"

deathMetal :: Genero
deathMetal = aumentarPublico (*1.1).agregoEstadoAlPublico "muerto"
------------------------------------------------------------- Bandas --------------------------------------------------------------
losRedondos = UnaBanda "los Redondos" ["legendaria","pogosa"] 45 rockNacional
soda = UnaBanda "soda" ["irrepetible"] 40 rockNacional
miranda = UnaBanda "miranda" ["insipida","incolora","inodora"] 60 pop
metallica = UnaBanda "metallica" ["legendaria","vendida"] 60 heavyMetal
megadeath = UnaBanda "megadeath" ["legendaria","pogosa"] 60 trashMetal
-------------------------------------------------------------Festivales ------------------------------------------------------------
festivalTest = UnFestival "Japon" 3000 ["indiferente"] [metallica,soda,miranda]

------------------------------------------------------------- Punto 3 --------------------------------------------------------------
theStrokes = UnaBanda "the Strokes" ["suicidio asistido","emocional","linda"] 45 (pop.heavyMetal)--lo pongo asi pq es mezcla de ambas

------------------------------------------------------------- Punto 4 --------------------------------------------------------------
suceder :: Festival->Festival
suceder festival = unaBandaToca (bandas festival) festival

unaBandaToca :: [Banda]->Festival->Festival
unaBandaToca bandas festival = foldl (flip tocar) festival bandas -- con flip hago que primero llegue el festival y dep la banda

------------------------------------------------------------- Punto 5 --------------------------------------------------------------
esVendida :: [String]->Bool--hago que solo trabaje con la descripcion asi no recibe datos inecesarios
esVendida descripciones = ((>=3).length) descripciones || elem ("vendida") descripciones

esAcustica :: Int->Bool
esAcustica = (>55)

esLegendaria :: Banda->Bool
esLegendaria banda = (elem "legendaria".descripciones) banda && ((>40).decibeles) banda

------------------------------------------------------------- Punto 6 --------------------------------------------------------------
popularidad :: Banda->Int
popularidad = (*100).length.descripciones

------------------------------------------------------------- Punto 7 --------------------------------------------------------------
buenFest :: Festival->Bool
buenFest festival = ((>1000).sum.map popularidad.bandas) festival && estaOrdenadoPorPopularidad (bandas festival)

estaOrdenadoPorPopularidad :: [Banda]->Bool
estaOrdenadoPorPopularidad [] = True
estaOrdenadoPorPopularidad (x:xs)= popularidad x >= (popularidad.head) xs  &&  estaOrdenadoPorPopularidad xs

------------------------------------------------------------- Punto 8 --------------------------------------------------------------
{-
Los conceptos de aplicacion parcial y composicion fueron utiles en la resolucion del parcial debido a que me permitieron en ciertos
casos pasar parametros de la forma point-free,evitando que .
El concepto de composicion fue utilizado en todos los puntos del parcial menos el 4.
El concepto de aplicacion parcial se utilizo tambien en todos los puntos del parcial salvo el 4.

------------------------------------------------------------- Punto 9 --------------------------------------------------------------
las funciones del punto 5 que trabajan con las descripciones pueden trabajar con listas infinitas ya que no necesitan evaluar todas
las descripciones que tenga la banda.Cuando encuentren la palabra que necesitan dentro de la lista infinita van a cortar la busqueda
-}