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
pop festival | (any (=="indiferente").estadoPublico) festival = (aumentarPublico (*2) .cambiarEstado "euforico") festival
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

