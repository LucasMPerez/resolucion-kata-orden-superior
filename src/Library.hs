{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Library where
import PdePreludat

data Casa = Casa {
  habitaciones :: [Habitacion],
  garage :: Bool,
  habitantes :: [Persona]
} deriving Show

data Habitacion = Habitacion {
  alto :: Number,
  ancho :: Number,
  largo :: Number
} deriving Show

data Persona = Persona {
  nombre :: String,
  edad :: Number
} deriving Show

unaCasa = Casa {
  habitaciones = [Habitacion 3 2 3, Habitacion 2 4 1],
  garage = False,
  habitantes = [Persona "juan" 37, Persona "cecilia" 35, Persona "santi" 4, Persona "fede" 7]
}


otraCasa = Casa {
  habitaciones = [Habitacion 4 2 1, Habitacion 2 2 1, Habitacion 2 2 3],
  garage = True,
  habitantes = [Persona "fer" 49, Persona "laura" 48, Persona "chiara" 17, Persona "melina" 20]
}

terceraCasa = Casa {
  habitaciones = [Habitacion 4 5 2, Habitacion 1 1 2],
  garage = True,
  habitantes = [Persona "wilson" 71, Persona "lourdes" 25]
}

casas = [unaCasa, otraCasa, terceraCasa]

-- Saber si una casa es grande, que es cuando todas las habitaciones miden más de 4 metros cuadrados
-- unaCasa es grande, mientras que otraCasa y terceraCasa no lo son
casaGrande :: Casa -> Bool
casaGrande  = all esHabitacionGrande . habitaciones 

esHabitacionGrande :: Habitacion -> Bool 
esHabitacionGrande habitacion = ( (>4) . volumen) habitacion

volumen :: Habitacion -> Number 
-- volumen habitacion = ancho habitacion * largo habitacion * alto habitacion
volumen (Habitacion alto ancho largo) = alto * ancho * largo

-- (con fold) Queremos saber la lista de personas que viven en una casa armando una cadena
-- de caracteres contiguo
-- P. ej: si preguntamos quiénes viven en unaCasa nos tiene que responder "juanceciliasantifede"
quienesViven :: Casa -> String
quienesViven casa = (foldl (flip ((++).nombre)) ""  . habitantes ) casa

quienesViven' casa = (foldr ((++).nombre)  "".habitantes) casa

-- Saber el total en metros cuadrados de las casas que tienen habitantes mayores de 45
-- Si le pasamos las tres casas, el total es 66 = 24 + 42 de la otraCasa y terceraCasa
tamanioCasaConPersonasMayores ::  [Casa] -> Number
tamanioCasaConPersonasMayores casas = ( sumOf volumenTotal  .filter tienePersonasMayores ) casas

volumenTotal :: Casa -> Number
volumenTotal = sumOf volumen . habitaciones

tienePersonasMayores :: Casa -> Bool 
tienePersonasMayores casa = (any ((>45).edad) . habitantes) casa 