module Library where
import PdePreludat
import Text.Read.Lex (numberToInteger, numberToFixed)



--------------------------- PUNTO 1 ---------------------------

type Gema = Personaje -> Personaje

data Guantelete = Guantelete {
    material :: String,
    gemas :: [Gema]
} deriving (Show)

data Personaje = Personaje {
    nombre :: String,
    edad :: Number,
    energia :: Number,
    habilidades :: [String],
    planeta :: String
} deriving (Show)

data Universo = Universo {
    personajes :: [Personaje]
} deriving (Show)


--------------------------- PRUEBAS ---------------------------


personajeLoco = Personaje {
    nombre = "Roberto",
    edad = 30,
    energia = 120,
    habilidades = ["Dormir", "Caminar", "Comer"],
    planeta = "Tierra"
}


universoLoco = Universo {
    personajes = [personajeLoco, personajeLoco, personajeLoco, personajeLoco]
}


--------------------------- AUXILIARES ---------------------------

cambiarEnergia :: Number -> Personaje -> Personaje
cambiarEnergia valor personaje = personaje {
    energia = valor
}

aumentarEnergia :: Number -> Personaje -> Personaje
aumentarEnergia valor personaje = personaje {
    energia = energia personaje + valor
}

cambiarHabilidades :: [String] -> Personaje -> Personaje
cambiarHabilidades valor personaje = personaje {
    habilidades = valor
}

cambiarPlaneta :: String -> Personaje -> Personaje
cambiarPlaneta valor personaje = personaje {
    planeta = valor
}

cambiarEdad :: Number -> Personaje -> Personaje
cambiarEdad valor personaje = personaje {
    edad = valor
}

-- | Retorna True si la energia del personaje luego de la primera gema es mayor a la energia del personaje luego de la segunda gema, sino False
esGemaMasPoderosa :: Gema -> Gema -> Personaje -> Bool
esGemaMasPoderosa primeraGema segundaGema personaje = (energia . primeraGema) personaje > (energia . segundaGema) personaje




--------------------------- PUNTO 2 ---------------------------


chasquido :: Universo -> Universo
chasquido universo = universo {
    personajes = take (((/ 2) . length . personajes) universo) (personajes universo)
}


esAptoParaPendex :: Universo -> Bool
esAptoParaPendex universo = any ((< 45) . edad) (personajes universo)

energiaTotal :: Universo -> Number
energiaTotal universo = sumOf energia (filter ((> 1) . length . habilidades) (personajes universo))
-- energiaTotal universo = foldr ((+) . energia) (0) (filter ((> 1) . length . habilidades) (personajes universo))



--------------------------- PUNTO 3 ---------------------------


laMente :: Number -> Gema
laMente valor = aumentarEnergia (-valor)

elAlma :: String -> Gema
elAlma habilidadAEliminar personaje = (aumentarEnergia (-10) . cambiarHabilidades (filter (habilidadAEliminar /=) (habilidades personaje))) personaje


elEspacio :: String -> Gema
elEspacio planetaATransportar = aumentarEnergia (-20) . cambiarPlaneta planetaATransportar

elPoder :: Gema
elPoder personaje = (cambiarEnergia 0 . cambiarHabilidades (if ((<= 2) . length . habilidades) personaje then [] else habilidades personaje)) personaje

elTiempo :: Gema
elTiempo personaje = (aumentarEnergia (-50) . cambiarEdad (max (edad personaje / 2) 18)) personaje

laGemaLoca :: Gema -> Gema
laGemaLoca gema = gema . gema



--------------------------- PUNTO 4 ---------------------------

guanteleteLoco = Guantelete {
    material = "Goma",
    gemas = [elTiempo, elAlma "usar Mjolnir", laGemaLoca (elAlma "programación en Haskell")]
}



--------------------------- PUNTO 5 ---------------------------

-- Utiliza las gemas de la última a la primera. En el enunciado no especificaba si debía ser de la primera a la última
-- o de la última a la primera, así que lo decidí según mi conveniencia.
utilizar :: Personaje -> [Gema] -> Personaje
utilizar = foldr ($)
-- utilizar = foldl (\personaje gema -> gema personaje)


-- El efecto de lado se produce a partir de la última gema hacia la primera. Las gemas se van aplicando sobre el mismo personaje.
-- Por ejemplo, si la lista de gemas contiene 2 gemas, al personaje primero le afectaría la segunda gema y luego la primera.



--------------------------- PUNTO 6 ---------------------------


gemaMasPoderosaRecursivo :: [Gema] -> Gema -> Personaje -> Gema
gemaMasPoderosaRecursivo [] gemaMasPoderosa _ = gemaMasPoderosa
gemaMasPoderosaRecursivo (gema:gemas) gemaMasPoderosa personaje = if esGemaMasPoderosa gema gemaMasPoderosa personaje then gemaMasPoderosaRecursivo gemas gemaMasPoderosa personaje else gemaMasPoderosaRecursivo gemas gema personaje

-- Si el guantelete no tiene gemas, se rompe la función. Esto tiene sentido, ya que una Gema es una función, y no se puede representar una función nula (a priori)
gemaMasPoderosa :: Guantelete -> Personaje -> Gema
gemaMasPoderosa guantelete = (gemaMasPoderosaRecursivo . gemas) guantelete ((head . gemas) guantelete)




--------------------------- PUNTO 7 ---------------------------


infinitasGemas :: Gema -> [Gema]
infinitasGemas gema = gema:(infinitasGemas gema)

guanteleteDeLocos :: Guantelete
guanteleteDeLocos = Guantelete "vesconite" (infinitasGemas elTiempo)

usoLasTresPrimerasGemas :: Guantelete -> Personaje -> Personaje
usoLasTresPrimerasGemas guantelete personaje = (utilizar personaje . take 3 . gemas) guantelete



---------- gemaMasPoderosa punisher guanteleteDeLocos ----------

---- Según el enunciado:
-- > gemaMasPoderosa punisher guanteleteDeLocos
---- Adaptación a mi código:
-- > gemaMasPoderosa guanteleteDeLocos personajeLoco

-- No se puede ejecutar, ya que no es necesario que se evalúe aún, porque le faltan parámetros para poder ejecutarse.
-- Más específicamente, le falta un personaje. Lo que retornaría gemaMasPoderosa es una función Gema, pero, ¿A quién se le aplica
-- esa función? Como no hay ningún personaje a quien aplicarla, simplemente es <una función> a secas.

------- En cambio, si intentamos ejecutar lo siguiente:
---- Según el enunciado:
-- > gemaMasPoderosa punisher guanteleteDeLocos punisher
---- Adaptación a mi código:
-- > gemaMasPoderosa guanteleteDeLocos personajeLoco personajeLoco

-- Ahora sí ya están todos los parámetros para que la función pueda resolverse.
-- En este caso, se puede ejecutar, pero nunca va a terminar la ejecución (nunca va a retornar un valor),
-- ya que, como es una lista infinita, se quedaría infinitamente buscando cuál es la gema más poderosa.
-- En este caso no aplica lazy evaluation, ya que nunca se sabe si la siguiente gema en la lista es más poderosa
-- que la anterior, por lo que hay que recorrer toda la lista, y no se puede recorrer completa una lista infinita.



---------- usoLasTresPrimerasGemas guanteleteDeLocos punisher ----------

------ Según el enunciado: > usoLasTresPrimerasGemas guanteleteDeLocos punisher
------ Adaptación a mi código: > usoLasTresPrimerasGemas guanteleteDeLocos personajeLoco
-- Se puede ejecutar, y siempre va a terminar la ejecución retornando un valor, ya que solamente está tomando las primeras 3 gemas,
-- no importa si las toma de una lista infinita o finita. Esa lista de 3 gemas pasa por la función utilizar, ejecuta la función
-- utilizar y retorna el valor.