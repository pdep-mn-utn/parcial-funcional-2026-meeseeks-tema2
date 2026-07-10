module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

-- =====================================================================
-- PUNTO 1: I'm Mr. Meeseeks!
-- =====================================================================

-- MODELO
data Meeseeks = Meeseeks {
    nombre            :: String,
    color             :: String,
    horasDeExistencia :: Number,
    tareaActual       :: String,
    historial         :: [Intento]
} deriving (Show, Eq)

data Intento = Intento {
    tarea        :: String,
    horasQueTomo :: Number
} deriving (Show, Eq)

type Caja = [Meeseeks]

colorQueMerece :: Meeseeks -> String
colorQueMerece meeseeks
  | horasDeExistencia meeseeks  < 2  && historialVacio meeseeks             = "celeste"
  | horasDeExistencia meeseeks <= 6                                         = "azul"
  | horasDeExistencia meeseeks <= 20 && (even . horasDeExistencia) meeseeks = "violeta"
  | otherwise                                                               = "negro"
  where historialVacio = null . historial

estaSufriendo :: Meeseeks -> Bool
estaSufriendo meeseeks =
    color meeseeks /= colorQueMerece meeseeks || (elem "explicar matematica" . map tarea . historial) meeseeks
  

-- =====================================================================
-- PUNTO 2: Look at me!
-- =====================================================================

esCajaAgotadora :: Number -> Caja -> Bool
esCajaAgotadora horas = any (all ((> horas) . horasQueTomo) . historial)

horasDesperdiciadasPorSufridos :: Caja -> Number
horasDesperdiciadasPorSufridos =
    sumOf (sumOf horasQueTomo . historial) . filter estaSufriendo

-- =====================================================================
-- PUNTO 3: Can do!
-- =====================================================================

type Interaccion = Meeseeks -> Meeseeks

-- Abstracción común: las interacciones modifican el nombre
modificarNombre :: (String -> String) -> Interaccion
modificarNombre modificacion meeseeks = meeseeks {
    nombre = ( modificacion . nombre ) meeseeks
}

felicitar :: Interaccion
felicitar meeseeks = modificarNombre ("Feliz " ++) $ meeseeks {
    horasDeExistencia = max 0 (horasDeExistencia meeseeks - 1)
}

asustar :: Interaccion
asustar meeseeks = modificarNombre (++ " (asustado)") $ meeseeks {
    horasDeExistencia = horasDeExistencia meeseeks + 2
}

reasignar :: String -> Interaccion
reasignar nuevaTarea meeseeks = modificarNombre (++ " (reasignado)") $ meeseeks {
    tareaActual = nuevaTarea
}

aterrar :: Interaccion
aterrar = asustar . asustar . asustar

-- REPL:
-- aterrar . reasignar "buscar el control remoto" . asustar . felicitar $ meeseeks

-- =====================================================================
-- PUNTO 4: Existence is pain!
-- =====================================================================

-- estaOrdenadaPorSufrimiento :: Caja -> Bool
-- estaOrdenadaPorSufrimiento []  = False
-- estaOrdenadaPorSufrimiento [_] = True
-- estaOrdenadaPorSufrimiento (meeseeks1 : meeseeks2 : resto) =
    -- horasDeExistencia meeseeks1 <= horasDeExistencia meeseeks2
    -- && estaOrdenadaPorSufrimiento (meeseeks2 : resto)

cumplenEnCadena :: (Meeseeks -> Meeseeks -> Bool) -> Caja -> Bool
cumplenEnCadena _        []                       = False
cumplenEnCadena _        [_]                      = True
cumplenEnCadena criterio (meeseeks1 : meeseeks2 : resto) =
    criterio meeseeks1 meeseeks2 && cumplenEnCadena criterio (meeseeks2 : resto)

estaOrdenadaPorSufrimiento :: Caja -> Bool
estaOrdenadaPorSufrimiento =
    cumplenEnCadena (\meeseeks1 meeseeks2 -> horasDeExistencia meeseeks1 <= horasDeExistencia meeseeks2) 