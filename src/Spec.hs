module Spec where
import PdePreludat
import Library
import Test.Hspec

-- =====================================================================
-- FIXTURES
-- =====================================================================

-- No sufre: 0.5h, historial vacio, nombre par -> merece celeste, color celeste
meeseeksFeliz :: Meeseeks
meeseeksFeliz = Meeseeks {
    nombre            = "Mr. Meeseeks",
    color             = "celeste",
    horasDeExistencia = 0.5,
    tareaActual       = "destapar una cerveza",
    historial         = []
}

meeseeksFelizNuevo :: Meeseeks
meeseeksFelizNuevo = meeseeksFeliz { horasDeExistencia = 0 }

-- No sufre: 3h -> merece azul, color azul
meeseeksAzul :: Meeseeks
meeseeksAzul = Meeseeks {
    nombre            = "Mr. Meeseeks Azul",
    color             = "azul",
    horasDeExistencia = 3,
    tareaActual       = "abrir un frasco",
    historial         = []
}

-- Sufre: tiene "explicar matematica" en su historial
meeseeksTraumatizado :: Meeseeks
meeseeksTraumatizado = Meeseeks {
    nombre            = "Mr. Meeseeks Trauma",
    color             = "azul",
    horasDeExistencia = 3,
    tareaActual       = "jugar",
    historial         = [
        Intento { tarea = "explicar matematica", horasQueTomo = 4 },
        Intento { tarea = "otra cosa",           horasQueTomo = 2 }
    ]
}

-- Sufre por desajuste: 15h impar en rango -> merece negro, color violeta
meeseeksDesajustado :: Meeseeks
meeseeksDesajustado = Meeseeks {
    nombre            = "Mr. Meeseeks Desajustado",
    color             = "violeta",
    horasDeExistencia = 15,
    tareaActual       = "ayudar a Beth",
    historial         = [ Intento { tarea = "algo", horasQueTomo = 9 } ]
}

-- 10h par -> merece violeta
meeseeksVioleta :: Meeseeks
meeseeksVioleta = Meeseeks {
    nombre            = "Mr. Meeseeks Violeta",
    color             = "violeta",
    horasDeExistencia = 10,
    tareaActual       = "x",
    historial         = [ Intento { tarea = "tarea previa", horasQueTomo = 1 } ]
}

-- Todos sus intentos superan 10 horas
meeseeksPesado :: Meeseeks
meeseeksPesado = Meeseeks {
    nombre            = "Mr. Meeseeks Pesado",
    color             = "negro",
    horasDeExistencia = 30,
    tareaActual       = "x",
    historial         = [
        Intento { tarea = "a", horasQueTomo = 12 },
        Intento { tarea = "b", horasQueTomo = 15 }
    ]
}

-- Intentos chicos (ninguno supera 10)
meeseeksLiviano :: Meeseeks
meeseeksLiviano = Meeseeks {
    nombre            = "Mr. Meeseeks Liviano",
    color             = "negro",
    horasDeExistencia = 30,
    tareaActual       = "x",
    historial         = [
        Intento { tarea = "a", horasQueTomo = 5 },
        Intento { tarea = "b", horasQueTomo = 3 }
    ]
}

-- Fixtures con horas controladas para ordenamiento
meeseeks1h :: Meeseeks
meeseeks1h = meeseeksFeliz { nombre = "uno", horasDeExistencia = 1 }

meeseeks3h :: Meeseeks
meeseeks3h = meeseeksFeliz { nombre = "tres", horasDeExistencia = 3 }

meeseeks5h :: Meeseeks
meeseeks5h = meeseeksFeliz { nombre = "cinco", horasDeExistencia = 5 }

cajaOrdenada :: Caja
cajaOrdenada = [meeseeks1h, meeseeks3h, meeseeks5h]

cajaDesordenada :: Caja
cajaDesordenada = [meeseeks1h, meeseeks5h, meeseeks3h]

-- Criterios para cumplenEnCadena (punto 4b)
tieneMenosOIgualHoras :: Meeseeks -> Meeseeks -> Bool
tieneMenosOIgualHoras meeseeks1 meeseeks2 =
    horasDeExistencia meeseeks1 <= horasDeExistencia meeseeks2

tieneNombreMasCortoOIgual :: Meeseeks -> Meeseeks -> Bool
tieneNombreMasCortoOIgual meeseeks1 meeseeks2 =
    length (nombre meeseeks1) <= length (nombre meeseeks2)

-- Fixtures con nombres de longitud creciente (1, 2 y 3 caracteres)
meeseeksCorto :: Meeseeks
meeseeksCorto = meeseeksFeliz { nombre = "a" }

meeseeksMedio :: Meeseeks
meeseeksMedio = meeseeksFeliz { nombre = "bb" }

meeseeksLargo :: Meeseeks
meeseeksLargo = meeseeksFeliz { nombre = "ccc" }

-- =====================================================================
-- TESTS
-- =====================================================================

correrTests :: IO ()
correrTests = hspec $ do
  describe "Punto 1b - colorQueMerece" $ do
    it "celeste si lleva menos de 2 horas Y su historial esta vacio" $
      colorQueMerece meeseeksFeliz `shouldBe` "celeste"
    it "celeste tambien con 0 horas e historial vacio" $
      colorQueMerece meeseeksFelizNuevo `shouldBe` "celeste"
    it "NO es celeste si lleva menos de 2 horas pero tiene historial (cae en azul)" $
      colorQueMerece (meeseeksFeliz { horasDeExistencia = 1, historial = [Intento "x" 1] }) `shouldBe` "azul"
    it "azul para 3 horas" $
      colorQueMerece meeseeksAzul `shouldBe` "azul"
    it "azul en el borde superior de 6 horas" $
      colorQueMerece (meeseeksAzul { horasDeExistencia = 6 }) `shouldBe` "azul"
    it "violeta para 10 horas (par) en rango" $
      colorQueMerece meeseeksVioleta `shouldBe` "violeta"
    it "violeta en el borde de 20 horas (par)" $
      colorQueMerece (meeseeksVioleta { horasDeExistencia = 20 }) `shouldBe` "violeta"
    it "NEGRO para horas impares dentro del rango (15h cae en otherwise)" $
      colorQueMerece meeseeksDesajustado `shouldBe` "negro"
    it "negro para mas de 20 horas" $
      colorQueMerece meeseeksPesado `shouldBe` "negro"

  describe "Punto 1c - estaSufriendo" $ do
    it "sufre si alguna vez intento explicar matematica" $
      meeseeksTraumatizado `shouldSatisfy` estaSufriendo
    it "sufre si el color no coincide con el que merece" $
      meeseeksDesajustado `shouldSatisfy` estaSufriendo
    it "no sufre cuando color coincide y nunca explico matematica" $
      meeseeksFeliz `shouldNotSatisfy` estaSufriendo

  describe "Punto 2a - esCajaAgotadora" $ do
    it "es agotadora si existe un Meeseeks con todos sus intentos por encima del umbral" $
      [meeseeksLiviano, meeseeksPesado] `shouldSatisfy` esCajaAgotadora 10
    it "no es agotadora si ningun Meeseeks tiene todos sus intentos por encima del umbral" $
      [meeseeksLiviano] `shouldNotSatisfy` esCajaAgotadora 10
    it "un Meeseeks sin historial cumple la condicion de manera vacua" $
      [meeseeksFeliz] `shouldSatisfy` esCajaAgotadora 10
    it "caja vacia no es agotadora" $
      ([] :: Caja) `shouldNotSatisfy` esCajaAgotadora 10

  describe "Punto 2b - horasDesperdiciadasPorSufridos" $ do
    it "suma las horas de los intentos de todos los Meeseeks que sufren" $
      horasDesperdiciadasPorSufridos [meeseeksFeliz, meeseeksTraumatizado, meeseeksDesajustado]
        `shouldBe` 15
    it "no cuenta a los Meeseeks que no sufren" $
      horasDesperdiciadasPorSufridos [meeseeksFeliz] `shouldBe` 0
    it "caja vacia desperdicia 0 horas" $
      horasDesperdiciadasPorSufridos [] `shouldBe` 0

  describe "Punto 3 - felicitar" $ do
    it "agrega el prefijo Feliz al nombre" $
      (nombre . felicitar) meeseeksFeliz `shouldBe` "Feliz Mr. Meeseeks"
    it "resta 1 hora de existencia" $
      (horasDeExistencia . felicitar) meeseeksAzul `shouldBe` 2
    it "no baja de 0 horas" $
      (horasDeExistencia . felicitar) meeseeksFeliz `shouldBe` 0

  describe "Punto 3 - asustar" $ do
    it "agrega el sufijo asustado al nombre" $
      (nombre . asustar) meeseeksFeliz `shouldBe` "Mr. Meeseeks (asustado)"
    it "suma 2 horas de existencia" $
      (horasDeExistencia . asustar) meeseeksAzul `shouldBe` 5

  describe "Punto 3 - reasignar" $ do
    it "cambia la tarea actual" $
      (tareaActual . reasignar "buscar el control remoto") meeseeksFeliz
        `shouldBe` "buscar el control remoto"
    it "agrega el sufijo reasignado al nombre" $
      (nombre . reasignar "x") meeseeksFeliz `shouldBe` "Mr. Meeseeks (reasignado)"

  describe "Punto 3 - aterrar" $ do
    it "produce el mismo resultado que asustar tres veces en las horas" $
      (horasDeExistencia . aterrar) meeseeksAzul
        `shouldBe` (horasDeExistencia . asustar . asustar . asustar) meeseeksAzul
    it "agrega el sufijo asustado tres veces" $
      (nombre . aterrar) meeseeksFeliz
        `shouldBe` "Mr. Meeseeks (asustado) (asustado) (asustado)"

  describe "Punto 4 - estaOrdenadaPorSufrimiento" $ do
    it "una caja con un solo Meeseeks esta ordenada" $
      estaOrdenadaPorSufrimiento [meeseeksFeliz] `shouldBe` True
    it "esta ordenada si las horas son no decrecientes" $
      cajaOrdenada `shouldSatisfy` estaOrdenadaPorSufrimiento
    it "no esta ordenada si un Meeseeks tiene mas horas que el siguiente" $
      cajaDesordenada `shouldNotSatisfy` estaOrdenadaPorSufrimiento
    it "acepta horas iguales entre consecutivos" $
      [meeseeks3h, meeseeks3h] `shouldSatisfy` estaOrdenadaPorSufrimiento

  describe "Punto 4b - cumplenEnCadena (generalizacion)" $ do
    it "una caja con un solo Meeseeks cumple la cadena" $
      cumplenEnCadena tieneMenosOIgualHoras [meeseeksFeliz] `shouldBe` True
    it "cumple si el criterio de horas vale para todos los pares" $
      cumplenEnCadena tieneMenosOIgualHoras cajaOrdenada `shouldBe` True
    it "no cumple si algun par viola el criterio de horas" $
      cumplenEnCadena tieneMenosOIgualHoras cajaDesordenada `shouldBe` False
    it "sirve con cualquier criterio: nombres de longitud no decreciente" $
      cumplenEnCadena tieneNombreMasCortoOIgual [meeseeksCorto, meeseeksMedio, meeseeksLargo]
        `shouldBe` True
    it "el mismo criterio de nombres detecta cuando no se cumple" $
      cumplenEnCadena tieneNombreMasCortoOIgual [meeseeksLargo, meeseeksCorto]
        `shouldBe` False
    it "estaOrdenadaPorSufrimiento coincide con cumplenEnCadena del criterio de horas" $
      estaOrdenadaPorSufrimiento cajaOrdenada
        `shouldBe` cumplenEnCadena tieneMenosOIgualHoras cajaOrdenada
